(in-package :laiko)

(defconstant +default-stack-size+ #x10000 "Default VM stack size")
(defconstant +default-pvar-size+ #x100 "Default PVAR size")
(defconstant +storage-size+ #x4000000 "Default storage size in bytes (64MB)")

(defun get-command-line-arguments ()
  "Get command-line arguments"
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl nil)

(defun getenv (var)
  "Get environment variable"
  #+sbcl (sb-ext:posix-getenv var)
  #-sbcl nil)

(defun main (&optional args)
  "Main entry point for Laiko Lisp emulator"
  (let ((args (or args (get-command-line-arguments))))
    (cond
      ((or (member "-info" args :test #'string-equal)
           (member "-INFO" args :test #'string-equal))
       (print-info))
      ((or (member "-help" args :test #'string-equal)
           (member "--help" args :test #'string-equal))
       (print-help))
      ((>= (length args) 1)
       (let ((sysout-path (first args)))
         (run-emulator sysout-path (rest args))))
      (t
       (format t "Laiko Common Lisp Emulator~%")
       (format t "Usage: laiko <sysout-file> [options]~%")
       (format t "Use -help for options~%")))))

(defun quit (code)
  #+sbcl (sb-ext:exit :code code)
  #-sbcl nil)

(defun print-info ()
  (format t "Laiko Lisp Emulator (Common Lisp)~%")
  (format t "Version 0.1.0~%")
  ;; Initialize handlers to get accurate count
  (laiko.vm:initialize-byte-opcode-map)
  (laiko.vm:initialize-opcode-handlers)
  (format t "Supports: ~D opcode handlers~%" (hash-table-count laiko.vm:*opcode-handlers*))
  (quit 0))

(defun print-help ()
  (format t "Options:~%")
  (format t "  <sysout-file>    Load and execute sysout file~%")
  (format t "  -info            Print system info~%")
  (format t "  -help            Print this message~%")
  (format t "  -trace <file>    Enable tracing to specified file~%")
  (format t "  -max-steps <N>   Limit execution to N steps (for debugging)~%")
  (quit 0))

(defun parse-integer-safe (str default)
  "Parse integer from string, returning default on failure"
  (handler-case
      (parse-integer str :junk-allowed t)
    (error () default)))

(defun parse-run-args (args)
  "Parse ARGS for run-emulator, returning (values trace-file max-steps)."
  (let ((trace-file nil)
        (max-steps 0)
        (remaining args))
    (loop while remaining
          for arg = (first remaining)
          do (cond
               ((string-equal arg "-trace")
                (when (second remaining)
                  (setf trace-file (second remaining))
                  (setf remaining (rest remaining))))
               ((string-equal arg "-max-steps")
                (when (second remaining)
                  (setf max-steps (parse-integer-safe (second remaining) 0))
                  (setf remaining (rest remaining)))))
             (setf remaining (rest remaining)))
    (values trace-file max-steps)))

(defun load-sysout-or-exit (sysout-path)
  "Load sysout at SYSOUT-PATH or exit with error code.
Returns (values ifpage fptovp virtual-memory) on success."
  (let ((ifpage nil)
        (fptovp nil)
        (virtual-memory nil))
    (handler-case
        (multiple-value-setq (ifpage fptovp virtual-memory)
          (laiko.data:load-sysout sysout-path))
      (laiko.utils:sysout-load-failed (e)
        (format t "Error loading sysout: ~A~%"
                (laiko.utils:sysout-load-failed-message e))
        (quit 1)))
    (format t "Sysout loaded successfully~%")
    (format t "  Process size: ~D MB~%" (laiko.data:ifpage-process-size ifpage))
    (format t "  Stack base: 0x~X~%" (laiko.data:ifpage-stackbase ifpage))
    (values ifpage fptovp virtual-memory)))

(defun ensure-runtime-pages (virtual-memory)
  "Allocate runtime Valspace pages in VIRTUAL-MEMORY if they are missing.

This mirrors the C/Zig behavior where Valspace/Defspace live in virtual
memory but may not be present in the sysout image."
  (let ((vals-page-start (ash #x180000 -9))  ; VP 3072
        (defs-page-start (ash #x140000 -9))  ; VP 2560
        (vals-pages #x200)                     ; 0x20000 bytes / #x200 = #x200 pages
        (defs-pages #x200))                    ; ditto for Defspace (currently ignored)
    (declare (ignore defs-page-start defs-pages))
    (when (< vals-page-start (length virtual-memory))
      (let ((vals-page (aref virtual-memory vals-page-start)))
        (unless vals-page
          (format t "  Allocating Valspace pages ~D-~D~%"
                  vals-page-start (+ vals-page-start vals-pages -1))
          (loop for vp from vals-page-start below (+ vals-page-start vals-pages)
                when (< vp (length virtual-memory))
                  do (setf (aref virtual-memory vp)
                           (make-array #x200 :element-type '(unsigned-byte 8)
                                             :initial-element 0))))))))

(defun initialize-vm-from-ifpage (vm ifpage virtual-memory)
  "Initialize VM PC, stack pointer, and TOS from IFPAGE and VIRTUAL-MEMORY.

CRITICAL FIX: Match C implementation exactly (maiko/src/main.c & initsout.c):
- FP = currentfxp (relative to stackspace)
- SP = stackbase (absolute LispPTR) + 2 DLwords
- TOS = *stackbase"
  (let ((currentfxp (laiko.data:ifpage-currentfxp ifpage))
        (stackbase (laiko.data:ifpage-stackbase ifpage)))
    (format t "IFPAGE currentfxp: 0x~X (DLword offset)~%" currentfxp)
    ;; Read the Frame Extension structure from virtual memory at currentfxp offset
    ;; currentfxp is a DLword offset from Stackspace (NOT from Lisp_world!)
    (let ((fx (laiko.data:read-fx-from-vm virtual-memory currentfxp)))
      (format t "Initial FX: fnheader=0x~X, pc=~D, nextblock=0x~X~%"
              (laiko.data:fx-fnheader fx)
              (laiko.data:fx-pc fx)
              (laiko.data:fx-nextblock fx))
      ;; PC = FuncObj + CURRENTFX->pc
      (let* ((fnheader-dlword (laiko.data:fx-fnheader fx))
             (func-obj-byte-offset (* fnheader-dlword 2))
             (pc-offset (laiko.data:fx-pc fx))
             (initial-pc (+ func-obj-byte-offset pc-offset)))
        (format t "  fnheader DLword offset: 0x~X~%" fnheader-dlword)
        (format t "  FuncObj byte offset: 0x~X (fnheader * 2)~%"
                func-obj-byte-offset)
        (format t "  FX->pc: ~D (byte offset)~%" pc-offset)
        (format t "  Initial PC: 0x~X (FuncObj + pc)~%" initial-pc)
        (setf (laiko.vm:vm-pc vm) initial-pc))
      ;; Initialize FP (Frame Pointer) from currentfxp + stackspace offset
      ;; Per C: PVar = NativeAligned2FromStackOffset(InterfacePage->currentfxp)
      ;; currentfxp is relative to stackspace.
      (let ((fp-byte-offset (+ laiko.data:+stackspace-byte-offset+ (* currentfxp 2))))
        (setf (laiko.vm:vm-frame-pointer-offset vm) fp-byte-offset)
        (format t "  Frame pointer (FP): 0x~X (stackspace+currentfxp*2 = 0x~X + 0x~X * 2)"
                fp-byte-offset laiko.data:+stackspace-byte-offset+ currentfxp))
      ;; Initialize SP (Stack Pointer)
      ;; Maiko logic:
      ;; 1. If ifpage->stackbase is valid (non-zero), use it.
      ;; 2. Else, derive from currentfxp->nextblock.
      ;;
      ;; NOTE: starter.sysout has stackbase=#x1800. On Little-Endian Maiko, this is
      ;; word-swapped to 0 (and faulthi becomes #x18). So Maiko sees stackbase=0.
      ;; We must mimic this and ignore #x1800.
      (let ((use-stackbase (and (not (zerop stackbase)) (/= stackbase #x1800))))
        (if use-stackbase
            (let* ((stackbase-bytes (* stackbase 2)) ;; stackbase is absolute LispPTR
                   (sp-final (+ stackbase-bytes 4)))
              (setf (laiko.vm:vm-stack-ptr-offset vm) sp-final)
              (format t "  Stack pointer (SP) from stackbase: 0x~X~%" sp-final)
              (setf (laiko.vm:vm-top-of-stack vm)
                    (laiko.vm:vm-read-lispptr vm stackbase-bytes)))
            ;; Fallback: Use nextblock from FX
            ;; We need to read nextblock from the FX at currentfxp
            (let* ((fx (laiko.data:read-fx-from-vm virtual-memory currentfxp))
                   (nextblock (laiko.data:fx-nextblock fx))
                   ;; Maiko SP is 0x12E8A (DLword) -> 0x25D14 (Byte).
                   ;; nextblock is 0x2E8A (DLword offset from StackSpace).
                   ;; Absolute DLword = 0x10000 + 0x2E8A = 0x12E8A.
                   ;; So SP = nextblock (absolute).
                   (nextblock-abs (+ (ash laiko.data:+stackspace-byte-offset+ -1) nextblock))
                   (sp-bytes (* nextblock-abs 2)))
              (setf (laiko.vm:vm-stack-ptr-offset vm) sp-bytes)
              (format t "  Stack pointer (SP) from nextblock: 0x~X (nextblock=0x~X)~%" 
                      sp-bytes nextblock)
              ;; When deriving from nextblock, TOS is typically 0
              (setf (laiko.vm:vm-top-of-stack vm) 0))))
        
      (setf (laiko.vm:vm-stack-base-offset vm) laiko.data:+stackspace-byte-offset+)
      (setf (laiko.vm:vm-stack-end-offset vm)
            (+ laiko.data:+stackspace-byte-offset+
               (* 2 (laiko.data:ifpage-endofstack ifpage))))
      
      ;; Initialize current frame from FX
      (let* ((fx (laiko.data:read-fx-from-vm virtual-memory currentfxp))
             (initial-frame (laiko.vm:make-stack-frame
                              :next-block (laiko.data:fx-nextblock fx)
                              :link (laiko.data:fx-alink fx)
                              :fn-header (laiko.data:fx-fnheader fx)
                              :pc-offset (laiko.data:fx-pc fx))))
          (setf (laiko.vm:vm-current-frame vm) initial-frame)
          (format t "  Initial frame: fnheader=0x~X, pc=~D, nextblock=0x~X, link=0x~X~%"
                  (laiko.data:fx-fnheader fx)
                  (laiko.data:fx-pc fx)
                  (laiko.data:fx-nextblock fx)
                  (laiko.data:fx-alink fx))))))

(defun create-and-initialize-vm (ifpage fptovp virtual-memory)
  "Create a VM and wire it up to IFPAGE, FPTOVP, and VIRTUAL-MEMORY."
  (let ((vm (laiko.vm:create-vm +default-stack-size+
                                :pvar-size +default-pvar-size+)))
    (setf (laiko.vm:vm-virtual-memory vm) virtual-memory)
    (setf (laiko.vm:vm-fptovp vm) fptovp)
    (setf (laiko.vm:vm-ifpage vm) ifpage)
    (setf (laiko.vm:vm-interrupt-state vm)
          (laiko.vm:create-interrupt-state))
    ;; Storage and GC
    (let ((storage (laiko.memory:create-storage :size +storage-size+)))
      (setf (laiko.vm:vm-storage vm) storage)
      (setf (laiko.vm:vm-gc vm) (laiko.memory:create-gc #x400)))
    ;; Runtime pages (Valspace, etc.)
    (ensure-runtime-pages virtual-memory)
    ;; Initialize PC/stack/TOS from IFPAGE/FX
    (initialize-vm-from-ifpage vm ifpage virtual-memory)
    vm))

(defun configure-step-limit (max-steps)
  "Set *max-trace-steps* based on MAX-STEPS arg and EMULATOR_MAX_STEPS."
  (let ((env-max-steps (laiko.vm:get-emulator-max-steps)))
    (when (or (> max-steps 0) (> env-max-steps 0))
      (setf laiko.vm:*max-trace-steps* (max max-steps env-max-steps))
      (format t "Step limit: ~D~%" laiko.vm:*max-trace-steps*))))

(defun configure-tracing (trace-file)
  "Open trace file if TRACE-FILE is non-nil or EMULATOR_MAX_STEPS is set.
Returns the effective trace file name (or NIL if tracing disabled)."
  (let ((env-max-steps (laiko.vm:get-emulator-max-steps)))
    (cond
      (trace-file
       (format t "Enabling trace to: ~A~%" trace-file)
       (laiko.vm:open-trace-file trace-file)
       trace-file)
      ((> env-max-steps 0)
       (let ((auto-name "lisp_emulator_execution_log.txt"))
         (format t "Auto-enabling trace to: ~A~%" auto-name)
         (laiko.vm:open-trace-file auto-name)
         auto-name))
      (t
       nil))))

(defun inspect-page-content (vm page-num offset)
  "Debug helper: Print content of a virtual memory page at specific offset"
  (let ((vmem (laiko.vm:vm-virtual-memory vm)))
    (if (and vmem (< page-num (length vmem)))
        (let ((page (aref vmem page-num)))
          (if page
              (progn
                (format t "Page ~D loaded. Value at offset ~D: " page-num offset)
                ;; Read 4 bytes at offset (Little Endian)
                (when (< (+ offset 4) (length page))
                  (format t "0x~2,'0X~2,'0X~2,'0X~2,'0X (LE) -> 0x~X~%"
                          (aref page (+ offset 3))
                          (aref page (+ offset 2))
                          (aref page (+ offset 1))
                          (aref page offset)
                          (logior (ash (aref page (+ offset 3)) 24)
                                  (ash (aref page (+ offset 2)) 16)
                                  (ash (aref page (+ offset 1)) 8)
                                  (aref page offset)))))
              (format t "Page ~D is SPARSE (NIL)~%" page-num)))
        (format t "Page ~D out of bounds~%" page-num))))

(defun run-dispatch-loop (vm)
  "Initialize opcode handlers and run the main dispatch loop for VM."
  (format t "Initializing opcode handlers...~%")
  (laiko.vm:initialize-byte-opcode-map)
  (laiko.vm:initialize-opcode-handlers)
  (format t "~D opcode handlers registered~%"
          (hash-table-count laiko.vm:*opcode-handlers*))
  
  ;; WORKAROUND: Seed known startup atoms with DEFS_OFFSET (0x140000).
  ;; Current parity traces show the first executed GVAR reads atom 14, while
  ;; earlier debugging also identified atom 522 as requiring the same bootstrap
  ;; value in some runs. Preserve both until startup initialization is modeled.
  (let ((vmem (laiko.vm:vm-virtual-memory vm)))
    (dolist (atom-index '(14 522))
      (format t "Patching Atom ~D...~%" atom-index)
      (let* ((valcell (laiko.data:get-valcell vmem atom-index))
             (page-num (ash valcell -9)))
        (unless (aref vmem page-num)
          (format t "Allocating AtomSpace Page ~D for Atom ~D~%" page-num atom-index)
          (setf (aref vmem page-num)
                (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
        (laiko.data:write-atom-value vmem atom-index #x140000)
        (format t "Atom ~D patched to 0x140000~%" atom-index))))

  ;; DEBUG: Inspect Bytecode Page
  (format t "Checking Bytecode Page 12408 (offset 304):~%")
  (inspect-page-content vm 12408 304)

  (format t "Starting execution...~%")
  ;; (handler-case
      (let* ((start-pc (laiko.vm:vm-pc vm))
             (bytecode (laiko.data:extract-bytecode-from-vm
                        (laiko.vm:vm-virtual-memory vm)
                        start-pc)))
        (if (and bytecode (arrayp bytecode))
            (progn
              (format t "Bytecode extracted: ~D bytes, start PC: 0x~X~%"
                      (length bytecode) start-pc)
              ;; Debug: check virtual memory pages
              (let ((vmem (laiko.vm:vm-virtual-memory vm)))
                (if vmem
                    (let ((loaded-pages 0))
                      (loop for i below (length vmem)
                            when (aref vmem i)
                              do (incf loaded-pages))
                      (format t "Virtual memory: ~D pages total, ~D pages loaded~%"
                              (length vmem) loaded-pages))
                    (format t "WARNING: Virtual memory is NIL~%")))
              (when (> (length bytecode) 0)
                (format t "First few bytes: ")
                (loop for i from 0 below (min 10 (length bytecode))
                      do (format t "~2,'0X " (aref bytecode i)))
                (format t "~%"))
              ;; Dispatch with base PC for trace reporting
              (laiko.vm:dispatch vm bytecode start-pc))
            (format t "Error: Could not extract bytecode from sysout (bytecode=~A)~%"
                    bytecode))))
    ;; (laiko.utils:vm-error (e)
    ;;   (format t "VM Error: ~A~%" (laiko.utils:vm-error-message e)))
    ;; (error (e)
    ;;   (format t "Execution error: ~A~%" e))))

(defun run-emulator (sysout-path args)
  "Run the emulator with given sysout file."
  (format t "Loading sysout: ~A~%" sysout-path)
  (multiple-value-bind (trace-file-arg max-steps)
      (parse-run-args args)
    (multiple-value-bind (ifpage fptovp virtual-memory)
        (load-sysout-or-exit sysout-path)
      (let* ((vm (create-and-initialize-vm ifpage fptovp virtual-memory))
             (_ (configure-step-limit max-steps))
             (trace-file (configure-tracing trace-file-arg)))
        (declare (ignore _))
        (run-dispatch-loop vm)
        (when trace-file
          (laiko.vm:trace-end))))
    (format t "Execution complete~%")
    (quit 0)))
