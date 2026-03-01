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

(defun main ()
  "Main entry point for Laiko Lisp emulator"
  (let ((args (get-command-line-arguments)))
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

CRITICAL FIX: Match C implementation exactly (maiko/src/main.c:1520-1530):
- C uses InterfacePage->currentfxp directly as the FX pointer
- C does NOT add stackspace offset - currentfxp IS the offset within stackspace
- FP = currentfxp (the frame extension pointer)
- SP = nextblock - 2 (where nextblock comes from CURRENTFX->nextblock)

The old Laiko code incorrectly:
- Used fx-nextblock to calculate SP (wrong source)
- Added stackspace-byte-offset (0x20000) which was incorrect

OLD: SP = stackspace-offset + (nextblock * 2) - 4 (WRONG)
NEW: SP = nextblock * 2 - 4 (CORRECT - no base offset)
NEW: FP = currentfxp (from IFPAGE directly)"
  (let ((currentfxp (laiko.data:ifpage-currentfxp ifpage))
        (stackbase (laiko.data:ifpage-stackbase ifpage)))
    (declare (ignore stackbase))
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
      ;; FIXED: Initialize FP (Frame Pointer) from currentfxp + stackspace offset
      ;; Per C: PVar = NativeAligned2FromStackOffset(InterfacePage->currentfxp)
      ;; stackspace-byte-offset = 0x20000 (STK_OFFSET * 2 = 0x10000 * 2)
      ;; currentfxp is DLword offset, convert to bytes: currentfxp * 2
      ;; So FP = stackspace-byte-offset + (currentfxp * 2)
      (let ((fp-byte-offset (+ laiko.data:+stackspace-byte-offset+ (* currentfxp 2))))
        (setf (laiko.vm:vm-frame-pointer-offset vm) fp-byte-offset)
        (format t "  Frame pointer (FP): 0x~X (stackspace+currentfxp*2 = 0x~X + 0x~X * 2)"
                fp-byte-offset laiko.data:+stackspace-byte-offset+ currentfxp))
      ;; FIXED: Initialize SP (Stack Pointer) from nextblock + stackspace offset
      ;; Per C: CurrentStackPTR = next68k - 2 where next68k = NativeAligned2FromStackOffset(CURRENTFX->nextblock)
      ;; stackspace-byte-offset = 0x20000 (STK_OFFSET * 2)
      ;; nextblock is a DLword offset, convert to bytes and subtract 2 DLwords (4 bytes)
      (let* ((nextblock (laiko.data:fx-nextblock fx))
             ;; SP = stackspace-byte-offset + (nextblock * 2) - 4
             (current-stack-ptr (+ laiko.data:+stackspace-byte-offset+ (- (* nextblock 2) 4))))
        (setf (laiko.vm:vm-stack-ptr-offset vm) current-stack-ptr)
        (setf (laiko.vm:vm-stack-base-offset vm) laiko.data:+stackspace-byte-offset+)
        (format t "  Stack pointer (SP): 0x~X (stackspace+nextblock*2 - 4 = 0x~X + 0x~X * 2 - 4)~%"
                current-stack-ptr laiko.data:+stackspace-byte-offset+ nextblock)
        ;; Initialize top-of-stack from the stack memory at SP
        (setf (laiko.vm:vm-top-of-stack vm)
              (laiko.vm:vm-read-lispptr vm current-stack-ptr))
        (format t "  Top-of-stack (cached): 0x~X~%"
                (laiko.vm:vm-top-of-stack vm))))))

(defun create-and-initialize-vm (ifpage fptovp virtual-memory)
  "Create a VM and wire it up to IFPAGE, FPTOVP, and VIRTUAL-MEMORY."
  (let ((vm (laiko.vm:create-vm +default-stack-size+
                                :pvar-size +default-pvar-size+)))
    (setf (laiko.vm:vm-virtual-memory vm) virtual-memory)
    (setf (laiko.vm:vm-fptovp vm) fptovp)
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

(defun run-dispatch-loop (vm)
  "Initialize opcode handlers and run the main dispatch loop for VM."
  (format t "Initializing opcode handlers...~%")
  (laiko.vm:initialize-byte-opcode-map)
  (laiko.vm:initialize-opcode-handlers)
  (format t "~D opcode handlers registered~%"
          (hash-table-count laiko.vm:*opcode-handlers*))
  (format t "Starting execution...~%")
  (handler-case
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
                    bytecode)))
    (laiko.utils:vm-error (e)
      (format t "VM Error: ~A~%" (laiko.utils:vm-error-message e)))
    (error (e)
      (format t "Execution error: ~A~%" e))))

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
