(in-package :maiko-lisp)

(defconstant +default-stack-size+ 65536 "Default VM stack size")
(defconstant +default-pvar-size+ 256 "Default PVAR size")
(defconstant +storage-size+ (* 64 1024 1024) "Default storage size in bytes (64MB)")

(defun get-command-line-arguments ()
  "Get command-line arguments"
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl nil)

(defun getenv (var)
  "Get environment variable"
  #+sbcl (sb-ext:posix-getenv var)
  #-sbcl nil)

(defun main ()
  "Main entry point for Maiko Lisp emulator"
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
       (format t "Maiko Common Lisp Emulator~%")
       (format t "Usage: maiko-lisp <sysout-file> [options]~%")
       (format t "Use -help for options~%")))))

(defun quit (code)
  #+sbcl (sb-ext:exit :code code)
  #-sbcl nil)

(defun print-info ()
  (format t "Maiko Lisp Emulator (Common Lisp)~%")
  (format t "Version 0.1.0~%")
  ;; Initialize handlers to get accurate count
  (maiko-lisp.vm:initialize-byte-opcode-map)
  (maiko-lisp.vm:initialize-opcode-handlers)
  (format t "Supports: ~D opcode handlers~%" (hash-table-count maiko-lisp.vm:*opcode-handlers*))
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

(defun run-emulator (sysout-path args)
  "Run the emulator with given sysout file"
  (format t "Loading sysout: ~A~%" sysout-path)

  ;; Parse command-line options
  (let ((trace-file nil)
        (max-steps 0))
    (loop while args
          do (cond
               ((string-equal (first args) "-trace")
                (when (second args)
                  (setf trace-file (second args))
                  (setf args (rest args))))
               ((string-equal (first args) "-max-steps")
                (when (second args)
                  (setf max-steps (parse-integer-safe (second args) 0))
                  (setf args (rest args)))))
          (setf args (rest args)))

    ;; Load sysout file
    (let ((ifpage nil)
          (fptovp nil)
          (virtual-memory nil))
      (handler-case
          (multiple-value-setq (ifpage fptovp virtual-memory)
            (maiko-lisp.data:load-sysout sysout-path))
        (maiko-lisp.utils:sysout-load-failed (e)
          (format t "Error loading sysout: ~A~%" (maiko-lisp.utils:sysout-load-failed-message e))
          (quit 1)))

      (format t "Sysout loaded successfully~%")
      (format t "  Process size: ~D MB~%" (maiko-lisp.data:ifpage-process-size ifpage))
      (format t "  Stack base: 0x~X~%" (maiko-lisp.data:ifpage-stackbase ifpage))

        ;; Create VM
        (let ((vm (maiko-lisp.vm:create-vm +default-stack-size+ :pvar-size +default-pvar-size+)))
          ;; Set up VM with sysout data
          (setf (maiko-lisp.vm:vm-virtual-memory vm) virtual-memory)
          (setf (maiko-lisp.vm:vm-fptovp vm) fptovp)
          (setf (maiko-lisp.vm:vm-interrupt-state vm) (maiko-lisp.vm:create-interrupt-state))

          ;; Create storage and GC
          (let ((storage (maiko-lisp.memory:create-storage :size +storage-size+)))
            (setf (maiko-lisp.vm:vm-storage vm) storage)
            (setf (maiko-lisp.vm:vm-gc vm) (maiko-lisp.memory:create-gc 1024)))

          ;; CRITICAL: Allocate runtime memory pages (Valspace, Defspace, etc.)
          ;; These are NOT loaded from sysout but allocated at runtime
          ;; Per maiko/src/initsout.c:269-273 and maiko/inc/lispmap.h:
          ;;   Valspace = NativeAligned2FromLAddr(VALS_OFFSET)
          ;;   VALS_OFFSET = 0xC0000 DLwords = 0x180000 bytes
          ;;   Defspace = NativeAligned2FromLAddr(DEFS_OFFSET)
          ;;   DEFS_OFFSET = 0xA0000 DLwords = 0x140000 bytes
          ;; These pages must be allocated in virtual_memory
          ;; For now, we'll check if they're already allocated (from sysout)
          ;; If not, we need to allocate them
          (let ((vals-page-start (ash #x180000 -9))  ; VP 3072
                (defs-page-start (ash #x140000 -9))  ; VP 2560
                (vals-pages 512)  ; 0x20000 bytes / 512 = 512 pages
                (defs-pages 512)) ; 0x20000 bytes / 512 = 512 pages
            (declare (ignore defs-page-start defs-pages))
            ;; Check if Valspace pages are allocated
            (when (< vals-page-start (length virtual-memory))
              (let ((vals-page (aref virtual-memory vals-page-start)))
                (unless vals-page
                  ;; Valspace not loaded from sysout - need to allocate
                  (format t "  Allocating Valspace pages ~D-~D~%"
                          vals-page-start (+ vals-page-start vals-pages -1))
                  (loop for vp from vals-page-start below (+ vals-page-start vals-pages)
                        when (< vp (length virtual-memory))
                        do (setf (aref virtual-memory vp)
                                 (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))))))

        ;; Initialize VM state from IFPAGE using FX structure
        (let ((currentfxp (maiko-lisp.data:ifpage-currentfxp ifpage))
              (stackbase (maiko-lisp.data:ifpage-stackbase ifpage)))
          (declare (ignore stackbase))
          ;; Read the Frame Extension structure from virtual memory
          ;; currentfxp is a DLword offset from Stackspace (NOT from Lisp_world!)
          ;; Per Zig vm_initialization.zig:38-46:
          ;;   STK_OFFSET = 0x00010000 (DLword offset)
          ;;   stackspace_byte_offset = STK_OFFSET * 2 = 0x20000
          ;;   frame_offset = stackspace_byte_offset + (currentfxp * 2)
          (let ((fx (maiko-lisp.data:read-fx-from-vm virtual-memory currentfxp)))
            (format t "Initial FX: fnheader=0x~X, pc=~D, nextblock=0x~X~%"
                    (maiko-lisp.data:fx-fnheader fx) (maiko-lisp.data:fx-pc fx) (maiko-lisp.data:fx-nextblock fx))
            ;; PC = FuncObj + CURRENTFX->pc
            ;; Per C FastRetCALL macro and Zig vm_initialization.zig:419-448:
            ;;   FuncObj = FX_FNHEADER * 2 (DLword to byte conversion)
            ;;   PC = FuncObj + CURRENTFX->pc (byte offset)
            (let* ((fnheader-dlword (maiko-lisp.data:fx-fnheader fx))
                   (func-obj-byte-offset (* fnheader-dlword 2))
                   (pc-offset (maiko-lisp.data:fx-pc fx))
                   (initial-pc (+ func-obj-byte-offset pc-offset)))
              (format t "  fnheader DLword offset: 0x~X~%" fnheader-dlword)
              (format t "  FuncObj byte offset: 0x~X (fnheader * 2)~%" func-obj-byte-offset)
              (format t "  FX->pc: ~D (byte offset)~%" pc-offset)
              (format t "  Initial PC: 0x~X (FuncObj + pc)~%" initial-pc)
              (setf (maiko-lisp.vm:vm-pc vm) initial-pc))
            ;; Stack pointer from FX->nextblock
            ;; Per Zig: CurrentStackPTR = Stackspace + nextblock - 2 DLwords
            (let* ((nextblock (maiko-lisp.data:fx-nextblock fx))
                   (stackspace-offset maiko-lisp.data:+stackspace-byte-offset+)
                   (current-stack-ptr (+ stackspace-offset (* nextblock 2) -4)))
              ;; Set stack pointer offset (byte offset into virtual_memory)
              (setf (maiko-lisp.vm:vm-stack-ptr-offset vm) current-stack-ptr)
              (setf (maiko-lisp.vm:vm-stack-base-offset vm) stackspace-offset)
              (format t "  Stack pointer (CurrentStackPTR): 0x~X~%" current-stack-ptr)
              ;; Initialize top-of-stack from the stack memory
              ;; Per C: TopOfStack is cached, read from CurrentStackPTR location
              (setf (maiko-lisp.vm:vm-top-of-stack vm)
                    (maiko-lisp.vm:vm-read-lispptr vm current-stack-ptr))
              (format t "  Top-of-stack (cached): 0x~X~%" (maiko-lisp.vm:vm-top-of-stack vm)))))

        ;; Set up step limiting (from command line or environment)
        (let ((env-max-steps (maiko-lisp.vm:get-emulator-max-steps)))
          (when (or (> max-steps 0) (> env-max-steps 0))
            (setf maiko-lisp.vm:*max-trace-steps* (max max-steps env-max-steps))
            (format t "Step limit: ~D~%" maiko-lisp.vm:*max-trace-steps*)))

        ;; Set up tracing - auto-enable if EMULATOR_MAX_STEPS is set, or if trace-file specified
        (let ((env-max-steps (maiko-lisp.vm:get-emulator-max-steps)))
          (cond
            (trace-file
             (format t "Enabling trace to: ~A~%" trace-file)
             (maiko-lisp.vm:open-trace-file trace-file))
            ((> env-max-steps 0)
             ;; Auto-enable tracing to standard location for parity testing
             ;; Write to current working directory (should be repo root when run from script)
             (let ((trace-file "lisp_emulator_execution_log.txt"))
               (format t "Auto-enabling trace to: ~A~%" trace-file)
               (maiko-lisp.vm:open-trace-file trace-file)))))

        ;; Initialize opcode handlers (must be done after all files are loaded)
        (format t "Initializing opcode handlers...~%")
        (maiko-lisp.vm:initialize-byte-opcode-map)
        (maiko-lisp.vm:initialize-opcode-handlers)
        (format t "~D opcode handlers registered~%" (hash-table-count maiko-lisp.vm:*opcode-handlers*))

        ;; Run the dispatch loop
        (format t "Starting execution...~%")
        (handler-case
            (let* ((start-pc (maiko-lisp.vm:vm-pc vm))
                   (bytecode (maiko-lisp.data:extract-bytecode-from-vm
                              (maiko-lisp.vm:vm-virtual-memory vm)
                              start-pc)))
              (if (and bytecode (arrayp bytecode))
                  (progn
                    (format t "Bytecode extracted: ~D bytes, start PC: 0x~X~%" (length bytecode) start-pc)
                    ;; Debug: Check virtual memory pages
                    (let ((vmem (maiko-lisp.vm:vm-virtual-memory vm)))
                      (if vmem
                          (let ((loaded-pages 0))
                            (loop for i below (length vmem)
                                  when (aref vmem i)
                                  do (incf loaded-pages))
                            (format t "Virtual memory: ~D pages total, ~D pages loaded~%" (length vmem) loaded-pages))
                          (format t "WARNING: Virtual memory is NIL~%")))
                    (when (> (length bytecode) 0)
                      (format t "First few bytes: ")
                      (loop for i from 0 below (min 10 (length bytecode))
                            do (format t "~2,'0X " (aref bytecode i)))
                      (format t "~%"))
                    ;; Dispatch with base PC for trace reporting
                    (maiko-lisp.vm:dispatch vm bytecode start-pc))
                  (format t "Error: Could not extract bytecode from sysout (bytecode=~A)~%" bytecode)))
          (maiko-lisp.utils:vm-error (e)
            (format t "VM Error: ~A~%" (maiko-lisp.utils:vm-error-message e)))
          (error (e)
            (format t "Execution error: ~A~%" e)))

        ;; Clean up tracing
        (when trace-file
          (maiko-lisp.vm:trace-end))))

    (format t "Execution complete~%")
    (quit 0)))
