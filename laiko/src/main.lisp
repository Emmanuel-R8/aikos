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
      (format t "  Process size: ~D MB~%" (ifpage-process-size ifpage))
      (format t "  Stack base: 0x~X~%" (ifpage-stackbase ifpage))
      
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
        
        ;; Initialize PC from IFPAGE
        (let ((pc-offset (ash (ifpage-currentfxp ifpage) 1)))  ;; DLword to byte
          (setf (maiko-lisp.vm:vm-pc vm) pc-offset)
          (format t "Initial PC: 0x~X (FX offset: 0x~X)~%" pc-offset (ifpage-currentfxp ifpage)))
        
        ;; Set up tracing if requested
        (when trace-file
          (format t "Enabling trace to: ~A~%" trace-file)
          (maiko-lisp.vm:open-trace-file trace-file)
          (when (> max-steps 0)
            (setf maiko-lisp.vm:*max-trace-steps* max-steps)))
        
        ;; Initialize opcode handlers (must be done after all files are loaded)
        (format t "Initializing opcode handlers...~%")
        (maiko-lisp.vm:initialize-byte-opcode-map)
        (maiko-lisp.vm:initialize-opcode-handlers)
        (format t "~D opcode handlers registered~%" (hash-table-count maiko-lisp.vm:*opcode-handlers*))
        
        ;; Run the dispatch loop
        (format t "Starting execution...~%")
        (handler-case
            (let ((bytecode (maiko-lisp.data:extract-bytecode-from-vm
                            (maiko-lisp.vm:vm-virtual-memory vm)
                            (maiko-lisp.vm:vm-pc vm))))
              (if bytecode
                  (maiko-lisp.vm:dispatch vm bytecode)
                  (format t "Error: Could not extract bytecode from sysout~%")))
          (maiko-lisp.utils:vm-error (e)
            (format t "VM Error: ~A~%" (maiko-lisp.utils:vm-error-message e)))
          (error (e)
            (format t "Execution error: ~A~%" e)))
        
        ;; Clean up tracing
        (when trace-file
          (maiko-lisp.vm:trace-end))))
    
    (format t "Execution complete~%")
    (quit 0)))
