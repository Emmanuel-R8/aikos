(in-package :maiko-lisp.vm)

;; Unified execution trace format
;; Compatible with C and Zig implementations
;; Format: LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES

(defvar *vm-trace-output* nil)
(defvar *trace-step* 0)
(defvar *max-trace-steps* 0)
(defvar *trace-line-number* 0)

(defun trace-enabled-p ()
  "Check if tracing is enabled."
  (and *vm-trace-output* (or (zerop *max-trace-steps*) (< *trace-step* *max-trace-steps*))))

(defun get-emulator-max-steps ()
  "Get EMULATOR_MAX_STEPS from environment."
  (let ((env (sb-ext:posix-getenv "EMULATOR_MAX_STEPS")))
    (if env (parse-integer env :junk-allowed t) 0)))

(defun init-trace ()
  "Initialize tracing from environment."
  (setf *max-trace-steps* (get-emulator-max-steps)))

(defun trace-log (vm pc opcode operands &key instruction-name)
  "Log execution state to trace file.
      
      Format: LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES"
  (when (trace-enabled-p)
    (incf *trace-step*)
    (let* ((sp (vm-stack-ptr vm))
           (tos (if (> sp 0) (aref (vm-stack vm) (1- sp)) 0))
           (n1 (if (> sp 1) (aref (vm-stack vm) (- sp 2)) 0))
           (n2 (if (> sp 2) (aref (vm-stack vm) (- sp 3)) 0))
           (page-num (ash pc -9))
           (page-off (logand pc #x1FF)))
      (format *vm-trace-output* "~D|0x~X|~A|0x~2,'0X|"
              *trace-line-number*
              pc
              (or instruction-name (symbol-name opcode))
              opcode)
      ;; Operands as hex bytes
      (if operands
          (format *vm-trace-output* "~{~2,'0X~^ ~}|" operands)
          (format *vm-trace-output* "-|"))
      ;; Registers (empty for now)
      (format *vm-trace-output* "|")
      ;; Flags (empty for now)
      (format *vm-trace-output* "|")
      ;; SP and FP
      (let ((fp (if (vm-current-frame vm) (sf-link (vm-current-frame vm)) 0)))
        (format *vm-trace-output* "SP:0x~X FP:0x~X|" sp fp))
      ;; Stack summary
      (format *vm-trace-output* "TOS:0x~X N1:0x~X N2:0x~X|" tos n1 n2)
      ;; Memory context
      (format *vm-trace-output* "@mem:[vpage:~D off:0x~X]|" page-num page-off)
      ;; FP_VP (empty)
      (format *vm-trace-output* "|")
      ;; BS_MEM (empty)
      (format *vm-trace-output* "|")
      ;; Notes (empty)
      (format *vm-trace-output* "~%")
      (incf *trace-line-number*))))

(defun trace-error (message)
  "Log error to trace."
  (when *vm-trace-output*
    (format *vm-trace-output* "ERROR: ~A~%" message)))

(defun trace-begin ()
  "Begin a trace session."
  (init-trace)
  (setf *trace-line-number* 0)
  (when (plusp *max-trace-steps*)
    (setf *trace-step* 0)))

(defun trace-end ()
  "End trace session."
  (when *vm-trace-output*
    (close *vm-trace-output*)
    (setf *vm-trace-output* nil)))

(defun open-trace-file (filename)
  "Open trace file for writing."
  (setf *vm-trace-output* (open filename :direction :output :if-exists :supersede)))

(defun get-trace-line-number ()
  "Get current trace line number."
  *trace-line-number*)
