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

      Format: LINE#|PC|INSTRUCTION|OPCODE|OPERANDS|REGISTERS|FLAGS|SP_FP|STACK_SUMMARY|MEMORY_CONTEXT|FP_VP_FO_VA|BS_MEM|NOTES
      Matches C/Zig unified trace format for parity comparison."
  (unless (trace-enabled-p)
    (return-from trace-log))
  (incf *trace-step*)
  (let* ((sp (vm-stack-ptr vm))
           (tos (if (> sp 0) (aref (vm-stack vm) (1- sp)) 0))
           (n1 (if (> sp 1) (aref (vm-stack vm) (- sp 2)) 0))
           (n2 (if (> sp 2) (aref (vm-stack vm) (- sp 3)) 0))
           (page-num (ash pc -9))
           (page-off (logand pc #x1FF))
           (instr-name (or instruction-name (symbol-name opcode)))
           (instr-padded (format nil "~16A" instr-name))) ; Pad to 16 chars
      ;; LINE# (6 digits, right-aligned)
      (format *vm-trace-output* "~6D|" *trace-line-number*)
      ;; PC (0x%06x format - 6 hex digits with leading zeros)
      (format *vm-trace-output* "0x~6,'0X|" pc)
      ;; INSTRUCTION (16 chars, left-aligned)
      (format *vm-trace-output* "~A|" instr-padded)
      ;; OPCODE (0x%02x format)
      (format *vm-trace-output* "0x~2,'0X|" opcode)
      ;; OPERANDS (20 chars, empty for now)
      (if operands
          (let ((operand-str (format nil "~{~2,'0X~^ ~}" operands)))
            (format *vm-trace-output* "~20A|" operand-str))
          (format *vm-trace-output* "~20A|" ""))
      ;; REGISTERS (comma-separated: r1:0x%04x,r2:0x%04x,r3:0x%02x)
      (let ((r1 (logand pc #xFFFF))
            (r2 (logand tos #xFFFF))
            (r3 (logand (ash tos -16) #xFF)))
        (format *vm-trace-output* "r1:0x~4,'0X,r2:0x~4,'0X,r3:0x~2,'0X|" r1 r2 r3))
      ;; FLAGS (comma-separated: Z:%d,N:%d,C:0)
      (let ((z (if (zerop tos) 1 0))
            (n (if (logbitp 31 tos) 1 0))) ; Check sign bit
        (format *vm-trace-output* "Z:~D,N:~D,C:0|" z n))
      ;; SP_FP (comma-separated: SP:0x%06x,FP:0x%06x)
      (let ((fp (if (vm-current-frame vm) (sf-link (vm-current-frame vm)) 0)))
        (format *vm-trace-output* "SP:0x~6,'0X,FP:0x~6,'0X|" sp fp))
      ;; STACK_SUMMARY (comma-separated: TOS:0x%08x,N1:0x%08x,N2:0x%08x)
      (format *vm-trace-output* "TOS:0x~8,'0X,N1:0x~8,'0X,N2:0x~8,'0X|" tos n1 n2)
      ;; MEMORY_CONTEXT (comma-separated: @mem:?,vpage:%u,off:0x%03x)
      (format *vm-trace-output* "@mem:?,vpage:~D,off:0x~3,'0X|" page-num page-off)
      ;; FP_VP_FO_VA (empty for now)
      (format *vm-trace-output* "|")
      ;; BS_MEM (empty for now)
      (format *vm-trace-output* "|")
      ;; NOTES (empty for now)
      (format *vm-trace-output* "~%")
      (force-output *vm-trace-output*) ; Ensure output is written
      (incf *trace-line-number*)))

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
    (force-output *vm-trace-output*) ; Flush before closing
    (close *vm-trace-output*)
    (setf *vm-trace-output* nil)))

(defun open-trace-file (filename)
  "Open trace file for writing."
  (handler-case
      (progn
        (setf *vm-trace-output* (open filename :direction :output :if-exists :supersede))
        (format t "Trace file opened: ~A (stream: ~A)~%" filename *vm-trace-output*))
    (error (e)
      (format t "WARNING: Failed to open trace file ~A: ~A~%" filename e)
      (setf *vm-trace-output* nil))))

(defun get-trace-line-number ()
  "Get current trace line number."
  *trace-line-number*)
