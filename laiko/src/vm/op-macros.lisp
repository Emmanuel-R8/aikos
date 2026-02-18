(in-package :maiko-lisp.vm)

;; To be used in vm/deispatch.lisp
(defvar *byte-opcode-map* (make-hash-table :test 'eql))
(defvar *instruction-length* (make-hash-table :test 'eql))


(defmacro defop (name &key instruction-length hexcode function-doc function-body)
  "Macro to create everything relevant to an opcode
  -
"
  ;; Register an instruction
  (let ((fn-name (intern (format nil "HANDLE-~A" name)
                         (symbol-package name))))

    `(setf (gethash ,hexcode *byte-opcode-map*) ,name)
    ;; "Register a handler function for an opcode"
    `(setf (gethash opcode *opcode-handlers*) handler))

    `(register-opcode-handler ,name ,fn-name)
    `(defun ,fn-name (vm)
       ,function-doc
       (declare (type vm vm))
       ,@function-body)))


(defop cons
       :instruction-length 1
       :hexcode #x1A
       :function-doc "CONS: Create new cons cell"
       :function-body (let* ((cdr (pop-stack vm))
                             (car (pop-stack vm))
                             (cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm)))
                             (new-cell (maiko-lisp.data:make-cons-cell :car-field car)))
                        (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell new-cell)
                        (maiko-lisp.data:set-cdr new-cell cell cdr)
                        (push-stack vm cell)))

; SHOULD CREATE:
;
; (register-opcode-handler 'cons #'handle-cons)
;
; (defun handle-cons (vm)
;   "CONS: Create new cons cell"
;   (declare (type vm vm))
;   (let* ((cdr (pop-stack vm))
;          (car (pop-stack vm))
;          (cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm)))
;          (new-cell (maiko-lisp.data:make-cons-cell :car-field car)))
;     (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell new-cell)
;     (maiko-lisp.data:set-cdr new-cell cell cdr)
;     (push-stack vm cell)))
