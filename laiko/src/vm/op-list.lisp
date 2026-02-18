(in-package :maiko-lisp.vm)

;; List operations
;; car, cdr, cons, rplaca, rplacd, createcell, rplcons
;; nth, nthcdr, last, listlength, append, reverse

;;; ===========================================================================
;; CORE LIST OPERATIONS
;;; ===========================================================================

(defop car #x01 1
  "CAR: Get first element of cons cell.
Replaces TOS with the CAR of the cons cell pointed to by TOS.
CAR of NIL is NIL; CAR of T is T.
Per maiko/src/car-cdr.c:N_OP_car()."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((tos (get-top-of-stack vm)))
    (cond
      ((zerop tos) ; NIL_PTR - CAR of NIL is NIL
       (set-top-of-stack vm 0))
      ((= tos 1) ; ATOM_T - CAR of T is T
       (set-top-of-stack vm 1))
      (t
       (let* ((storage (vm-storage vm))
              (byte-offset (ash tos 1)) ; Convert word offset to byte offset
              (cell-obj (maiko-lisp.memory:get-cons-cell storage byte-offset))
              (car-field (maiko-lisp.data:cons-car-field cell-obj))
              (cdr-code (maiko-lisp.data:cons-cdr-code cell-obj)))
         ;; Handle CDR_INDIRECT case: car_field points to another cons cell
         (if (= cdr-code maiko-lisp.data:+cdr-indirect+)
             ;; Indirect: follow the pointer
             (let* ((indirect-byte-offset (ash car-field 1))
                    (indirect-cell (maiko-lisp.memory:get-cons-cell storage indirect-byte-offset)))
               (set-top-of-stack vm (maiko-lisp.data:cons-car-field indirect-cell)))
             ;; Normal case: return car_field directly
             (set-top-of-stack vm car-field)))))))

(defop cdr #x02 1
  "CDR: Get rest of cons cell.
Replaces TOS with the CDR of the cons cell pointed to by TOS.
CDR of NIL is NIL.
Per maiko/src/car-cdr.c:N_OP_cdr()."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((tos (get-top-of-stack vm)))
    (cond
      ((zerop tos) ; NIL_PTR - CDR of NIL is NIL
       (set-top-of-stack vm 0))
      (t
       (let* ((storage (vm-storage vm))
              (byte-offset (ash tos 1)) ; Convert word offset to byte offset
              (cell-obj (maiko-lisp.memory:get-cons-cell storage byte-offset))
              (cdr-value (maiko-lisp.data:decode-cdr cell-obj tos)))
         (set-top-of-stack vm cdr-value))))))

(defop cons #x1A 1
  "CONS: Create new cons cell from top two stack items.
Pops CAR and CDR, pushes new cons cell.
Allocates memory in cons area."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects t  ; Allocates memory
  (let* ((cdr (pop-stack vm))
         (car (pop-stack vm))
         (cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm)))
         (new-cell (maiko-lisp.data:make-cons-cell :car-field car)))
    (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell new-cell)
    (maiko-lisp.data:set-cdr new-cell cell cdr)
    (push-stack vm cell)))

(defop rplaca #x18 1
  "RPLACA: Replace CAR of cons cell.
Pops NEW-CAR and CELL, pushes CELL.
Destructively modifies the cons cell."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects t  ; Destructive
  (let ((cell (pop-stack vm))
        (new-car (pop-stack vm)))
    (let ((cell-obj (maiko-lisp.memory:get-cons-cell (vm-storage vm) cell)))
      (maiko-lisp.data:set-car cell-obj new-car)
      (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell cell-obj))
    (push-stack vm cell)))

(defop rplacd #x19 1
  "RPLACD: Replace CDR of cons cell.
Pops NEW-CDR and CELL, pushes CELL.
Destructively modifies the cons cell."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects t  ; Destructive
  (let ((cell (pop-stack vm))
        (new-cdr (pop-stack vm)))
    (let ((cell-obj (maiko-lisp.memory:get-cons-cell (vm-storage vm) cell)))
      (maiko-lisp.data:set-cdr cell-obj cell new-cdr)
      (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell cell-obj))
    (push-stack vm cell)))

(defop createcell #x1F 1
  "CREATECELL: Allocate an empty cons cell.
Allocates memory and pushes the new cell pointer."
  :operands nil
  :stack-effect (:push 1)
  :category :list-operations
  :side-effects t  ; Allocates memory
  (let ((cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm))))
    (push-stack vm cell)))

(defop rplcons #x26 1
  "RPLCONS: Create cons and push all components.
Like CONS but also pushes CDR and CAR back onto stack.
Used for list construction patterns."
  :operands nil
  :stack-effect (:pop 2 :push 3)
  :category :list-operations
  :side-effects t  ; Allocates memory
  (let* ((cdr (pop-stack vm))
         (car (pop-stack vm))
         (cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm)))
         (new-cell (maiko-lisp.data:make-cons-cell :car-field car)))
    (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell new-cell)
    (maiko-lisp.data:set-cdr new-cell cell cdr)
    (push-stack vm cdr)
    (push-stack vm car)
    (push-stack vm cell)))

;;; ===========================================================================
;; EXTENDED LIST OPERATIONS
;;; ===========================================================================

(defop nth #x27 1
  "NTH: Get Nth element of list (0-based).
Pops N and LIST, pushes the Nth element."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((n (pop-stack vm))
        (list (pop-stack vm)))
    (let ((result (vm-nthcdr vm n list)))
      (if (zerop result)
          (push-stack vm 0)
          (let ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) result)))
            (push-stack vm (maiko-lisp.data:get-car cell)))))))

(defop nthcdr #x28 1
  "NTHCDR: Get Nth CDR of list (0-based).
Pops N and LIST, pushes the Nth CDR."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((n (pop-stack vm))
        (list (pop-stack vm)))
    (push-stack vm (vm-nthcdr vm n list))))

(defop last #x29 1
  "LAST: Get last cons cell of list.
Pops LIST, pushes the last cons cell (not the last element)."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((list (pop-stack vm)))
    (if (zerop list)
        (push-stack vm 0)
        (let* ((curr list)
               (cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) list))
               (cdr-val (maiko-lisp.data:get-cdr cell list)))
          (loop while (not (zerop cdr-val))
                do (setf curr cdr-val)
                   (setf cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) cdr-val))
                   (setf cdr-val (maiko-lisp.data:get-cdr cell cdr-val)))
          (push-stack vm curr)))))

(defop listlength #x2A 1
  "LISTLENGTH: Get length of list.
Pops LIST, pushes the length as a small positive integer."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((list (pop-stack vm)))
    (let ((len (if (zerop list) 0 (list-length-helper vm list))))
      (push-stack vm (ash len 1))))) ; Convert to SMALLPOSP format

(defop append #x2B 1
  "APPEND: Append two lists.
Pops LIST2 and LIST1, pushes new list with LIST1's elements followed by LIST2.
Allocates new cons cells for LIST1's structure."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects t  ; Allocates memory
  (let ((list2 (pop-stack vm))
        (list1 (pop-stack vm)))
    (if (zerop list1)
        (push-stack vm list2)
        (let ((copy (vm-copy-list vm list1)))
          (let ((last-cell (lastcdr vm copy)))
            (maiko-lisp.data:set-cdr
             (maiko-lisp.memory:get-cons-cell (vm-storage vm) last-cell)
             last-cell
             list2))
          (push-stack vm copy)))))

(defop reverse #x2C 1
  "REVERSE: Reverse a list.
Pops LIST, pushes a new list with elements in reverse order.
Allocates new cons cells."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :list-operations
  :side-effects t  ; Allocates memory
  (let ((list (pop-stack vm)))
    (let ((result 0))
      (loop while (not (zerop list))
            do (let* ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) list))
                      (car-val (maiko-lisp.data:get-car cell)))
                 (let ((new-cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm))))
                   (maiko-lisp.data:set-car
                    (maiko-lisp.memory:get-cons-cell (vm-storage vm) new-cell)
                    car-val)
                   (maiko-lisp.data:set-cdr
                    (maiko-lisp.memory:get-cons-cell (vm-storage vm) new-cell)
                    new-cell
                    result)
                   (setf result new-cell)
                   (setf list (maiko-lisp.data:get-cdr cell list)))))
      (push-stack vm result))))

;;; ===========================================================================
;; HELPER FUNCTIONS (not opcodes)
;;; ===========================================================================

(defun vm-nthcdr (vm n list)
  "Get Nth CDR of list (helper for NTH and NTHCDR opcodes)."
  (declare (type vm vm)
           (type (integer 0 *) n)
           (type maiko-lisp.utils:lisp-ptr list))
  (if (zerop n)
      list
      (progn
        (loop repeat n
              do (if (zerop list)
                     (return 0)
                     (let ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) list)))
                       (setf list (maiko-lisp.data:get-cdr cell list)))))
        list)))

(defun list-length-helper (vm list)
  "Compute list length recursively (helper for LISTLENGTH opcode)."
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr list))
  (if (zerop list)
      0
      (let* ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) list))
             (cdr-val (maiko-lisp.data:get-cdr cell list)))
        (1+ (list-length-helper vm cdr-val)))))

(defun vm-copy-list (vm list)
  "Shallow copy of a list (helper for APPEND opcode)."
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr list))
  (if (zerop list)
      0
      (let ((result 0)
            (last-new 0))
        (loop with curr = list
              while (not (zerop curr))
              do (let* ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) curr))
                        (car-val (maiko-lisp.data:get-car cell)))
                   (let ((new-cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm))))
                     (maiko-lisp.data:set-car
                      (maiko-lisp.memory:get-cons-cell (vm-storage vm) new-cell)
                      car-val)
                     (maiko-lisp.data:set-cdr
                      (maiko-lisp.memory:get-cons-cell (vm-storage vm) new-cell)
                      new-cell
                      0)
                     (if (zerop result)
                         (setf result new-cell)
                         (maiko-lisp.data:set-cdr
                          (maiko-lisp.memory:get-cons-cell (vm-storage vm) last-new)
                          last-new
                          new-cell))
                     (setf last-new new-cell)))
              (setf curr (maiko-lisp.data:get-cdr
                          (maiko-lisp.memory:get-cons-cell (vm-storage vm) curr)
                          curr)))
        result)))

(defun lastcdr (vm list)
  "Get the last cons cell of a list (helper for APPEND opcode)."
  (declare (type vm vm)
           (type maiko-lisp.utils:lisp-ptr list))
  (if (zerop list)
      0
      (let* ((curr list)
             (cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) list))
             (cdr-val (maiko-lisp.data:get-cdr cell list)))
        (loop while (not (zerop cdr-val))
              do (setf curr cdr-val)
                 (setf cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) cdr-val))
                 (setf cdr-val (maiko-lisp.data:get-cdr cell cdr-val)))
        curr)))
