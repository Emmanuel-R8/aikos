(in-package :laiko.vm)

;; List operations
;; car, cdr, cons, rplaca, rplacd, createcell, rplcons
;; nth, nthcdr, last, listlength, append, reverse

;;; ===========================================================================
;; CORE LIST OPERATIONS
;;; ===========================================================================

(defop car :hexcode #x01 :instruction-length 1
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
              (cell-obj (laiko.memory:get-cons-cell storage byte-offset))
              (car-field (laiko.data:cons-car-field cell-obj))
              (cdr-code (laiko.data:cons-cdr-code cell-obj)))
         ;; Handle CDR_INDIRECT case: car_field points to another cons cell
         (if (= cdr-code laiko.data:+cdr-indirect+)
             ;; Indirect: follow the pointer
             (let* ((indirect-byte-offset (ash car-field 1))
                    (indirect-cell (laiko.memory:get-cons-cell storage indirect-byte-offset)))
               (set-top-of-stack vm (laiko.data:cons-car-field indirect-cell)))
             ;; Normal case: return car_field directly
             (set-top-of-stack vm car-field)))))))

(defop cdr :hexcode #x02 :instruction-length 1
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
              (cell-obj (laiko.memory:get-cons-cell storage byte-offset))
              (cdr-value (laiko.data:decode-cdr cell-obj tos)))
         (set-top-of-stack vm cdr-value))))))

(defop cons :hexcode #x1A :instruction-length 1
  "CONS: Create new cons cell from top two stack items.
Pops CAR and CDR, pushes new cons cell.
Allocates memory in cons area."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects t  ; Allocates memory
  (let* ((cdr (pop-stack vm))
         (car (pop-stack vm))
         (cell (laiko.memory:allocate-cons-cell (vm-storage vm)))
         (new-cell (laiko.data:make-cons-cell :car-field car)))
    (laiko.memory:put-cons-cell (vm-storage vm) cell new-cell)
    (laiko.data:set-cdr new-cell cell cdr)
    (push-stack vm cell)))

(defop rplaca :hexcode #x18 :instruction-length 1
  "RPLACA: Replace CAR of cons cell.
Pops NEW-CAR and CELL, pushes CELL.
Destructively modifies the cons cell."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects t  ; Destructive
  (let ((cell (pop-stack vm))
        (new-car (pop-stack vm)))
    (let ((cell-obj (laiko.memory:get-cons-cell (vm-storage vm) cell)))
      (laiko.data:set-car cell-obj new-car)
      (laiko.memory:put-cons-cell (vm-storage vm) cell cell-obj))
    (push-stack vm cell)))

(defop rplacd :hexcode #x19 :instruction-length 1
  "RPLACD: Replace CDR of cons cell.
Pops NEW-CDR and CELL, pushes CELL.
Destructively modifies the cons cell."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects t  ; Destructive
  (let ((cell (pop-stack vm))
        (new-cdr (pop-stack vm)))
    (let ((cell-obj (laiko.memory:get-cons-cell (vm-storage vm) cell)))
      (laiko.data:set-cdr cell-obj cell new-cdr)
      (laiko.memory:put-cons-cell (vm-storage vm) cell cell-obj))
    (push-stack vm cell)))

(defop createcell :hexcode #x1F :instruction-length 1
  "CREATECELL: Allocate an empty cons cell.
Allocates memory and pushes the new cell pointer."
  :operands nil
  :stack-effect (:push 1)
  :category :list-operations
  :side-effects t  ; Allocates memory
  (let ((cell (laiko.memory:allocate-cons-cell (vm-storage vm))))
    (push-stack vm cell)))

(defop rplcons :hexcode #x26 :instruction-length 1
  "RPLCONS: Create cons and push all components.
Like CONS but also pushes CDR and CAR back onto stack.
Used for list construction patterns."
  :operands nil
  :stack-effect (:pop 2 :push 3)
  :category :list-operations
  :side-effects t  ; Allocates memory
  (let* ((cdr (pop-stack vm))
         (car (pop-stack vm))
         (cell (laiko.memory:allocate-cons-cell (vm-storage vm)))
         (new-cell (laiko.data:make-cons-cell :car-field car)))
    (laiko.memory:put-cons-cell (vm-storage vm) cell new-cell)
    (laiko.data:set-cdr new-cell cell cdr)
    (push-stack vm cdr)
    (push-stack vm car)
    (push-stack vm cell)))

;;; ===========================================================================
;; EXTENDED LIST OPERATIONS
;;; ===========================================================================

(defop nth :hexcode #x27 :instruction-length 1
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
          (let ((cell (laiko.memory:get-cons-cell (vm-storage vm) result)))
            (push-stack vm (laiko.data:get-car cell)))))))

(defop nthcdr :hexcode #x28 :instruction-length 1
  "NTHCDR: Get Nth CDR of list (0-based).
Pops N and LIST, pushes the Nth CDR."
  :operands nil
  :stack-effect (:pop 2 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((n (pop-stack vm))
        (list (pop-stack vm)))
    (push-stack vm (vm-nthcdr vm n list))))

(defop last :hexcode #x29 :instruction-length 1
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
               (cell (laiko.memory:get-cons-cell (vm-storage vm) list))
               (cdr-val (laiko.data:get-cdr cell list)))
          (loop while (not (zerop cdr-val))
                do (setf curr cdr-val)
                   (setf cell (laiko.memory:get-cons-cell (vm-storage vm) cdr-val))
                   (setf cdr-val (laiko.data:get-cdr cell cdr-val)))
          (push-stack vm curr)))))

(defop listlength :hexcode #x2A :instruction-length 1
  "LISTLENGTH: Get length of list.
Pops LIST, pushes the length as a small positive integer."
  :operands nil
  :stack-effect (:pop 1 :push 1)
  :category :list-operations
  :side-effects nil
  (let ((list (pop-stack vm)))
    (let ((len (if (zerop list) 0 (list-length-helper vm list))))
      (push-stack vm (ash len 1))))) ; Convert to SMALLPOSP format

(defop append :hexcode #x2B :instruction-length 1
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
            (laiko.data:set-cdr
             (laiko.memory:get-cons-cell (vm-storage vm) last-cell)
             last-cell
             list2))
          (push-stack vm copy)))))

(defop reverse :hexcode #x2C :instruction-length 1
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
            do (let* ((cell (laiko.memory:get-cons-cell (vm-storage vm) list))
                      (car-val (laiko.data:get-car cell)))
                 (let ((new-cell (laiko.memory:allocate-cons-cell (vm-storage vm))))
                   (laiko.data:set-car
                    (laiko.memory:get-cons-cell (vm-storage vm) new-cell)
                    car-val)
                   (laiko.data:set-cdr
                    (laiko.memory:get-cons-cell (vm-storage vm) new-cell)
                    new-cell
                    result)
                   (setf result new-cell)
                   (setf list (laiko.data:get-cdr cell list)))))
      (push-stack vm result))))

;;; ===========================================================================
;; HELPER FUNCTIONS (not opcodes)
;;; ===========================================================================

(defun vm-nthcdr (vm n list)
  "Get Nth CDR of list (helper for NTH and NTHCDR opcodes)."
  (declare (type vm vm)
           (type (integer 0 *) n)
           (type laiko.utils:lisp-ptr list))
  (if (zerop n)
      list
      (progn
        (loop repeat n
              do (if (zerop list)
                     (return 0)
                     (let ((cell (laiko.memory:get-cons-cell (vm-storage vm) list)))
                       (setf list (laiko.data:get-cdr cell list)))))
        list)))

(defun list-length-helper (vm list)
  "Compute list length recursively (helper for LISTLENGTH opcode)."
  (declare (type vm vm)
           (type laiko.utils:lisp-ptr list))
  (if (zerop list)
      0
      (let* ((cell (laiko.memory:get-cons-cell (vm-storage vm) list))
             (cdr-val (laiko.data:get-cdr cell list)))
        (1+ (list-length-helper vm cdr-val)))))

(defun vm-copy-list (vm list)
  "Shallow copy of a list (helper for APPEND opcode)."
  (declare (type vm vm)
           (type laiko.utils:lisp-ptr list))
  (if (zerop list)
      0
      (let ((result 0)
            (last-new 0))
        (loop with curr = list
              while (not (zerop curr))
              do (let* ((cell (laiko.memory:get-cons-cell (vm-storage vm) curr))
                        (car-val (laiko.data:get-car cell)))
                   (let ((new-cell (laiko.memory:allocate-cons-cell (vm-storage vm))))
                     (laiko.data:set-car
                      (laiko.memory:get-cons-cell (vm-storage vm) new-cell)
                      car-val)
                     (laiko.data:set-cdr
                      (laiko.memory:get-cons-cell (vm-storage vm) new-cell)
                      new-cell
                      0)
                     (if (zerop result)
                         (setf result new-cell)
                         (laiko.data:set-cdr
                          (laiko.memory:get-cons-cell (vm-storage vm) last-new)
                          last-new
                          new-cell))
                     (setf last-new new-cell)))
                 (setf curr (laiko.data:get-cdr
                             (laiko.memory:get-cons-cell (vm-storage vm) curr)
                             curr)))
        result)))

(defun lastcdr (vm list)
  "Get the last cons cell of a list (helper for APPEND opcode)."
  (declare (type vm vm)
           (type laiko.utils:lisp-ptr list))
  (if (zerop list)
      0
      (let* ((curr list)
             (cell (laiko.memory:get-cons-cell (vm-storage vm) list))
             (cdr-val (laiko.data:get-cdr cell list)))
        (loop while (not (zerop cdr-val))
              do (setf curr cdr-val)
                 (setf cell (laiko.memory:get-cons-cell (vm-storage vm) cdr-val))
                 (setf cdr-val (laiko.data:get-cdr cell cdr-val)))
        curr)))
