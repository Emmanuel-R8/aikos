(in-package :maiko-lisp.vm)

;; List operations
;; car, cdr, cons, rplaca, rplacd, createcell, rplcons

(defun handle-car (vm)
  "CAR: Get first element of cons cell
   Per maiko/src/car-cdr.c:N_OP_car() - operates on TOS, replaces TOS"
  (declare (type vm vm))
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

(defun handle-cdr (vm)
  "CDR: Get rest of cons cell
   Per maiko/src/car-cdr.c:N_OP_cdr() - operates on TOS, replaces TOS"
  (declare (type vm vm))
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

(defun handle-cons (vm)
  "CONS: Create new cons cell"
  (declare (type vm vm))
  (let* ((cdr (pop-stack vm))
         (car (pop-stack vm))
         (cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm)))
         (new-cell (maiko-lisp.data:make-cons-cell :car-field car)))
    (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell new-cell)
    (maiko-lisp.data:set-cdr new-cell cell cdr)
    (push-stack vm cell)))

(defun handle-rplaca (vm)
  "RPLACA: Replace car of cons cell"
  (declare (type vm vm))
  (let ((cell (pop-stack vm))
        (new-car (pop-stack vm)))
    (let ((cell-obj (maiko-lisp.memory:get-cons-cell (vm-storage vm) cell)))
      (maiko-lisp.data:set-car cell-obj new-car)
      (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell cell-obj))
    (push-stack vm cell)))

(defun handle-rplacd (vm)
  "RPLACD: Replace cdr of cons cell"
  (declare (type vm vm))
  (let ((cell (pop-stack vm))
        (new-cdr (pop-stack vm)))
    (let ((cell-obj (maiko-lisp.memory:get-cons-cell (vm-storage vm) cell)))
      (maiko-lisp.data:set-cdr cell-obj cell new-cdr)
      (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell cell-obj))
    (push-stack vm cell)))

(defun handle-createcell (vm)
  "CREATECELL: Allocate empty cons cell"
  (declare (type vm vm))
  (let ((cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm))))
    (push-stack vm cell)))

(defun handle-rplcons (vm)
  "RPLCONS: Create cons and push components"
  (declare (type vm vm))
  (let* ((cdr (pop-stack vm))
         (car (pop-stack vm))
         (cell (maiko-lisp.memory:allocate-cons-cell (vm-storage vm)))
         (new-cell (maiko-lisp.data:make-cons-cell :car-field car)))
    (maiko-lisp.memory:put-cons-cell (vm-storage vm) cell new-cell)
    (maiko-lisp.data:set-cdr new-cell cell cdr)
    (push-stack vm cdr)
    (push-stack vm car)
    (push-stack vm cell)))

;; Additional list operations

(defun vm-nthcdr (vm n list)
  "Get nth cdr of list"
  (declare (type vm vm)
           (type (integer 0 *) n)
           (type maiko-lisp.utils:lisp-ptr list))
  (if (zerop n)
      list
      (progn
        (loop repeat n
              do (if (zerop list)
                     (return 0)
                     (let* ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) list)))
                       (setf list (maiko-lisp.data:get-cdr cell list)))))
        list)))

(defun handle-nth (vm)
  "NTH: Get nth element of list (0-based)"
  (declare (type vm vm))
  (let ((n (pop-stack vm))
        (list (pop-stack vm)))
    (let ((result (vm-nthcdr vm n list)))
      (if (zerop result)
          (push-stack vm 0)
          (let ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) result)))
            (push-stack vm (maiko-lisp.data:get-car cell))))))

  (defun handle-nthcdr (vm)
    "NTHCDR: Get nth cdr of list (0-based)"
    (declare (type vm vm))
    (let ((n (pop-stack vm))
          (list (pop-stack vm)))
      (push-stack vm (vm-nthcdr vm n list))))

  (defun handle-last (vm)
    "LAST: Get last cons cell of list"
    (declare (type vm vm))
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

  (defun list-length-helper (vm list)
    "Helper to compute list length recursively"
    (declare (type vm vm)
             (type maiko-lisp.utils:lisp-ptr list))
    (if (zerop list)
        0
        (let* ((cell (maiko-lisp.memory:get-cons-cell (vm-storage vm) list))
               (cdr-val (maiko-lisp.data:get-cdr cell list)))
          (1+ (list-length-helper vm cdr-val)))))

  (defun handle-list-length (vm)
    "LISTLENGTH: Get length of list"
    (declare (type vm vm))
    (let ((list (pop-stack vm)))
      (let ((len (if (zerop list) 0 (list-length-helper vm list))))
        (push-stack vm (ash len 1)))))

  (defun handle-append (vm)
    "APPEND: Append two lists"
    (declare (type vm vm))
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
            (push-stack vm copy))))

    (defun handle-reverse (vm)
      "REVERSE: Reverse a list"
      (declare (type vm vm))
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

    (defun vm-copy-list (vm list)
      "Shallow copy of a list"
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
      "Get the last cdr of a list (the NIL at the end)"
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
            curr)))))

