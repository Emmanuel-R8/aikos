(in-package :maiko-lisp.memory)

;; Storage management
;; Per rewrite documentation memory/storage-allocation.md

(defstruct (storage (:conc-name storage-))
  "Storage structure"
  (heap nil :type (simple-array (unsigned-byte 8) (*)))
  (free-list nil :type list)
  (size 0 :type (integer 0 *)))

(defun make-storage (size)
  "Create storage with given size"
  (declare (type (integer 1 *) size))
  (make-storage :heap (make-array size
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)
                :size size))

(defun allocate (storage size)
  "Allocate memory from storage. Returns offset into heap."
  (declare (type storage storage)
           (type (integer 1 *) size))
  (let ((heap (storage-heap storage))
        (free-list (storage-free-list storage)))
    ;; Simple allocation: find free space in heap
    ;; TODO: Implement proper free list management
    (let ((heap-size (length heap))
          (alignment 4)) ; 4-byte alignment for LispPTR
      (loop for i from 0 below heap-size by alignment
            when (and (<= (+ i size) heap-size)
                      (every #'zerop (subseq heap i (min (+ i size) heap-size))))
            do
            ;; Mark as allocated (simple approach - just return offset)
            (return-from allocate i))
      ;; Edge case: Memory allocation failure per task T072
      ;; Trigger storage full interrupt
      (error 'maiko-lisp.utils:memory-error
             :message (format nil "Storage full: requested ~A bytes, available ~A bytes"
                             size heap-size)))))

(defun allocate-cons-cell (storage)
  "Allocate a cons cell (8 bytes). Returns LispPTR to cons cell."
  (declare (type storage storage))
  (let ((offset (allocate storage 8)))
    ;; Convert offset to LispPTR (for now, just return offset)
    ;; In full implementation, would map to virtual address space
    offset))

(defun get-cons-cell (storage ptr)
  "Get cons cell at given pointer. Returns cons-cell structure."
  (declare (type storage storage)
           (type maiko-lisp.utils:lisp-ptr ptr))
  (let ((heap (storage-heap storage))
        (offset (if (< ptr (length heap)) ptr 0)))
    ;; Read cons cell from heap
    ;; Cons cell: 8 bytes = 2 DLwords
    ;; CAR: bytes 0-3 (LispPTR)
    ;; CDR code: byte 6
    (let ((car-field (logior (aref heap offset)
                            (ash (aref heap (+ offset 1)) 8)
                            (ash (aref heap (+ offset 2)) 16)
                            (ash (aref heap (+ offset 3)) 24)))
          (cdr-code (aref heap (+ offset 6))))
      (maiko-lisp.data:make-cons-cell :car-field car-field :cdr-code cdr-code))))

(defun set-cons-cell (storage ptr cons-cell)
  "Set cons cell at given pointer."
  (declare (type storage storage)
           (type maiko-lisp.utils:lisp-ptr ptr)
           (type maiko-lisp.data:cons-cell cons-cell))
  (let ((heap (storage-heap storage))
        (offset (if (< ptr (length heap)) ptr 0))
        (car-field (maiko-lisp.data:cons-car-field cons-cell))
        (cdr-code (maiko-lisp.data:cons-cdr-code cons-cell)))
    ;; Write cons cell to heap
    (setf (aref heap offset) (logand car-field #xFF))
    (setf (aref heap (+ offset 1)) (logand (ash car-field -8) #xFF))
    (setf (aref heap (+ offset 2)) (logand (ash car-field -16) #xFF))
    (setf (aref heap (+ offset 3)) (logand (ash car-field -24) #xFF))
    (setf (aref heap (+ offset 6)) cdr-code)))

(defun deallocate (storage ptr)
  "Deallocate memory at given offset"
  (declare (type storage storage)
           (type (integer 0 *) ptr))
  ;; Simple deallocation: zero out memory
  ;; TODO: Implement proper free list management
  (let ((heap (storage-heap storage)))
    (when (< ptr (length heap))
      (fill heap 0 :start ptr)
      nil)))

;; Cons cell size: 8 bytes (2 DLwords, 4-byte aligned)
(defconstant +cons-cell-size+ 8)

(defun allocate-cons-cell (storage)
  "Allocate a cons cell from storage. Returns LispPTR address."
  (declare (type storage storage))
  (let ((offset (allocate storage +cons-cell-size+)))
    ;; Initialize cons cell to zero
    (let ((heap (storage-heap storage)))
      (fill heap 0 :start offset :end (+ offset +cons-cell-size+)))
    ;; Return as LispPTR (offset into heap)
    offset))

(defun get-cons-cell (storage ptr)
  "Get cons cell from storage at given LispPTR address.
   Returns cons-cell structure."
  (declare (type storage storage)
           (type maiko-lisp.utils:lisp-ptr ptr))
  (let ((heap (storage-heap storage))
        (offset ptr))
    (when (>= offset (length heap))
      (error 'maiko-lisp.utils:invalid-address
             :message (format nil "Cons cell address out of bounds: ~A" ptr)))

    ;; Read cons cell from heap (8 bytes)
    ;; Layout: car_field (4 bytes), reserved (2 bytes), cdr_code (1 byte), padding (1 byte)
    (let ((car-field (logior (aref heap offset)
                             (ash (aref heap (+ offset 1)) 8)
                             (ash (aref heap (+ offset 2)) 16)
                             (ash (aref heap (+ offset 3)) 24)))
          (cdr-code (aref heap (+ offset 6))))
      (maiko-lisp.data:make-cons-cell :car-field car-field :cdr-code cdr-code))))

(defun put-cons-cell (storage ptr cons-cell)
  "Write cons cell to storage at given LispPTR address."
  (declare (type storage storage)
           (type maiko-lisp.utils:lisp-ptr ptr)
           (type maiko-lisp.data:cons-cell cons-cell))
  (let ((heap (storage-heap storage))
        (offset ptr)
        (car-field (maiko-lisp.data:cons-car-field cons-cell))
        (cdr-code (maiko-lisp.data:cons-cdr-code cons-cell)))
    (when (>= offset (length heap))
      (error 'maiko-lisp.utils:invalid-address
             :message (format nil "Cons cell address out of bounds: ~A" ptr)))

    ;; Write cons cell to heap (little-endian)
    (setf (aref heap offset) (logand car-field #xFF))
    (setf (aref heap (+ offset 1)) (logand (ash car-field -8) #xFF))
    (setf (aref heap (+ offset 2)) (logand (ash car-field -16) #xFF))
    (setf (aref heap (+ offset 3)) (logand (ash car-field -24) #xFF))
    ;; Reserved bytes (offset 4-5) remain zero
    (setf (aref heap (+ offset 6)) cdr-code)
    ;; Padding byte (offset 7) remains zero
    ))

(defun check-storage-full (storage)
  "Check if storage is full. Returns true if storage is full."
  (declare (type storage storage))
  (let ((heap (storage-heap storage))
        (heap-size (length heap)))
    ;; Simple check: see if we can allocate at least one cons cell
    (let ((cons-size 8))
      (loop for i from 0 below heap-size by 4
            when (and (<= (+ i cons-size) heap-size)
                     (every #'zerop (subseq heap i (min (+ i cons-size) heap-size))))
            do (return-from check-storage-full nil))
      ;; No free space found
      t)))
