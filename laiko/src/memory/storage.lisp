(in-package :maiko-lisp.memory)

;; Storage management
;; Per rewrite documentation memory/storage-allocation.md

(defstruct (storage (:conc-name storage-))
  "Storage structure"
  (heap nil :type (simple-array (unsigned-byte 8) (*)))
  (free-list nil :type list)
  (size 0 :type (integer 0 *)))

(defun create-storage (&key (size 0))
  "Create a storage with the given heap size in bytes"
  (make-storage :heap (make-array size :element-type '(unsigned-byte 8))
                :size size))

(defconstant +cons-cell-size+ 8 "Size of cons cell in bytes")

(defun allocate (storage size)
  "Allocate memory from storage. Returns offset into heap."
  (declare (type storage storage)
           (type (integer 1 *) size))
  (let ((heap (storage-heap storage)))
    (let ((heap-size (length heap))
          (alignment 4))
      (loop for i from 0 below heap-size by alignment
            when (and (<= (+ i size) heap-size)
                      (every #'zerop (subseq heap i (min (+ i size) heap-size))))
              do (return-from allocate i))
      (error 'maiko-lisp.utils:memory-error
             :message (format nil "Storage full: requested ~A bytes, available ~A bytes"
                              size heap-size)))))

(defun allocate-cons-cell (storage)
  "Allocate a cons cell from storage. Returns LispPTR address."
  (declare (type storage storage))
  (let ((offset (allocate storage +cons-cell-size+)))
    (let ((heap (storage-heap storage)))
      (fill heap 0 :start offset :end (+ offset +cons-cell-size+)))
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
    (let ((car-field (logior (aref heap offset)
                             (ash (aref heap (+ offset 1)) 8)
                             (ash (aref heap (+ offset 2)) 16)
                             (ash (aref heap (+ offset 3)) 24)))
          (cdr-code (aref heap (+ offset 6))))
      (maiko-lisp.data:make-cons-cell :car-field car-field :cdr-code cdr-code))))

(defun set-cons-cell (storage ptr cons-cell)
  "Set cons cell at given pointer (alias for put-cons-cell)."
  (put-cons-cell storage ptr cons-cell))

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
    (setf (aref heap offset) (logand car-field #xFF))
    (setf (aref heap (+ offset 1)) (logand (ash car-field -8) #xFF))
    (setf (aref heap (+ offset 2)) (logand (ash car-field -16) #xFF))
    (setf (aref heap (+ offset 3)) (logand (ash car-field -24) #xFF))
    (setf (aref heap (+ offset 6)) cdr-code)))

(defun deallocate (storage ptr)
  "Deallocate memory at given offset"
  (declare (type storage storage)
           (type (integer 0 *) ptr))
  (let ((heap (storage-heap storage)))
    (when (< ptr (length heap))
      (fill heap 0 :start ptr)
      nil)))

(defun check-storage-full (storage)
  "Check if storage is full. Returns true if storage is full."
  (declare (type storage storage))
  (let* ((heap (storage-heap storage))
         (heap-size (length heap))
         (cons-size 8))
    (loop for i from 0 below heap-size by 4
          when (and (<= (+ i cons-size) heap-size)
                    (every #'zerop (subseq heap i (min (+ i cons-size) heap-size))))
            do (return-from check-storage-full nil))
    t))

;; Low-level memory access functions for BitBLT and other operations
(defun vm-read-byte (storage addr)
  "Read a single byte from storage at given address."
  (declare (type storage storage)
           (type (unsigned-byte 32) addr))
  (let ((heap (storage-heap storage)))
    (when (>= addr (length heap))
      (error 'maiko-lisp.utils:invalid-address
             :message (format nil "Read address out of bounds: ~A" addr)))
    (aref heap addr)))

(defun vm-write-byte (storage addr value)
  "Write a single byte to storage at given address."
  (declare (type storage storage)
           (type (unsigned-byte 32) addr)
           (type (unsigned-byte 8) value))
  (let ((heap (storage-heap storage)))
    (when (>= addr (length heap))
      (error 'maiko-lisp.utils:invalid-address
             :message (format nil "Write address out of bounds: ~A" addr)))
    (setf (aref heap addr) value)))

(defun vm-read-word (storage addr)
  "Read a 32-bit word from storage at given address (little-endian)."
  (declare (type storage storage)
           (type (unsigned-byte 32) addr))
  (let ((heap (storage-heap storage)))
    (when (>= addr (- (length heap) 3))
      (error 'maiko-lisp.utils:invalid-address
             :message (format nil "Read word address out of bounds: ~A" addr)))
    (logior (aref heap addr)
            (ash (aref heap (+ addr 1)) 8)
            (ash (aref heap (+ addr 2)) 16)
            (ash (aref heap (+ addr 3)) 24))))

(defun vm-write-word (storage addr value)
  "Write a 32-bit word to storage at given address (little-endian)."
  (declare (type storage storage)
           (type (unsigned-byte 32) addr)
           (type (unsigned-byte 32) value))
  (let ((heap (storage-heap storage)))
    (when (>= addr (- (length heap) 3))
      (error 'maiko-lisp.utils:invalid-address
             :message (format nil "Write word address out of bounds: ~A" addr)))
    (setf (aref heap addr) (logand value #xFF))
    (setf (aref heap (+ addr 1)) (logand (ash value -8) #xFF))
    (setf (aref heap (+ addr 2)) (logand (ash value -16) #xFF))
    (setf (aref heap (+ addr 3)) (logand (ash value -24) #xFF))))

(defun read-dword (storage addr)
  "Read a 64-bit double-word from storage at given address."
  (declare (type storage storage)
           (type (unsigned-byte 32) addr))
  (let ((low (vm-read-word storage addr))
        (high (vm-read-word storage (+ addr 4))))
    (logior (ash high 32) low)))

(defun write-dword (storage addr value)
  "Write a 64-bit double-word to storage at given address."
  (declare (type storage storage)
           (type (unsigned-byte 32) addr)
           (type (unsigned-byte 64) value))
  (vm-write-word storage addr (logand value #xFFFFFFFF))
  (vm-write-word storage (+ addr 4) (ash value -32)))
