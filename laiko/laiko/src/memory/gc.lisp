(in-package :maiko-lisp.memory)

;; Garbage collection
;; Per rewrite documentation memory/garbage-collection.md

;; Hash table entry structure (simplified version)
(defstruct (gc-entry (:conc-name gce-))
  "GC hash table entry"
  (ptr 0 :type maiko-lisp.utils:lisp-ptr)
  (refcnt 0 :type (integer 0 *))
  (stackref nil :type boolean))

(defstruct (gc (:conc-name gc-))
  "GC structure"
  (hash-table nil :type hash-table)
  (enabled t :type boolean)
  (reclaim-countdown nil :type (or null (integer 0 *))))

(defun make-gc (hash-table-size)
  "Create GC with given hash table size"
  (declare (type (integer 1 *) hash-table-size))
  (make-gc :hash-table (make-hash-table :test 'eql :size hash-table-size)
           :enabled t
           :reclaim-countdown nil))

(defun refcntp (ptr)
  "Check if pointer is reference-counted"
  (declare (type maiko-lisp.utils:lisp-ptr ptr))
  ;; For now, assume all heap objects are reference-counted
  ;; In full implementation, check type tag
  (not (zerop ptr)))

(defun add-ref (gc ptr)
  "Add reference to object (ADDREF operation)"
  (declare (type gc gc)
           (type maiko-lisp.utils:lisp-ptr ptr))
  (unless (refcntp ptr)
    (return-from add-ref))

  (let ((ht (gc-hash-table gc))
        (entry (gethash ptr ht)))
    (if entry
        ;; Entry exists: increment reference count
        (incf (gce-refcnt entry))
        ;; New entry: create with initial count of 2
        ;; (1 for the object itself, 1 for the new reference)
        (setf (gethash ptr ht) (make-gc-entry :ptr ptr :refcnt 2 :stackref nil)))))

(defun del-ref (gc ptr)
  "Remove reference to object (DELREF operation)"
  (declare (type gc gc)
           (type maiko-lisp.utils:lisp-ptr ptr))
  (unless (refcntp ptr)
    (return-from del-ref))

  (let ((ht (gc-hash-table gc))
        (entry (gethash ptr ht)))
    (when entry
      (decf (gce-refcnt entry))
      ;; If count reaches zero, mark for reclamation
      (when (<= (gce-refcnt entry) 0)
        ;; Remove from hash table (will be reclaimed in collect phase)
        (remhash ptr ht)))))

(defun mark-stack-ref (gc ptr)
  "Mark object as stack-referenced (STKREF operation)"
  (declare (type gc gc)
           (type maiko-lisp.utils:lisp-ptr ptr))
  (unless (refcntp ptr)
    (return-from mark-stack-ref))

  (let ((ht (gc-hash-table gc))
        (entry (gethash ptr ht)))
    (if entry
        (setf (gce-stackref entry) t)
        ;; Create new entry with stack reference
        (setf (gethash ptr ht) (make-gc-entry :ptr ptr :refcnt 1 :stackref t)))))

(defun find-in-hash-table (gc ptr)
  "Find entry in hash table for given pointer"
  (declare (type gc gc)
           (type maiko-lisp.utils:lisp-ptr ptr))
  (gethash ptr (gc-hash-table gc)))

(defun collect (gc storage)
  "Run garbage collection"
  (declare (type gc gc)
           (type storage storage))
  (unless (gc-enabled gc)
    (return-from collect))

  ;; Scan hash table for objects with zero references
  (let ((ht (gc-hash-table gc))
        (to-reclaim nil))
    (maphash (lambda (ptr entry)
               (when (<= (gce-refcnt entry) 0)
                 (push ptr to-reclaim)))
             ht)

    ;; Reclaim unreferenced objects
    (dolist (ptr to-reclaim)
      ;; Remove from hash table
      (remhash ptr ht)
      ;; TODO: Deallocate memory from storage
      ;; For now, just remove from hash table
      )

    (length to-reclaim)))

;; GC Coordination with Common Lisp's GC
;; Per research.md: Use explicit memory pinning and coordinate GC triggers

(defun coordinate-gc-with-common-lisp (gc storage)
  "Coordinate Maiko's reference-counting GC with Common Lisp's GC.
   This function ensures that:
   1. Maiko-managed objects are pinned to prevent Common Lisp GC from moving them
   2. Common Lisp GC is triggered only at safe points
   3. Cross-references are handled correctly"
  (declare (type gc gc)
           (type storage storage))

  ;; Pin storage heap to prevent Common Lisp GC from moving it
  ;; In SBCL, we use sb-sys:with-pinned-objects
  #+sbcl
  (sb-sys:with-pinned-objects ((storage-heap storage))
    ;; Run Maiko GC first
    (let ((reclaimed (collect gc storage)))
      ;; If we reclaimed a significant amount, trigger Common Lisp GC
      ;; This is a safe point for Common Lisp GC
      (when (> reclaimed 100)
        #+sbcl (sb-ext:gc :full nil)
        #-sbcl nil)
      reclaimed))

  #-sbcl
  ;; For non-SBCL implementations, just run Maiko GC
  (collect gc storage))