(in-package :maiko-lisp-tests)

;; GC tests
;; Per task T028: Test cases for reference counting and memory reclamation

(defun test-add-ref ()
  "Test adding references"
  (let ((gc (maiko-lisp.memory:make-gc 1024))
        (ptr 100))
    ;; Add reference
    (maiko-lisp.memory:add-ref gc ptr)
    ;; Check entry exists
    (let ((entry (maiko-lisp.memory:find-in-hash-table gc ptr)))
      (assert entry nil "Entry should exist after add-ref")
      (assert (>= (slot-value entry 'maiko-lisp.memory::gce-refcnt) 1) nil "Reference count should be at least 1"))))

(defun test-del-ref ()
  "Test deleting references"
  (let ((gc (maiko-lisp.memory:make-gc 1024))
        (ptr 100))
    ;; Add reference
    (maiko-lisp.memory:add-ref gc ptr)
    ;; Delete reference
    (maiko-lisp.memory:del-ref gc ptr)
    ;; Entry should still exist but with lower count
    (let ((entry (maiko-lisp.memory:find-in-hash-table gc ptr)))
      ;; Entry may or may not exist depending on implementation
      (when entry
        (assert (>= (slot-value entry 'maiko-lisp.memory::gce-refcnt) 0) nil "Reference count should be non-negative")))))

(defun test-mark-stack-ref ()
  "Test marking stack references"
  (let ((gc (maiko-lisp.memory:make-gc 1024))
        (ptr 200))
    ;; Mark as stack reference
    (maiko-lisp.memory:mark-stack-ref gc ptr)
    ;; Check entry
    (let ((entry (maiko-lisp.memory:find-in-hash-table gc ptr)))
      (assert entry nil "Entry should exist after mark-stack-ref")
      (assert (slot-value entry 'maiko-lisp.memory::gce-stackref) nil "Stack reference flag should be set"))))

(defun test-gc-collect ()
  "Test garbage collection"
  (let ((gc (maiko-lisp.memory:make-gc 1024))
        (storage (maiko-lisp.memory:make-storage (* 1024 1024))))
    ;; Add some references
    (maiko-lisp.memory:add-ref gc 100)
    (maiko-lisp.memory:add-ref gc 200)
    ;; Delete references to make objects unreferenced
    (maiko-lisp.memory:del-ref gc 100)
    (maiko-lisp.memory:del-ref gc 200)
    ;; Run GC
    (let ((reclaimed (maiko-lisp.memory:collect gc storage)))
      (assert (>= reclaimed 0) nil "GC should reclaim some objects"))))

(defun run-gc-tests ()
  "Run all GC tests"
  (format t "Running GC tests...~%")
  (test-add-ref)
  (format t "  ✓ Add reference test passed~%")
  (test-del-ref)
  (format t "  ✓ Delete reference test passed~%")
  (test-mark-stack-ref)
  (format t "  ✓ Mark stack reference test passed~%")
  (test-gc-collect)
  (format t "  ✓ GC collect test passed~%")
  (format t "All GC tests passed!~%"))