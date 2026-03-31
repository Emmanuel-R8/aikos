;;;; verify-opcode-loading.lisp - Phase 0: Opcode Handler Loading Verification
;;;;
;;;; This script verifies that all opcode handlers are correctly registered
;;;; in the *opcode-metadata* hash table and the dispatch arrays.

;; Load ASDF if not already loaded
(unless (find-package :asdf)
  (require :asdf))

;; Load the system definition
(load "laiko.asd")
(asdf:load-system :laiko)

(in-package :laiko.vm)

(format t "~%=== Phase 0: Opcode Handler Loading Verification ===~%~%")

;; Count defined opcodes in *opcode-metadata*
(let ((opcode-count (hash-table-count *opcode-metadata*))
      (defined-hexcode-count (hash-table-count *defined-hexcodes*)))
  (format t "Opcode Metadata Statistics:~%")
  (format t "  Total opcodes defined: ~D~%" opcode-count)
  (format t "  Unique hexcodes defined: ~D~%" defined-hexcode-count)
  (format t "~%"))

;; Check dispatch arrays
(let ((non-nil-handlers 0)
      (non-nil-names 0)
      (non-nil-lengths 0)
      (nil-handlers '())
      (nil-names '())
      (nil-lengths '()))

  (loop for i from 0 below #x100
        do (when (aref *opcode-handlers-array* i)
             (incf non-nil-handlers))
        do (when (aref *opcode-names* i)
             (incf non-nil-names))
        do (when (> (aref *instruction-lengths* i) 0)
             (incf non-nil-lengths))
        do (unless (aref *opcode-handlers-array* i)
             (push i nil-handlers))
        do (unless (aref *opcode-names* i)
             (push i nil-names))
        do (unless (> (aref *instruction-lengths* i) 0)
             (push i nil-lengths)))

  (format t "Dispatch Array Statistics:~%")
  (format t "  Non-nil handlers: ~D/256 (~,1F%)~%"
          non-nil-handlers (* 100.0 (/ non-nil-handlers 256)))
  (format t "  Non-nil names: ~D/256 (~,1F%)~%"
          non-nil-names (* 100.0 (/ non-nil-names 256)))
  (format t "  Non-zero lengths: ~D/256 (~,1F%)~%"
          non-nil-lengths (* 100.0 (/ non-nil-lengths 256)))
  (format t "~%")

  (when nil-handlers
    (format t "Opcodes without handlers (first 20):~%")
    (format t "  ~{~A~^, ~}~%" (subseq (reverse nil-handlers) 0 (min 20 (length nil-handlers))))
    (format t "~%"))

  (when nil-names
    (format t "Opcodes without names (first 20):~%")
    (format t "  ~{~A~^, ~}~%" (subseq (reverse nil-names) 0 (min 20 (length nil-names))))
    (format t "~%"))

  (when nil-lengths
    (format t "Opcodes without lengths (first 20):~%")
    (format t "  ~{~A~^, ~}~%" (subseq (reverse nil-lengths) 0 (min 20 (length nil-lengths))))
    (format t "~%")))

;; List all defined opcodes by category
(format t "Opcodes by Category:~%")
(let ((categories (make-hash-table :test 'eq)))
  (maphash (lambda (name metadata)
             (let ((category (getf metadata :category :uncategorized)))
               (push name (gethash category categories))))
           *opcode-metadata*)

  (let ((category-list '()))
    (maphash (lambda (category opcodes)
               (push category category-list))
             categories)
    (dolist (category (sort category-list #'string< :key #'symbol-name))
      (let ((opcodes (gethash category categories)))
        (format t "  ~A: ~D opcodes~%" category (length opcodes)))))
  (format t "~%"))

;; Check for duplicate hexcodes
(format t "Checking for duplicate hexcodes...~%")
(let ((hexcode-map (make-hash-table :test 'eql))
      (duplicates '()))
  (maphash (lambda (name metadata)
             (let ((hexcode (getf metadata :hexcode)))
               (when hexcode
                 (if (gethash hexcode hexcode-map)
                     (push (list hexcode (gethash hexcode hexcode-map) name) duplicates)
                     (setf (gethash hexcode hexcode-map) name)))))
           *opcode-metadata*)

  (if duplicates
      (progn
        (format t "  WARNING: Found ~D duplicate hexcodes:~%" (length duplicates))
        (dolist (dup duplicates)
          (destructuring-bind (hexcode old-name new-name) dup
            (format t "    0x~2,'0X: ~S -> ~S~%" hexcode old-name new-name))))
      (format t "  No duplicate hexcodes found.~%"))
  (format t "~%"))

(format t "=== Verification Complete ===~%")
(sb-ext:exit :code 0)