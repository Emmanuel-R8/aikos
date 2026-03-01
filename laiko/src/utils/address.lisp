(in-package :laiko.utils)

;; Address translation utilities
;; Per data-model.md and rewrite documentation memory/address-translation.md
;; Per task T073: Platform-specific handling (endianness, word size)

(defun translate-address-2 (ptr fptovp)
  "Translate 2-byte aligned address using FPtoVP mapping.
   Returns a native pointer to DLword array."
  (declare (type lisp-ptr ptr)
           (type (simple-array lisp-ptr (*)) fptovp))
  (let ((page-num (laiko.memory:get-page-number ptr))
        (page-offset (laiko.memory:get-page-offset ptr)))
    ;; Check bounds
    (when (>= page-num (length fptovp))
      (error 'laiko.utils:invalid-address
             :message (format nil "Page number ~A out of bounds" page-num)))
    ;; Get virtual page base from FPtoVP table
    (let ((virtual-page-base (aref fptovp page-num)))
      (when (zerop virtual-page-base)
        (error 'laiko.utils:invalid-address
               :message (format nil "Page ~A not mapped" page-num)))
      ;; Calculate native address
      ;; For now, assume direct mapping (will need proper page mapping later)
      (let ((native-base virtual-page-base)
            (offset-in-words (ash page-offset -1))) ; Divide by 2 for DLword alignment
        ;; Return as a pointer-like value (in Common Lisp, we'll use arrays)
        ;; This is a simplified version - full implementation needs proper memory mapping
        (+ native-base offset-in-words)))))

(defun translate-address-4 (ptr fptovp)
  "Translate 4-byte aligned address using FPtoVP mapping.
   Returns a native pointer to u32 array."
  (declare (type lisp-ptr ptr)
           (type (simple-array lisp-ptr (*)) fptovp))
  (let ((page-num (laiko.memory:get-page-number ptr))
        (page-offset (laiko.memory:get-page-offset ptr)))
    ;; Check bounds
    (when (>= page-num (length fptovp))
      (error 'laiko.utils:invalid-address
             :message (format nil "Page number ~A out of bounds" page-num)))
    ;; Get virtual page base from FPtoVP table
    (let ((virtual-page-base (aref fptovp page-num)))
      (when (zerop virtual-page-base)
        (error 'laiko.utils:invalid-address
               :message (format nil "Page ~A not mapped" page-num)))
      ;; Calculate native address
      ;; For now, assume direct mapping (will need proper page mapping later)
      (let ((native-base virtual-page-base)
            (offset-in-words (ash page-offset -2))) ; Divide by 4 for u32 alignment
        ;; Return as a pointer-like value
        (+ native-base offset-in-words)))))
