(in-package :laiko.memory)

;; Virtual memory management
;; Per rewrite documentation memory/virtual-memory.md

;; Import page number/offset functions from layout
(import 'laiko.memory:get-page-number)
(import 'laiko.memory:get-page-offset)

(defstruct (virtual-memory (:conc-name vmem-))
  "Virtual memory structure"
  (fptovp nil :type (simple-array laiko.utils:lisp-ptr (*)))
  (page-map nil :type (simple-array (unsigned-byte 8) (*)))
  (size 0 :type (integer 0 *)))

(defun create-virtual-memory (size)
  "Create virtual memory with given size"
  (declare (type (integer 1 *) size))
  (make-virtual-memory :fptovp (make-array size
                                           :element-type 'laiko.utils:lisp-ptr
                                           :initial-element 0)
                       :page-map (make-array size
                                             :element-type '(unsigned-byte 8)
                                             :initial-element 0)
                       :size size))

(defun translate-address (vmem ptr size)
  "Translate virtual address to native address using FPtoVP mapping"
  (declare (type virtual-memory vmem)
           (type laiko.utils:lisp-ptr ptr)
           (type (integer 1 4) size))
  (let ((fptovp (vmem-fptovp vmem))
        (page-num (get-page-number ptr))
        (page-offset (get-page-offset ptr)))
    ;; Check if page is mapped
    (when (>= page-num (length fptovp))
      (error 'laiko.utils:invalid-address
             :message (format nil "Page number ~A out of bounds" page-num)))
    (let ((virtual-page (aref fptovp page-num)))
      (when (zerop virtual-page)
        (error 'laiko.utils:invalid-address
               :message (format nil "Page ~A not mapped" page-num)))
      ;; Calculate native address: virtual_page_base + offset
      ;; For now, assume direct mapping (will need proper page mapping later)
      (+ (ash virtual-page 8) page-offset))))

(defun map-page (vmem file-page virtual-page)
  "Map file page to virtual page in FPtoVP table"
  (declare (type virtual-memory vmem)
           (type (integer 0 *) file-page virtual-page))
  (let ((fptovp (vmem-fptovp vmem)))
    (when (>= file-page (length fptovp))
      (error 'laiko.utils:memory-error
             :message (format nil "File page ~A out of bounds" file-page)))
    (setf (aref fptovp file-page) virtual-page)))
