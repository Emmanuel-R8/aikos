(in-package :maiko-lisp-tests)

(format t "~%=== Marker Encoding/Decoding Debug ===~%")

;; Test encoding (with correct 16-bit shift)
(let* ((total 2)
       (pvar-offset 10)
       (count-encoded (logand (logxor total #x7FFF) #x7FFF))
       (offset-encoded (logand pvar-offset #x7FFF))
       (marker (logior #x80000000
                      (ash count-encoded 16)
                      offset-encoded)))
  (format t "Encoding:~%")
  (format t "  total=~A, pvar-offset=~A~%" total pvar-offset)
  (format t "  count-encoded (logxor ~A #x7FFF = #x~X)~%" total count-encoded)
  (format t "  offset-encoded = ~A~%" offset-encoded)
  (format t "  ash(count-encoded, 16) = #x~X~%" (ash count-encoded 16))
  (format t "  marker = #x~X~%" marker)
  (format t "  marker in binary (low 32 bits): ~B~%" (logand marker #xFFFFFFFF))
  
    ;; Test decoding immediately (using ldb for logical extraction)
    (let* ((decoded-count (logxor (ldb (byte 15 16) marker) #x7FFF))
           (decoded-offset (ldb (byte 15 0) marker)))
      (format t "~%Decoding (using ldb):~%")
      (format t "  decoded-count = logxor(ldb(byte 15 16, marker)=#x~X, #x7FFF) = ~A~%" 
              (ldb (byte 15 16) marker) decoded-count)
      (format t "  decoded-offset = ldb(byte 15 0, marker) = #x~X = ~A~%" 
              (ldb (byte 15 0) marker) decoded-offset)
    (format t "  Expected: count=2, offset=10~%")
    (format t "  Got: count=~A, offset=~A~%" decoded-count decoded-offset)))

(format t "~%=== VM Test ===~%")

;; Create a fresh VM
(let ((vm (maiko-lisp.vm:create-vm 1024)))
  ;; Set up initial state
  (setf (maiko-lisp.vm::vm-pvar-ptr vm) 10)
  (maiko-lisp.vm:push-stack vm 42)
  
  (format t "Before BIND:~%")
  (format t "  stack-ptr=~A~%" (maiko-lisp.vm:vm-stack-ptr vm))
  (format t "  stack[0]=~X~%" (aref (maiko-lisp.vm:vm-stack vm) 0))
  
  ;; Execute BIND
  (maiko-lisp.vm:handle-bind vm '(#x11))
  
  (format t "~%After BIND:~%")
  (format t "  stack-ptr=~A~%" (maiko-lisp.vm:vm-stack-ptr vm))
  (let ((marker (aref (maiko-lisp.vm:vm-stack vm) 1)))
    (format t "  stack[1]=~X (binary: ~B)~%" marker (logand marker #xFFFFF))
    
    ;; Now test find-binding-marker
    (format t "~%Calling find-binding-marker...~%")
    (multiple-value-bind (count pvar-offset)
        (maiko-lisp.vm::find-binding-marker vm (maiko-lisp.vm:vm-stack-ptr vm))
      (format t "Result: count=~A, pvar-offset=~A~%" count pvar-offset)
      (format t "Expected: count=2, pvar-offset=10~%"))))
