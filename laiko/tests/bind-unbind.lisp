(in-package :maiko-lisp-tests)

(defun test-bind-unbind ()
  "Test BIND/UNBIND variable binding operations"
  (format t "~%Testing BIND/UNBIND operations...~%")
  
  ;; Test 1: Simple BIND - bind 1 variable to NIL, 1 from stack
  (format t "~%  Test 1: BIND with n1=1, n2=1~%")
  (let ((vm (maiko-lisp.vm:create-vm 1024)))
    ;; pvar-ptr starts at 0, BIND will allocate at offset 0
    (maiko-lisp.vm:push-stack vm 42)
    ;; Bind 1 to NIL, 1 from stack
    (maiko-lisp.vm:handle-bind vm '(#x11))
    ;; Check PVAR slots
    (let ((slot0 (maiko-lisp.vm:get-pvar-slot vm 0))
          (slot1 (maiko-lisp.vm:get-pvar-slot vm 1)))
      (format t "    PVAR0 (bound to NIL): ~A~%" slot0)
      (format t "    PVAR1 (bound to 42): ~A~%" slot1)
      (assert (= slot0 0) nil "PVAR0 should be 0 (NIL), got ~A" slot0)
      (assert (= slot1 42) nil "PVAR1 should be 42, got ~A" slot1)))
  
  ;; Test 2: BIND with more variables
  (format t "~%  Test 2: BIND with n1=2, n2=1~%")
  (let ((vm (maiko-lisp.vm:create-vm 1024)))
    (maiko-lisp.vm:push-stack vm 100)
    (maiko-lisp.vm:handle-bind vm '(#x21))
    (let ((slot0 (maiko-lisp.vm:get-pvar-slot vm 0))
          (slot1 (maiko-lisp.vm:get-pvar-slot vm 1))
          (slot2 (maiko-lisp.vm:get-pvar-slot vm 2)))
      (format t "    PVAR0: ~A, PVAR1: ~A, PVAR2: ~A~%" slot0 slot1 slot2)
      (assert (= slot0 0) nil "PVAR0 should be 0")
      (assert (= slot1 0) nil "PVAR1 should be 0")
      (assert (= slot2 100) nil "PVAR2 should be 100")))
  
  ;; Test 3: UNBIND - restore bindings
  (format t "~%  Test 3: UNBIND restoration~%")
  (let ((vm (maiko-lisp.vm:create-vm 1024)))
    ;; Push value
    (maiko-lisp.vm:push-stack vm 99)
    ;; Bind
    (maiko-lisp.vm:handle-bind vm '(#x11))
    ;; Verify bound
    (let ((slot1-before (maiko-lisp.vm:get-pvar-slot vm 1)))
      (format t "    PVAR1 after BIND: ~A~%" slot1-before)
      (assert (= slot1-before 99) nil "PVAR1 should be 99 before UNBIND"))
    ;; Unbind
    (maiko-lisp.vm:handle-unbind vm)
    ;; Check restoration
    (let ((slot0 (maiko-lisp.vm:get-pvar-slot vm 0))
          (slot1 (maiko-lisp.vm:get-pvar-slot vm 1)))
      (format t "    PVAR0 after UNBIND: ~A~%" slot0)
      (format t "    PVAR1 after UNBIND: ~A~%" slot1)
      (assert (= slot0 #xFFFFFFFF) nil "PVAR0 should be unbound marker")
      (assert (= slot1 #xFFFFFFFF) nil "PVAR1 should be unbound marker")))
  
  ;; Test 4: Nested BIND/UNBIND
  (format t "~%  Test 4: Nested BIND/UNBIND~%")
  (let ((vm (maiko-lisp.vm:create-vm 1024)))
    ;; First bind
    (maiko-lisp.vm:push-stack vm 111)
    (maiko-lisp.vm:handle-bind vm '(#x11))
    (format t "    After first BIND: PVAR1=~A~%" (maiko-lisp.vm:get-pvar-slot vm 1))
    ;; Second bind at same level
    (maiko-lisp.vm:push-stack vm 222)
    (maiko-lisp.vm:handle-bind vm '(#x11))
    (format t "    After second BIND: PVAR1=~A, PVAR3=~A~%"
            (maiko-lisp.vm:get-pvar-slot vm 1)
            (maiko-lisp.vm:get-pvar-slot vm 3))
    ;; First UNBIND should restore inner
    (maiko-lisp.vm:handle-unbind vm)
    (format t "    After first UNBIND: PVAR1=~A, PVAR3=~A~%"
            (maiko-lisp.vm:get-pvar-slot vm 1)
            (maiko-lisp.vm:get-pvar-slot vm 3)))
  
  (format t "~%All BIND/UNBIND tests passed!~%"))

;; Run tests
(test-bind-unbind)
