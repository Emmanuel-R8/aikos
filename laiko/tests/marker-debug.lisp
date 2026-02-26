(in-package :maiko-lisp-tests)

(defun test-marker-encoding ()
  "Debug marker encoding"
  (let* ((total 2)
         (pvar-offset 10)
         (count-encoded (logand (logxor total #x7FFF) #x7FFF))
         (marker (logior #x80000000 (ash count-encoded 1) pvar-offset)))
    (format t "total=~A pvar-offset=~A~%" total pvar-offset)
    (format t "count-encoded=~X~%" count-encoded)
    (format t "marker=~X~%" marker)
    (let ((decoded-count (logxor (logand (ash marker -1) #x7FFF) #x7FFF))
          (let ((decoded-offset (logand marker #xFFFF)))
            (format t "decoded-count=~A decoded-offset=~A~%" decoded-count decoded-offset)))))

  (test-marker-encoding)
