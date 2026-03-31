(format t "Posix argv: ~A~%" sb-ext:*posix-argv*)
(format t "cdr: ~A~%" (cdr sb-ext:*posix-argv*))
(format t "rest cdr: ~A~%" (rest (cdr sb-ext:*posix-argv*)))
(quit)
