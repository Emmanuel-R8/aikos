(in-package :maiko-lisp)

;; Helper to get command-line arguments (works with or without UIOP)
(defun get-command-line-arguments ()
  "Get command-line arguments"
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl (if (find-package :uiop)
             (uiop:command-line-arguments)
             nil))

;; Helper to get environment variable (works with or without UIOP)
(defun getenv (var)
  "Get environment variable"
  #+sbcl (sb-ext:posix-getenv var)
  #-sbcl (if (find-package :uiop)
             (uiop:getenv var)
             nil))

(defun main ()
  "Main entry point for Maiko Lisp emulator"
  (let ((args (get-command-line-arguments)))
    (cond
      ((or (member "-info" args :test #'string-equal)
           (member "-INFO" args :test #'string-equal))
       (print-info))
      ((or (member "-help" args :test #'string-equal)
           (member "-HELP" args :test #'string-equal)
           (member "--help" args :test #'string-equal))
       (print-help))
      (t
       (let ((sysout-path (or (find-sysout-path args)
                              (getenv "LDESRCESYSOUT"))))
         (if sysout-path
             (run-emulator sysout-path args)
             (progn
               (format t "No sysout file specified. Use -sysout <file> or set LDESRCESYSOUT environment variable.~%")
               (format t "Use -help for usage information.~%")
               (quit 1))))))))

(defun quit (code)
  "Exit with code"
  #+sbcl (sb-ext:exit :code code)
  #-sbcl (if (find-package :uiop)
             (uiop:quit code)
             (progn (format t "Exiting with code ~A~%" code) (ext:quit))))

(defun print-info ()
  "Print system information"
  (format t "Emulator for Medley Interlisp (Common Lisp implementation)~%")
  (format t "Version: 0.1.0~%")
  (format t "Built with ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
  (quit 0))

(defun print-help ()
  "Print help message"
  (format t "either setenv LDESRCESYSOUT or do:~%")
  (format t "medley [<sysout-name>] [<options>]~%")
  (format t "-info                    Print general info about the system~%")
  (format t "-help                    Print this message~%")
  (format t "-sysout <file>           Specify sysout file~%")
  (format t "-pixelscale <n>          The amount of pixels to show for one Medley screen pixel.~%")
  (format t "-fg/-foreground <color>  Screen foreground color, default Black.  X color name or #RRBBGG hex~%")
  (format t "-bg/-background <color> Screen background color, default White.  X color name or #RRBBGG hex~%")
  (format t "-sc[reen] <w>x<h>       The Medley screen geometry~%")
  (format t "-t/-title <title>       The window title~%")
  (format t "-timer <interval>       Timer interval (undocumented)~%")
  (format t "-m <size>               Virtual memory size in Mega Bytes (undocumented)~%")
  (format t "-NF                      Don't fork (for debugging)~%")
  (format t "-INIT                    Init sysout, no packaged (undocumented)~%")
  (quit 0))

(defun find-sysout-path (args)
  "Find sysout path from command line arguments"
  (let ((sysout-index (position "-sysout" args :test #'string-equal)))
    (cond
      (sysout-index
       (when (< (1+ sysout-index) (length args))
         (nth (1+ sysout-index) args)))
      (t
       ;; Check for positional argument (first non-option)
       (find-if (lambda (arg) (not (eql (char arg 0) #\-))) args)))))

(defun parse-options (args)
  "Parse command line options"
  (let ((options (make-hash-table :test 'equal)))
    (loop for i from 0 below (length args)
          for arg = (nth i args)
          do
          (cond
            ((string-equal arg "-sc" :end2 3)
             (when (< (1+ i) (length args))
               (let ((geometry (nth (1+ i) args)))
                 (multiple-value-bind (width height)
                     (parse-geometry geometry)
                   (setf (gethash "screen-width" options) width
                         (gethash "screen-height" options) height))
                 (incf i))))
            ((string-equal arg "-pixelscale")
             (when (< (1+ i) (length args))
               (setf (gethash "pixel-scale" options)
                     (parse-integer (nth (1+ i) args)))
               (incf i)))
            ((or (string-equal arg "-fg")
                 (string-equal arg "-foreground"))
             (when (< (1+ i) (length args))
               (setf (gethash "foreground-color" options)
                     (nth (1+ i) args))
               (incf i)))
            ((or (string-equal arg "-bg")
                 (string-equal arg "-background"))
             (when (< (1+ i) (length args))
               (setf (gethash "background-color" options)
                     (nth (1+ i) args))
               (incf i)))
            ((or (string-equal arg "-t")
                 (string-equal arg "-title"))
             (when (< (1+ i) (length args))
               (setf (gethash "window-title" options)
                     (nth (1+ i) args))
               (incf i)))
            ((string-equal arg "-timer")
             (when (< (1+ i) (length args))
               (setf (gethash "timer-interval" options)
                     (parse-integer (nth (1+ i) args)))
               (incf i)))
            ((string-equal arg "-m")
             (when (< (1+ i) (length args))
               (setf (gethash "memory-size-mb" options)
                     (parse-integer (nth (1+ i) args)))
               (incf i)))
            ((string-equal arg "-NF")
             (setf (gethash "no-fork" options) t))
            ((string-equal arg "-INIT")
             (setf (gethash "init-mode" options) t))))
    options))

(defun parse-geometry (geometry)
  "Parse WIDTHxHEIGHT geometry string"
  (let ((x-pos (position #\x geometry)))
    (if x-pos
        (values (parse-integer geometry :end x-pos)
                (parse-integer geometry :start (1+ x-pos)))
        (error "Invalid geometry format: ~A" geometry))))

(defun run-emulator (sysout-path args)
  "Run the emulator with given sysout file"
  (let ((options (parse-options args)))
    (format t "Loading sysout file: ~A~%" sysout-path)

    ;; Print parsed options
    (when (gethash "screen-width" options)
      (format t "Screen geometry: ~Ax~A~%"
              (gethash "screen-width" options)
              (gethash "screen-height" options)))
    (when (gethash "pixel-scale" options)
      (format t "Pixel scale: ~A~%" (gethash "pixel-scale" options)))
    (when (gethash "window-title" options)
      (format t "Window title: ~A~%" (gethash "window-title" options)))

    ;; Initialize VM
    ;; Stack size in DLwords (16-bit words)
    ;; Default: 64KB stack = 32768 DLwords
    (let ((stack-size-dlwords (or (gethash "memory-size-mb" options) 32768))
          (vm (maiko-lisp.vm:make-vm stack-size-dlwords)))

      ;; Initialize storage
      (let ((storage-size (* (or (gethash "memory-size-mb" options) 16) 1024 1024))) ; Default 16MB
            (storage (maiko-lisp.memory:make-storage storage-size)))
        (setf (maiko-lisp.vm:vm-storage vm) storage))

      ;; Initialize GC
      (let ((gc (maiko-lisp.memory:make-gc 1024))) ; Default 1024 entry hash table
        (setf (maiko-lisp.vm:vm-gc vm) gc))

      ;; Load sysout file
      (handler-case
          (multiple-value-bind (ifpage file-contents)
              (maiko-lisp.data:load-sysout sysout-path)
            (format t "Sysout loaded successfully~%")
            (format t "  Version: ~A~%" (maiko-lisp.data:ifpage-lversion ifpage))
            (format t "  Process size: ~A MB~%" (maiko-lisp.data:ifpage-process-size ifpage))
            (format t "  Active pages: ~A~%" (maiko-lisp.data:ifpage-nactivepages ifpage))

            ;; Initialize virtual memory with FPtoVP mapping
            (let ((nactive-pages (maiko-lisp.data:ifpage-nactivepages ifpage))
                  (fptovp-start (maiko-lisp.data:ifpage-fptovpstart ifpage)))
              (when (> nactive-pages 0)
                (let ((vmem (maiko-lisp.memory:make-virtual-memory nactive-pages)))
                  ;; Initialize FPtoVP mapping from sysout file
                  ;; For now, create identity mapping (file page = virtual page)
                  ;; In full implementation, would read FPtoVP table from sysout file
                  (loop for i from 0 below nactive-pages
                        do (maiko-lisp.memory:map-page vmem i i))
                  (setf (maiko-lisp.vm:vm-virtual-memory vm) vmem))))

            ;; Initialize VM state from IFPAGE
            (setf (maiko-lisp.vm:vm-pc vm) 0)
            (setf (maiko-lisp.vm:vm-stack-ptr vm) 0)

            ;; Set stack base from IFPAGE
            (let ((stack-base (maiko-lisp.data:ifpage-stackbase ifpage))
                  (end-of-stack (maiko-lisp.data:ifpage-endofstack ifpage)))
              (when (and (> stack-base 0) (> end-of-stack 0))
                ;; Stack base and end are set in IFPAGE
                (format t "  Stack base: ~X, End: ~X~%" stack-base end-of-stack)))

            ;; Initialize display if screen geometry is specified
            (let ((display nil))
              (when (gethash "screen-width" options)
                (let ((width (gethash "screen-width" options))
                      (height (gethash "screen-height" options)))
                  (handler-case
                      (progn
                        (setf display (maiko-lisp.display:init-display width height))
                        (format t "Display initialized: ~Ax~A~%" width height))
                    (maiko-lisp.utils:display-error (err)
                      (format t "Display initialization failed: ~A~%" (maiko-lisp.utils:display-error-message err))
                      (format t "Continuing without display...~%")))))

              ;; TODO: Extract bytecode from sysout file
              ;; For now, bytecode extraction requires understanding sysout file format
              ;; This will be implemented when bytecode loading is needed

              ;; For now, create a simple test bytecode array
              ;; This will be replaced with actual sysout bytecode extraction
              (let ((test-code (make-array 10
                                           :element-type 'maiko-lisp.utils:bytecode
                                           :initial-contents '(#xD8 #xD9 #xBF #x68 #x69 0 0 0 0 0))))
                (format t "VM initialized, entering dispatch loop~%")
                (handler-case
                    (progn
                      ;; Main execution loop
                      (loop
                        ;; Poll for events if display is available
                        (when display
                          (let ((events (maiko-lisp.display:poll-events display)))
                            (dolist (event events)
                              ;; Process keyboard events
                              (when (typep event 'maiko-lisp.io:keyboard-event)
                                (maiko-lisp.io:enqueue-key-event event))
                              ;; Process mouse events
                              (when (typep event 'maiko-lisp.io:mouse-event)
                                (maiko-lisp.io:update-mouse-position
                                 (maiko-lisp.io:mouse-x event)
                                 (maiko-lisp.io:mouse-y event)))))
                          ;; Flush display updates
                          (maiko-lisp.display:flush-display-region display 0 0
                                                                    (maiko-lisp.display:display-width display)
                                                                    (maiko-lisp.display:display-height display)))
                        ;; Execute VM dispatch
                        (maiko-lisp.vm:dispatch vm test-code)
                        ;; Check for exit condition (would be set by VM or user)
                        (return)))
                  (maiko-lisp.utils:vm-error (err)
                    (format t "VM error: ~A~%" (maiko-lisp.utils:vm-error-message err))
                    (quit 1))
                  (error (err)
                    (format t "Error: ~A~%" err)
                    (quit 1)))
                (format t "VM execution complete~%")
                ;; Cleanup display
                (when display
                  (maiko-lisp.display:destroy-display display)))))
        (maiko-lisp.utils:sysout-load-failed (err)
          (format t "Failed to load sysout file: ~A~%" (maiko-lisp.utils:sysout-load-failed-message err))
          (quit 1))
        (error (err)
          (format t "Error loading sysout: ~A~%" err)
          (quit 1))))))