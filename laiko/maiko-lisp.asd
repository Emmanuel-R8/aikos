(defsystem "maiko-lisp"
  :description "Maiko emulator implementation in Common Lisp"
  :version "0.1.0"
  :author "Maiko Project"
  :license "Same as Maiko"
  :depends-on (
               ;; Optional dependencies (will use if available):
               ;; :uiop
               ;; :alexandria
               ;; :cl-sdl3
               ;; :cl-sdl3-image
               )
  :components ((:module "src"
                :components
                ((:file "package")
                 (:module "utils"
                  :pathname "utils"
                  :components
                  ((:file "types")
                   (:file "errors")
                   (:file "address")))
                 (:module "data"
                  :pathname "data"
                  :components
                  ((:file "cons")
                   (:file "array")
                   (:file "function-header")
                   (:file "sysout")))
                 (:module "vm"
                  :pathname "vm"
                  :components
                  ((:file "stack")
                   (:file "dispatch")
                   (:file "opcodes")
                   (:file "function")
                   (:file "interrupt")))
                 (:module "memory"
                  :pathname "memory"
                  :components
                  ((:file "storage")
                   (:file "gc")
                   (:file "virtual")
                   (:file "layout")))
                 (:module "io"
                  :pathname "io"
                  :components
                  ((:file "keyboard")
                   (:file "mouse")
                   (:file "filesystem")))
                 (:module "display"
                  :pathname "display"
                  :components
                  ((:file "sdl-backend")
                   (:file "graphics")
                   (:file "events")))
                 (:file "main"))))
  :in-order-to ((test-op (test-op "maiko-lisp/tests"))))

(defsystem "maiko-lisp/tests"
  :description "Tests for maiko-lisp"
  :depends-on ("maiko-lisp" "fiveam")
  :components ((:module "tests"
                :components
                ((:file "test-suite")
                 (:file "test-vm")
                 (:file "test-memory")
                 (:file "test-opcodes"))))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :maiko-lisp-tests)))
