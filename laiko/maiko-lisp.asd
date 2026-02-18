;;; ASDF system definition for Maiko Lisp
;;; Requires ASDF to be loaded first

(asdf:defsystem "maiko-lisp"
  :description "Maiko emulator implementation in Common Lisp"
  :version "0.1.0"
  :author "Maiko Project"
  :license "Same as Maiko"
  :depends-on ()
  ;; Optional dependencies (will use if available):
  ;; :uiop
  ;; :alexandria
  ;; :cl-sdl3
  ;; :cl-sdl3-image
  
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
                   (:file "bytecode")
                   (:file "frame-extension" :depends-on ("bytecode"))
                   (:file "sysout-utils")
                   (:file "sysout")))
                 (:module "memory"
                  :pathname "memory"
                  :components
                  ((:file "storage")
                   (:file "gc")
                   (:file "virtual")
                   (:file "layout")))
                 (:module "vm"
                  :pathname "vm"
                  :components
                  ((:file "stack")
                   (:file "interrupt")
                   (:file "trace")
                   (:file "dispatch")
                   (:file "op-stack")
                   (:file "op-arithmetic")
                   (:file "op-list")
                   (:file "op-comparison")
                   (:file "op-variable")
                   (:file "op-control")
                   (:file "op-memory")
                   (:file "op-logic")
                   (:file "op-const")
                   (:file "op-misc")
                   (:file "op-graphics")
                   (:file "opcodes-main")
                   (:file "function")))
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
                 (:file "main")))))
