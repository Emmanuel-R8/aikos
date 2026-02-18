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
                    (:file "op-macros")
                    (:file "dispatch" :depends-on ("op-macros"))
                    (:file "op-stack" :depends-on ("op-macros" "dispatch"))
                    (:file "op-arithmetic" :depends-on ("op-macros" "dispatch"))
                    (:file "op-list" :depends-on ("op-macros" "dispatch"))
                    (:file "op-comparison" :depends-on ("op-macros" "dispatch"))
                    (:file "op-variable" :depends-on ("op-macros" "dispatch"))
                    (:file "op-control" :depends-on ("op-macros" "dispatch"))
                    (:file "op-memory" :depends-on ("op-macros" "dispatch"))
                    (:file "op-logic" :depends-on ("op-macros" "dispatch"))
                    (:file "op-const" :depends-on ("op-macros" "dispatch"))
                    (:file "op-misc" :depends-on ("op-macros" "dispatch"))
                    (:file "op-graphics" :depends-on ("op-macros" "dispatch"))
                    (:file "opcodes-main" :depends-on ("op-macros" "dispatch"))
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
