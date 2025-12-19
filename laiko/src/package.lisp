(defpackage :maiko-lisp
  (:use :cl)
  (:nicknames :ml)
  (:export
   ;; Main entry point
   #:main

   ;; VM core
   #:vm
   #:dispatch
   #:execute-opcode

   ;; Stack
   #:stack-frame
   #:allocate-stack-frame
   #:free-stack-frame

   ;; Memory
   #:storage
   #:gc
   #:virtual-memory

   ;; Data structures
   #:cons-cell
   #:array-header
   #:function-header
   #:sysout

   ;; Types
   #:lisp-ptr
   #:dlword
   #:bytecode

   ;; Errors
   #:vm-error
   #:memory-error
   #:display-error
   #:io-error
   #:stack-overflow
   #:invalid-address
   #:sysout-load-failed))

(defpackage :maiko-lisp.utils
  (:use :cl)
  (:export
   #:lisp-ptr
   #:dlword
   #:bytecode
   #:hiloc
   #:loloc
   #:vm-error
   #:memory-error
   #:display-error
   #:io-error))

(defpackage :maiko-lisp.vm
  (:use :cl)
  (:export
   #:vm
   #:stack-frame
   #:dispatch
   #:execute-opcode
   #:handle-opcode
   #:check-interrupts
   #:handle-interrupt
   #:interrupt-state
   #:make-interrupt-state
   #:set-interrupt-flag
   #:clear-interrupt-flag
   #:make-vm
   #:push-stack
   #:pop-stack
   #:get-top-of-stack
   #:set-top-of-stack
   #:allocate-stack-frame
   #:free-stack-frame
   #:call-function
   #:return-from-function))

(defpackage :maiko-lisp.memory
  (:use :cl)
  (:export
   #:storage
   #:make-storage
   #:allocate
   #:deallocate
   #:allocate-cons-cell
   #:allocate-array
   #:get-cons-cell
   #:set-cons-cell
   #:gc
   #:make-gc
   #:add-ref
   #:del-ref
   #:mark-stack-ref
   #:find-in-hash-table
   #:collect
   #:virtual-memory
   #:get-page-number
   #:get-page-offset
   #:make-storage
   #:make-gc
   #:allocate
   #:allocate-cons-cell
   #:add-ref
   #:del-ref
   #:mark-stack-ref
   #:collect))

(defpackage :maiko-lisp.data
  (:use :cl)
  (:export
   #:cons-cell
   #:make-cons-cell
   #:cons-car-field
   #:cons-cdr-code
   #:decode-cdr
   #:get-car
   #:set-car
   #:+cdr-nil+
   #:+cdr-indirect+
   #:+cdr-onpage-min+
   #:+cdr-onpage-max+
   #:array-header
   #:function-header
   #:get-start-pc
   #:get-num-args
   #:get-num-locals
   #:sysout
   #:ifpage
   #:load-sysout
   #:validate-sysout))

(defpackage :maiko-lisp.io
  (:use :cl)
  (:export
   #:keyboard-event
   #:make-keyboard-event
   #:kbd-keycode
   #:kbd-modifiers
   #:kbd-pressed-p
   #:translate-keycode
   #:enqueue-key-event
   #:dequeue-key-event
   #:mouse-event
   #:make-mouse-event
   #:mouse-x
   #:mouse-y
   #:mouse-buttons
   #:mouse-pressed-p
   #:translate-mouse-event
   #:update-mouse-position
   #:get-mouse-position
   #:translate-pathname
   #:open-file
   #:close-file))

(defpackage :maiko-lisp.display
  (:use :cl)
  (:export
   #:display-interface
   #:make-display-interface
   #:display-window
   #:display-renderer
   #:display-width
   #:display-height
   #:display-buffer
   #:init-display
   #:destroy-display
   #:render-region
   #:bitblt
   #:flush-display-region
   #:poll-events
   #:wait-for-event))

(defpackage :maiko-lisp-tests
  (:use :cl :maiko-lisp.vm :maiko-lisp.memory :maiko-lisp.data :maiko-lisp.utils)
  (:export
   #:run-opcode-tests
   #:run-stack-tests
   #:run-dispatch-tests
   #:run-memory-tests
   #:run-gc-tests
   #:run-sysout-tests))
