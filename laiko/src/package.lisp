(defpackage :maiko-lisp
  (:use :cl)
  (:nicknames :ml)
  (:export
    #:main
    #:print-info
    #:print-help
    #:run-emulator
    #:quit
    #:get-command-line-arguments
    #:getenv))

(defpackage :maiko-lisp.utils
  (:use :cl)
  (:export
    #:lisp-ptr
    #:dlword
    #:bytecode
    #:hiloc
    #:loloc
    #:little-endian-p
    #:+nil-ptr+
    #:+t-ptr+
    #:vm-error
    #:vm-error-message
    #:memory-error
    #:memory-error-message
    #:display-error
    #:display-error-message
    #:io-error
    #:io-error-message
    #:stack-overflow
    #:stack-overflow-message
    #:invalid-address
    #:vm-division-by-zero
    #:vm-arithmetic-error
    #:vm-arithmetic-error-message
    #:sysout-load-failed
    #:sysout-load-failed-message))

(defpackage :maiko-lisp.vm
  (:use :cl)
  (:export
    #:vm
    #:vm-storage
    #:vm-virtual-memory
    #:vm-fptovp
    #:vm-gc
    #:vm-interrupt-state
    #:vm-pc
    #:vm-return-pc
    #:vm-stack
    #:vm-stack-ptr
    #:vm-stack-size
    #:vm-current-frame
    #:stack-frame
    #:dispatch
    #:execute-opcode
    #:handle-opcode
    #:check-interrupts
    #:handle-interrupt
    #:interrupt-state
    #:create-interrupt-state
    #:set-interrupt-flag
    #:clear-interrupt-flag
    #:create-vm
    #:push-stack
    #:pop-stack
    #:get-top-of-stack
    #:set-top-of-stack
    #:allocate-stack-frame
    #:free-stack-frame
    #:call-function
    #:return-from-function
    #:trace-enabled-p
    #:trace-log
    #:trace-error
    #:trace-begin
    #:trace-end
    #:open-trace-file
    #:initialize-byte-opcode-map
    #:*byte-opcode-map*
    #:*max-trace-steps*
    #:*trace-step*
    #:get-emulator-max-steps
    ;; Binding operations
    #:+bind-marker-msb+
    #:+unbound-marker+
    #:get-pvar-slot
    #:set-pvar-slot
    ;; Opcode handlers
    #:handle-nil #:handle-t #:handle-pop #:handle-push #:handle-copy
    #:handle-const-0 #:handle-const-1
    #:handle-iplus2 #:handle-idifference #:handle-itimes2 #:handle-iquo #:handle-irem
    #:handle-iminus #:handle-idivide #:handle-imod #:handle-plus2 #:handle-difference #:handle-times2 #:handle-quotient
    #:handle-car #:handle-cdr #:handle-cons #:handle-rplaca #:handle-rplacd #:handle-createcell #:handle-rplcons
    #:handle-eq #:handle-eql #:handle-lessp #:handle-greaterp #:handle-leq #:handle-geq #:handle-equal #:handle-numequal #:handle-igreaterp
    #:handle-ivar0 #:handle-ivar1 #:handle-ivar2 #:handle-ivar3 #:handle-ivar4 #:handle-ivar5 #:handle-ivar6
    #:handle-pvar0 #:handle-pvar1 #:handle-pvar2 #:handle-pvar3 #:handle-pvar4 #:handle-pvar5 #:handle-pvar6
    #:handle-fvar0 #:handle-fvar1 #:handle-fvar2 #:handle-fvar3 #:handle-fvar4 #:handle-fvar5 #:handle-fvar6
    #:handle-gvar #:handle-arg0 #:handle-myargcount
    #:handle-pvarsetpop0 #:handle-pvarsetpop1 #:handle-pvarsetpop2 #:handle-pvarsetpop3 #:handle-pvarsetpop4 #:handle-pvarsetpop5 #:handle-pvarsetpop6
    #:handle-return #:handle-jump #:handle-jumpif #:handle-jumpifnil
    #:handle-jump0 #:handle-jump1 #:handle-jump2 #:handle-jump3 #:handle-jump4 #:handle-jump5 #:handle-jump6 #:handle-jump7
    #:handle-jump8 #:handle-jump9 #:handle-jump10 #:handle-jump11 #:handle-jump12 #:handle-jump13 #:handle-jump14 #:handle-jump15
    #:handle-fjump0 #:handle-fjump1 #:handle-fjump2 #:handle-fjump3 #:handle-fjump4 #:handle-fjump5 #:handle-fjump6 #:handle-fjump7
    #:handle-fjump8 #:handle-fjump9 #:handle-fjump10 #:handle-fjump11 #:handle-fjump12 #:handle-fjump13 #:handle-fjump14 #:handle-fjump15
    #:handle-tjump0 #:handle-tjump1 #:handle-tjump2 #:handle-tjump3 #:handle-tjump4 #:handle-tjump5 #:handle-tjump6 #:handle-tjump7
    #:handle-tjump8 #:handle-tjump9 #:handle-tjump10 #:handle-tjump11 #:handle-tjump12 #:handle-tjump13 #:handle-tjump14 #:handle-tjump15
    #:handle-jumpx #:handle-fjumpx #:handle-tjumpx
    #:handle-fn0 #:handle-fn1 #:handle-fn2 #:handle-fn3 #:handle-fn4 #:handle-fnx #:handle-applyfn
    #:handle-aref1 #:handle-aset1 #:handle-aref2 #:handle-aset2
    #:handle-getael1 #:handle-getael2 #:handle-setael1 #:handle-setael2
    #:handle-getbasebyte #:handle-putbasebyte #:handle-getbase-n #:handle-getbaseptr-n #:handle-putbase-n #:handle-putbaseptr-n #:handle-addbase
    #:handle-logand #:handle-logior #:handle-logxor #:handle-lognot #:handle-lsh
    #:handle-llsh1 #:handle-llsh8 #:handle-lrsh1 #:handle-lrsh8
    #:handle-logor2 #:handle-logand2 #:handle-logxor2 #:handle-hiloc #:handle-loloc
    #:handle-bind #:handle-unbind #:handle-dunbind #:handle-typep #:handle-fixp #:handle-smallp
    #:handle-charcode #:handle-charn #:handle-store #:handle-unwind
    #:handle-listget #:handle-assoc #:handle-fmemb #:handle-stkscan #:handle-pop-n #:handle-gcref
    #:handle-aconst #:handle-sic #:handle-snic #:handle-sicx #:handle-gconst
    #:handle-makenumber #:handle-cl-equal #:handle-swap #:handle-nop
    #:handle-fplus2 #:handle-fdifference #:handle-ftimes2 #:handle-fquotient
    #:handle-eq-alt #:handle-greaterp-alt #:handle-equal-alt
    #:handle-nth #:handle-nthcdr #:handle-last #:handle-list-length #:handle-append #:handle-reverse
    ;; Handler registry
    #:*opcode-handlers* #:register-opcode-handler #:get-opcode-handler #:initialize-opcode-handlers #:dispatch-opcode
    #:*trace-line-number*))

(defpackage :maiko-lisp.memory
  (:use :cl)
  (:export
    #:storage
    #:make-storage
    #:create-storage
    #:allocate
    #:deallocate
    #:allocate-cons-cell
    #:allocate-array
    #:get-cons-cell
    #:set-cons-cell
    #:put-cons-cell
    #:gc
    #:create-gc
    #:add-ref
    #:del-ref
    #:mark-stack-ref
    #:find-in-hash-table
    #:collect
    #:virtual-memory
    #:get-page-number
    #:get-page-offset
    #:+mds-offset+))

(defpackage :maiko-lisp.data
  (:use :cl)
  (:export
    #:cons-cell
    #:make-cons-cell
    #:cons-car-field
    #:cons-cdr-code
    #:decode-cdr
    #:get-car
    #:get-cdr
    #:set-car
    #:set-cdr
    #:+cdr-nil+
    #:+cdr-indirect+
    #:+cdr-onpage-min+
    #:+cdr-onpage-max+
    #:get-list-length
    #:array-header
    #:function-header
    #:get-start-pc
    #:get-num-args
    #:get-num-locals
    #:get-fvar-offset
    #:get-closure-environment
    #:get-global-value
    #:set-global-value
    #:sysout
    #:ifpage
    #:load-sysout
    #:validate-sysout
    #:read-ifpage
    #:read-fptovp-table
    #:read-page
    #:extract-bytecode-from-vm
    #:get-vm-byte
    #:get-vm-word
    #:ifpage-process-size
    #:ifpage-stackbase
    #:ifpage-currentfxp))

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
    #:maiko-translate-pathname
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
