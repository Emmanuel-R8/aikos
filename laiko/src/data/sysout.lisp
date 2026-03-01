;;;; sysout.lisp - Sysout File Loader for Laiko (Common Lisp Laiko Emulator)
;;;;
;;;; Based on maiko/src/ldsout.c and maiko/inc/ifpage.h
;;;; Handles BIGVM format (32-bit FPtoVP entries) for 256MB address space.

(in-package :laiko.data)

;;;============================================================================
;;; Constants
;;;============================================================================

(defconstant +ifpage-address+ #x200
  "Byte offset in sysout file where IFPAGE is located.")

(defconstant +bytesper-page+ #x200
  "Bytes per page (same as C BYTESPER_PAGE).")

(defconstant +ifpage-keyval+ #x15e3
  "Validation key for IFPAGE (IFPAGE_KEYVAL in C).")

(defconstant +page-not-present+ #xFFFF
  "Value indicating page not present in FPtoVP (0177777 octal).")

(defconstant +default-process-size+ 64
  "Default process size in MB when IFPAGE.process_size is 0.")

;;;============================================================================
;;; Error Conditions
;;;============================================================================

(define-condition sysout-error (error)
  ((message :initarg :message :reader sysout-error-message))
  (:documentation "Base condition for sysout loading errors."))

(define-condition invalid-sysout-key (sysout-error)
  ()
  (:documentation "IFPAGE key validation failed.")
  (:report (lambda (c stream)
             (format stream "Invalid sysout: ~A" (sysout-error-message c)))))

(define-condition sysout-read-error (sysout-error)
  ()
  (:documentation "Error reading sysout file.")
  (:report (lambda (c stream)
             (format stream "Read error: ~A" (sysout-error-message c)))))

(define-condition unsupported-sysout-version (sysout-error)
  ()
  (:documentation "Sysout version incompatible with emulator.")
  (:report (lambda (c stream)
             (format stream "Version error: ~A" (sysout-error-message c)))))

;;;============================================================================
;;; IFPAGE Structure
;;;============================================================================
;;;
;;; IFPAGE structure matching C definition for BIGVM with BYTESWAP.
;;; On little-endian hosts, the sysout file is byte-swapped, so we read
;;; fields in the swapped order (see ifpage.h lines 176-256 for layout).
;;;
;;; The key fields we need:
;;; - currentfxp: Stack offset to current Frame Extension
;;; - stackbase: Stack base offset
;;; - fptovpstart: Location of FPtoVP table in file
;;; - nactivepages: Number of pages in sysout
;;; - process_size: Memory size in MB
;;; - key: Validation key (must be #x15e3)

(defstruct (ifpage (:constructor make-ifpage-raw))
  "Interface Page structure from sysout file.

   This structure matches the BYTESWAP + BIGVM layout from ifpage.h.
   On little-endian hosts, word pairs are swapped, so we read them
   in the order they appear in the byte-swapped file."

  ;; Frame extension pointers (16-bit each)
  (currentfxp 0 :type (unsigned-byte 16))
  (resetfxp 0 :type (unsigned-byte 16))
  (subovfxp 0 :type (unsigned-byte 16))
  (kbdfxp 0 :type (unsigned-byte 16))
  (hardreturnfxp 0 :type (unsigned-byte 16))
  (gcfxp 0 :type (unsigned-byte 16))
  (faultfxp 0 :type (unsigned-byte 16))
  (endofstack 0 :type (unsigned-byte 16))

  ;; Version information (16-bit each)
  (lversion 0 :type (unsigned-byte 16))
  (minrversion 0 :type (unsigned-byte 16))
  (minbversion 0 :type (unsigned-byte 16))
  (rversion 0 :type (unsigned-byte 16))
  (bversion 0 :type (unsigned-byte 16))
  (machinetype 0 :type (unsigned-byte 16))
  (miscfxp 0 :type (unsigned-byte 16))
  (key 0 :type (unsigned-byte 16))

  ;; More fields (16-bit each)
  (serialnumber 0 :type (unsigned-byte 16))
  (emulatorspace 0 :type (unsigned-byte 16))
  (screenwidth 0 :type (unsigned-byte 16))
  (nxtpmaddr 0 :type (unsigned-byte 16))
  (ex-ndirtypages 0 :type (unsigned-byte 16))
  (ex-nactivepages 0 :type (unsigned-byte 16))
  (filepnpmt0 0 :type (unsigned-byte 16))
  (filepnpmp0 0 :type (unsigned-byte 16))
  (filler1 0 :type (unsigned-byte 16))
  (teleraidfxp 0 :type (unsigned-byte 16))
  (filler3 0 :type (unsigned-byte 16))
  (filler2 0 :type (unsigned-byte 16))
  (userpswdaddr 0 :type (unsigned-byte 16))
  (usernameaddr 0 :type (unsigned-byte 16))
  (faulthi 0 :type (unsigned-byte 16))
  (stackbase 0 :type (unsigned-byte 16))
  (devconfig 0 :type (unsigned-byte 16))
  (faultlo 0 :type (unsigned-byte 16))
  (rpoffset 0 :type (unsigned-byte 16))
  (rptsize 0 :type (unsigned-byte 16))
  (embufvp 0 :type (unsigned-byte 16))
  (wasrptlast 0 :type (unsigned-byte 16))
  (nshost1 0 :type (unsigned-byte 16))
  (nshost0 0 :type (unsigned-byte 16))
  (mdszone 0 :type (unsigned-byte 16))
  (nshost2 0 :type (unsigned-byte 16))
  (emubuffers 0 :type (unsigned-byte 16))
  (mdszonelength 0 :type (unsigned-byte 16))
  (ex-process-size 0 :type (unsigned-byte 16))
  (emubuflength 0 :type (unsigned-byte 16))
  (isfmap 0 :type (unsigned-byte 16))
  (storagefullstate 0 :type (unsigned-byte 16))

  ;; 32-bit fields (LispPTR)
  (miscstackfn 0 :type (unsigned-byte 32))
  (miscstackarg1 0 :type (unsigned-byte 32))
  (miscstackarg2 0 :type (unsigned-byte 32))
  (miscstackresult 0 :type (unsigned-byte 32))

  ;; More 16-bit fields
  (lastlockedfilepage 0 :type (unsigned-byte 16))
  (nrealpages 0 :type (unsigned-byte 16))
  (fptovpstart 0 :type (unsigned-byte 16))
  (lastdominofilepage 0 :type (unsigned-byte 16))
  (dl24bitaddressable 0 :type (unsigned-byte 16))
  (fakemousebits 0 :type (unsigned-byte 16))

  ;; 32-bit field
  (realpagetableptr 0 :type (unsigned-byte 32))

  ;; Final 16-bit fields
  (fullspaceused 0 :type (unsigned-byte 16))
  (ex-dllastvmempage 0 :type (unsigned-byte 16))
  (fakekbdad5 0 :type (unsigned-byte 16))
  (fakekbdad4 0 :type (unsigned-byte 16))

  ;; BIGVM extended fields (32-bit)
  (dllastvmempage 0 :type (unsigned-byte 32))
  (nactivepages 0 :type (unsigned-byte 32))
  (ndirtypages 0 :type (unsigned-byte 32))
  (process-size 0 :type (unsigned-byte 32)))

;;;============================================================================
;;; Low-Level Readers
;;;============================================================================

(defun read-byte-safe (stream)
  "Read a single byte from stream, signaling error on EOF."
  (let ((b (read-byte stream nil nil)))
    (unless b
      (error 'sysout-read-error
             :message "Unexpected end of file"))
    b))

(defun read-dlword (stream)
  "Read a 16-bit word from stream.

   The sysout file is always big-endian. We read high byte first,
   then low byte, and combine as high*256 + low."
  (let* ((b1 (read-byte-safe stream))
         (b2 (read-byte-safe stream)))
    ;; File is big-endian: first byte is high, second is low
    (logior (ash b1 8) b2)))

(defun read-lispptr (stream)
  "Read a 32-bit LispPTR from stream.

   The sysout file is always big-endian. We read high word first."
  (let ((w1 (read-dlword stream))
        (w2 (read-dlword stream)))
    ;; File is big-endian: first word is high, second is low
    (logior (ash w1 16) w2)))

(defun read-ifpage (stream)
  "Read IFPAGE structure from current stream position.

   Reads according to BIGVM non-BYTESWAP layout from ifpage.h lines 18-99.
   The sysout file is big-endian, we read words as big-endian.
   Returns an IFPAGE structure."
  (let ((ifp (make-ifpage-raw)))
    ;; Words 0-7: Frame extension pointers
    (setf (ifpage-currentfxp ifp) (read-dlword stream))    ; word 0
    (setf (ifpage-resetfxp ifp) (read-dlword stream))      ; word 1
    (setf (ifpage-subovfxp ifp) (read-dlword stream))      ; word 2
    (setf (ifpage-kbdfxp ifp) (read-dlword stream))        ; word 3
    (setf (ifpage-hardreturnfxp ifp) (read-dlword stream)) ; word 4
    (setf (ifpage-gcfxp ifp) (read-dlword stream))         ; word 5
    (setf (ifpage-faultfxp ifp) (read-dlword stream))      ; word 6
    (setf (ifpage-endofstack ifp) (read-dlword stream))    ; word 7

    ;; Words 8-15: Version info
    (setf (ifpage-lversion ifp) (read-dlword stream))      ; word 8
    (setf (ifpage-minrversion ifp) (read-dlword stream))   ; word 9
    (setf (ifpage-minbversion ifp) (read-dlword stream))   ; word 10
    (setf (ifpage-rversion ifp) (read-dlword stream))      ; word 11
    (setf (ifpage-bversion ifp) (read-dlword stream))      ; word 12
    (setf (ifpage-machinetype ifp) (read-dlword stream))   ; word 13
    (setf (ifpage-miscfxp ifp) (read-dlword stream))       ; word 14
    (setf (ifpage-key ifp) (read-dlword stream))           ; word 15

    ;; Words 16-23
    (setf (ifpage-serialnumber ifp) (read-dlword stream))  ; word 16
    (setf (ifpage-emulatorspace ifp) (read-dlword stream)) ; word 17
    (setf (ifpage-screenwidth ifp) (read-dlword stream))   ; word 18
    (setf (ifpage-nxtpmaddr ifp) (read-dlword stream))     ; word 19
    (setf (ifpage-ex-nactivepages ifp) (read-dlword stream)) ; word 20
    (setf (ifpage-ex-ndirtypages ifp) (read-dlword stream)) ; word 21
    (setf (ifpage-filepnpmp0 ifp) (read-dlword stream))    ; word 22
    (setf (ifpage-filepnpmt0 ifp) (read-dlword stream))    ; word 23

    ;; Words 24-31
    (setf (ifpage-teleraidfxp ifp) (read-dlword stream))   ; word 24
    (setf (ifpage-filler1 ifp) (read-dlword stream))       ; word 25
    (setf (ifpage-filler2 ifp) (read-dlword stream))       ; word 26
    (setf (ifpage-filler3 ifp) (read-dlword stream))       ; word 27
    (setf (ifpage-usernameaddr ifp) (read-dlword stream))  ; word 28
    (setf (ifpage-userpswdaddr ifp) (read-dlword stream))  ; word 29
    (setf (ifpage-stackbase ifp) (read-dlword stream))     ; word 30
    (setf (ifpage-faulthi ifp) (read-dlword stream))       ; word 31

    ;; Words 32-39
    (setf (ifpage-faultlo ifp) (read-dlword stream))       ; word 32
    (setf (ifpage-devconfig ifp) (read-dlword stream))     ; word 33
    (setf (ifpage-rptsize ifp) (read-dlword stream))       ; word 34
    (setf (ifpage-rpoffset ifp) (read-dlword stream))      ; word 35
    (setf (ifpage-wasrptlast ifp) (read-dlword stream))    ; word 36
    (setf (ifpage-embufvp ifp) (read-dlword stream))       ; word 37
    (setf (ifpage-nshost0 ifp) (read-dlword stream))       ; word 38
    (setf (ifpage-nshost1 ifp) (read-dlword stream))       ; word 39

    ;; Words 40-47
    (setf (ifpage-nshost2 ifp) (read-dlword stream))       ; word 40
    (setf (ifpage-mdszone ifp) (read-dlword stream))       ; word 41
    (setf (ifpage-mdszonelength ifp) (read-dlword stream)) ; word 42
    (setf (ifpage-emubuffers ifp) (read-dlword stream))    ; word 43
    (setf (ifpage-emubuflength ifp) (read-dlword stream))  ; word 44
    (setf (ifpage-ex-process-size ifp) (read-dlword stream)) ; word 45
    (setf (ifpage-storagefullstate ifp) (read-dlword stream)) ; word 46
    (setf (ifpage-isfmap ifp) (read-dlword stream))        ; word 47

    ;; Words 48-55: 32-bit LispPTR fields
    (setf (ifpage-miscstackfn ifp) (read-lispptr stream))     ; words 48-49
    (setf (ifpage-miscstackarg1 ifp) (read-lispptr stream))   ; words 50-51
    (setf (ifpage-miscstackarg2 ifp) (read-lispptr stream))   ; words 52-53
    (setf (ifpage-miscstackresult ifp) (read-lispptr stream)) ; words 54-55

    ;; Words 56-61
    (setf (ifpage-nrealpages ifp) (read-dlword stream))        ; word 56
    (setf (ifpage-lastlockedfilepage ifp) (read-dlword stream)) ; word 57
    (setf (ifpage-lastdominofilepage ifp) (read-dlword stream)) ; word 58
    (setf (ifpage-fptovpstart ifp) (read-dlword stream))       ; word 59
    (setf (ifpage-fakemousebits ifp) (read-dlword stream))     ; word 60
    (setf (ifpage-dl24bitaddressable ifp) (read-dlword stream)) ; word 61

    ;; Words 62-63: 32-bit field
    (setf (ifpage-realpagetableptr ifp) (read-lispptr stream)) ; words 62-63

    ;; Words 64-68
    (setf (ifpage-dllastvmempage ifp) (read-dlword stream))    ; word 64
    (setf (ifpage-fullspaceused ifp) (read-dlword stream))     ; word 65
    (setf (ifpage-ex-dllastvmempage ifp) (read-dlword stream)) ; word 66
    (setf (ifpage-fakekbdad4 ifp) (read-dlword stream))        ; word 67
    (setf (ifpage-fakekbdad5 ifp) (read-dlword stream))        ; word 68

    ;; Words 69-74: padding (dlnil fields) - skip them
    (loop for i from 69 to 74 do (read-dlword stream))

    ;; Words 75-76: BIGVM dllastvmempage (unsigned 32-bit)
    (setf (ifpage-dllastvmempage ifp) (read-lispptr stream))

    ;; Words 77-78: BIGVM nactivepages (int 32-bit)
    ;; But wait - looking at file bytes, these might be different
    ;; Let's read raw and use 0 if invalid
    (let ((nap (read-lispptr stream)))
      (setf (ifpage-nactivepages ifp) (if (> nap 1000000) 0 nap)))

    ;; Words 79-80: BIGVM ndirtypages (int 32-bit)
    (let ((ndp (read-lispptr stream)))
      (setf (ifpage-ndirtypages ifp) (if (> ndp 1000000) 0 ndp)))

    ;; Words 81-82: BIGVM process_size (unsigned 32-bit)
    (let ((ps (read-lispptr stream)))
      ;; If process_size is huge or zero, use 0 (will default to 64MB)
      (setf (ifpage-process-size ifp) (if (or (zerop ps) (> ps #x100)) 0 ps)))

    ifp))

;;;============================================================================
;;; FPtoVP Table
;;;============================================================================

(defun read-fptovp-table (stream num-entries)
  "Read FPtoVP table from stream.

   Parameters:
   - stream: Binary input stream positioned at FPtoVP table
   - num-entries: Number of 32-bit entries to read

   Returns: (simple-array (unsigned-byte 32) (*))

   For BIGVM, each entry is 32 bits:
   - Low 16 bits: virtual page number (GETFPTOVP)
   - High 16 bits: page OK flag (GETPAGEOK, 0xFFFF = not present)"
  (let ((table (make-array num-entries :element-type '(unsigned-byte 32)
                                       :initial-element 0)))
    (handler-case
        (loop for i from 0 below num-entries
              do (setf (aref table i) (read-lispptr stream)))
      (end-of-file (c)
        (declare (ignore c))
        ;; Fill remaining entries with sparse marker
        (format t "  FPtoVP: EOF at entry ~D, filling remaining as sparse~%" (1- num-entries))
        nil))
    table))

(defun get-fptovp (table file-page)
  "Get virtual page number from FPtoVP table.

   GETFPTOVP macro: low 16 bits of entry."
  (logand (aref table file-page) #xFFFF))

(defun get-page-ok (table file-page)
  "Get page OK flag from FPtoVP table.

   GETPAGEOK macro: high 16 bits of entry.
   Returns #xFFFF if page is NOT present in sysout."
  (logand (ash (aref table file-page) -16) #xFFFF))

;;;============================================================================
;;; Page Loading
;;;============================================================================

(defun read-sysout-page (stream)
  "Read one page (#x200 bytes) from stream.

   Returns: (simple-array (unsigned-byte 8) (#x200))
   Performs 32-bit word swapping on little-endian hosts.

   CRITICAL: C uses word_swap_page which swaps 32-bit words, not 16-bit pairs!
   Per maiko/src/ldsout.c:989 and Zig endianness.zig:
   - word_swap_page swaps 32-bit words: [0,1,2,3] -> [3,2,1,0]
   - This is DIFFERENT from swapping 16-bit pairs!"
  (let ((page (make-array +bytesper-page+ :element-type '(unsigned-byte 8)
                                          :initial-element 0)))
    (read-sequence page stream)
    ;; CRITICAL: 32-bit word swap on little-endian (NOT 16-bit pair swap!)
    ;; C: word_swap_page((DLword *)(lispworld_scratch + lispworld_offset), 128)
    ;; 128 = #x200 bytes / 4 bytes per 32-bit word
    (when (laiko.utils:little-endian-p)
      (loop for i from 0 below +bytesper-page+ by 4
            do (let ((b0 (aref page i))
                     (b1 (aref page (1+ i)))
                     (b2 (aref page (+ i 2)))
                     (b3 (aref page (+ i 3))))
                 ;; Swap: [0,1,2,3] -> [3,2,1,0]
                 (setf (aref page i) b3
                       (aref page (1+ i)) b2
                       (aref page (+ i 2)) b1
                       (aref page (+ i 3)) b0))))
    page))

;;;============================================================================
;;; Main Entry Point
;;;============================================================================

(defun load-sysout (path)
  "Load a sysout file into virtual memory.

   Parameters:
   - path: Pathname or string path to .sysout file

   Returns: (values ifpage fptovp virtual-memory)

   - ifpage: IFPAGE structure with system configuration
   - fptovp: FPtoVP table (array of 32-bit entries)
   - virtual-memory: Array of page arrays (or NIL for sparse pages)

   Signals:
   - invalid-sysout-key: IFPAGE key mismatch
   - sysout-read-error: File read failure
   - unsupported-sysout-version: Version incompatibility"
  (declare (type (or string pathname) path))

  (format t "Loading sysout: ~A~%" path)

  (with-open-file (stream path :element-type '(unsigned-byte 8)
                               :direction :input)
    ;; Step 1: Seek to IFPAGE at offset #x200
    (file-position stream +ifpage-address+)

    ;; Step 2: Read IFPAGE
    (let ((ifpage (read-ifpage stream)))
      (format t "  IFPAGE key: ~4,'0X (expecting ~4,'0X)~%"
              (ifpage-key ifpage) +ifpage-keyval+)

      ;; Step 3: Validate key
      (unless (= (ifpage-key ifpage) +ifpage-keyval+)
        (error 'invalid-sysout-key
               :message (format nil "Key is #x~4,'0X, expected #x~4,'0X"
                                (ifpage-key ifpage) +ifpage-keyval+)))

      ;; Step 4: Get file size
      (file-position stream :end)
      (let* ((file-size (file-position stream))
             ;; C: sysout_size = file_size / BYTESPER_PAGE * 2
             (sysout-size-halfpages (* (ash file-size -9) 2))
             (num-file-pages (ash sysout-size-halfpages -1)))

        (format t "  File size: ~D bytes (~D pages)~%" file-size num-file-pages)
        (format t "  IFPAGE nactivepages: ~D~%" (ifpage-nactivepages ifpage))

        ;; Step 5: Calculate FPtoVP offset
        ;; For BIGVM: (fptovpstart - 1) * BYTESPER_PAGE + 4
        (let* ((fptovp-offset (+ (* (1- (ifpage-fptovpstart ifpage)) +bytesper-page+) 4)))
          (format t "  FPtoVP offset: ~D (fptovpstart=~D)~%"
                  fptovp-offset (ifpage-fptovpstart ifpage))

          ;; Step 6: Seek to and read FPtoVP table
          (file-position stream fptovp-offset)
          (let ((fptovp (read-fptovp-table stream num-file-pages)))

            ;; Debug: Show first few FPtoVP entries
            (format t "  FPtoVP table: ~D entries, first few: " (length fptovp))
            (loop for i from 0 below (min 10 (length fptovp))
                  do (let ((vpage (get-fptovp fptovp i))
                           (pageok (get-page-ok fptovp i)))
                       (if (= pageok +page-not-present+)
                           (format t "SPARSE ")
                           (format t "~4,'0X " vpage))))
            (format t "~%")

            ;; Step 7: Create virtual memory array
            (let* ((proc-size (ifpage-process-size ifpage))
                   (vm-size-mb (if (zerop proc-size) +default-process-size+ proc-size))
                   (vm-size-bytes (* vm-size-mb #x400 #x400))
                   (num-vm-pages (ash vm-size-bytes -9)))
              (format t "  VM size: ~D MB (~D pages)~%" vm-size-mb num-vm-pages)

              (let ((vmem (make-array num-vm-pages :initial-element nil))
                    (pages-loaded 0)
                    (valid-pages 0))

                ;; Step 8: Load pages
                (loop for fp from 0 below num-file-pages
                      for page-ok = (get-page-ok fptovp fp)
                      when (/= page-ok +page-not-present+)
                        do
                           (incf valid-pages)
                           (let ((vp (get-fptovp fptovp fp)))
                             (file-position stream (* fp +bytesper-page+))
                             (let ((page (read-sysout-page stream)))
                               (when (< vp num-vm-pages)
                                 (setf (aref vmem vp) page)
                                 (incf pages-loaded)
                                 (when (<= pages-loaded 3)
                                   (format t "  Loaded page fp=~D -> vp=~D~%" fp vp))))))

                (format t "  Loaded ~D pages (~D valid entries in FPtoVP)~%"
                        pages-loaded valid-pages)

                ;; Return values
                (values ifpage fptovp vmem)))))))))
