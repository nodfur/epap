(require :asdf)
(asdf:load-system :cffi)
(asdf:load-system :babel)

(defpackage :epapi
  (:use :common-lisp :cffi :asdf :uiop))

(in-package :epapi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition failed-equality-assertion (error)
  ((actual :initarg :actual :reader assertion-actual)
   (expected :initarg :expected :reader assertion-expected))

  (:report (lambda (condition stream)
             (format stream "Expected: ~A~%  Actual: ~A"
                     (assertion-expected condition)
                     (assertion-actual condition)))))

(define-condition test-ok (condition) ())

(defmacro assert-equalp (actual expected)
  (let ((a (gensym))
        (e (gensym)))
    `(let ((,a ,actual)
           (,e ,expected))
       (if (equalp ,a ,e)
           (signal 'test-ok)
           (error 'failed-equality-assertion :expected ,e :actual ,a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-library epapi
  (:unix (:or "./zig-out/lib/libepapi.so"
              "./zig-out/lib/libepapi.dylib")))

(use-foreign-library epapi)

;; (defcfun "epap_render_text" :uint32
;;   (font (:pointer (:struct font-data)))
;;   (string :string)
;;   (bitmap (:pointer :uint8))
;;   (screen-width :uint32)
;;   (x :uint32)
;;   (y :uint32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcenum (spi-bit-order :uint8)
  :least-significant-bit-first
  :most-significant-bit-first)

(defcenum (spi-data-mode :uint8)
  :mode-0 :mode-1 :mode-2 :mode-3)

(defcenum (spi-clock-divider :uint16)
  (:divider-32 32)
  (:divider-16 16))

(defcenum (gpio-function-select :uint8)
  :input :output)

(defcfun "bcm2835_init" :boolean)
(defcfun "bcm2835_close" :boolean)
(defcfun "bcm2835_spi_begin" :boolean)
(defcfun "bcm2835_spi_end" :boolean)

(defcfun "bcm2835_spi_setBitOrder" :void
  (order spi-bit-order))
(defcfun "bcm2835_spi_setDataMode" :void
  (mode spi-data-mode))
(defcfun "bcm2835_spi_setClockDivider" :void
  (divider spi-clock-divider))

(defcfun "bcm2835_gpio_write" :void
  (pin :uint8)
  (bit :uint8))

(defcfun "bcm2835_gpio_lev" :uint8
  (pin :uint8))

(defcfun "bcm2835_gpio_fsel" :void
  (pin :uint8)
  (mode gpio-function-select))

(defcfun "bcm2835_delay" :void
  (milliseconds :uint32))

(defcfun "bcm2835_delayMicroseconds" :void
  (microseconds :uint64))

(defcfun "bcm2835_spi_transfer" :uint8
  (value :uint8))

(deftype pin ()
  '(member :rst :cs :busy))

(deftype packet-type ()
  '(member :read :write :command))

(deftype command-id ()
  '(member
    :run :standby :sleep :read-register :write-register
    :vcom :dev-info :load-img-area-start :load-img-end
    :display-area :display-area-buf))

(deftype register ()
  '(member
    :I80CPCR
    :LISAR
    :LISAR+2
    :LUTAFSR
    :UP1SR+2
    :BGVR))

(defun register-number (register)
  (ecase register
    (:I80CPCR #x4)
    (:LISAR #x208)
    (:LISAR+2 (+ 2 #x208))
    (:LUTAFSR #x1224)
    (:UP1SR+2 (+ 2 #x138))
    (:BGVR #x1250)))

(defun pin-number (pin)
  (ecase pin
    (:rst 17)
    (:cs 8)
    (:busy 24)))

(defun packet-type-number (packet-type)
  (ecase packet-type
    (:read #x1000)
    (:write #x0000)
    (:command #x6000)))

(defun command-number (command-id)
  (ecase command-id
    (:run #x1)
    (:standby #x2)
    (:sleep #x3)
    (:read-register #x10)
    (:write-register #x11)
    (:vcom #x39)
    (:dev-info #x302)
    (:load-img-area-start #x21)
    (:load-img-end #x22)
    (:display-area #x34)
    (:display-area-buf #x37)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-bcm2835 ()
  (unless (bcm2835-init)
    (error "bcm2835_init failed"))
  (unless (bcm2835-spi-begin)
    (error "bcm2835_spi_begin failed"))

  (bcm2835-spi-setBitOrder :most-significant-bit-first)
  (bcm2835-spi-setDataMode :mode-0)
  (bcm2835-spi-setClockDivider :divider-32))

(defun gpio-write (pin bit)
  (bcm2835-gpio-write (pin-number pin) bit))

(defun close-bcm2835 ()
  (gpio-write :cs 0)
  (gpio-write :rst 0)
  (bcm2835-spi-end)
  (unless (bcm2835-close)
    (error "bcm2835_close failed")))

(defun gpio-mode (pin mode)
  (bcm2835-gpio-fsel (pin-number pin) mode))

(defun gpio-read (pin)
  (bcm2835-gpio-lev (pin-number pin)))

(defun gpio-wait ()
  (loop repeat 10000000
        when (= 1 (gpio-read :busy))
          return t))

(defun delay-milliseconds (ms)
  (bcm2835-delay ms))

(defun delay-microseconds (us)
  (bcm2835-delaymicroseconds us))

(defun spi-write-byte (x)
  (bcm2835-spi-transfer x))

(defun spi-write-word (x)
  (spi-write-byte (ldb (byte 8 8) x))
  (spi-write-byte (ldb (byte 8 0) x)))

(defun cs-low ()
  (gpio-write :cs 0))

(defun cs-high ()
  (gpio-write :cs 1))

(defun initialize-gpio ()
  (gpio-mode :rst :output)
  (gpio-mode :cs :output)
  (gpio-mode :busy :input)
  (cs-high))

(defun start-packet (packet-type)
  (gpio-wait)
  (cs-low)
  (spi-write-word (packet-type-number packet-type))
  (gpio-wait))

(defun write-command (cmd)
  (start-packet :command)
  (spi-write-word (command-number cmd))
  (cs-high))

(defun write-word-packet (word)
  (start-packet :write)
  (spi-write-word word)
  (cs-high))

(defun write-word-packets (word-list)
  (loop for word in word-list
        do (write-word-packet word)))

(defun write-register (register value)
  (write-command :write-register)
  (write-word-packet (register-number register))
  (write-word-packet value))

(defun spi-read-byte ()
  (bcm2835-spi-transfer 0))

(defun spi-read-word ()
  (let* ((high (spi-read-byte))
         (low (spi-read-byte)))
    (dpb high (byte 8 8) low)))

(defun spi-read-address ()
  (let* ((low (spi-read-word))
         (high (spi-read-word)))
    (dpb high (byte 16 16) low)))

(defun spi-read-bytes (count)
  ;; slightly inefficient
  (remove-if #'zerop
             (make-array
              (list count)
              :element-type '(unsigned-byte 8)
              :initial-contents (loop repeat count
                                      collect (spi-read-byte)))))

(defun start-reading ()
  (start-packet :read)
  (spi-read-word) ;; read a dummy word
  (gpio-wait))

(defun request-word ()
  (start-reading)
  (prog1 (spi-read-word)
    (cs-high)))

(defun bpp-pixel-format (bits)
  (ecase bits
    (2 0)
    (3 1)
    (4 2)
    (8 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct system-info
  width height address firmware-version lut-version)

(defun get-system-info ()
  (write-command :dev-info)
  (start-reading)
  (prog1
      (make-system-info
       :width (spi-read-word)
       :height (spi-read-word)
       :address (spi-read-address)
       :firmware-version
       (babel:octets-to-string (spi-read-bytes 16) :encoding :ascii)
       :lut-version
       (babel:octets-to-string (spi-read-bytes 16) :encoding :ascii))
    (cs-high)))

(defun reset-display ()
  (gpio-write :rst 1)
  (delay-milliseconds 200)
  (gpio-write :rst 0)
  (delay-milliseconds 10)
  (gpio-write :rst 1)
  (delay-milliseconds 200))

(defun set-vcom (vcom)
  (write-command :vcom)
  (write-word-packet 1)
  (write-word-packet (round (* 1000 (abs vcom)))))

(defun initialize-display ()
  (reset-display)
  (write-command :run)
  (prog1 (get-system-info)
    (write-register :I80CPCR 1)
    (set-vcom *vcom*)))

(defun enter-sleep-mode ()
  (write-command :sleep))

(defun trace-cmd ()
  (trace write-command write-register write-word-packet))

(defun trace-io ()
  (trace
   spi-write-word spi-write-byte
   spi-read-word spi-read-byte spi-read-address spi-read-bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *vcom* -1.73)
(defvar *display-width*)
(defvar *display-height*)
(defvar *base-address*)
(defvar *c-frame*)

(defun boot ()
  (initialize-bcm2835)
  (initialize-gpio)
  (unwind-protect
       (initialize-display)
    (enter-sleep-mode)
    (close-bcm2835)))

(defvar *text-initialized* nil)

(define-condition epapi-error (error)
  ((function :initarg :function-name :reader epapi-error-function)
   (args :initarg :args :reader epapi-error-args)))

(defmacro try (body)
  (let ((v (gensym))
        (fn (car body))
        (args (cdr body)))
    `(let ((,v (,fn ,@args)))
       (if (= 0 ,v)
           nil
           (error 'epapi-error :function ',fn :args ',args)))))

(defun display-bitmap-size ()
  (/ (* *display-width* *display-height*) 8))

(defun allocate-c-frame ()
  (foreign-alloc :uint8
                 :initial-element #xFF
                 :count (display-bitmap-size)))

(defun byte-vector->bit-array (array width height)
  (let ((bit-array (make-array (list height width) :element-type 'bit)))
    (prog1 bit-array
      (loop
        for y from 0 below height
        do (loop
             for x from 0 below width
             do (let* ((pixel-index (+ (* y width) x))
                       (byte (aref array (truncate pixel-index 8))))
                  (setf (bit bit-array y x)
                        (if (logbitp (mod pixel-index 8) byte) 1 0))))))))

(defun unit-test ()
  (assert-equalp
   (byte-vector->bit-array #(1) 1 8)
   (make-array '(8 1)
               :element-type 'bit
               :initial-contents '((1) (0) (0) (0) (0) (0) (0) (0)))))

;; (defun foo ()
;;   (let* ((*display-width* 32)
;;          (*display-height* 24)
;;          (*c-frame* (allocate-c-frame)))
;;     (try (epap-render-text *font-cozette* "Hey!" *c-frame* *display-width* 0 0))
;;     (let ((array (foreign-array-to-lisp
;;                   *c-frame*
;;                   (list :array :uint8 (display-bitmap-size))
;;                   :element-type :unsigned-byte)))
;;       (prin1 array)
;;       (prog1 (byte-vector->bit-array array *display-width* *display-height*)
;;         (foreign-array-free *c-frame*)))))

;; (defun start-text ()
;;   (try (epap-start-text))
;;   (try (epap-load-font "./fonts/cozette.bdf" 13 *font-cozette*))
;;   (try (epap-load-font "./fonts/DMMono-Regular.ttf" 9 *font-dm-mono*)))

;; (unless *text-initialized*
;;   (start-text)
;;   (setf *text-initialized* t))

;; (defun test-text (font-path font-height width)
;;   (try (epap-render-text *font-cozette* "foo" *c-frame* *display-width* 0 0)))

(unit-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "FT_Error_String" :string (error-code :int32))

(defcfun "FT_Init_FreeType" :int32 (library :pointer))
(defcfun "FT_Done_FreeType" :int32 (library :pointer))

;; 12*4 + 4*4 + 8*2 + 10*4
;; (+ (* 4 12) (* 4 4) (* 8 2) (* 10 4))

(defcstruct (freetype-face)
  (:num-faces :int64)
  (:face-index :int64)
  (:face-flags :int64)
  (:style-flags :int64)
  (:num-glyphs :int64)
  (:family-name :string)
  (:style-name :string)
  (:num-fixed-sizes :int32)
  (:available-sizes :pointer)
  (:num-charmaps :int32)
  (:charmaps :pointer)
  (:generic :pointer)
  (:generic-finalizer :pointer)

  ;; these are only relevant for scalable outlines
  (:bbox (:array :int64 4))
  (:units-per-em :int16)
  (:ascender :int16)
  (:descender :int16)
  (:height :int16)
  (:max-advance-width :int16)
  (:max-advance-height :int16)
  (:underline-position :int16)
  (:underline-thickness :int16)

  (:glyph (:pointer (:struct freetype-glyph)))
  (:size :pointer)
  (:charmap :pointer))

(defcenum (glyph-format :uint32)
  :none
  (:composite #x636f6d70)
  (:bitmap #x62697473)
  (:outline #x6f75746c))

(char-code #\b)

(defcstruct glyph-metrics
  (:width :int64)
  (:height :int64)
  (:horizontal-bearing-x :int64)
  (:horizontal-bearing-y :int64)
  (:horizontal-advance :int64)
  (:vertical-bearing-x :int64)
  (:vertical-bearing-y :int64)
  (:vertical-advance :int64))

(defcstruct freetype-outline
  (:n-contours :int16)
  (:n-points :int16)
  (:points :pointer)
  (:tags :pointer)
  (:contours :pointer)
  (:flags :int32))

(defcstruct freetype-glyph-slot
  (:library :pointer)
  (:face (:pointer (:struct freetype-face)))
  (:next (:pointer (:struct freetype-glyph-slot)))
  (:glyph-index :uint32)
  (:generic :pointer)
  (:generic-finalizer :pointer)
  (:metrics (:struct glyph-metrics))
  (:linear-horizontal-advance :int64)
  (:linear-vertical-advance :int64)
  (:advance-x :int64)
  (:advance-y :int64)
  (:format glyph-format)
  (:bitmap (:struct freetype-bitmap))
  (:bitmap-left :int32)
  (:bitmap-top :int32)
  (:outline (:struct freetype-outline))
  (:num-subglyphs :uint32)
  (:subglyphs :pointer)
  (:control-data :pointer)
  (:control-length :int64)
  (:lsb-delta :int64)
  (:rsb-delta :int64)
  (:other :pointer)
  (:internal :pointer))

(defcstruct freetype-glyph
  (:library :pointer)
  (:class :pointer)
  (:format glyph-format)
  (:advance-x :int64)
  (:advance-y :int64))

(defcenum (pixel-mode :uint8)
  :none :mono :gray :gray2 :gray4 :lcd :lcd-vv :bgra)

(defcstruct freetype-bitmap
  (:rows :uint32)
  (:width :uint32)
  (:pitch :int32)
  (:buffer :pointer)
  (:num-grays :uint16)
  (:pixel-mode pixel-mode)
  (:palette-mode :uint8)
  (:palette :pointer))

(defcfun "FT_New_Face" :int32
  (library :pointer)
  (path :string)
  (face-index :long)
  (face-ptr (:pointer (:struct freetype-face))))

(defcfun "FT_Set_Pixel_Sizes" :int32
  (face :pointer)
  (width :uint32)
  (height :uint32))

(defcfun "FT_Load_Glyph" :int32
  (face :pointer)
  (glyph-index :uint32)
  (load-flags :int32))

(defcenum ft-render-mode
  :normal :light :mono :lcd :lcd-v :sdf)

(defun ft-load-target (mode)
  (ash (foreign-enum-value 'ft-render-mode mode) 16))

(defun load-flag-number (flag)
  (ecase flag
    (:render (ash 1 2))
    (:target-mono (ft-load-target :mono))
    (:force-autohint (ash 1 5))))

(defun make-load-flags (flags)
  (apply #'logior (mapcar #'load-flag-number flags)))

(defcfun "hb_ft_font_create_referenced" :pointer
  (freetype-font :pointer))

(defcfun "hb_ft_font_set_funcs" :void
  (font :pointer))

(defcfun "hb_buffer_create" :pointer)
(defcfun "hb_buffer_destroy" :void (buffer :pointer))

(defcenum hb-direction
  (:invalid 0)
  (:left-to-right 4)
  :right-to-left
  :top-to-bottom
  :bottom-to-top)

(defcfun "hb_buffer_set_direction" :void
  (buffer :pointer)
  (direction hb-direction))

(defcfun "hb_script_from_string" :uint32
  (string :string)
  (len :int))

(defcfun "hb_buffer_set_script" :void
  (buffer :pointer)
  (script :uint32))

(defcfun "hb_buffer_set_language" :void
  (buffer :pointer)
  (language :pointer))

(defcfun "hb_language_from_string" :pointer
  (string :string)
  (len :int))

(defcfun "hb_buffer_add_utf8" :void
  (buffer :pointer)
  (text :string)
  (length :int32)
  (offset :uint32)
  (count :int32))

(defcfun "hb_shape" :void
  (font :pointer)
  (buffer :pointer)
  (features :pointer)
  (num-features :uint32))

(defcstruct (glyph-info :size 20)
  (:codepoint :uint32)
  (:cluster :uint32 :offset 8))

(defcstruct (glyph-position :size 20)
  (:x-advance :int32)
  (:y-advance :int32)
  (:x-offset :int32)
  (:y-offset :int32))

(defcstruct glyph-extents
  (:x-bearing :int32)
  (:y-bearing :int32)
  (:width :int32)
  (:height :int32))

;; (defstruct glyph-info
;;   codepoint cluster)

;; (defstruct glyph-position
;;   x-offset y-offset x-advance y-advance)

;; (defstruct glyph-extents
;;   x-bearing y-bearing width height)

;; (defmethod translate-from-foreign (pointer (type %glyph-info))
;;   (apply #'make-glyph-info (call-next-method)))

;; (defmethod translate-from-foreign (pointer (type %glyph-position))
;;   (apply #'make-glyph-position (call-next-method)))

;; (defmethod translate-from-foreign (pointer (type %glyph-extents))
;;   (apply #'make-glyph-extents (call-next-method)))

(defcfun "hb_buffer_get_glyph_infos"
    (:pointer (:struct glyph-info))
  (buffer :pointer)
  (glyph-count (:pointer :uint32)))

(defcfun "hb_buffer_has_positions" :boolean
  (buffer :pointer))

(defcfun "hb_buffer_get_glyph_positions"
    (:pointer (:struct glyph-position))
  (buffer :pointer)
  (glyph-count (:pointer :uint32)))

(defcfun "hb_font_get_glyph_extents"
    :bool
  (font :pointer)
  (glyph :uint32)
  (extents (:pointer (:struct glyph-extents))))

(define-condition freetype-error (error)
  ((code :initarg :code :reader freetype-error-code)))

(defun check-freetype-result (result)
  (unless (zerop result)
    (error 'freetype-error :code result)))

(defun initialize-freetype ()
  (let ((freetype (foreign-alloc :pointer)))
    (check-freetype-result
     (ft-init-freetype freetype))
    (mem-ref freetype :pointer)))

(defstruct font
  height
  freetype-ptr
  harfbuzz-ptr)

(defun load-font (path height)
  (let* ((freetype-ptr (load-freetype-font path height))
         (harfbuzz-ptr (hb-ft-font-create-referenced freetype-ptr)))
    (hb-ft-font-set-funcs harfbuzz-ptr)
    (make-font
     :height height
     :freetype-ptr freetype-ptr
     :harfbuzz-ptr harfbuzz-ptr)))

(defvar *freetype* (initialize-freetype))

;; (setf *freetype* (initialize-freetype))

(defun load-freetype-font (path height)
  (let ((face-ptr (foreign-alloc :pointer)))
    (check-freetype-result
     (ft-new-face *freetype* path 0 face-ptr))
    (let ((face (mem-ref face-ptr :pointer)))
      (prog1 face
        (check-freetype-result
         (ft-set-pixel-sizes face 0 height))))))

(defvar *font-cozette*
  (load-font "./fonts/cozette.bdf" 13))

(defvar *font-dm-mono*
  (load-font "./fonts/DMMono-Regular.ttf" 9))

(defvar *font-concrete-roman*
  (load-font "./fonts/computer-modern/cmunorm.otf" 10))

(defun shape-text (text &key
                          font
                          (language "en")
                          (direction :left-to-right)
                          (script "Latn"))
  (let ((buffer (hb-buffer-create)))
    (hb-buffer-set-direction buffer direction)
    (hb-buffer-set-script buffer (hb-script-from-string script -1))
    (hb-buffer-set-language buffer (hb-language-from-string language -1))
    (hb-buffer-add-utf8 buffer text -1 0 -1)
    (hb-shape (font-harfbuzz-ptr font) buffer (null-pointer) 0)
    (with-foreign-object (glyph-count :uint32)
      (let* ((glyph-infos
               (foreign-array-to-lisp
                (hb-buffer-get-glyph-infos buffer glyph-count)
                `(:array (:struct glyph-info) ,(mem-aref glyph-count :uint32))))
             (glyph-positions
               (foreign-array-to-lisp
                (hb-buffer-get-glyph-positions buffer glyph-count)
                `(:array (:struct glyph-position) ,(mem-aref glyph-count :uint32)))))
        (prog1
            (list glyph-infos glyph-positions)
          (hb-buffer-destroy buffer))))))

(assert-equalp
 (shape-text "xyz" :font *font-cozette*)
 (list #((:cluster 0 :codepoint 121)
         (:cluster 1 :codepoint 122)
         (:cluster 2 :codepoint 123))
       #((:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384)
         (:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384)
         (:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384))))

(defun read-glyph-slot (font)
  (mem-ref
   (getf
    (mem-ref (font-freetype-ptr font) '(:struct freetype-face))
    :glyph)
   '(:struct freetype-glyph-slot)))

(defun load-glyph (font glyph-id)
  (check-freetype-result
   (ft-load-glyph (font-freetype-ptr font) glyph-id
                  (make-load-flags '(:render :target-mono :force-autohint))))
  (with-foreign-object (extents '(:struct glyph-extents))
    (unless (hb-font-get-glyph-extents (font-harfbuzz-ptr font)
                                       glyph-id extents)
      (error "harfbuzz: failed to get glyph extents for glyph ~A" glyph-id))
    (mem-aref extents '(:struct glyph-extents))))

(assert-equalp
 (load-glyph *font-cozette* 121)
 '(:height -384 :width 320 :y-bearing 384 :x-bearing 64))

(read-glyph-slot *font-concrete-roman*)
