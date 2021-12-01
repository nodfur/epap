(require :asdf)
(asdf:load-system :cffi)
(asdf:load-system :babel)

(defpackage :epapi
  (:use :common-lisp :cffi :asdf :uiop))

(in-package :epapi)

(define-foreign-library epapi
  (:unix (:or "./zig-out/lib/libepapi.so"
              "./zig-out/lib/libepapi.dylib"))
  (:darwin "./zig-out/lib/libepapi.dylib"))

(use-foreign-library epapi)

(defun reload ()
  (uiop:run-program "make" :output t)
  (use-foreign-library epapi))

(defcfun "epap_start_broadcom" :uint32)
(defcfun "epap_stop_broadcom" :uint32)
(defcfun "epap_start_text" :uint32)

(defcstruct display-info
  (panel-width :uint16)
  (panel-height :uint16)
  (base-address :uint32)
  (firmware-version :char :count 16)
  (lut-version :char :count 16))

(defcstruct font-data
  (freetype-ptr :pointer)
  (harfbuzz-ptr :pointer))

(defcfun "epap_start_display" :uint32
  (vcom :double)
  (info (:pointer (:struct display-info))))

(defcfun "epap_sleep" :uint32)

(defcfun "epap_clear" :uint32
  (info (:pointer (:struct display-info)))
  (byte :uint8)
  (mode :uint8))

(defcfun "epap_load_font" :uint32
  (path :string)
  (height :uint32)
  (font (:pointer (:struct font-data))))

(defcfun "epap_render_text" :uint32
  (font (:pointer (:struct font-data)))
  (string :string)
  (bitmap (:pointer :uint8))
  (screen-width :uint32)
  (x :uint32)
  (y :uint32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "bcm2835_gpio_write" :void
  (pin :uint8)
  (bit :uint8))

(defcfun "bcm2835_gpio_lev" :uint8
  (pin :uint8))

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

(defun gpio-write (pin bit)
  (bcm2835-gpio-write (pin-number pin) bit))

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

(defun initialize-display (vcom)
  (reset-display)
  (write-command :run)
  (prog1 (get-system-info)
    (write-register :I80CPCR 1)
    (set-vcom vcom)))

(defun trace-cmd ()
  (trace write-command write-register write-word-packet))

(defun trace-io ()
  (trace
   spi-write-word spi-write-byte
   spi-read-word spi-read-byte spi-read-address spi-read-bytes))

(defun boot ()
  (epap-start-broadcom)
  (unwind-protect
       (initialize-display -1.73)
    (epap-sleep)
    (epap-stop-broadcom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *display-width*)
(defvar *display-height*)
(defvar *base-address*)
(defvar *c-frame*)

(defvar *text-initialized* nil)

(defvar *font-cozette*
  (foreign-alloc '(:struct font-data)))

(define-condition epapi-error (error)
  ((function :initarg :function-name :reader epapi-error-function)
   (args :initarg :args :reader epapi-error-args)))

(define-condition failed-equality-assertion (error)
  ((actual :initarg :actual :reader assertion-actual)
   (expected :initarg :expected :reader assertion-expected))

  (:report (lambda (condition stream)
             (format stream "Expected: ~A~%  Actual: ~A"
                     (assertion-expected condition)
                     (assertion-actual condition)))))


(define-condition test-ok (condition) ())

(defmacro try (body)
  (let ((v (gensym))
        (fn (car body))
        (args (cdr body)))
    `(let ((,v (,fn ,@args)))
       (if (= 0 ,v)
           nil
           (error 'epapi-error :function ',fn :args ',args)))))

(defun read-display-info ()
  (with-foreign-object (info '(:struct display-info))
    (prog1 info
      (try (epap-start-display -1.73d0 info))
      (with-foreign-slots
          ((panel-width panel-height base-address)
           info (:struct display-info))
        (setf *display-width* panel-width
              *display-height* panel-height
              *base-address* base-address)))))

(defun display-bitmap-size ()
  (/ (* *display-width* *display-height*) 8))

(defun allocate-c-frame ()
  (foreign-alloc :uint8
                 :initial-element #xFF
                 :count (display-bitmap-size)))

(defun test ()
  (try (epap-start-broadcom))
  (let ((info (read-display-info)))
    (print :clearing-white-init)
    (try (epap-clear info #xff 0)))
  (setf *c-frame* (allocate-c-frame))
  (print :sleeping)
  (try (epap-sleep))
  (try (epap-stop-broadcom)))

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

(defmacro assert-equalp (actual expected)
  (let ((a (gensym))
        (e (gensym)))
    `(let ((,a ,actual)
           (,e ,expected))
       (if (equalp ,a ,e)
           (signal 'test-ok)
           (error 'failed-equality-assertion :expected ,e :actual ,a)))))

(defun unit-test ()
  (assert-equalp
   (byte-vector->bit-array #(1) 1 8)
   (make-array '(8 1)
               :element-type 'bit
               :initial-contents '((1) (0) (0) (0) (0) (0) (0) (0)))))

(defun foo ()
  (let* ((*display-width* 32)
         (*display-height* 24)
         (*c-frame* (allocate-c-frame)))
    (try (epap-render-text *font-cozette* "Hey!" *c-frame* *display-width* 0 0))
    (let ((array (foreign-array-to-lisp
                  *c-frame*
                  (list :array :uint8 (display-bitmap-size))
                  :element-type :unsigned-byte)))
      (prin1 array)
      (prog1 (byte-vector->bit-array array *display-width* *display-height*)
        (foreign-array-free *c-frame*)))))

(defun start-text ()
  (try (epap-start-text))
  (try (epap-load-font "./fonts/cozette.bdf" 13 *font-cozette*)))

(unless *text-initialized*
  (start-text)
  (setf *text-initialized* t))

(defun test-text (font-path font-height width)
  (try (epap-render-text *font-cozette* "foo" *c-frame* *display-width* 0 0)))

(unit-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "FT_Init_FreeType" :int32 (library :pointer))
(defcfun "FT_Done_FreeType" :int32 (library :pointer))

(defcfun "FT_New_Face" :int32
  (library :pointer)
  (path :string)
  (face-index :long)
  (face-ptr :pointer))

(defcfun "FT_Set_Pixel_Sizes" :int32
  (face :pointer)
  (width :uint32)
  (height :uint32))

(defcfun "hb_ft_font_create_referenced" :pointer
  (freetype-font :pointer))

(defcfun "hb_ft_font_set_funcs" :void
  (font :pointer))

(defcfun "hb_buffer_create" :pointer)

(defcenum hb-direction
  :invalid :left-to-right :right-to-left :top-to-bottom :bottom-to-top)

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

(defcstruct glyph-info
  (codepoint :uint32)
  (cluster :uint32))

(defcstruct glyph-position
  (x-advance :int32)
  (y-advance :int32)
  (x-offset :int32)
  (y-offset :int32))

(defcfun "hb_buffer_get_glyph_infos"
    (:pointer (:struct glyph-info))
  (buffer :pointer)
  (glyph-count (:pointer :uint32)))

(defcfun "hb_buffer_get_glyph_positions"
    (:pointer (:struct glyph-position))
  (buffer :pointer)
  (glyph-count (:pointer :uint32)))

(defcstruct glyph-extents
  (x-bearing :int32)
  (y-bearing :int32)
  (width :int32)
  (height :int32))

(defcfun "hb_font_get_glyph_extents"
    :bool
  (font :pointer)
  (glyph :uint32)
  (extents (:pointer glyph-extents)))
