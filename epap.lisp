(require :asdf)
(asdf:load-system :cffi)

(defpackage :epapi
  (:use :common-lisp :cffi :asdf :uiop))

(in-package :epapi)

(define-foreign-library epapi
  (:unix "./zig-out/lib/libepapi.so"))

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

(defvar *display-width*)
(defvar *display-height*)
(defvar *base-address*)
(defvar *c-frame*)

(defvar *font-cozette*
  (foreign-alloc '(:struct font-data)))

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
      (loop for y from 0 below height
            do (loop
                 for x from 0 below width
                 and byte = (aref array (truncate (+ (* y width) x) 8))
                 do (setf (bit bit-array y x)
                          (if (logbitp (mod x 8) byte) 1 0)))))))

(defun foo ()
  (let* ((*display-width* 32)
         (*display-height* 24)
         (*c-frame* (allocate-c-frame)))
    (try (epap-render-text *font-cozette* "foo" *c-frame* *display-width* 0 0))
    (let ((array (foreign-array-to-lisp
                  *c-frame*
                  (list :array :uint8 (display-bitmap-size))
                  :element-type :unsigned-byte)))
      (prin1 array)
      (prog1 (byte-vector->bit-array array *display-width* *display-height*)
        (foreign-array-free *c-frame*)))))

(defun test-text (font-path font-height width)
  (try (epap-start-text))
  (try (epap-load-font "./fonts/cozette.bdf" 13 *font-cozette*))
  (try (epap-render-text *font-cozette* "foo" *c-frame* *display-width* 0 0)))

(defun c-frame-to-lisp ()
  (let ((array (make-array (list *display-width* *display-height*)
                           :element-type bit)))
    (loop for y from 0 below *display-height*
          for x from 0 below *display-width*
          with pixel = (foreign-aref *c-frame* '())
          ))
  )
