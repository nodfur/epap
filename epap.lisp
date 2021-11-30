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

(defcfun "epap_start_display" :uint32
  (vcom :double)
  (info (:pointer (:struct display-info))))

(defcfun "epap_sleep" :uint32)

(defvar *display-width*)
(defvar *display-height*)
(defvar *base-address*)

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
    (try (epap-start-display -1.73d0 info))
    (with-foreign-slots
        ((panel-width panel-height base-address)
         info (:struct display-info))
      (setf *display-width* panel-width
            *display-height* panel-height
            *base-address* base-address))))

(defun test ()
  (try (epap-start-broadcom))
  (read-display-info)
  (try (epap-sleep))
  (try (epap-stop-broadcom)))

(defun yolo ()
  (uiop:run-program "make && git save && git push" :output t))

(test)
