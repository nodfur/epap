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
  (use-foreign-library epapi)
  )

(defcfun "epap_start_broadcom" :uint32)
(defcfun "epap_stop_broadcom" :uint32)
(defcfun "epap_start_text" :uint32)

(defcstruct display-info
  (panel-width :uint16)
  (panel-height :uint16)
  (memory-address :uint32)
  (firmware-version :char :count 16)
  (lut-version :char :count 16))

(defcfun "epap_start_display" :uint32
  (vcom :double)
  (info (:pointer (:struct display-info))))

(defcfun "epap_sleep" :uint32)

(uiop:run-program "whoami" :output :string)

(defclass display ()
  ((width :initarg :width :accessor :display-width)
   (height :initarg :height :accessor :display-height)))

(defvar *display*)

(defun get-display ()
  (with-foreign-object (info '(:struct display-info))
    (epap-start-display -1.73d0 info)
    (with-foreign-slots
        ((panel-width panel-height)
         info (:struct display-info))
      (make-instance 'display
                     :width panel-width
                     :height panel-height))))

(defun test ()
  (epap-start-broadcom)
  (setf *display* (get-display))
  (epap-stop-broadcom))

(defun yolo ()
  (uiop:run-program "make && git save && git push" :output t))

(test)
