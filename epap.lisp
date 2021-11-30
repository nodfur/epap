(require :asdf)
(asdf:load-system :cffi)

(defpackage :epapi
  (:use :common-lisp :cffi :asdf :uiop))

(in-package :epapi)

(define-foreign-library epapi
  (:unix "./zig-out/lib/libepapi.so"))

(use-foreign-library epapi)

(defun reload ()
  (uiop:run-program "make")
  (use-foreign-library epapi)
  )


(defcfun "epap_start_broadcom" :uint32)
(defcfun "epap_stop_broadcom" :uint32)

(epap-start-broadcom)
(epap-stop-broadcom)
