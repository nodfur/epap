(require :asdf)
(asdf:load-system :cffi)

(defpackage :epap
  (:use :common-lisp :cffi))

(in-package :epap)

(define-foreign-library epapi
  (:unix "./zig-out/lib/libepapi.so"))

(use-foreign-library epapi)
