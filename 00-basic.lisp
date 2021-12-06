(defpackage :epap
  (:use :common-lisp :cffi :babel :zpng :base64))

(in-package :epap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic testing framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *current-test*)

(defmacro test (name &body body)
  `(let ((*current-test* ,name))
     ,@body))

(define-condition failed-equality-assertion (error)
  ((actual :initarg :actual :reader assertion-actual)
   (expected :initarg :expected :reader assertion-expected)
   (test :initform *current-test* :reader assertion-test))

  (:report (lambda (condition stream)
             (format stream "Test: ~A~%Expected: ~A~%  Actual: ~A"
                     (assertion-test condition)
                     (assertion-expected condition)
                     (assertion-actual condition)))))

(define-condition test-ok (condition) ())

(defmacro assert-equalp (actual expected)
  (let ((a (gensym))
        (e (gensym)))
    `(let ((,a ,actual)
           (,e ,expected))
       (if (equalp ,a ,e)
           (prog1 :ok (signal 'test-ok))
           (error 'failed-equality-assertion :expected ,e :actual ,a)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dry run functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *dry-run* nil)
(defvar *dry-run-log* ())

(defmacro maybe-dry-run (message &body body)
  `(if *dry-run*
       (setf *dry-run-log* (cons ,message *dry-run-log*))
       (progn ,@body)))

(defmacro dry-run (&body body)
  `(let ((*dry-run* t)
         (*dry-run-log* ()))
     (values (progn ,@body)
             (nreverse *dry-run-log*))))

(defun no-dry-run ()
  (when *dry-run*
    (error "no frobbing on dry run")))
