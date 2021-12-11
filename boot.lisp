(ql:quickload '(:cffi))

(defun pkg-config-add-lib (libname)
  (let ((process (sb-ext:run-program "/usr/bin/env"
                                     (list "pkg-config" libname "--libs-only-L")
                                     :input t :output :stream :wait t)))
    (let ((stream (sb-ext:process-output process)))
      (loop for line = (read-line stream nil nil)
            while line do
              ;; Drop "-L" part, and add '/' to the end. '/' IS necessary!
              (loop for item in (uiop:split-string line)
                    do (pushnew (pathname (concatenate 'string (subseq item 2) "/"))
                                cffi:*foreign-library-directories*)))
      (prog1 cffi:*foreign-library-directories*
        (sb-ext:process-close process)))))

;; (setf cffi:*foreign-library-directories* '())

(pkg-config-add-lib "libcrypto")
(pkg-config-add-lib "libpng")

(ql:quickload '(:trivia
                :babel
                :cffi
                :zpng
                :cl-base64
                :cl-ppcre
                :trivia
                :parenscript
                :spinneret
                :css-lite
                :hunchentoot
                :iterate
                :printv
                :dexador
                :cl-json
                :png
                ))

(progn
  (in-package :cl-user)
  (asdf:load-system :epap))

(epap::web-app)
