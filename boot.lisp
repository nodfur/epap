(ql:quickload '(:cffi :cffi-toolchain :cl-ppcre))

(defun run-pkg-config (name command)
  (uiop:split-string
   (ppcre:regex-replace
    "\\n$"
    (uiop:run-program `("pkg-config" ,name ,command)
                      :output :string)
    "")))

(defun pkg-config-library (name)
  (let ((compiler-flags (run-pkg-config name "--cflags"))
        (linker-flags (run-pkg-config name "--libs-only-L")))
    (loop for item in linker-flags do
      (pushnew (pathname (format nil "~a/" (subseq item 2)))
               cffi:*foreign-library-directories*
               :test #'equal))
    (loop for item in compiler-flags do
      (pushnew item cffi-toolchain:*cc-flags* :test #'equal))))

;; (setf cffi:*foreign-library-directories* '())

(pkg-config-library "libcrypto")
(pkg-config-library "libpng")

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
