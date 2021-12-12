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
(pkg-config-library "freetype2")
(pkg-config-library "harfbuzz")

(ql:quickload
 '(
   #:iterate          ; a looping macro
   #:trivia           ; pattern matching
   #:printv           ; easy code tracing

   #:babel            ; character codec
   #:cffi             ; C library interface

   #:cl-base64        ; base 64 codec
   #:cl-json          ; JSON codec
   #:cl-ppcre         ; Perl-compatible regexps

   #:dexador          ; HTTP client
   #:hunchentoot      ; HTTP server
   #:spinneret        ; HTML with S-expressions
   #:css-lite         ; CSS with S-expressions
   #:parenscript      ; JavaScript with Lisp syntax

   #:png              ; read & write PNG files
   #:zpng             ; Lisp-native PNG library
   ))

(progn
  (in-package :cl-user)
  (asdf:load-system "epap"))

(epap::web-app)
