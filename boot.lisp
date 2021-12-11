(load "slime/swank-loader.lisp")
(swank-loader:init)
(swank:create-server
 :interface "0.0.0.0"
 :port 4005
 :style :spawn
 :dont-close t)

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
                ))
