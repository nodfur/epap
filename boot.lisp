(load "slime/swank-loader.lisp")
(swank-loader:init)
(swank:create-server
 :interface "0.0.0.0"
 :port 4005
 :style :spawn
 :dont-close t)
