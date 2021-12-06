(swank:create-server
 :interface "0.0.0.0"
 :port 4005
 :style :spawn
 :dont-close t)

(asdf:load-system :epap)
