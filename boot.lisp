(swank:create-server
 :interface "urbion.brockman.se.beta.tailscale.net"
 :port 4005
 :style :spawn
 :dont-close t)

(asdf:load-system :epap)
