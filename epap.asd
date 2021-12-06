(defsystem "epap"
  :depends-on ("cffi" "babel" "zpng" "cl-base64")
  :serial t
  :components ((:file "00-basic")
               (:file "01-foreign")
               (:file "02-bcm2385")
               (:file "03-it8591")
               (:file "04-paper")))
