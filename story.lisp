(defpackage :epap-chronicle
  (:use :common-lisp))

(in-package :epap-chronicle)

(defmacro year (year &body body)
  `(progn ,@body))
(defmacro month (month &body body)
  `(progn ,@body))
(defmacro day (day weekday &body body)
  `(progn ,@body))

(defmacro hmm (id &body body) nil)
(defmacro yay (id &body body) nil)
(defmacro todo (id &body body) nil)
(defmacro done (id &body body) nil)

(year 2021
  (month :december
    (day 7 :tuesday
      (yay CHRONICLE-STARTED
        (this is a funny idea)
        (it's like an issue tracker inside the program)
        (we can reference symbols)
        (there is a function #'EPAP::DRAW-LETTER))

      (todo NEED-CLEAN-SCREEN-RESET
        (now I can only blank the screen in the quick mode)
        (but this isn't enough to wipe away the e-ink ghosting)
        (should implement the slow blanking function)
        (should fix #'EPAP::COPY-AREA-TO-FRAMEBUFFER)
        (should fix #'EPAP::DISPLAY-AREA))

      (hmm CONSIDERING-GRAYSCALE-BUFFER
        (now our local buffer is a bitmap)
        (but maybe it should be a 4-bit grayscale matrix)
        (then we can render it as a bitmap or a grayscale)
        (but let's do NEED-CLEAN-SCREEN-RESET in a simple way))

      (done NEED-CLEAN-SCREEN-RESET
        implemented #'EPAP::INITIALIZE-BLANK-DISPLAY))))
