(defpackage :epap-chronicle
  (:use :common-lisp))

(in-package :epap-chronicle)

(defmacro hmm (id &body body) nil)
(defmacro yay (id &body body) nil)
(defmacro todo (id &body body) nil)
(defmacro done (id &body body) nil)
(defmacro code (&body body) nil)
(defmacro date (weekday day month year) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(date :tuesday 7 :december 2021)

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
  (made #'EPAP::INITIALIZE-BLANK-DISPLAY))

(hmm VARIABLE-WIDTH-POETRY
  (now we assume fixed-width fonts)
  (but I want to look at poems in the :CONCRETE-ROMAN font)
  (well we can already do text shaping with HARFBUZZ)
  (so I can just do that with live updates)
  (see #'EPAP::DRAW-TEXT-LINE)
  (todo LIVE-RENDER-POEM-WITH-SHAPING))

(yay SELF-CHRONICLING-SYSTEM
  (imagine viewing the chronicle itself on the e-ink screen)
  (this would be a beautiful way to start the day)
  (epap::with-font :dm-mono 48
    (epap::live-poetry
     200 200
     (cl:with-output-to-string (text)
       (cl:let ((cl:*print-right-margin* 46))
         (cl:print '((date :tuesday 7 :december 2021)
                     (yay CHRONICLE-STARTED
                       (this is a funny idea)
                       (like an issue tracker inside the program)
                       (we can reference symbols
                           (there is a function #'EPAP::DRAW-LETTER))))
                   text))))))

(yay VARIABLE-WIDTH-POETRY
  (done LIVE-RENDER-POEM-WITH-SHAPING
    (see #'EPAP::POEM)
    (see #'EPAP::TYPESET-LINE)))

(ok going home)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(date :wednesday 8 :august 2021)

(yay EXHIBITING-HAN-SHAN
  (now there is #'EPAP::COLD-MOUNTAIN)
  (it is beautiful to see poems appearing like this)
  (hmm STREAMING-GPT3-POETRY
    (todo SIMPLE-GPT3-API
      (same thing as my old Telegram thing))
    (maybe GPT-3 integration will be a primary fun thing)))

(hmm TYPESETTING
  (now we can do basic text shaping and rendering)
  (but we want a more capable typesetting language)
  (like (CENTERING-HEADINGS)
        (PARAGRAPH-WRAPPING (like Knuth))
        (MULTI-COLUMN-LAYOUTS)
        (CONSTRAINT-SOLVING))
  (code
    (page
     (center
      (with-font sans 92
        "Words from Cold Mountain")
      (with-font sans 64
        "Twenty-Seven Poems by Han-Shan"))))
  (todo RESEARCH-LISP-TYPESETTING))
