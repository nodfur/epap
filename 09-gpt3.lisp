;;; This code is from Mark Watson's GitHub repository.
;;;
;;;   URL: <https://github.com/mark-watson/loving-common-lisp>
;;;   Commit: 534c9f2fda1bd44f2e22332ad7e7cceef8530e12
;;;
;;; From Watson's repository README:
;;;
;;; > This repo is for the 6th edition of my book that was released May 30, 2020
;;; >
;;; > Open source examples for my book "Loving Common Lisp, or the
;;; > Savvy Programmer's Secret Weapon" that is available at
;;; > https://leanpub.com/lovinglisp My book is available to read free
;;; > online and the PDF for the book is licensed with Creative Common
;;; > share with no modifications or commercial reuse: this means that
;;; > you are encouraged to share copies (up to date copies only,
;;; > please) of the PDF with friends and co-workers. New editions of
;;; > the book are free if you have purchased the book before. Many
;;; > thanks to the people who support my writing by purchasing
;;; > the book!
;;; >
;;; > The source code in this repository may be used either under the
;;; > Apache 2 or the LGPL version 3 licenses. Use whichever license
;;; > that works best for you.
;;;
;;; The AGPL3 license for the EPAP repository is not "directly"
;;; compatible with LGPL3, but the LGPL3 allows us to relicense this
;;; particular file as GPL3.
;;;

(defpackage :openai
  (:use :common-lisp)
  (:export ()))

(in-package #:openai)

(defparameter *openai-davinci-model-host*
  "https://api.openai.com/v1/engines/davinci/completions")

(defun friendly-getenv (x)
  (let ((value (uiop:getenv x)))
    (or value (restart-case (error "missing environment variable ~A" x)
                (:setenv (value)
                 :report "Set the environment variable"
                 :interactive (lambda ()
                                (format t "New value for ~A: " x)
                                (let ((new-value (eval (read))))
                                  (setf (uiop:getenv x) new-value)))
                  value)))))

(defun openai-secret-key ()
  (friendly-getenv "OPENAI_KEY"))

(defun openai-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    ;;(princ curl-command)
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        ;; extract text (this might change if OpenAI changes JSON return format):
        (cdar (cadr (nth 4 json-as-list)))))))

(defun completions (starter-text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl " *openai-davinci-model-host*
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (openai-secret-key) "\" "
           " -d '{\"prompt\": \"" starter-text "\", \"max_tokens\": "
           (write-to-string max-tokens)  "}'")))
    (openai-helper curl-command)))

(defun summarize (some-text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl " *openai-davinci-model-host*
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (openai-secret-key) "\" "
           " -d '{\"prompt\": \"" some-text "\", \"max_tokens\": "
           (write-to-string max-tokens) ", \"presence_penalty\": 0.0"
           ", \"temperature\": 0.3, \"top_p\": 1.0, \"frequency_penalty\": 0.0 }'")))
    (openai-helper curl-command)))

(defun answer-question (question-text max-tokens)
  (let* ((q-text
          (concatenate
           'string
           "\nQ: " question-text "\nA:"))
         (curl-command
          (concatenate
           'string
           "curl " *openai-davinci-model-host*
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (openai-secret-key) "\" "
           " -d '{\"prompt\": \"" q-text "\", \"max_tokens\": "
           (write-to-string max-tokens) ", \"presence_penalty\": 0.0, \"stop\": [\"\\n\"]"
           ", \"temperature\": 0.0, \"top_p\": 1.0, \"frequency_penalty\": 0.0 }'"))
         (answer (openai-helper curl-command))
         (index (search "nQ:" answer)))
    (if index
        (string-trim " " (subseq answer 0 index))
        (string-trim " " answer))))

#|

(print (openai:completions "The President went to Congress" 20))

(print (openai:summarize "Jupiter is the fifth planet from the Sun and
the largest in the Solar System. It is a gas giant with a mass
one-thousandth that of the Sun, but two-and-a-half times that of all
the other planets in the Solar System combined. Jupiter is one of the
brightest objects visible to the naked eye in the night sky, and has
been known to ancient civilizations since before recorded history. It
is named after the Roman god Jupiter.[19] When viewed from Earth,
Jupiter can be bright enough for its reflected light to cast visible
shadows,[20] and is on average the third-brightest natural object in
the night sky after the Moon and Venus." 30))

(print (openai:answer-question "Where were the 1992 Olympics held?" 60))
(print (openai:answer-question "Where is the Valley of Kings?" 60))
|#
