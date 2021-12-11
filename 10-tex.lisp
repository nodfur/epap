;;
;; Copyright (C) 2021  Restless Hypermedia, Inc.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;

(in-package :epap)

(defun latex-preamble ()
  (format t "\\documentclass[11pt,twocolumn]{extarticle}
\\usepackage[paperwidth=209.66mm,paperheight=157.25mm,margin=0.8cm,includefoot]{geometry}
\\usepackage[width=209.66mm,height=157.25mm,center,frame,noinfo]{crop}
\\begin{document}
"))

(defun latex-postamble ()
  (format t "
\\end{document}"))

(defun call-with-latex-to-png (f)
  (uiop:with-temporary-file (:stream stream :pathname pathname
                             :prefix "epap" :type "tex")
    (let ((*standard-output* stream))
      (funcall f))
    :close-stream
    (let ((basename (pathname-name pathname)))
      (uiop:with-current-directory ((uiop:pathname-directory-pathname pathname))
        (uiop:run-program (list "latex" basename)
                          :output :interactive)
        (uiop:run-program (list "dvipng" "-D" "226.785" basename)
                          :output :interactive)
        (uiop:run-program "ls" :output :interactive)
        (png:decode-file
         (make-pathname
          :name (format nil "~A1" basename)
          :type "png"
          :defaults pathname))))))

(defmacro latex-png (&body body)
  `(call-with-latex-to-png (lambda ()
                             (latex-preamble)
                             ,@body
                             (latex-postamble))))

(defun display-image (image)
  (destructuring-bind (height width channels) (array-dimensions image)
    (declare (ignore channels))
    (let ((*local-framebuffer*
            (make-array (list *display-height* *display-width*)
                        :element-type 'bit)))
      (loop for y from 0 below (min *display-height* height)
            do (loop for x from 0 below (min *display-width* width)
                     do (setf (sbit *local-framebuffer* y x)
                              (if (< (aref image y x 0) 240) 1 0))))
      (write-whole-framebuffer)
      (refresh))))

(defmacro with-display (&body body)
  `(progn
     (start-display)
     (initialize-blank-display)
     (unwind-protect (progn ,@body)
       (format t "epap: putting display to sleep~%")
       (goodnight))))

(defun latex-demo ()
  (with-display
    (display-image (latex-png (format t "Hello, world!")))
    (sleep 5)))

(defun real-latex-demo ()
  (for-real
    (start-display)
    (initialize-blank-display)))

(defparameter *chapman-prompt*
  "Various religions, philosophies, and systems claim to have answers. Some are complicated, and they all seem quite different. When you strip away the details, though, there are only a half dozen fundamental answers. Each is appealing in its own way, but also problematic. Understanding clearly what is right and wrong about each approach can resolve the underlying problem.")

(defun read-from-emacs (prompt)
  (swank::read-from-minibuffer-in-emacs prompt))

(defmacro display-latex (&body document)
  `(display-image
    (latex-png ,@document)))

(defun babble-2 (prompt)
  (with-display
    (loop
      do
         (display-latex
           (format t "\\textsl{~a} ~a"
                   prompt (openai:completions prompt :tokens 200)))
      until (not (swank:y-or-n-p-in-emacs "Continue?")))))

(defun babble ()
  (start-display)
  (initialize-blank-display)
  (loop with lines = () do
    (let* ((prompt
             (swank::read-from-minibuffer-in-emacs "GPT-3: ")))
      (unless prompt (return))
      (push (format nil "\\textsc{Prompt.} ~a" prompt) lines)
      (let ((babble (openai:answer-question prompt 20)))
        (push (format nil "\\textsc{Answer.} ~a"
                      (ppcre:regex-replace-all "&" babble "\\\\&")) lines))
      (display-image
       (latex-png
         (loop for line in (reverse lines) do
           (format t "~a~%~%" line))))
      (unless (swank:y-or-n-p-in-emacs "Continue?")
        (return))))
  (goodnight))

;; (initialize-blank-display)

