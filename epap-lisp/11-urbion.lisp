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

(defun blank ()
  (initialize-blank-display))

(defun paper-lisp-logo ()
  (with-font :dm-mono 128
    (live-poetry 520 520 "Paper Lisp"))
  (with-font :concrete-roman 64
    (live-poetry 540 670 "Restless Hypermedia, Inc.")))

(defun shut-down ()
  (lets-sleep)
  ($ "shutdown" "now"))

(defvar *minute-timer* nil)

(defun lets-sleep ()
  (start-display)
  (blank)
  (write-whole-framebuffer)
  ; (paper-lisp-logo)
  (with-font :dm-mono 36
    (with-live-update nil
      (poem 630 500 "Plug in the power to start.")))
  (enter-sleep-mode))

(defun lets-roll ()
  (start-display)
  (blank)
  (write-whole-framebuffer)
  (paper-lisp-logo)
  (if *minute-timer*
      (sb-ext:unschedule-timer *minute-timer*)
      (setf *minute-timer* (sb-ext:make-timer #'update-clock)))
  (sb-ext:schedule-timer *minute-timer* 0 :repeat-interval 60)
  (enter-sleep-mode))

(defun update-clock ()
  (start-display)
  (clear-local-area 0 0 *display-width* 120)
  (write-whole-framebuffer)
  (setf (uiop:getenv "TZ") "Europe/Stockholm")
  (with-font :dm-mono 36
    (with-live-update nil
      (poem 525 60 ($$ "date" "+It's %R on %A, %e %B."))))
  (enter-sleep-mode))

(defun show-some-info ()
  (progn
    (blank)
    (write-whole-framebuffer)
    (with-font :fantasque-bold 30
      (poem 120 80 "> uname -a"))
    (with-font :fantasque 30
      (poem 120 (round (+ 80 (* 32 1.5)))
            (ppcre:regex-replace-all "\\t" ($$ "uname" "-a") "  "))
      (with-font :fantasque-bold 30
        (poem 120 (round (+ 80 (* 32 1.5 2))) "> ping -nc 4 google.com"))

      (poem 120 (round (+ 80 (* 32 1.5 3)))
            ($$ "ping" "-nc" "4" "google.com")))))

(defun boot ()
  (setf *freetype* (initialize-freetype))
  (setf *dry-run* nil)
  (start-display)
  (progn
    (blank)
    (write-whole-framebuffer)
    (with-font :concrete-roman 128
      (poem 600 500 "Hello, world!"))
    (with-font :dm-mono 48
      (poem 650 700 "Lisp system booting...")))
  (stop-display))

(defun save-boot-program ()
  (sb-ext:save-lisp-and-die "epap-boot" :toplevel #'boot :executable t))
