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

(defpackage :knuth-plass
  (:use :common-lisp))

(in-package :knuth-plass)

(defparameter *paragraph-width* 68)

(defparameter *space-glue* '(glue 2 1 1))
(defparameter *paragraph-end-glue* '(glue 0 1000 0))
(defparameter *paragraph-end-penalty* '(forced-break))

(defun string-to-sequence (string)
  (let ((sequence
          (loop
            for word in (uiop:split-string string)
            appending `((box ,(length word) ,word)
                        ,*space-glue*))))
    (concatenate 'vector
                 (butlast sequence)
                 (list *paragraph-end-glue*)
                 (list *paragraph-end-penalty*))))

(defparameter *example*
  (string-to-sequence
   "One thing that is not evident in the greedy algorithm is that we are implicitly defining a score for how good each line break is; and, a metric for how good the total set of line breaks we've chosen are, together. Knuth & Plass' intuition was to explicitly define the scoring system (and the metric), and then choose a scoring system and metric that resulted in both a computationally tractable algorithm, and one that gives high quality results."))

;; One thing that is not evident in the greedy algorithm is that we
;; are implicitly defining a score for how good each line break is;
;; and, a metric for how good the total set of line breaks we've
;; chosen are, together. Knuth & Plass' intuition was to explicitly
;; define the scoring system (and the metric), and then choose a
;; scoring system and metric that resulted in both a computationally
;; tractable algorithm, and one that gives high quality results.

(defun normal-width (item)
  (ecase (car item)
    (box (cadr item))
    (glue (cadr item))
    (forced-break 0)))

(defun stretchability (item)
  (ecase (car item)
    (box 0)
    (glue (nth 2 item))
    (forced-break 0)))

(defun shrinkability (item)
  (ecase (car item)
    (box 0)
    (glue (nth 3 item))
    (forced-break 0)))

(defun minimum-width (item)
  (- (normal-width item) (shrinkability item)))

(defun maximum-width (item)
  (+ (normal-width item) (stretchability item)))

(defparameter *sequence* *example*)

(defun next-feasible-breakpoint (origin)
  (loop for i from origin below (length *sequence*)
        for item = (svref *sequence* i)
        summing (minimum-width item) into minimum
        summing (maximum-width item) into maximum
        do (format t "~A ~A ~A~%" item minimum maximum)
        when (and (<= minimum *paragraph-width*)
                  (>= maximum *paragraph-width*))
          return i))

(defun find-next-feasible-breakpoint (origin)
  (loop
    for i from origin below (length *sequence*)
    when (is-breakpoint-feasible origin i)
      return i))

(defun draw-line (a b)
  (loop
    for i from a below b
    collecting (draw-item (svref *sequence* i))))

(defun algorithm (sequence)
  (let ((active '(0)))
    (loop )
    )
  )
