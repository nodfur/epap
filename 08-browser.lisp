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

(defun web-app () 
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

  (hunchentoot:define-easy-handler (epap :uri "/epap") ()
    (with-html-string
      (:html
       (:head (:title "EPAP"))
       (:body
        (let* ((*display-width* 1872)
               (*display-height* 1404)
               (*local-framebuffer* (make-array (list *display-height* *display-width*)
                                                :element-type 'bit)))
          )
        (:img :src (format nil "data:image/png;base64,~S" "")))))))
