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

(defun draw-bitmap (canvas bitmap origin-x origin-y)
  (destructuring-bind (&key buffer pitch width rows &allow-other-keys) bitmap
    (dotimes (i rows)
      (dotimes (j width)
        (let* ((source-byte-index (+ (* i pitch) (truncate j 8)))
               (source-bit-index (- 7 (mod j 8)))
               (source-byte (mem-aref buffer :uint8 source-byte-index))
               (source-bit (if (logbitp source-bit-index source-byte) 1 0)))
          (setf (bit canvas (+ origin-y i) (+ origin-x j)) source-bit))))))

(defun draw-text-line (text &key canvas origin-x origin-y)
  (destructuring-bind (glyph-infos glyph-positions)
      (shape-text text)
    (loop with x = origin-x and y = origin-y
          for info across glyph-infos
          and position across glyph-positions
          do (destructuring-bind (&key x-bearing y-bearing &allow-other-keys)
                 (load-glyph *current-font* (getf info :codepoint))
               (destructuring-bind
                   (&key x-advance y-advance x-offset y-offset)
                   position
                 (draw-bitmap canvas (read-glyph-bitmap *current-font*)
                              (+ (truncate x-offset 64)
                                 (truncate x-bearing 64)
                                 x)
                              (+ (truncate y-offset 64)
                                 (- (font-height *current-font*)
                                    (truncate y-bearing 64))
                                 y))
                 (incf x (truncate x-advance 64))
                 (incf y (truncate y-advance 64)))))))

(defun test-draw-line (text &key width height)
  (let ((canvas (make-array (list height width)
                            :element-type 'bit
                            :initial-element 0)))
    (prog1 canvas
      (draw-text-line text :canvas canvas :origin-x 0 :origin-y 0))))

(defun draw-text-locally (x y text)
  (draw-text-line text :canvas *local-framebuffer* :origin-x x :origin-y y))

(test 'draw-cozette-line
  (assert-equalp
   (with-font :cozette 13
     (test-draw-line "Foo!" :width 24 :height 16))
   #2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
       (0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
       (0 1 0 0 0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 1 0 0)
       (0 1 1 1 1 0 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 1 0 0)
       (0 1 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 1 0 0)
       (0 1 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 1 0 0)
       (0 1 0 0 0 0 0 1 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0)
       (0 1 0 0 0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 1 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(defun test-write-area-to-framebuffer (width height text &optional (x 0) (y 0))
  (write-area-to-framebuffer
   :address *framebuffer-address*
   :rectangle (list :x x :y y :w width :h height)
   :bitmap (test-draw-line text :width width :height height)))

;; (defun clear-framebuffer ()
;;   (write-area-to-framebuffer
;;    :address *framebuffer-address*
;;    :rectangle (list :x 0 :y 0 :w *display-width* :h *display-height*)
;;    :bitmap (make-array (list *display-height* *display-width*)
;;                        :element-type 'bit
;;                        :initial-element 1)))

(defun display-area-monochrome (x y width height)
  (display-area :address *framebuffer-address*
                :rectangle (list :x x :y y :w width :h height)
                :mode :fast-monochrome))

(defun lets-go ()
  (with-font :dm-mono 64
    (let ((x 0)
          (y 0)
          (w (* 64 5))
          (h 128))
      (clear-local-framebuffer)
      (draw-text-locally x y " Foo!")
      (copy-area-to-framebuffer x y w h)
      (display-area-monochrome x y w h))))

(defun lets-animate ()
  (let ((y 256))
    (with-font :dm-mono 32
      (loop
        for c across "HELLO, WORLD!"
        with i = 0
        do (progn
             (draw-text-locally (* i 33) y (string c))
             (copy-area-to-framebuffer (* i 33) y 33 64)
             (display-area-monochrome (* i 33) y 33 64)
             (incf i))))))

;; (change-font :dm-mono 64)

(defun draw-letter (letter x y w h)
  (draw-text-locally x y (string letter))
  (copy-area-to-framebuffer x y w h)
  (display-area-monochrome x y w h))

(defun write-whole-framebuffer ()
  (copy-area-to-framebuffer 0 0 *display-width* *display-height*))

(defun refresh ()
  (display-area-monochrome 0 0 *display-width* *display-height*))

;; (draw-letter #\A (* 32 4) 128)

(defun slowly-clear ()
  (progn
    (clear-local-framebuffer)
    (write-whole-framebuffer)
    (refresh)))

;; (slowly-clear)

;;(draw-letter #\C (* 16 27) 450)

;; (draw-letter #\y (* 16 9) (+ 1024 128))

(change-font :dm-mono 48)

(defun poem (dx dy text)
  (loop for c across text
        with x = 0 and y = 0
        do
           (if (eql c #\Newline)
               (progn
                 (delay-milliseconds 500)
                 (setf x 0)
                 (incf y 60))
               (progn
                 (draw-letter c (+ dx x) (+ dy y) 48 72)
                 (incf x 28)))))

(defun anecdote-of-the-jar ()
  (poem 500 100
        "I placed a jar in Tennessee,
And round it was, upon a hill.
It made the slovenly wilderness
Surround that hill.

The wilderness rose up to it,
And sprawled around, no longer wild.
The jar was round upon the ground
And tall and of a port in air.

It took dominion everywhere.
The jar was gray and bare.
It did not give of bird or bush,
Like nothing else in Tennessee."))
