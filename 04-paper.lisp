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

(defvar *current-font*)

(defparameter *font-table*
  '(:cozette "./fonts/cozette.bdf"
    :dm-mono "./fonts/DMMono-Regular.ttf"
    :concrete-roman "./fonts/computer-modern/cmunorm.otf"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *text-initialized* nil)

(defun display-bitmap-size ()
  (/ (* *display-width* *display-height*) 8))

(defun allocate-c-frame ()
  (foreign-alloc :uint8
                 :initial-element #xFF
                 :count (display-bitmap-size)))

(defun byte-vector->bit-array (array width height)
  (let ((bit-array (make-array (list height width) :element-type 'bit)))
    (prog1 bit-array
      (loop
        for y from 0 below height
        do (loop
             for x from 0 below width
             do (let* ((pixel-index (+ (* y width) x))
                       (byte (aref array (truncate pixel-index 8))))
                  (setf (bit bit-array y x)
                        (if (logbitp (mod pixel-index 8) byte) 1 0))))))))

(test 'byte-vector->bit-array
  (assert-equalp
   (byte-vector->bit-array #(1) 1 8)
   (make-array '(8 1)
               :element-type 'bit
               :initial-contents '((1) (0) (0) (0) (0) (0) (0) (0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's do some drawing to the screen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-display ()
  (maybe-dry-run '(wait-for-display)
    (sb-ext:with-timeout 5
      (loop until (= 0 (read-register :lutafsr))))))

(defparameter *image-endianness* 0)     ;; little
(defparameter *image-rotation* 0)       ;; normal
(defparameter *image-bpp* 3)            ;; bitmap

(defun set-framebuffer-address (address)
  (write-register :lisar+2 (ldb (byte 16 16) address))
  (write-register :lisar+0 (ldb (byte 16 0) address)))

(defun read-word-from-bitmap (bitmap row offset)
  (unless (zerop (mod offset 16))
    (error "bitmap offset ~A not word aligned" offset))
  (loop for i from 0 below 16
        sum (ash (bit bitmap row (+ offset i)) (- 15 i))))

(test 'read-word-from-bitmap
  (let ((canvas (make-array '(32 32) :element-type 'bit)))
    (setf (bit canvas 0 0) 1
          (bit canvas 0 1) 1
          (bit canvas 1 30) 1)
    (assert-equalp
     (read-word-from-bitmap canvas 0 0)
     #b1100000000000000)
    (assert-equalp
     (read-word-from-bitmap canvas 0 16)
     0)
    (assert-equalp
     (read-word-from-bitmap canvas 1 16)
     #b0000000000000010)))

(defmacro note (x)
  `(maybe-dry-run ,x nil))

(defun write-area-to-framebuffer (&key address rectangle bitmap)
  "The rectangle should be word-aligned."
  (wait-for-display)
  (set-framebuffer-address address)
  (destructuring-bind (&key x y w h) rectangle
    (let* ((format (logior (ash *image-endianness* 8)
                           (ash *image-bpp* 4)
                           (ash *image-rotation* 0)))
           (args (list format x y (/ w 8) h)))
      (write-command :load-img-area-start)
      (write-word-packets args))
    (loop
      for i from y below (+ y h)
      do (loop
           for j from x by 16 below (+ x w)
           do
              (note `(pixels ,i ,j))
              (write-word-packet (read-word-from-bitmap bitmap i j)))))
  (write-command :load-img-end))

(defun write-register-bit (register &key index bit)
  (let ((value (read-register register)))
    (write-register register (dpb bit (byte 1 index) value))))

(defparameter *a2-mode* 6)
(defparameter *initialize-mode* 0)

(defun display-area (&key address rectangle mode)
  (destructuring-bind (&key x y w h) rectangle
    (ecase mode
      (:fast-monochrome
       (progn
         (write-register-bit :up1sr+2 :index 2 :bit 1)
         (write-register :bgvr #xf0)
         (write-command :display-area-buf)
         (write-word-packets
          (list x y w h 6
                (ldb (byte 16 0) address)
                (ldb (byte 16 16) address)))
         (wait-for-display)
         (write-register-bit :up1sr+2 :index 2 :bit 0)))
      (:initialize
       (progn
         (write-command :display-area)
         (write-word-packets
          (list x y w h *initialize-mode*)))))))

(defmacro for-real (&body body)
  `(let ((*dry-run* nil))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "FT_Error_String" :string (error-code :int32))
(defcfun "FT_Init_FreeType" :int32 (library :pointer))
(defcfun "FT_Done_FreeType" :int32 (library :pointer))

(defcstruct glyph-metrics
  (:width :int64)
  (:height :int64)
  (:horizontal-bearing-x :int64)
  (:horizontal-bearing-y :int64)
  (:horizontal-advance :int64)
  (:vertical-bearing-x :int64)
  (:vertical-bearing-y :int64)
  (:vertical-advance :int64))

(defcenum (glyph-format :uint32)
  :none
  (:composite #x636f6d70)
  (:bitmap #x62697473)
  (:outline #x6f75746c))

(defcenum (pixel-mode :uint8)
  :none :mono :gray :gray2 :gray4 :lcd :lcd-vv :bgra)

(defcstruct freetype-bitmap
  (:rows :uint32)
  (:width :uint32)
  (:pitch :int32)
  (:buffer :pointer)
  (:num-grays :uint16)
  (:pixel-mode pixel-mode)
  (:palette-mode :uint8)
  (:palette :pointer))

(defcstruct freetype-outline
  (:n-contours :int16)
  (:n-points :int16)
  (:points :pointer)
  (:tags :pointer)
  (:contours :pointer)
  (:flags :int32))

(defcstruct freetype-glyph-slot
  (:library :pointer)
  (:face :pointer )
  (:next (:pointer (:struct freetype-glyph-slot)))
  (:glyph-index :uint32)
  (:generic :pointer)
  (:generic-finalizer :pointer)
  (:metrics (:struct glyph-metrics))
  (:linear-horizontal-advance :int64)
  (:linear-vertical-advance :int64)
  (:advance-x :int64)
  (:advance-y :int64)
  (:format glyph-format)
  (:bitmap (:struct freetype-bitmap))
  (:bitmap-left :int32)
  (:bitmap-top :int32)
  (:outline (:struct freetype-outline))
  (:num-subglyphs :uint32)
  (:subglyphs :pointer)
  (:control-data :pointer)
  (:control-length :int64)
  (:lsb-delta :int64)
  (:rsb-delta :int64)
  (:other :pointer)
  (:internal :pointer))

(defcstruct (freetype-face)
  (:num-faces :int64)
  (:face-index :int64)
  (:face-flags :int64)
  (:style-flags :int64)
  (:num-glyphs :int64)
  (:family-name :string)
  (:style-name :string)
  (:num-fixed-sizes :int32)
  (:available-sizes :pointer)
  (:num-charmaps :int32)
  (:charmaps :pointer)
  (:generic :pointer)
  (:generic-finalizer :pointer)

  ;; these are only relevant for scalable outlines
  (:bbox (:array :int64 4))
  (:units-per-em :int16)
  (:ascender :int16)
  (:descender :int16)
  (:height :int16)
  (:max-advance-width :int16)
  (:max-advance-height :int16)
  (:underline-position :int16)
  (:underline-thickness :int16)

  (:glyph (:pointer (:struct freetype-glyph-slot)))
  (:size :pointer)
  (:charmap :pointer))

(defcfun "FT_New_Face" :int32
  (library :pointer)
  (path :string)
  (face-index :long)
  (face-ptr (:pointer (:struct freetype-face))))

(defcfun "FT_Set_Pixel_Sizes" :int32
  (face :pointer)
  (width :uint32)
  (height :uint32))

(defcfun "FT_Load_Glyph" :int32
  (face :pointer)
  (glyph-index :uint32)
  (load-flags :int32))

(defcenum ft-render-mode
  :normal :light :mono :lcd :lcd-v :sdf)

(defun ft-load-target (mode)
  (ash (foreign-enum-value 'ft-render-mode mode) 16))

(defun load-flag-number (flag)
  (ecase flag
    (:render (ash 1 2))
    (:target-mono (ft-load-target :mono))
    (:force-autohint (ash 1 5))))

(defun make-load-flags (flags)
  (apply #'logior (mapcar #'load-flag-number flags)))

(defcfun "hb_ft_font_create_referenced" :pointer
  (freetype-font :pointer))

(defcfun "hb_ft_font_set_funcs" :void
  (font :pointer))

(defcfun "hb_buffer_create" :pointer)
(defcfun "hb_buffer_destroy" :void (buffer :pointer))

(defcenum hb-direction
  (:invalid 0)
  (:left-to-right 4)
  :right-to-left
  :top-to-bottom
  :bottom-to-top)

(defcfun "hb_buffer_set_direction" :void
  (buffer :pointer)
  (direction hb-direction))

(defcfun "hb_script_from_string" :uint32
  (string :string)
  (len :int))

(defcfun "hb_buffer_set_script" :void
  (buffer :pointer)
  (script :uint32))

(defcfun "hb_buffer_set_language" :void
  (buffer :pointer)
  (language :pointer))

(defcfun "hb_language_from_string" :pointer
  (string :string)
  (len :int))

(defcfun "hb_buffer_add_utf8" :void
  (buffer :pointer)
  (text :string)
  (length :int32)
  (offset :uint32)
  (count :int32))

(defcfun "hb_shape" :void
  (font :pointer)
  (buffer :pointer)
  (features :pointer)
  (num-features :uint32))

(defcstruct (glyph-info :size 20)
  (:codepoint :uint32)
  (:cluster :uint32 :offset 8))

(defcstruct (glyph-position :size 20)
  (:x-advance :int32)
  (:y-advance :int32)
  (:x-offset :int32)
  (:y-offset :int32))

(defcstruct glyph-extents
  (:x-bearing :int32)
  (:y-bearing :int32)
  (:width :int32)
  (:height :int32))

(defcfun "hb_buffer_get_glyph_infos"
    (:pointer (:struct glyph-info))
  (buffer :pointer)
  (glyph-count (:pointer :uint32)))

(defcfun "hb_buffer_has_positions" :boolean
  (buffer :pointer))

(defcfun "hb_buffer_get_glyph_positions"
    (:pointer (:struct glyph-position))
  (buffer :pointer)
  (glyph-count (:pointer :uint32)))

(defcfun "hb_font_get_glyph_extents"
    :bool
  (font :pointer)
  (glyph :uint32)
  (extents (:pointer (:struct glyph-extents))))

(define-condition freetype-error (error)
  ((code :initarg :code :reader freetype-error-code)))

(defun check-freetype-result (result)
  (unless (zerop result)
    (error 'freetype-error :code result)))

(defun initialize-freetype ()
  (let ((freetype (foreign-alloc :pointer)))
    (check-freetype-result
     (ft-init-freetype freetype))
    (mem-ref freetype :pointer)))

(defstruct font
  height
  freetype-ptr
  harfbuzz-ptr)

(defvar *freetype* (initialize-freetype))

;; (setf *freetype* (initialize-freetype))

(defun load-freetype-font (path height)
  (let ((face-ptr (foreign-alloc :pointer)))
    (check-freetype-result
     (ft-new-face *freetype* path 0 face-ptr))
    (let ((face (mem-ref face-ptr :pointer)))
      (prog1 face
        (check-freetype-result
         (ft-set-pixel-sizes face 0 height))))))

(defun load-font (path height)
  (let* ((freetype-ptr (load-freetype-font path height))
         (harfbuzz-ptr (hb-ft-font-create-referenced freetype-ptr)))
    (hb-ft-font-set-funcs harfbuzz-ptr)
    (make-font
     :height height
     :freetype-ptr freetype-ptr
     :harfbuzz-ptr harfbuzz-ptr)))

(defun find-font (name height)
  (let ((path (getf *font-table* name)))
    (if path (load-font path height)
        (error "unknown font name ~A" name))))

(defmacro with-font (name height &body body)
  `(let ((*current-font* (find-font ,name ,height)))
     ,@body))

(defun shape-text (text &key
                          (language "en")
                          (direction :left-to-right)
                          (script "Latn"))
  (let ((buffer (hb-buffer-create)))
    (hb-buffer-set-direction buffer direction)
    (hb-buffer-set-script buffer (hb-script-from-string script -1))
    (hb-buffer-set-language buffer (hb-language-from-string language -1))
    (hb-buffer-add-utf8 buffer text -1 0 -1)
    (hb-shape (font-harfbuzz-ptr *current-font*) buffer (null-pointer) 0)
    (with-foreign-object (glyph-count :uint32)
      (let* ((glyph-infos
               (foreign-array-to-lisp
                (hb-buffer-get-glyph-infos buffer glyph-count)
                `(:array (:struct glyph-info) ,(mem-ref glyph-count :uint32))))
             (glyph-positions
               (foreign-array-to-lisp
                (hb-buffer-get-glyph-positions buffer glyph-count)
                `(:array (:struct glyph-position) ,(mem-ref glyph-count :uint32)))))
        (prog1
            (list glyph-infos glyph-positions)
          (hb-buffer-destroy buffer))))))

(test 'shape-text
  (assert-equalp
   (with-font :cozette 13
     (shape-text "xyz"))
   (list #((:cluster 0 :codepoint 121)
           (:cluster 1 :codepoint 122)
           (:cluster 2 :codepoint 123))
         #((:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384)
           (:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384)
           (:y-offset 0 :x-offset 0 :y-advance 0 :x-advance 384)))))

(defun read-glyph-slot (font)
  (mem-ref
   (getf
    (mem-ref (font-freetype-ptr font) '(:struct freetype-face))
    :glyph)
   '(:struct freetype-glyph-slot)))

(defun read-glyph-bitmap (font)
  (getf (read-glyph-slot font) :bitmap))

(defun load-glyph (font glyph-id)
  (check-freetype-result
   (ft-load-glyph (font-freetype-ptr font) glyph-id
                  (make-load-flags '(:render :target-mono :force-autohint))))
  (with-foreign-object (extents '(:struct glyph-extents))
    (unless (hb-font-get-glyph-extents (font-harfbuzz-ptr font)
                                       glyph-id extents)
      (error "harfbuzz: failed to get glyph extents for glyph ~A" glyph-id))
    (mem-ref extents '(:struct glyph-extents))))

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
  (let ((canvas (make-array (list height width) :element-type 'bit)))
    (prog1 canvas
      (draw-text-line text :canvas canvas :origin-x 0 :origin-y 0))))

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

(defun test-write-area-to-framebuffer (width height text)
  (write-area-to-framebuffer
   :address *framebuffer-address*
   :rectangle (list :x 0 :y 0 :w width :h height)
   :bitmap (test-draw-line text :width width :height height)))

(defun clear-framebuffer ()
  (write-area-to-framebuffer
   :address *framebuffer-address*
   :rectangle (list :x 0 :y 0 :w *display-width* :h *display-height*)
   :bitmap (make-array (list *display-height* *display-width*)
                       :element-type 'bit
                       :initial-element 0)))

(defun test-display-area ()
  (display-area :address *framebuffer-address*
                :rectangle '(:x 0 :y 0 :w 128 :h 32)
                :mode :fast-monochrome))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (= 1 (aref array i j)))))

(defun canvas-to-png (canvas)
  (let* ((height (array-dimension canvas 0))
         (width (array-dimension canvas 1))
         (png (make-instance 'zpng:png
                             :color-type :grayscale
                             :width width
                             :height height))
         (image (zpng:data-array png)))
    (dotimes (y height png)
      (dotimes (x width)
        (setf (aref image y x 0) (aref canvas y x))))))

(defun elisp-fun-png (width height text)
  (canvas-to-png (test-draw-line text :width width :height height)))

(defun elisp-fun-xbm (width height text)
  ;; very slow
  (let* ((canvas
           (test-draw-line text :width width :height height))
         (bits (2d-array-to-list canvas))
         (elisp `(slime-media-insert-image
                  (create-image
                   (vconcat (mapcar (lambda (x)
                                      (apply #'bool-vector x))
                                    (quote ,bits)))
                   'xbm t :width ,width :height ,height)
                  " ")))
    (if *dry-run* elisp (swank:eval-in-emacs elisp))))

;; (setq slime-enable-evaluate-in-emacs t)

(defun build-zig-function (name code)
  ;; For some weird reason, Zig when called from the Lisp process like this
  ;; seems to generate object files without the exported functions.
  (let* ((source-path
           (format nil "zig-lisp/~A.zig" name))
         (build-command
           (format nil "zig build-lib -dynamic ~A" source-path))
         (library-filename
           (format nil "lib~A.so" name))
         (library-path
           (format nil "zig-lisp/~A" library-filename)))
    (with-open-file (source-file source-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-sequence code source-file)
      (uiop:run-program build-command
                        :output :interactive
                        :error-output :interactive)
      (uiop:run-program (format nil "mv ~A zig-lisp/" library-filename))
      (cffi::register-foreign-library name `((:unix ,library-path)))
      (load-foreign-library name))))
