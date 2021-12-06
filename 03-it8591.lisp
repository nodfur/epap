;;
;; Driver for the IT8591 e-paper display controller via GPIO/SPI from
;; the Raspberry Pi 400.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module parameters and interesting variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *vcom* nil
  "This is a unit-specific voltage value.")

(defvar *display-width*
  "The discovered pixel width of the e-paper screen.")

(defvar *display-height*
  "The discovered pixel height of the e-paper screen.")

(defvar *framebuffer-address*
  ;; I don't really know how this works.
  ;; Can we use multiple framebuffers?
  ;; Can they be composited together somehow?
  "The discovered address of the default framebuffer.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level protocol stuff for the IT8591 controller.
;;
;; We introduce GPIO pins, commands, packets, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype pin ()
  '(member :rst :cs :busy))

(deftype packet-type ()
  '(member :read :write :command))

(deftype command-id ()
  '(member
    :run :standby :sleep :read-register :write-register
    :vcom :dev-info :load-img-area-start :load-img-end
    :display-area :display-area-buf))

(deftype register ()
  '(member
    :I80CPCR
    :LISAR+0
    :LISAR+2
    :LUTAFSR
    :UP1SR+2
    :BGVR))

(defun register-number (register)
  (ecase register
    (:I80CPCR #x4)
    (:LISAR+0 (+ #x200 #x8))
    (:LISAR+2 (+ #x200 #x8 2))
    (:LUTAFSR (+ #x1000 #x224))
    (:UP1SR+2 (+ #x1000 #x138 2))
    (:BGVR (+ #x1000 #x250))))

(defun pin-number (pin)
  (ecase pin
    (:rst 17)
    (:cs 8)
    (:busy 24)))

(defun packet-type-number (packet-type)
  (ecase packet-type
    (:read #x1000)
    (:write #x0000)
    (:command #x6000)))

(defun command-number (command-id)
  (ecase command-id
    (:run #x1)
    (:standby #x2)
    (:sleep #x3)
    (:read-register #x10)
    (:write-register #x11)
    (:vcom #x39)
    (:dev-info #x302)
    (:load-img-area-start #x21)
    (:load-img-end #x22)
    (:display-area #x34)
    (:display-area-buf #x37)))

(defun initialize-bcm2835 ()
  (no-dry-run)

  (unless (bcm2835-init)
    (error "bcm2835_init failed"))
  (unless (bcm2835-spi-begin)
    (error "bcm2835_spi_begin failed"))

  (bcm2835-spi-setBitOrder :most-significant-bit-first)
  (bcm2835-spi-setDataMode :mode-0)
  (bcm2835-spi-setClockDivider :divider-32))

(defun close-bcm2835 ()
  (no-dry-run)
  (gpio-write :cs 0)
  (gpio-write :rst 0)
  (bcm2835-spi-end)
  (unless (bcm2835-close)
    (error "bcm2835_close failed")))

(defun gpio-mode (pin mode)
  (no-dry-run)
  (bcm2835-gpio-fsel (pin-number pin) mode))

(defun gpio-read (pin)
  (no-dry-run)
  (bcm2835-gpio-lev (pin-number pin)))

(defun gpio-wait ()
  (sb-ext:with-timeout 5
    (loop until (= 1 (gpio-read :busy)))))

(defun cs-low ()
  (gpio-write :cs 0))

(defun cs-high ()
  (gpio-write :cs 1))

(defun initialize-gpio ()
  (gpio-mode :rst :output)
  (gpio-mode :cs :output)
  (gpio-mode :busy :input)
  (cs-high))

(defun start-packet (packet-type)
  (gpio-wait)
  (cs-low)
  (spi-write-word (packet-type-number packet-type))
  (gpio-wait))

(defun write-command (cmd)
  (maybe-dry-run `(write-command ,cmd)
    (start-packet :command)
    (spi-write-word (command-number cmd))
    (cs-high)))

(defun spi-write-word (x)
  (no-dry-run)
  (spi-write-byte (ldb (byte 8 8) x))
  (spi-write-byte (ldb (byte 8 0) x)))

(defun spi-read-word ()
  (let* ((high (spi-read-byte))
         (low (spi-read-byte)))
    (dpb high (byte 8 8) low)))

(defun spi-read-address ()
  (let* ((low (spi-read-word))
         (high (spi-read-word)))
    (dpb high (byte 16 16) low)))

(defun start-reading ()
  (start-packet :read)
  (spi-read-word) ;; read a dummy word
  (gpio-wait))

(defun request-word ()
  (start-reading)
  (prog1 (spi-read-word)
    (cs-high)))

(defun write-word-packet (word)
  (maybe-dry-run `(write-word-packet ,word)
    (start-packet :write)
    (spi-write-word word)
    (cs-high)))

(defun write-word-packets (word-list)
  (maybe-dry-run `(write-word-packets ,word-list)
    (loop for word in word-list
          do (write-word-packet word))))

(defun write-register (register value)
  (maybe-dry-run `(write-register ,register ,value)
    (write-command :write-register)
    (write-word-packet (register-number register))
    (write-word-packet value)))

(defun read-register (register)
  (write-command :read-register)
  (write-word-packet (register-number register))
  (request-word))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Having established the communication primitives for the IT8591 protocol,
;; we can define composite operations that read and write multiple words.
;;
;; Now we start to talk about pixel formats, panel widths, framebuffers,
;; partial updates, and so on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bpp-pixel-format (bits)
  (ecase bits
    (2 0)
    (3 1)
    (4 2)
    (8 3)))

(defstruct system-info
  width height address firmware-version lut-version)

(defun get-system-info ()
  (write-command :dev-info)
  (start-reading)
  (prog1
      (make-system-info
       :width (spi-read-word)
       :height (spi-read-word)
       :address (spi-read-address)
       :firmware-version
       (babel:octets-to-string (spi-read-bytes 16) :encoding :ascii)
       :lut-version
       (babel:octets-to-string (spi-read-bytes 16) :encoding :ascii))
    (cs-high)))

(defun reset-display ()
  (gpio-write :rst 1)
  (delay-milliseconds 200)
  (gpio-write :rst 0)
  (delay-milliseconds 10)
  (gpio-write :rst 1)
  (delay-milliseconds 200))

(defun set-vcom (vcom)
  (write-command :vcom)
  (write-word-packet 1)
  (write-word-packet (round (* 1000 (abs vcom)))))

(defun initialize-display ()
  (reset-display)
  (write-command :run)
  (prog1 (get-system-info)
    (write-register :I80CPCR 1)
    (set-vcom
     (or *vcom* (restart-case (error "VCOM value required")
                  (provide-vcom-value (vcom) vcom))))))

(defun enter-sleep-mode ()
  (write-command :sleep))

(defun start-display ()
  (initialize-bcm2835)
  (initialize-gpio)
  (let ((info (initialize-display)))
    (prog1 info
      (setf *display-width* (system-info-width info)
            *display-height* (system-info-height info)
            *framebuffer-address* (system-info-address info)))))

(defun stop-display ()
  (enter-sleep-mode)
  (close-bcm2835))

(defmacro with-display-running (&body body)
  `(progn
     (start-display)
     (unwind-protect (progn ,@body)
       (stop-display))))
