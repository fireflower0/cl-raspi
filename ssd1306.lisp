(defpackage :cl-raspi/ssd1306
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :+black+
           :+white+
           :+inverse+
           :ssd1306-init
           :ssd1306-clear
           :ssd1306-display
           :ssd1306-draw-pixel
           :ssd1306-draw-line
           :ssd1306-draw-rect
           :ssd1306-draw-fill-rect))
(in-package :cl-raspi/ssd1306)

;; Constants
(defconstant +ssd1306-i2c-address+           #X3C)
(defconstant +ssd1306-set-contrast+          #X81)
(defconstant +ssd1306-display-all-on-resume+ #XA4)
(defconstant +ssd1306-display-all-on+        #XA5)
(defconstant +ssd1306-normal-display+        #XA6)
(defconstant +ssd1306-invert-display+        #XA7)
(defconstant +ssd1306-display-off+           #XAE)
(defconstant +ssd1306-display-on+            #XAF)
(defconstant +ssd1306-set-display-offset+    #XD3)
(defconstant +ssd1306-set-com-pins+          #XDA)
(defconstant +ssd1306-set-vcom-detect+       #XDB)
(defconstant +ssd1306-set-display-clock-div+ #XD5)
(defconstant +ssd1306-set-pre-charge+        #XD9)
(defconstant +ssd1306-set-multi-plex+        #XA8)
(defconstant +ssd1306-set-low-column+        #X00)
(defconstant +ssd1306-set-high-column+       #X10)
(defconstant +ssd1306-set-start-line+        #X40)
(defconstant +ssd1306-memory-mode+           #X20)
(defconstant +ssd1306-column-addr+           #X21)
(defconstant +ssd1306-page-addr+             #X22)
(defconstant +ssd1306-com-scan-inc+          #XC0)
(defconstant +ssd1306-com-scan-dec+          #XC8)
(defconstant +ssd1306-seg-remap+             #XA0)
(defconstant +ssd1306-charge-pump+           #X8D)
(defconstant +ssd1306-external-vcc+          #X01)
(defconstant +ssd1306-switch-cap-vcc+        #X02)

;; Scrolling constants
(defconstant +ssd1306-activate-scroll+                      #X2F)
(defconstant +ssd1306-deactivate-scroll+                    #X2E)
(defconstant +ssd1306-set-vertical-scroll-area+             #XA3)
(defconstant +ssd1306-right-horizontal-scroll+              #X26)
(defconstant +ssd1306-left-horizontal-scroll+               #X27)
(defconstant +ssd1306-vertical-and-right-horizontal-scroll+ #X29)
(defconstant +ssd1306-vertical-and-left-horizontal-scroll+  #X2A)

;; Color
(defconstant +black+   0)
(defconstant +white+   1)
(defconstant +inverse+ 2)

;; Parameters
(defparameter *width*  128)
(defparameter *height* 64)
(defparameter *pages*  (/ *height* 8))
(defparameter *buffer* (make-array `(,(* *width* *pages*))
                                   :initial-element 0))
(defparameter *vcc-state* 0)

;; File Descriptor
(defparameter *fd* (wiringpi-i2c-setup +ssd1306-i2c-address+))

;;; I2C Bus data format
;; +----+-----+-+-+-+-+-+-+
;; | Co | D/C |0|0|0|0|0|0|
;; +----+-----+-+-+-+-+-+-+
;; Co bit = 0 (continue), D/C# = 0 (command)
(defconstant +command+ #X00)
;; Co bit = 0 (continue), D/C# = 1 (data)
(defconstant +data+    #X40)
;; Co bit = 1 (One command only), D/C# = 0 (command)
(defconstant +control+ #X80)
;; Co bit = 1 (One command only), D/C# = 1 (data)
(defconstant +onedata+ #XC0)

(defun i2c-write (type value)
  (wiringpi-i2c-write-reg8 *fd* type value))

(defun command (value)
  (i2c-write +command+ value))

(defun data (value)
  (i2c-write +data+ value))

(defun control (value)
  (i2c-write +control+ value))

(defun onedata (value)
  (i2c-write +onedata+ value))

(defun write-list (func data)
  (dotimes (n (length data))
    (funcall func (aref data n))))

(defun ssd1306-init (&optional (vcc-state +ssd1306-switch-cap-vcc+))
  (setf *vcc-state* vcc-state)
  (command +ssd1306-display-off+)
  (command +ssd1306-set-display-clock-div+)
  (command #X80) ; the suggested ratio
  (command +ssd1306-set-multi-plex+)
  (command #X3F)
  (command +ssd1306-set-display-offset+)
  (command #X00) ; no offset
  (command (logior +ssd1306-set-start-line+ #X00))
  (command +ssd1306-charge-pump+)
  (if (= vcc-state +ssd1306-external-vcc+)
      (command #X10)
      (command #X14))
  (command +ssd1306-memory-mode+)
  (command #X00)
  (command (logior +ssd1306-seg-remap+ #X01))
  (command +ssd1306-com-scan-dec+)
  (command +ssd1306-set-com-pins+)
  (command #X12)
  (command +ssd1306-set-contrast+)
  (if (= vcc-state +ssd1306-external-vcc+)
      (command #X9F)
      (command #XCF))
  (command +ssd1306-set-pre-charge+)
  (if (= vcc-state +ssd1306-external-vcc+)
      (command #X22)
      (command #XF1))
  (command +ssd1306-set-vcom-detect+)
  (command #X40)
  (command +ssd1306-display-all-on-resume+)
  (command +ssd1306-normal-display+)
  (command +ssd1306-display-on+))

(defun ssd1306-clear ()
  (fill *buffer* 0))

(defun ssd1306-set-contrast (contrast)
  (when (or (< contrast 0) (> contrast 255))
    (error "Contrast must be a value from 0 to 255"))
  (command +ssd1306-set-contrast+)
  (command contrast))

(defun ssd1306-display ()
  (command +ssd1306-column-addr+)
  (command 0)             ; Column start address (0 = reset)
  (command (- *width* 1)) ; Column end address
  (command +ssd1306-page-addr+)
  (command 0)             ; Page start address (0 = reset)
  (command (- *pages* 1)) ; Page end address
  (do ((i 0 (+ i 16)))
      ((= i (length *buffer*)))
    (write-list #'data (subseq *buffer* i (+ i 16)))))

(defun ssd1306-clear-display ()
  (ssd1306-clear)
  (ssd1306-display))

(defun ssd1306-draw-pixel (x y &key (color +white+))
  (when (or (< x 0) (> x 127))
    (error "x must be a value from 0 to 127"))
  (when (or (< y 0) (> y 64))
    (error "y must be a value from 0 to 63"))
  (let ((index (+ x (* (floor y *pages*) *width*)))
        (value (ash 1 (rem y 8))))
    (setf (aref *buffer* index)
          (cond ((= color +white+)
                 (logior (aref *buffer* index) value))
                ((= color +black+)
                 (logand (aref *buffer* index) (lognot value)))
                ((= color +inverse+)
                 (logxor (aref *buffer* index) value))))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun ssd1306-draw-line (sx sy dx dy &key (color +white+))
  (let* ((x sx) (y sy)
         (wx (abs (- dx sx)))
         (wy (abs (- dy sy)))
         (diff 0))
    (while (or (/= x dx) (/= y dy))
      (ssd1306-draw-pixel x y :color color)
      (if (>= wx wy)
          (progn
            (if (< sx dx)
                (unless (>= x 127)
                  (incf x))
                (unless (<= x 0)
                  (decf x)))
            (incf diff (ash (- dy sy) 1))
            (if (> diff wx)
                (unless (>= y 63)
                  (incf y)
                  (decf diff (ash wx 1)))
                (unless (<= y 0)
                  (decf y)
                  (incf diff (ash wx 1)))))
          (progn
            (if (< sy dy)
                (unless (>= y 63)
                  (incf y))
                (unless (<= y 0)
                  (decf y)))
            (incf diff (ash (- dx sx) 1))
            (if (> diff wy)
                (unless (>= x 127)
                  (incf x)
                  (decf diff (ash wy 1)))
                (unless (<= x 0)
                  (decf x)
                  (incf diff (ash wy 1)))))))))

(defun ssd1306-draw-rect (x y w h &key (color +white+))
  (ssd1306-draw-line x y (+ x w) y :color color)
  (ssd1306-draw-line (+ x w) y (+ x w) (+ y h) :color color)
  (ssd1306-draw-line (+ x w) (+ y h) x (+ y h) :color color)
  (ssd1306-draw-line x (+ y h) x y) :color color)

(defun ssd1306-draw-fill-rect (x y w h &key (color +white+))
  (dotimes (n h)
    (ssd1306-draw-line x (+ y n) (+ x w) (+ y n) :color color)))
