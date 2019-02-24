(defpackage :cl-raspi/src/i2c-oled-ssd1306-graphics
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/i2c-oled-ssd1306-graphics)

(defconstant +i2c-addr+ #X3C)

(defconstant +ssd1306-disp-on+           #XAF)
(defconstant +ssd1306-disp-off+          #XAE)
(defconstant +ssd1306-set-disp-clk-div+  #XD5)
(defconstant +ssd1306-set-multiplex+     #XA8)
(defconstant +ssd1306-set-disp-offset+   #XD3)
(defconstant +ssd1306-set-start-line+    #X40)
(defconstant +ssd1306-seg-re-map+        #XA1)
(defconstant +ssd1306-com-scan-inc+      #XC0)
(defconstant +ssd1306-com-scan-dec+      #XC8)
(defconstant +ssd1306-set-com-pins+      #XDA)
(defconstant +ssd1306-set-contrast+      #X81)
(defconstant +ssd1306-disp-allon-resume+ #XA4)
(defconstant +ssd1306-normal-disp+       #XA6)
(defconstant +ssd1306-charge-pump+       #X8D)
(defconstant +ssd1306-deactivate-scroll+ #X2E)
(defconstant +ssd1306-set-mem-addr-mode+ #X20)
(defconstant +ssd1306-set-column-addr+   #X21)
(defconstant +ssd1306-set-page-addr+     #X22)

(defparameter *ssd1306-lcd-width*         128)
(defparameter *ssd1306-lcd-height*        64)

;;; I2C Bus data format
;; +----+-----+-+-+-+-+-+-+
;; | Co | D/C |0|0|0|0|0|0|
;; +----+-----+-+-+-+-+-+-+
;; Co bit = 0 (continue), D/C# = 0 (command)
(defconstant +ssd1306-command+ #X00)
;; Co bit = 0 (continue), D/C# = 1 (data)
(defconstant +ssd1306-data+    #X40)
;; Co bit = 1 (One command only), D/C# = 0 (command)
(defconstant +ssd1306-control+ #X80)
;; Co bit = 1 (One command only), D/C# = 1 (data)
(defconstant +ssd1306-onedata+ #XC0)

(defun ssd1306-command (fd value)
  (wiringpi-i2c-write-reg8 fd +ssd1306-command+ value))

(defun ssd1306-data (fd value)
  (wiringpi-i2c-write-reg8 fd +ssd1306-data+ value))

(defun ssd1306-control (fd value)
  (wiringpi-i2c-write-reg8 fd +ssd1306-control+ value))

(defun ssd1306-onedata (fd value)
  (wiringpi-i2c-write-reg8 fd +ssd1306-onedata+ value))

(defun init (fd)
  ;; Display Off                         #XAE
  (ssd1306-command fd +ssd1306-disp-off+)
  ;; Set MUX Raio                        #XA8, #X3F(63)
  (ssd1306-command fd +ssd1306-set-multiplex+)
  (ssd1306-command fd (1- *ssd1306-lcd-height*))
  ;; Set Display Offset                  #XD3, #X00
  (ssd1306-command fd +ssd1306-set-disp-offset+)
  (ssd1306-command fd #X00)   ; no offset
  ;; Set Display Start Line              #X40
  (ssd1306-command fd +ssd1306-set-start-line+)
  ;; Set Segment re-map                  #XA0/#XA1
  (ssd1306-command fd +ssd1306-seg-re-map+)
  ;; Set COM Output Scan Direction       #XC0/#XC8
  (ssd1306-command fd +ssd1306-com-scan-dec+)
  ;; Set COM Pins hardware configuration #XDA, #X02
  (ssd1306-command fd +ssd1306-set-com-pins+)
  (ssd1306-command fd #X12)
  ;; Set Contrast Control                #X81, #X7F
  (ssd1306-command fd +ssd1306-set-contrast+)
  (ssd1306-command fd #X7F)
  ;; Disable Entire Display On           #XA4
  (ssd1306-command fd +ssd1306-disp-allon-resume+)
  ;; Set Normal Display                  #XA6
  (ssd1306-command fd +ssd1306-normal-disp+)
  ;; Set Osc Frequency                   #XD5, #X80
  (ssd1306-command fd +ssd1306-set-disp-clk-div+)
  (ssd1306-command fd #X80)   ; the suggested ratio 0x80
  ;; Deactivate scroll                   #X2E
  (ssd1306-command fd +ssd1306-deactivate-scroll+)
  ;; Set Memory Addressing Mode          #X20, #X10
  (ssd1306-command fd +ssd1306-set-mem-addr-mode+)
  (ssd1306-command fd #X10)   ; Page addressing Mode
  ;; Set Column Address                  #X21
  (ssd1306-command fd +ssd1306-set-column-addr+)
  (ssd1306-command fd 0)      ; Column Start Address
  (ssd1306-command fd 127)    ; Column Stop Address
  ;; Set Page Address                    #X22
  (ssd1306-command fd +ssd1306-set-page-addr+)
  (ssd1306-command fd 0)      ; Vertical start position
  (ssd1306-command fd 7)      ; Vertical end position
  ;; Enable change pump regulator        #X8D, #X14
  (ssd1306-command fd +ssd1306-charge-pump+)
  (ssd1306-command fd #X14)
  ;; Display On                          #XAF
  (ssd1306-command fd +ssd1306-disp-on+))

(defun clear (fd)
  (dotimes (i 8)
    (ssd1306-control fd (logior #XB0 i))   ; set page start address
    (dotimes (j 16)
      (dotimes (k 8)
        (ssd1306-data fd #X00)))))

(defun point (fd x y)
  (ssd1306-command fd (+ #X00 (logand (+ x 2) #X0F)))
  (ssd1306-command fd (+ #X10 (ash (+ x 2) -4)))
  (ssd1306-command fd (+ #XB0 (ash y -3)))
  (ssd1306-command fd #XE0)
  (ssd1306-onedata fd (ash 1 (logand y #X07)))
  (ssd1306-command fd #XEE))

(defparameter *x0* 32)
(defparameter *y0* 32)
(defparameter *v* '(1 0))

(defun moveto (x y)
  (setf *x0* x)
  (setf *y0* y))

(defun drawto (fd x y)
  (let* ((dx (abs (- x *x0*)))
         (dy (abs (- y *y0*)))
         (sx (if (< *x0* x) 1 -1))
         (sy (if (< *y0* y) 1 -1))
         (err (- dx dy))
         e2)
    (loop
      (point fd *x0* *y0*)
      (when (and (= *x0* x) (= *y0* y)) (return))
      (setf e2 (ash err 1))
      (when (> e2 (- dy)) (decf err dy) (incf *x0* sx))
      (when (< e2 dx) (incf err dx) (incf *y0* sy))
      (delay 10))))

(defun main ()
  (let ((fd (wiringpi-i2c-setup +i2c-addr+)))
    (init fd)
    (clear fd)
    (moveto 0 0)
    (drawto fd 127 63)))

(defun forward (fd size)
  (drawto fd
   (+ *x0* (* (first *v*) size))
   (+ *y0* (* (second *v*) size))))

(defun left ()
  (setf *v* (list (- (second *v*)) (first *v*))))

(defun right ()
  (setf *v* (list (second *v*) (- (first *v*)))))

(defun ldragon (fd size level)
  (cond
   ((= level 0) (forward fd size))
   (t
    (ldragon fd size (- level 1))
    (left)
    (rdragon fd size (- level 1)))))

(defun rdragon (fd size level)
  (cond
   ((= level 0) (forward fd size))
   (t
    (ldragon fd size (- level 1))
    (right)
    (rdragon fd size (- level 1)))))