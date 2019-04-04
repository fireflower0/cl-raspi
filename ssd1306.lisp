(defpackage :cl-raspi/ssd1306
  (:use :cl
        :cl-raspi/lib-wiring-pi))
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

;; Parameters
(defparameter *width*  128)
(defparameter *height* 64)
(defparameter *pages*  (/ *height* 8))
(defparameter *buffer* (make-array `(,(* *width* *pages*))
                                   :initial-element 0))

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

(defun byte-to-hex (data)
  (let ((str ""))
    (dolist (n (coerce data 'list))
      (setf str (concatenate 'string str (write-to-string n))))
    (format nil "~X" (parse-integer str :radix 2))))

(defun write-list (func data)
  (let ((msb (byte-to-hex (subseq data 0 8)))
        (lsb (byte-to-hex (subseq data 8 16))))
    (funcall func (parse-integer (concatenate 'string msb lsb) :radix 16))))

(defun ssd1306-init (&optional (vcc-state +ssd1306-switch-cap-vcc+))
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

(defun ssd1306-display ()
  (command +ssd1306-column-addr+)
  (command 0)             ; Column start address (0 = reset)
  (command (- *width* 1)) ; Column end address
  (command +ssd1306-page-addr+)
  (command 0)             ; Page start address (0 = reset)
  (command (- pages 1))   ; Page end address
  (do ((i 0 (+ i 16)))
      ((= i (length *buffer*)))
    (write-list #'data (subseq *buffer* i (+ i 16)))))
