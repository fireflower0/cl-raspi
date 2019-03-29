(defpackage :cl-raspi/bo1602dgrnjb
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :bo1602dgrnjb-init
           :bo1602dgrnjb-text-area-clear
           :bo1602dgrnjb-text-area-all-clear
           :bo1602dgrnjb-text
           :bo1602dgrnjb-icon-display
           :bo1602dgrnjb-icon-all-clear))
(in-package :cl-raspi/bo1602dgrnjb)

;; I2C device address (0x3e)
(defconstant +i2c-addr-lcd+ #X3E)
(defparameter *lcd-fd* nil)

;; LCD contrast (0x00-0x0F)
(defconstant +contrast+ #X0A)

;; Text Line
(defconstant +column-size+ 16)
(defconstant +first-line+   #X80)
(defconstant +second-line+  #XC0)

;; Icon address
(defconstant +icon-num+ 13)
(defparameter *icon-antenna*  '(#X40 #X10))
(defparameter *icon-phone*    '(#X42 #X10))
(defparameter *icon-sound*    '(#X44 #X10))
(defparameter *icon-input*    '(#X46 #X10))
(defparameter *icon-up*       '(#X47 #X10))
(defparameter *icon-down*     '(#X47 #X08))
(defparameter *icon-keylock*  '(#X49 #X10))
(defparameter *icon-mute*     '(#X4B #X10))
(defparameter *icon-battery1* '(#X4D #X10))
(defparameter *icon-battery2* '(#X4D #X08))
(defparameter *icon-battery3* '(#X4D #X04))
(defparameter *icon-battery4* '(#X4D #X02))
(defparameter *icon-other*    '(#X4F #X10))
(defparameter *icon-addr-arr*
  (make-array +icon-num+
              :initial-contents
              `(,*icon-antenna*
                ,*icon-phone*
                ,*icon-sound*
                ,*icon-input*
                ,*icon-up*
                ,*icon-down*
                ,*icon-keylock*
                ,*icon-mute*
                ,*icon-battery1*
                ,*icon-battery2*
                ,*icon-battery3*
                ,*icon-battery4*
                ,*icon-other*)))

(defun i2c-command-write (data)
  (unless *lcd-fd*
    (error "Not initialized! Execute the bo1602dgrnjb-init function"))
  (wiringpi-i2c-write-reg8 *lcd-fd* #X00 data))

(defun i2c-data-write (data)
  (unless *lcd-fd*
    (error "Not initialized! Execute the bo1602dgrnjb-init function"))
  (wiringpi-i2c-write-reg8 *lcd-fd* #X40 data))

(defun bo1602dgrnjb-init ()
  (setf *lcd-fd* (wiringpi-i2c-setup +i2c-addr-lcd+))
  (let ((fcnt (logior (logand +contrast+ #X0F) #X70)))
    (i2c-command-write #X38) ; Function set : 8bit, 2 line
    (i2c-command-write #X39) ; Function set : 8bit, 2 line, IS=1
    (i2c-command-write #X14) ; Internal OSC freq
    (i2c-command-write fcnt) ; Contrast set
    (i2c-command-write #X5F) ; Power/ICON/Constract
    (i2c-command-write #X6A) ; Follower control
    (delay 300)                                  ; Wait time (300 ms)
    (i2c-command-write #X39) ; Function set : 8 bit, 2 line, IS=1
    (i2c-command-write #X06) ; Entry mode set
    (i2c-command-write #X0C) ; Display on/off
    (i2c-command-write #X01) ; Clear display
    (delay 30)                                   ; Wait time (0.3 ms)
    (i2c-command-write #X02) ; Return home
    (delay 30)))                                 ; Wait time (0.3 ms)

(defun bo1602dgrnjb-get-line-addr (line)
  (let ((line-addr (cond ((= line 1) +first-line+)
                         ((= line 2) +second-line+))))
    line-addr))

(defun bo1602dgrnjb-text-area-clear (line)
  (unless (bo1602dgrnjb-line-num-p line)
    (error "Invalid argument! line:~A(1 or 2)" line))
  (let ((line-addr (bo1602dgrnjb-get-line-addr line)))
    (i2c-command-write line-addr)
    (dotimes (count +column-size+)
      (i2c-data-write #X20))))

(defun bo1602dgrnjb-text-area-all-clear ()
  (bo1602dgrnjb-text-area-clear 1)
  (bo1602dgrnjb-text-area-clear 2))

(defun bo1602dgrnjb-line-num-p (line)
  (and (numberp line)
       (or (= line 1)
           (= line 2))))

(defun bo1602dgrnjb-char-p (char)
  (let ((code (char-code char)))
    (and (>= code #B00000000)
         (<= code #B11111111))))

(defun bo1602dgrnjb-string-p (entry)
  (and (stringp entry)
       (every #'bo1602dgrnjb-char-p entry)
       (<= (length entry) +column-size+)))

(defun bo1602dgrnjb-text (line entry)
  (unless (and (bo1602dgrnjb-line-num-p line)
               (bo1602dgrnjb-string-p entry))
    (error "Invalid argument! line:~A(1 or 2) entry:~A(Type string) length:~A(<= 16)"
           line entry (length entry)))
  (bo1602dgrnjb-text-area-clear line)
  (let ((line-addr (bo1602dgrnjb-get-line-addr line)))
    (i2c-command-write line-addr)
    (loop :for char :across entry
       :do (i2c-data-write (char-code char)))))

(defun bo1602dgrnjb-get-icon-addr (icon-addr)
  (first icon-addr))

(defun bo1602dgrnjb-get-icon-ram-bit (icon-addr)
  (second icon-addr))

(defun bo1602dgrnjb-icon (icon-addr)
  (i2c-command-write (bo1602dgrnjb-get-icon-addr icon-addr))
  (i2c-data-write    (bo1602dgrnjb-get-icon-ram-bit icon-addr)))

(defun bo1602dgrnjb-icon-index-p (index)
  (and (numberp index)
       (>= index 0)
       (< index +icon-num+)))

(defun bo1602dgrnjb-icon-display (index)
  (unless (bo1602dgrnjb-icon-index-p index)
    (error "Invalid index: ~A(0-12)" index))
  (bo1602dgrnjb-icon (aref *icon-addr-arr* index)))

(defun bo1602dgrnjb-icon-all-clear ()
  (bo1602dgrnjb-icon '(#X40 #X00))  ; Antenna clear
  (bo1602dgrnjb-icon '(#X42 #X00))  ; Phone clear
  (bo1602dgrnjb-icon '(#X44 #X00))  ; Sound clear
  (bo1602dgrnjb-icon '(#X46 #X00))  ; Input clear
  (bo1602dgrnjb-icon '(#X47 #X00))  ; Up Down clear
  (bo1602dgrnjb-icon '(#X49 #X00))  ; KeyLock clear
  (bo1602dgrnjb-icon '(#X4B #X00))  ; Mute clear
  (bo1602dgrnjb-icon '(#X4D #X00))  ; Battery clear
  (bo1602dgrnjb-icon '(#X4F #X00))) ; Other clear
