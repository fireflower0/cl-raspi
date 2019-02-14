(defpackage :cl-raspi/src/i2c-lcd-ltk
  (:use :cl
        :ltk
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/i2c-lcd-ltk)

;; I2C device address (0x3e)
(defconstant +i2c-addr+ #X3E)

;; LCD contrast (0x00-0x0F)
(defconstant +contrast+ #X0A)

;; LCD column (16)
(defconstant +column+ 16)

(defun init (fd)
  (let ((fcnt (logior (logand +contrast+ #X0F) #X70)))
    (wiringpi-i2c-write-reg8 fd #X00 #X38) ; Function set : 8bit, 2 line
    (wiringpi-i2c-write-reg8 fd #X00 #X39) ; Function set : 8bit, 2 line, IS=1
    (wiringpi-i2c-write-reg8 fd #X00 #X14) ; Internal OSC freq
    (wiringpi-i2c-write-reg8 fd #X00 fcnt) ; Contrast set
    (wiringpi-i2c-write-reg8 fd #X00 #X5F) ; Power/ICON/Constract
    (wiringpi-i2c-write-reg8 fd #X00 #X6A) ; Follower control
    (delay 300)                            ; Wait time (300 ms)
    (wiringpi-i2c-write-reg8 fd #X00 #X39) ; Function set : 8 bit, 2 line, IS=1
    (wiringpi-i2c-write-reg8 fd #X00 #X06) ; Entry mode set
    (wiringpi-i2c-write-reg8 fd #X00 #X0C) ; Display on/off
    (wiringpi-i2c-write-reg8 fd #X00 #X01) ; Clear display
    (delay 30)                             ; Wait time (0.3 ms)
    (wiringpi-i2c-write-reg8 fd #X00 #X02) ; Return home
    (delay 30)))                           ; Wait time (0.3 ms)))

(defun display-char (fd line entry)
  (wiringpi-i2c-write-reg8 fd #X00 line)    ; Set cursor first line
  (dotimes (count +column+)                 ; Clear first line
    (wiringpi-i2c-write-reg8 fd #X40 #X20))
  (wiringpi-i2c-write-reg8 fd #X00 line)    ; Reset cursor first line
  (loop :for char :across (text entry)      ; Display string
     :do (wiringpi-i2c-write-reg8 fd #X40 (char-code char))))

(defun display-text (fd)
  (let ((lbl1 (make-instance 'label :text "First line" :width 60))
        (entry1 (make-instance 'entry))
        (btn1 (make-instance 'button :text "Button1"))
        (lbl2 (make-instance 'label :text "Second line" :width 60))
        (entry2 (make-instance 'entry))
        (btn2 (make-instance 'button :text "Button2")))
    (setf (command btn1) (lambda () (display-char fd #X80 entry1)))
    (setf (command btn2) (lambda () (display-char fd #XC0 entry2)))
    (focus entry1)
    (pack (list lbl1 entry1 btn1 lbl2 entry2 btn2) :fill :x)))

(defun control-icon (fd arg1 arg2)
  (wiringpi-i2c-write-reg8 fd #X00 arg1)
  (wiringpi-i2c-write-reg8 fd #X40 arg2))

(defun control-icon-clear (fd)
  (control-icon fd #X40 #X00)   ; Antenna clear
  (control-icon fd #X42 #X00)   ; Phone clear
  (control-icon fd #X44 #X00)   ; Sound clear
  (control-icon fd #X46 #X00)   ; Input clear
  (control-icon fd #X47 #X00)   ; Up Down clear
  (control-icon fd #X49 #X00)   ; KeyLock clear
  (control-icon fd #X4B #X00)   ; Mute clear
  (control-icon fd #X4D #X00)   ; Battery clear
  (control-icon fd #X4F #X00))  ; Other clear

(defun display-icon (fd)
  (let* ((f    (make-instance 'frame))
         (lbl3 (make-instance 'label  :text "Icon Control Button"))
         (b1   (make-instance 'button :text "Antenna"))
         (b2   (make-instance 'button :text "Phone"))
         (b3   (make-instance 'button :text "Sound"))
         (b4   (make-instance 'button :text "Input"))
         (b5   (make-instance 'button :text "Up"))
         (b6   (make-instance 'button :text "Down"))
         (b7   (make-instance 'button :text "KeyLock"))
         (b8   (make-instance 'button :text "Mute"))
         (b9   (make-instance 'button :text "Battery1"))
         (bA   (make-instance 'button :text "Battery2"))
         (bB   (make-instance 'button :text "Battery3"))
         (bC   (make-instance 'button :text "Battery4"))
         (bD   (make-instance 'button :text "Other"))
         (bClr (make-instance 'button :text "Clear")))
    (setf (command b1)   (lambda () (control-icon fd #X40 #X10)))   ; Antenna
    (setf (command b2)   (lambda () (control-icon fd #X42 #X10)))   ; Phone
    (setf (command b3)   (lambda () (control-icon fd #X44 #X10)))   ; Sound
    (setf (command b4)   (lambda () (control-icon fd #X46 #X10)))   ; Input
    (setf (command b5)   (lambda () (control-icon fd #X47 #X10)))   ; Up
    (setf (command b6)   (lambda () (control-icon fd #X47 #X08)))   ; Down
    (setf (command b7)   (lambda () (control-icon fd #X49 #X10)))   ; KeyLock
    (setf (command b8)   (lambda () (control-icon fd #X4B #X10)))   ; Mute
    (setf (command b9)   (lambda () (control-icon fd #X4D #X10)))   ; Battery1
    (setf (command bA)   (lambda () (control-icon fd #X4D #X08)))   ; Battery2
    (setf (command bB)   (lambda () (control-icon fd #X4D #X04)))   ; Battery3
    (setf (command bC)   (lambda () (control-icon fd #X4D #X02)))   ; Battery4
    (setf (command bD)   (lambda () (control-icon fd #X4F #X10)))   ; Other
    (setf (command bClr) (lambda () (control-icon-clear fd)))
    (pack lbl3)
    (pack (list b1 b2 b3 b4 b5 b6 b7 b8 b9 bA bB bC bD) :side :left)
    (pack bClr :fill :x)
    (configure f :borderwidth 3)
    (configure f :relief :sunken)))

(defun main ()
  (let  ((fd (wiringpi-i2c-setup +i2c-addr+)))
    (init fd)
    (with-ltk ()
      (wm-title *tk* "MI2CLCD-01 Control Application")
      (bind *tk* "<Alt-q>" (lambda (event)
                             (setq *exit-mainloop* t)))
      (display-text fd)
      (display-icon fd))))
