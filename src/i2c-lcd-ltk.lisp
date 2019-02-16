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

(defun display-text (fd line entry)
  (wiringpi-i2c-write-reg8 fd #X00 line)    ; Set cursor first line
  (dotimes (count +column+)                 ; Clear first line
    (wiringpi-i2c-write-reg8 fd #X40 #X20))
  (wiringpi-i2c-write-reg8 fd #X00 line)    ; Reset cursor first line
  (loop :for char :across (text entry)      ; Display string
     :do (wiringpi-i2c-write-reg8 fd #X40 (char-code char))))

(defun ltk-control-text (fd)
  (let ((lbl1   (make-instance 'label :text "First line" :width 60))
        (entry1 (make-instance 'entry))
        (btn1   (make-instance 'button :text "Button1"))
        (lbl2   (make-instance 'label :text "Second line" :width 60))
        (entry2 (make-instance 'entry))
        (btn2   (make-instance 'button :text "Button2")))
    (setf (command btn1) (lambda () (display-text fd #X80 entry1)))
    (setf (command btn2) (lambda () (display-text fd #XC0 entry2)))
    (focus entry1)
    (pack (list lbl1 entry1 btn1 lbl2 entry2 btn2) :fill :x)))

(defun display-icon (fd arg1 arg2)
  (wiringpi-i2c-write-reg8 fd #X00 arg1)
  (wiringpi-i2c-write-reg8 fd #X40 arg2))

(defun clear-icon (fd)
  (display-icon fd #X40 #X00)   ; Antenna clear
  (display-icon fd #X42 #X00)   ; Phone clear
  (display-icon fd #X44 #X00)   ; Sound clear
  (display-icon fd #X46 #X00)   ; Input clear
  (display-icon fd #X47 #X00)   ; Up Down clear
  (display-icon fd #X49 #X00)   ; KeyLock clear
  (display-icon fd #X4B #X00)   ; Mute clear
  (display-icon fd #X4D #X00)   ; Battery clear
  (display-icon fd #X4F #X00))  ; Other clear

(defun ltk-control-icon (fd)
  (let* ((frm-icon     (make-instance 'frame))
         (lbl-icon     (make-instance 'label  :text "Icon Control Button"))
         (btn-antenna  (make-instance 'button :text "Antenna"))
         (btn-phone    (make-instance 'button :text "Phone"))
         (btn-sound    (make-instance 'button :text "Sound"))
         (btn-input    (make-instance 'button :text "Input"))
         (btn-up       (make-instance 'button :text "Up"))
         (btn-down     (make-instance 'button :text "Down"))
         (btn-keylock  (make-instance 'button :text "KeyLock"))
         (btn-mute     (make-instance 'button :text "Mute"))
         (btn-battery1 (make-instance 'button :text "Battery1"))
         (btn-battery2 (make-instance 'button :text "Battery2"))
         (btn-battery3 (make-instance 'button :text "Battery3"))
         (btn-battery4 (make-instance 'button :text "Battery4"))
         (btn-other    (make-instance 'button :text "Other"))
         (btn-clear    (make-instance 'button :text "Clear")))
    (setf (command btn-antenna)  (lambda () (display-icon fd #X40 #X10)))
    (setf (command btn-phone)    (lambda () (display-icon fd #X42 #X10)))
    (setf (command btn-sound)    (lambda () (display-icon fd #X44 #X10)))
    (setf (command btn-input)    (lambda () (display-icon fd #X46 #X10)))
    (setf (command btn-up)       (lambda () (display-icon fd #X47 #X10)))
    (setf (command btn-down)     (lambda () (display-icon fd #X47 #X08)))
    (setf (command btn-keylock)  (lambda () (display-icon fd #X49 #X10)))
    (setf (command btn-mute)     (lambda () (display-icon fd #X4B #X10)))
    (setf (command btn-battery1) (lambda () (display-icon fd #X4D #X10)))
    (setf (command btn-battery2) (lambda () (display-icon fd #X4D #X08)))
    (setf (command btn-battery3) (lambda () (display-icon fd #X4D #X04)))
    (setf (command btn-battery4) (lambda () (display-icon fd #X4D #X02)))
    (setf (command btn-other)    (lambda () (display-icon fd #X4F #X10)))
    (setf (command btn-clear)    (lambda () (clear-icon fd)))
    (pack lbl-icon)
    (pack (list btn-antenna
                btn-phone
                btn-sound
                btn-input
                btn-up
                btn-down
                btn-keylock
                btn-mute
                btn-battery1
                btn-battery2
                btn-battery3
                btn-battery4) :side :left)
    (pack btn-clear :fill :x)
    (configure frm-icon :borderwidth 3)
    (configure frm-icon :relief :sunken)))

(defun main ()
  (let  ((fd (wiringpi-i2c-setup +i2c-addr+)))
    (init fd)
    (with-ltk ()
      (wm-title *tk* "MI2CLCD-01 Control Application")
      (bind *tk* "<Alt-q>" (lambda (event)
                             (setq *exit-mainloop* t)))
      (ltk-control-text fd)
      (ltk-control-icon fd))))
