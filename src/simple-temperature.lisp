(defpackage :cl-raspi/src/simple-temperature
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/simple-temperature)

;; I2C device address (0x48)
(defconstant +i2c-addr-adt7410+ #X48)

;; I2C device address (0x3e)
(defconstant +i2c-addr-lcd+ #X3E)

;; LCD contrast (0x00-0x0F)
(defconstant +contrast+ #X0A)

;; LCD column (16)
(defconstant +column+ 16)

(defun lcd-init (fd)
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

(defun byte-swap (num-value)
  (let* ((str-value  (write-to-string num-value :base 16))
         (temp-msb   (subseq str-value 0 2))
         (temp-lsb   (subseq str-value 2)))
    (parse-integer (concatenate 'string temp-lsb temp-msb)
                   :radix 16)))

(defun get-data (fd)
  (* (byte-swap (wiringpi-i2c-read-reg16 fd #X00)) 0.0078))

(defun main ()
  (let ((adt7410-fd (wiringpi-i2c-setup +i2c-addr-adt7410+))
        (lcd-fd     (wiringpi-i2c-setup +i2c-addr-lcd+)))
    (lcd-init lcd-fd)
    (wiringpi-i2c-write-reg8 adt7410-fd #X03 #X80)
    (display-text lcd-fd #X80 (get-data adt7410-fd))))
