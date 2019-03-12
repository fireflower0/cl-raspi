(defpackage :cl-raspi/src/buzzer
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/buzzer)

(defconstant +pin+ 23)

(defun main ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ +pwm-output+)
  (loop
     (digital-write +pin+ 1)
     (delay 1000)
     (digital-write +pin+ 0)
     (delay 1000)))
