(defpackage :cl-raspi/src/servomotor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/servomotor)

(defconstant +pin+ 12)

(defparameter *move* 0)

(defun main ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ 2)
  (pwm-set-mode 0)
  (pwm-set-range 1024)
  (pwm-set-clock 375)
  (loop
    (setf *move* (read))
    (pwm-write +pin+ *move*)))