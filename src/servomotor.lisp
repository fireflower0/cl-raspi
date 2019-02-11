(defpackage :cl-raspi/src/servomotor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/servomotor)

(defconstant +pin+ 12)

(defparameter *set-degree* 74)

(defun main ()
  (let ((move-deg (* (+ (/ (* 4.75 *set-degree*) 90) 7.25) (/ 1024 100))))
    (wiringpi-setup-gpio)
    (pin-mode +pin+ 2)
    (pwm-set-mode 0)
    (pwm-set-range 1024)
    (pwm-set-clock 192)
    (pwm-write +pin+ move-deg)))