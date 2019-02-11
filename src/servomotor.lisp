(defpackage :cl-raspi/src/servomotor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/servomotor)

(defconstant +pin+ 12)

(defun init ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ +pwm-output+)
  (pwm-set-mode +pwm-mode-ms+)
  (pwm-set-range 1024)
  (pwm-set-clock 375))

(defun main ()
  (init)
  (let ((move 0))
    (loop
      (setf move (read))
      (pwm-write +pin+ move))))