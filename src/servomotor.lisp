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
  (let ((set-degree 0)
        (move-deg 0))
    (loop
      (setf set-degree (read))
      (when (and (<= set-degree 90) (>= set-degree -90))
        (setf move-deg (floor (+ 81 (* (/ 41 90) set-degree))))
        (pwm-write +pin+ move-deg)))))