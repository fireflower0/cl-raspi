(defpackage :cl-raspi/src/gpio-input
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/gpio-input)

(defconstant +pin+ 17)

(defun main ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ 0)
  (pull-updn-control +pin+ 2)
  (loop
     (if (= (digital-read +pin+) 0)
         (format t "Switch ON~%")
         (format t "Switch OFF~%"))
     (delay 500)))
