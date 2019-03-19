(defpackage :cl-raspi/src/button
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/button)

(defconstant +pin+ 17)

(defun main ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ +input+)
  (pull-updn-control +pin+ +pud-up+)
  (loop
     (if (= (digital-read +pin+) 0)
         (format t "Switch ON~%")
         (format t "Switch OFF~%"))
     (delay 500)))
