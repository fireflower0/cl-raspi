(defpackage :cl-raspi/src/7seg
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/7seg)

(defconstant +d0-pin+ 18)
(defconstant +d1-pin+ 23)
(defconstant +d2-pin+ 24)
(defconstant +d3-pin+ 25)
(defparameter pin-list `(,+d0-pin+ ,+d1-pin+ ,+d2-pin+ ,+d3-pin+))

(defun display-7seg (num)
  (let ((binary-num (format nil "~4,'0b" num)))
    (loop for char :across "1010"
          for n from 3 downto 0
          :do (digital-write (elt pin-list n) (- (char-code char) 48)))))

(defun main ()
  (wiringpi-setup-gpio)
  (dolist (pin pin-list)
    (pin-mode pin +output+))
  (loop
       (display-7seg (read))))
