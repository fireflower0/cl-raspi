(defpackage :cl-raspi/src/7seg
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/7seg)

(defconstant +d0-pin+ 18)
(defconstant +d1-pin+ 23)
(defconstant +d2-pin+ 24)
(defconstant +d3-pin+ 25)

(defun display-7seg (num)
  (let ((binary-num (format nil "~4,'0b" num)))
    (digital-write +d0-pin+ (subseq binary-num 3 4))
    (digital-write +d1-pin+ (subseq binary-num 2 3))
    (digital-write +d2-pin+ (subseq binary-num 1 2))
    (digital-write +d3-pin+ (subseq binary-num 0 1))))

(defun main ()
  (wiringpi-setup-gpio)
  (pin-mode +d0-pin+ +output+)
  (pin-mode +d1-pin+ +output+)
  (pin-mode +d2-pin+ +output+)
  (pin-mode +d3-pin+ +output+)
  (loop
       (display-7seg (read))))
