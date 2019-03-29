(defpackage :cl-raspi/examples/simple-temperature
  (:use :cl
        :cl-raspi/lib-wiring-pi
        :cl-raspi/bo1602dgrnjb)
  (:export :main))
(in-package :cl-raspi/examples/simple-temperature)

;; I2C device address (0x48)
(defconstant +i2c-addr-adt7410+ #X48)

(defun byte-swap (num-value)
  (let* ((str-value  (write-to-string num-value :base 16))
         (temp-msb   (subseq str-value 0 2))
         (temp-lsb   (subseq str-value 2)))
    (parse-integer (concatenate 'string temp-lsb temp-msb)
                   :radix 16)))

(defun get-data (fd)
  (* (byte-swap (wiringpi-i2c-read-reg16 fd #X00)) 0.0078))

(defun get-date ()
  (multiple-value-bind (second minute hour date month)
      (decode-universal-time (get-universal-time))
    (format nil "~A/~A ~A:~A:~A" month date hour minute second)))

(defun main ()
  (let ((adt7410-fd (wiringpi-i2c-setup +i2c-addr-adt7410+)))
    (bo1602dgrnjb-init)
    (wiringpi-i2c-write-reg8 adt7410-fd #X03 #X80)
    (loop
      (bo1602dgrnjb-text 1 (get-date))
      (bo1602dgrnjb-text 2 (format nil "temp:~,2f" (get-data adt7410-fd)))
      (delay 1000))))
