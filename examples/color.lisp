(defpackage :cl-raspi/examples/color
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/examples/color)

(defconstant +green-pin+ 27)
(defconstant +blue-pin+ 22)
(defconstant +red-pin+ 17)
(defconstant +initial-value+ 0)
(defconstant +pwm-range+ 100)

(defun main ()
  ;; GPIO初期化
  (wiringpi-setup-gpio)
  ;; ピンモード設定
  (pin-mode +green-pin+ +output+)
  (pin-mode +blue-pin+  +output+)
  (pin-mode +red-pin+   +output+)
  (loop
     ;; PWM出力設定
     (soft-pwm-create +green-pin+ +initial-value+ +pwm-range+)
     (soft-pwm-create +blue-pin+  +initial-value+ +pwm-range+)
     (soft-pwm-create +red-pin+   +initial-value+ +pwm-range+)
     ;; PWMで各端子に出力
     (soft-pwm-write +green-pin+ (random 100))
     (soft-pwm-write +blue-pin+  (random 100))
     (soft-pwm-write +red-pin+   (random 100))
     ;; 待機
     (delay 500)))
