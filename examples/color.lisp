(defpackage :cl-raspi/examples/color
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/examples/color)

(defconstant +green-pin+ 18)
(defconstant +blue-pin+  23)
(defconstant +red-pin+   24)

(defun main ()
  ;; GPIO初期化
  (wiringpi-setup-gpio)
  ;; ピンモード設定
  (pin-mode +green-pin+ +output+)
  (pin-mode +blue-pin+  +output+)
  (pin-mode +red-pin+   +output+)
  ;; PWM出力設定
  (soft-pwm-create +green-pin+ 0 100)
  (soft-pwm-create +blue-pin+  0 100)
  (soft-pwm-create +red-pin+   0 100)
  ;; PWMで各端子に出力
  (soft-pwm-write +green-pin+ 30)
  (soft-pwm-write +blue-pin+  50)
  (soft-pwm-write +red-pin+  100))
