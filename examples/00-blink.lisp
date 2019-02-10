(in-package :cl-raspi-examples)

(require 'cffi)

(defun blink ()
  (wiringpi-setup-gpio)
  (pin-mode +gpio-11+ +gpio-output-mode+)

  ;; Infinite loop (Ctrl-c exits loop)
  (loop
     (digital-write +gpio-11+ 1)   ; Turn on LED
     (delay 500)                   ; Delay 500(ms)
     (digital-write +gpio-11+ 0)   ; Turn off LED
     (delay 500)))                 ; Delay 500(ms)
