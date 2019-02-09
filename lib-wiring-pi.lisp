(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :wiringpi-setup-gpio
           :pin-mode
           :digital-write
           :delay))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library lib-wiring-pi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;;; Constant

;; GPIO11(pin 23)
(defconstant +gpio-11+ 11)

;; GPIO Mode
(defconstant +gpio-input-mode+ 0)
(defconstant +gpio-output-mode+ 1)

;;; API

;; Init wiringPi
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

;; GPIO pin mode setting
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; Output control of GPIO pin
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
