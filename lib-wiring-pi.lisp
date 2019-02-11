(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :wiringpi-setup-gpio
           :pin-mode
           :digital-read
           :digital-write
           :pull-updn-control
           :delay))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;;; API

;; Init wiringPi
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

;; GPIO pin mode setting
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; Read the status of the GPIO pin
(defcfun ("digitalRead" digital-read) :int
  (pin :int))

;; Output control of GPIO pin
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))

;; Set the state when nothing is connected to the terminal
(defcfun ("pullUpDnControl" pull-updn-control) :void
  (pin :int) (pud :int))

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
