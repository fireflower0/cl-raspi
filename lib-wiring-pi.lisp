(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :wiringpi-setup-gpio
           :pin-mode
           :digital-read
           :digital-write
           :pull-updn-control
           :pwm-set-mode
           :pwm-set-range
           :pwm-set-clock
           :pwm-write
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

;; PWM set mode
(defcfun ("pwmSetMode" pwm-set-mode) :void
  (mode :int))

;; PWM set range (default 1024)
(defcfun ("pwmSetRange" pwm-set-range) :void
  (range :uint))

;; PWM set clock
(defcfun ("pwmSetClock" pwm-set-clock) :void
  (divisor :int))

;; PWM write
(defcfun ("pwmWrite" pwm-write) :void
  (pin :int) (value :int))

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
