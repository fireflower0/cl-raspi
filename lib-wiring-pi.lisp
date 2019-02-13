(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :+input+
           :+output+
           :+pwm-output+
           :+pwm-mode-ms+
           :+pwm-mode-bal+
           :+pud-off+
           :+pud-down+
           :+pud-up+
           :wiringpi-setup-gpio
           :pin-mode
           :digital-read
           :digital-write
           :pull-updn-control
           :pwm-set-mode
           :pwm-set-range
           :pwm-set-clock
           :pwm-write
           :wiringpi-i2c-setup
           :wiringpi-i2c-write-reg8
           :wiringpi-i2c-read-reg16
           :wiringpi-spi-setup
           :wiringpi-spi-data-rw
           :delay))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;;; Constant

;; Pin mode
(defconstant +input+      0)
(defconstant +output+     1)
(defconstant +pwm-output+ 2)

;; PWM
(defconstant +pwm-mode-ms+  0)
(defconstant +pwm-mode-bal+ 1)

;; Pull up/down/none
(defconstant +pud-off+  0)
(defconstant +pud-down+ 1)
(defconstant +pud-up+   2)

;;;; API

;;; Core Library

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

;;; PWM Library

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

;;; I2C Library

;; Initialization of the I2C systems.
(defcfun ("wiringPiI2CSetup" wiringpi-i2c-setup) :int
  (fd :int))

;; Writes 8-bit data to the instructed device register.
(defcfun ("wiringPiI2CWriteReg8" wiringpi-i2c-write-reg8) :int
  (fd :int) (reg :int) (data :int))

;; It reads the 16-bit value from the indicated device register.
(defcfun ("wiringPiI2CReadReg16" wiringpi-i2c-read-reg16) :int
  (fd :int) (reg :int))

;;; SPI Library

;; SPI initialization
(defcfun ("wiringPiSPISetup" wiringpi-spi-setup) :int
  (channel :int) (speed :int))

;; Execute concurrent write/read transactions on the selected SPI bus
(defcfun ("wiringPiSPIDataRW" wiringpi-spi-data-rw) :int
  (channel :int) (data :pointer) (len :int))

;;; Other

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
