(defpackage :cl-raspi/examples/ssd1306-draw-shape
  (:use :cl
        :cl-raspi/lib-wiring-pi
        :cl-raspi/ssd1306)
  (:export :main))
(in-package :cl-raspi/examples/ssd1306-draw-shape)

(defun draw-line ()
  (ssd1306-clear-display)
  (ssd1306-draw-line 0 0 127 63)
  (ssd1306-draw-line 127 0 0 63)
  (ssd1306-display))

(defun draw-rect ()
  (ssd1306-clear-display)
  (ssd1306-draw-rect 10 10 44 44 :color +white+)
  (ssd1306-draw-fill-rect 74 10 44 44 :color +white+)
  (ssd1306-display))

(defun main ()
  (ssd1306-init)
  (draw-line)
  (delay 1000)
  (draw-rect)
  (delay 1000)
  (ssd1306-clear-display))