(defpackage :cl-raspi/src/3-axis-acceleration-sensor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/3-axis-acceleration-sensor)

(defconstant +spi-cs+ 0)                ; Select target SPI device
(defconstant +spi-speed+ 100000)        ; SPI communication speed

(defconstant +out-x-l+ #X28)            ; OUT_X Low
(defconstant +out-x-h+ #X29)            ; OUT_X High
(defconstant +out-y-l+ #X2A)            ; OUT_Y Low
(defconstant +out-y-h+ #X2B)            ; OUT_Y High
(defconstant +out-z-l+ #X2C)            ; OUT_Z Low
(defconstant +out-z-h+ #X2D)            ; OUT_Z High

(defconstant +who-am-i+  #X0F)          ; WHO_AM_I
(defconstant +ctrl-reg1+ #X20)          ; CTRL_REG1
(defconstant +ctrl-reg4+ #X23)          ; CTRL_REG4

(defconstant +read+  #X80)              ; Read
(defconstant +write+ #X3F)              ; Write

(defconstant +pin+    8)                ; CS
(defconstant +output+ 1)
(defconstant +high+   1)
(defconstant +low+    0)

(defun spi-data-rw (channel data &optional (len (length data)))
  (let ((mp (foreign-alloc :unsigned-char :count len :initial-contents data)))
    (digital-write +pin+ +low+)
    (wiringpi-spi-data-rw channel mp len)
    (digital-write +pin+ +high+)
    (let ((rval (loop for i from 0 below len
                   collect (mem-aref mp :unsigned-char i))))
      (foreign-free mp)
      rval)))

(defun spi-read (read-addr)
  (let (outdat out)
    (setq outdat (list (logior read-addr +read+) #X00))
    (setq out (spi-data-rw +spi-cs+ outdat))
    (nth 1 out)))

(defun spi-write (write-addr data)
  (spi-data-rw +spi-cs+ (list (logand write-addr +write+) data)))

(defun conv-two-byte (high low)
  (let (dat)
    (setq dat (logior (ash high 8) low))
    (if (>= high #X80)
        (setq dat (- dat 65536)))
    (setq dat (ash dat -4))
    dat))

(defun main ()
  (let (lb hb x y z)
    (wiringpi-spi-setup +spi-cs+ +spi-speed+)
    (wiringpi-setup-gpio)
    (pin-mode +pin+ +output+)
    (digital-write +pin+ +high+)

    (if (equal (spi-read +who-am-i+) #X33)
        (format t "I AM LIS3DH~%")
        (return-from main nil))
    (spi-write +ctrl-reg1+ #X77)

    (loop
       ;; Get X axis data
       (setq lb (spi-read +out-x-l+))
       (setq hb (spi-read +out-x-h+))
       (setq x  (conv-two-byte hb lb))

       ;; Get Y axis data
       (setq lb (spi-read +out-y-l+))
       (setq hb (spi-read +out-y-h+))
       (setq y  (conv-two-byte hb lb))

       ;; Get Z axis data
       (setq lb (spi-read +out-z-l+))
       (setq hb (spi-read +out-z-h+))
       (setq z  (conv-two-byte hb lb))

       (format t "x=~6d y=~6d z=~6d~%" x y z)

       (delay 500))))
