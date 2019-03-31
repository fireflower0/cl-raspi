(defpackage :cl-raspi/examples/3-axis-acceleration-sensor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:import-from :cffi)
  (:export :main))
(in-package :cl-raspi/examples/3-axis-acceleration-sensor)

(defconstant +spi-cs+     0)
(defconstant +spi-speed+  100000)
(defconstant +out-x-l+    #X28)
(defconstant +out-x-h+    #X29)
(defconstant +out-y-l+    #X2A)
(defconstant +out-y-h+    #X2B)
(defconstant +out-z-l+    #X2C)
(defconstant +out-z-h+    #X2D)
(defconstant +who-am-i+   #X0F)
(defconstant +lis3dh+     #X33)
(defconstant +ctrl-reg1+  #X20)
(defconstant +xyz-enable+ #X7F)
(defconstant +read+       #X80)
(defconstant +write+      #X3F)

(defun spi-data-rw (channel data &optional (len (length data)))
  (let ((mp (cffi:foreign-alloc :unsigned-char :count len :initial-contents data)))
    (wiringpi-spi-data-rw channel mp len)
    (let ((rval (loop for i from 0 below len
                   collect (cffi:mem-aref mp :unsigned-char i))))
      (cffi:foreign-free mp)
      rval)))

(defun spi-read (read-addr)
  (let* ((outdat (list (logior read-addr +read+) #X00))
         (out    (spi-data-rw +spi-cs+ outdat)))
    (nth 1 out)))

(defun spi-write (write-addr data)
  (spi-data-rw +spi-cs+ (list (logand write-addr +write+) data)))

;; -0x800 ~ 0x7FF
(defun conv-two-byte (high low)
  (let ((dat (logior (ash high 8) low)))
    (when (>= high #X80)
      (decf dat #X10000))
    (ash dat -4)))

(defun get-acceleration (low-reg high-reg)
  (let ((lb (spi-read low-reg))
        (hb (spi-read high-reg)))
    (conv-two-byte hb lb)))

(defun main ()
  (wiringpi-spi-setup +spi-cs+ +spi-speed+)
  (if (equal (spi-read +who-am-i+) +lis3dh+)
      (format t "I AM LIS3DH~%")
      (return-from main nil))
  (spi-write +ctrl-reg1+ +xyz-enable+)
  (loop
    (format t "x=~6d y=~6d z=~6d~%"
            (get-acceleration +out-x-l+ +out-x-h+)
            (get-acceleration +out-y-l+ +out-y-h+)
            (get-acceleration +out-z-l+ +out-z-h+))
    (delay 500)))
