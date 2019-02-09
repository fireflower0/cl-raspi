(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :author "fireflower0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-raspi/lib-wiring-pi"))

(defsystem "cl-raspi/examples"
    :class :package-inferred-system
    :version "0.1.0"
    :author "fireflower0"
    :license "MIT"
    :depends-on ("cffi")
    :pathname "examples"
    :serial t
    :components ((:file "00-blink")))
