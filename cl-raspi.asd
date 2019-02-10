(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :author "fireflower0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-raspi/lib-wiring-pi"
                 "cl-raspi/src/blink"))