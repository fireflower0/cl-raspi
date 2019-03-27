(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :author "fireflower0"
    :license "MIT"
    :depends-on ("cffi"
                 "ltk"
                 "cl-raspi/lib-wiring-pi"
                 "cl-raspi/bo1602dgrnjb"
                 "cl-raspi/src/blink"
                 "cl-raspi/src/button"
                 "cl-raspi/src/servomotor"
                 "cl-raspi/src/i2c-temperature-sensor"
                 "cl-raspi/src/3-axis-acceleration-sensor"
                 "cl-raspi/src/i2c-lcd-ltk"
                 "cl-raspi/src/i2c-oled-ssd1306"
                 "cl-raspi/src/i2c-oled-ssd1306-graphics"
                 "cl-raspi/src/buzzer"
                 "cl-raspi/src/7seg"
                 "cl-raspi/src/color"
                 "cl-raspi/src/simple-temperature"))
