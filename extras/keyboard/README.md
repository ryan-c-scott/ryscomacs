# What is this?

These files are for making my myriad keyboards function similarly to one another in an effort to limit the amount of context switching I have to do.

## pok3r-esque.dat

This is for custom or modded keyboards that support the EasyAVR firmware.
The layout in here more or less emulates a Pok3r and has its boot mode binding set as Fn+R-ALT+Shift+Esc, L-CTRL as Fn, and Capslock replaced with L-CTRL

### Keyboard Flashing
My keyboard, the Filco Majestouch:

``` text
    Controller:     Tiger Lily
    Keyboard model: 	Filco Majestouch / Rosewill RK-9000
    Microcontroller: 	ATmega32u2
    External logic: 	2 HC42 4-10 decoders
    Matrix size: 	8 rows by 18 columns
    LED channels: 	3 with PWM
    Design date: 	2016-11-14
```

#### Windows
Flashing under windows didn't seem to work with either AVRDude or dfu-programmer.
Getting ATmel Flip installed was annoying but fairly easy

1. Download flip (search atmel flip to find the download)
2. Install
3. Put keyboard into programming mode
4. Go to device manager and find the atmega32u2 unknown device
5. Point to `c:/Program Files(x86)/atmel/usb` for the driver

At that point EasyAVR should be able to flash using flip just fine.

It's worth noting that windows can become confused and think that the alt key or similar is being held down, so it may be necessary to restart.

#### Linux/OSX
__TODO__

## karabiner.json

This karabiner configuration should go into ~/.config/karabiner/
It maps the capslock as ctrl, left ctrl as a fn, and otherwise uses a pok3r layout.
