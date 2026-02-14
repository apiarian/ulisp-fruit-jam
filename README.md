# uLisp for the Adafruit Fruit Jam

A version of the [uLisp](http://www.ulisp.com/) interpreter for the [Adafruit Fruit Jam](https://www.adafruit.com/product/6295) (RP2350B), turning it into a standalone Lisp computer — connect a USB keyboard, an HDMI monitor, power on, and you're in a Lisp REPL.

Based on [uLisp ARM Release 4.9](http://www.ulisp.com/show?5CSS) (9th February 2026).

## What's New

This fork adds USB keyboard + mouse input, an HDMI terminal + graphics display, a 5-voice wavetable synthesizer with ADSR envelopes, Wi-Fi networking, SD card storage, and a hardware escape button — everything needed to use the Fruit Jam as a self-contained Lisp machine without a host computer.

### Display (fruitjam_terminal.h + fruitjam_graphics.h)

- Single `DVHSTX8` display at **400×300 @ 8bpp** (800×600 HDMI, pixel-doubled)
- **Text mode:** 66×37 character terminal with VT100 escape sequences, blinking cursor, 8 ANSI colors, scrolling
- **Graphics mode:** Full 400×300 pixel framebuffer with 256-color palette, accessed via uLisp GFX primitives (`draw-pixel`, `fill-rect`, `draw-circle`, etc.)
- `(graphics-mode)` / `(text-mode)` switch instantly — no hardware reconfiguration, just clear and redraw
- `(gfx-test)` built-in demo function
- Terminal text persists through graphics mode — returns exactly where you left off

### USB Host Keyboard + Mouse (fruitjam_usbhost.h + fruitjam_graphics.h)

- PIO USB host on core1 (PIO 2 / DMA channel 3 to avoid HSTX conflicts)
- HID keyboard → 256-byte ring buffer → `gserial()` on core0
- US keyboard layout with shift, ctrl, caps lock
- Key repeat (500ms delay, 50ms rate)
- Automatic recovery from PIO USB deaf states and core1 lockups
- **USB mouse** with boot-protocol and generic HID report descriptor parsing
- Mouse cursor: 8×8 arrow sprite with save-under buffer, auto-hidden during draw calls
- `(mouse-x)`, `(mouse-y)`, `(mouse-buttons)`, `(mouse-click)` — read mouse state from Lisp
- `(mouse-show)`, `(mouse-hide)` — show/hide the cursor in graphics mode
- `(paint)` — built-in mouse-driven drawing demo

### Hardware Escape Button (fruitjam_escape.h)

- BUTTON1 (GPIO0) configured as a GPIO interrupt
- Press it at any time to abort a running program and return to the REPL
- Works even if USB host or the Lisp program is hung
- If in graphics mode, automatically switches back to text mode
- Silences all audio voices immediately (no notes left playing after abort)
- Checked in `testescape()` and the `gserial()` input wait loop for immediate response

### Audio — 5-Voice Wavetable Synthesizer (fruitjam_audio.h)

- **4 wavetable tone voices** (0–3) + **1 noise voice** (4) — inspired by classic sound chips (SID, AY-3-8910, NES APU)
- Built-in waveforms: sine, square, triangle, sawtooth, noise (LFSR)
- **Custom wavetables:** pass a 256-element uLisp array to `audio-wave` for arbitrary waveform shapes
- **ADSR envelopes** per voice: attack/decay/release in milliseconds, sustain level 0–255
- `audio-note` auto-triggers envelope, `audio-release` fades to silence
- **Headphone detection:** auto-switches between speaker and 3.5mm headphone jack
- `(audio-output mode)` for manual routing: auto (default), speaker, headphone, or both
- Hardware: TLV320DAC3100 I2S DAC, PIO 0 for I2S output, DMA channel 4, 22050 Hz sample rate

```lisp
;; Play a C major chord with sine waves
(audio-wave 0 1) (audio-wave 1 1) (audio-wave 2 1)
(audio-vol 0 120) (audio-vol 1 120) (audio-vol 2 120)
(audio-note 0 60) (audio-note 1 64) (audio-note 2 67)

;; Plucky arpeggio with envelope
(audio-wave 0 2)                         ; square wave
(audio-vol 0 180)
(audio-envelope 0 5 50 0 100)            ; quick attack, short decay, no sustain
(dolist (n '(48 55 52 60 48 55 52 60))
  (audio-note 0 n) (delay 120))

;; Custom wavetable from Lisp
(let ((wt (make-array 256)))
  (dotimes (i 256)
    (setf (aref wt i) (truncate (* 127 (sin (* 6.283 (/ i 256.0)))) 1)))
  (audio-wave 0 wt))

(audio-stop-all)                          ; silence everything
```

**Built-in demos:** `(audio-test)`, `(noise-test)`, `(waveform-demo)`, `(poly-demo)`, `(envelope-demo)` — all accept an optional volume parameter (0–255).

**12 Lisp functions:** `audio-wave`, `audio-freq`, `audio-note`, `audio-vol`, `audio-master-vol`, `audio-stop`, `audio-stop-all`, `audio-playing`, `audio-envelope`, `audio-trigger`, `audio-release`, `audio-output`

### SD Card Support

- `save-image` / `load-image` save and restore the entire workspace to MicroSD
- `with-sd-card` provides text file I/O for reading/writing `.lsp` source files

### Wi-Fi (via ESP32-C6)

- Onboard ESP32-C6 coprocessor over SPI, using the `WiFiNINA - Adafruit Fork` library
- `(wifi-connect "ssid" "password")` — connect to a network, returns IP address
- `(wifi-localip)` — current IP address
- `(get-time)` — current date/time via NTP (requires Wi-Fi connection)
- `(with-client (str "host" port) ...)` — HTTP requests and TCP connections
- `(wifi-server)`, `(wifi-softap ...)` — run a server or create an access point

## Architecture

```
Core 0: uLisp interpreter + display (DVHSTX8 400×300) + audio synthesis
Core 1: USB host keyboard + mouse (PIO USB via TinyUSB)

Hardware: HSTX → HDMI, PIO 0 → I2S audio, PIO 2 → USB host
DMA: 0–2 = HSTX video, 3 = PIO USB, 4 = I2S audio
Interrupts: BUTTON1 (GPIO0) → escape, DMA_IRQ_1 → audio
Headphone detect: polled via I2C every 500ms (not GPIO IRQ — single callback per core)
```

The display uses a single `DVHSTX8` object for both terminal and graphics. Text mode renders characters via `Adafruit_GFX::drawChar()` into the pixel framebuffer with a character grid stored in RAM for scrolling and restore. Graphics mode clears the framebuffer and enables GFX drawing primitives. Switching between modes is instant — no HSTX reinitialization, no PLL changes, no DMA teardown.

Audio synthesis runs on core0 in the `testescape()` idle loop, filling a 1024-sample ring buffer that DMA streams to the TLV320DAC3100 via PIO I2S. The synth mixes 5 voices (wavetable lookup + ADSR envelope) per sample — about 0.5% CPU at 22050 Hz.

All Fruit Jam-specific code lives in separate `.h` files, included from the board config block. The main `.ino` has minimal `#if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)` blocks outside the board config section, keeping the diff against upstream uLisp small for easy merging of future releases.

### Workspace

With `DVHSTX8` at 400×300, the workspace is **36,000 objects** (ARM). The framebuffer (~145KB) is permanently allocated from heap. PSRAM support (1,000,000 objects) is a future goal pending resolution of the HSTX+PSRAM coexistence issue.

## Building

Open `ulisp-fruit-jam.ino` in the Arduino IDE with the [Adafruit RP2350 board package](https://github.com/earlephilhower/arduino-pico) installed. Select **Adafruit Fruit Jam** as the board.

### Arduino IDE Settings (Tools menu)

| Setting | Value |
|---------|-------|
| **Board** | Adafruit Fruit Jam |
| **USB Stack** | Adafruit TinyUSB |
| **Flash Size** | 16MB (no FS) |
| **CPU Speed** | 150 MHz (overridden to 240 MHz by DVHSTX at runtime) |

⚠️ **USB Stack must be "Adafruit TinyUSB"** — not "Adafruit TinyUSB Host (native)". The "Host (native)" option disables USB device mode, which kills serial output and the serial REPL. The correct setting enables dual-mode: native USB = device (serial), PIO USB = host (keyboard).

⚠️ **Remove any user-installed `Adafruit_TinyUSB_Library`** from `~/Arduino/libraries/` — use only the version bundled with the board package. Version mismatches between a user-installed library and the board package can cause USB host init hangs.

### Patched Libraries

**Pico PIO USB** — The installed `Pico_PIO_USB` library (v0.7.2) has infinite busy-wait loops that can permanently lock core1. A patched copy with microsecond timeouts is symlinked at `~/Arduino/libraries/Pico_PIO_USB` → `~/code/Pico-PIO-USB-patched`. See REFERENCE.md for details.

**Adafruit DVI HSTX** *(optional, no longer needed)* — A patched copy at `~/Arduino/libraries/Adafruit_DVI_HSTX` → `~/code/Adafruit-DVI-HSTX` fixes a `%` character rendering bug in `DVHSTXText` mode. Since this project now uses `DVHSTX8` instead of `DVHSTXText`, the patch has no effect and can be safely reverted:

```bash
rm ~/Arduino/libraries/Adafruit_DVI_HSTX
mv ~/Arduino/libraries/Adafruit_DVI_HSTX.bak ~/Arduino/libraries/Adafruit_DVI_HSTX
```

## Future Work

- **PSRAM** — 8MB / 1M objects (blocked on HSTX coexistence)
- **Better terminal font** — replace the 6×8 bitmap with a more readable font (8×16 VGA, Terminus, or converted Intel One Mono)
- **Line editor** — enable uLisp's built-in tab completion, paren highlighting, and command recall on HDMI
- **Autorun** — boot directly into a saved program from SD card
- **Buttons / NeoPixels** — expose BUTTON2/3 and the 5 onboard NeoPixels to Lisp
- **USB Gamepad** — HID gamepad input for games

## Links

- [uLisp](http://www.ulisp.com/) — the upstream Lisp interpreter
- [uLisp Fruit Jam page](http://www.ulisp.com/show?5CSS)
- [Adafruit Fruit Jam](https://www.adafruit.com/product/6295)
