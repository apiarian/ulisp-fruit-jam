# uLisp for the Adafruit Fruit Jam

A version of the [uLisp](http://www.ulisp.com/) interpreter for the [Adafruit Fruit Jam](https://www.adafruit.com/product/6295) (RP2350B), turning it into a standalone Lisp computer — connect a USB keyboard, an HDMI monitor, power on, and you're in a Lisp REPL.

Based on [uLisp ARM Release 4.9](http://www.ulisp.com/show?5CSS) (9th February 2026).

## What's New

This fork adds USB keyboard + mouse input, an HDMI terminal + graphics display, keyboard input for Lisp programs (with modifier key support), a 5-voice wavetable synthesizer with ADSR envelopes, NeoPixel control, a screensaver with NeoPixel wave animation, Wi-Fi networking, SD card storage, and a hardware escape button — everything needed to use the Fruit Jam as a self-contained Lisp machine without a host computer.

### Display (fruitjam_terminal.h + fruitjam_graphics.h)

- Single `DVHSTX8` display at **512×384 @ 8bpp** (1024×768 HDMI, pixel-doubled)
- **Text mode:** 64×48 character terminal with VT100 escape sequences, blinking cursor, 8 ANSI colors, scrolling
- **[unscii-8-thin](https://github.com/viznut/unscii) font** — 8×8 pixel bitmap font used throughout (terminal and graphics mode text). Clean, readable, designed for screens. Replaces the default Adafruit_GFX 5×7 font.
- **Line editor:** Tab autocomplete of both **user-defined and built-in symbols** (cycles through all matches, wrapping around), parenthesis matching (highlights matching `(` in green when `)` is typed), 8-entry command history (Up/Down arrows), in-line cursor movement (Left/Right/Home/End with insert and delete at any position), Ctrl-C abort, Ctrl-U erase line. Inspired by the [Cardputer](http://www.ulisp.com/show?52G4) and [PicoCalc](http://www.ulisp.com/show?56ZO) uLisp machines.
- **Terminal bell:** BEL character (`\a` / 0x07) triggers a brief yellow screen-border flash and a short audio blip (~1047 Hz sine, 60ms). Fires on buffer-full in the line editor and from Lisp via `(princ (code-char 7))`.
- **Graphics mode:** Full 512×384 pixel framebuffer with 256-color palette, accessed via uLisp GFX primitives (`draw-pixel`, `fill-rect`, `draw-circle`, etc.)
- `(graphics-mode)` / `(text-mode)` switch instantly — no hardware reconfiguration, just clear and redraw
- `(demo)` built-in interactive demo (paint + sound + buttons + LEDs)
- Terminal text persists through graphics mode — returns exactly where you left off

### USB Host Keyboard + Mouse (fruitjam_usbhost.h + fruitjam_graphics.h)

- PIO USB host on core1 (PIO 2 / DMA channel 3 to avoid HSTX conflicts)
- HID keyboard → 256-entry 16-bit ring buffer → `gserial()` on core0 (REPL) or `(keyboard)` / `(wait-keyboard)` (Lisp programs)
- US keyboard layout with shift, ctrl, caps lock
- Key repeat (500ms delay, 50ms rate)
- Automatic recovery from PIO USB deaf states and core1 lockups
- **USB mouse** with boot-protocol and generic HID report descriptor parsing
- Mouse cursor: 8×8 arrow sprite with save-under buffer, auto-hidden during draw calls
- `(mouse-x)`, `(mouse-y)`, `(mouse-buttons)`, `(mouse-click)` — read mouse state from Lisp
- `(mouse-show)`, `(mouse-hide)` — show/hide the cursor in graphics mode

### Keyboard Input for Lisp Programs

- **`(keyboard)`** — non-blocking, returns next key as an integer or `nil` if buffer is empty
- **`(wait-keyboard)`** — blocking, waits for a keypress (supports escape via button 1)
- **`(keyboard-flush)`** — discards all pending keys (useful when entering a game loop or switching modes)
- Return value encodes both key and modifier state in a single integer:
  - `(key-code k)` — extracts the key code (low byte): ASCII for printable keys, `*key-...*` constants for special keys
  - `(key-mod k)` — extracts the modifier bitmask (high byte)
- **21 key constants** defined at boot: `*key-up*`, `*key-down*`, `*key-left*`, `*key-right*`, `*key-home*`, `*key-end*`, `*key-pgup*`, `*key-pgdn*`, `*key-insert*`, `*key-f1*` through `*key-f12*`
- **4 modifier constants**: `*mod-ctrl*`, `*mod-shift*`, `*mod-alt*`, `*mod-super*` — each matches both left and right versions
- Test modifiers with: `(> (logand (key-mod k) *mod-ctrl*) 0)`

```lisp
;; Simple keyboard event loop
(graphics-mode)
(loop
  (let ((k (keyboard)))
    (when k
      (let ((kc (key-code k)))
        (cond
          ((= kc 27) (text-mode) (return))     ; Escape → quit
          ((= kc *key-up*) (move-up))           ; arrow keys
          ((and (> (logand (key-mod k) *mod-ctrl*) 0)
                (= kc 19)) (save)))))))         ; Ctrl+S
```

### Hardware Escape Button (fruitjam_escape.h)

- BUTTON1 (GPIO0) configured as a GPIO interrupt
- **Short press:** abort a running program and return to the REPL
- **Ctrl-C on keyboard:** equivalent to a short press — aborts input and returns to the REPL prompt
- **Long press (≥1s):** also triggers USB host power-cycle recovery (forces keyboard re-enumeration)
- Works even if USB host or the Lisp program is hung
- If in graphics mode, automatically switches back to text mode
- Silences all audio voices immediately (no notes left playing after abort)
- Clears NeoPixels (turns them off)
- Checked in `testescape()` and the `gserial()` input wait loop for immediate response

### Audio — 5-Voice Wavetable Synthesizer (fruitjam_audio.h)

- **4 wavetable tone voices** (0–3) + **1 noise voice** (4) — inspired by classic sound chips (SID, AY-3-8910, NES APU)
- Built-in waveforms: sine, square, triangle, sawtooth, noise (LFSR)
- **Custom wavetables:** pass a 256-element uLisp array to `audio-wave` for arbitrary waveform shapes
- **ADSR envelopes** per voice: attack/decay/release in milliseconds, sustain level 0–255
- `audio-note` auto-triggers envelope, `audio-release` fades to silence
- **Self-releasing notes:** `(audio-note voice note duration)` — optional duration in ms schedules auto-release from note start, enabling fire-and-forget sound effects without blocking
- **Headphone detection:** auto-switches between speaker and 3.5mm headphone jack
- `(audio-output mode)` for manual routing: `:auto` (default), `:speaker`, `:headphone`, or `:both`
- Hardware: TLV320DAC3100 I2S DAC, PIO 0 for I2S output, DMA channel 4, 22050 Hz sample rate

```lisp
;; Play a C major chord with sine waves
(audio-wave 0 :sine) (audio-wave 1 :sine) (audio-wave 2 :sine)
(audio-vol 0 120) (audio-vol 1 120) (audio-vol 2 120)
(audio-note 0 60) (audio-note 1 64) (audio-note 2 67)

;; Fire-and-forget sound effect (150ms square blip, no envelope needed)
(audio-wave 0 :square) (audio-vol 0 150)
(audio-note 0 72 150)

;; Plucky arpeggio with envelope
(audio-wave 0 :square)                   ; square wave
(audio-vol 0 180)
(audio-envelope 0 5 50 0 100)            ; quick attack, short decay, no sustain
(dolist (n '(48 55 52 60 48 55 52 60))
  (audio-note 0 n) (delay 120))

;; Custom wavetable from Lisp
(let ((wt (make-array 256)))
  (dotimes (i 256)
    (setf (aref wt i) (truncate (* 127 (sin (* 6.283 (/ i 256.0)))) 1)))
  (audio-wave 0 wt))

(audio-output :speaker)                    ; force speaker output
(audio-output :auto)                      ; restore auto-switching
(audio-stop-all)                          ; silence everything
```

**Built-in demo:** `(demo)` — an interactive paint app that showcases audio (UI click/blip sound effects, startup jingle), mouse drawing, button input (color/size cycling), keyboard shortcuts (Escape to quit, c/s/x for color/size/clear), and NeoPixels (reflecting current brush color).

**12 Lisp functions:** `audio-wave`, `audio-freq`, `audio-note`, `audio-vol`, `audio-master-vol`, `audio-stop`, `audio-stop-all`, `audio-playing`, `audio-envelope`, `audio-trigger`, `audio-release`, `audio-output`

### NeoPixel Control (fruitjam_neopixel.h)

- 5 onboard NeoPixels (WS2812, GPIO 32) accessible from Lisp
- API matches the [official uLisp NeoPixel extension](http://www.ulisp.com/show?4GMV)
- PIO 2 hardware driver (autonomous timing, immune to CPU interrupts)
- HSV colors, gamma correction, rainbow fills

```lisp
(pixels-set-pixel-color 0 32 0 0)   ; pixel 0 = red
(pixels-set-pixel-color 1 0 32 0)   ; pixel 1 = green
(pixels-show)                         ; transmit to hardware

(pixels-rainbow)                      ; fill with rainbow
(pixels-show)
```

**8 Lisp functions:** `pixels-begin`, `pixels-clear`, `pixels-fill`, `pixels-set-pixel-color`, `pixels-color`, `pixels-color-hsv`, `pixels-show`, `pixels-rainbow`

### Screensaver (fruitjam_screensaver.h)

- Activates after **5 minutes** of no keyboard or serial input at the REPL
- Blanks the HDMI screen to black (prevents burn-in on static text)
- If the NeoPixels are all off, starts a gentle **rolling wave** animation — a warm white gaussian peak sweeps back and forth across the 5 LEDs
- If the NeoPixels are already in use (set by user code), only blanks the screen
- Any keypress or hardware button (BUTTON1/2/3) instantly wakes: restores the terminal and turns off the wave
- Does not activate during graphics mode; exiting graphics mode resets the idle timer
- `(set-screensaver seconds)` — configure timeout (default 300); `(set-screensaver 0)` to disable; `(set-screensaver)` returns current timeout

### Button Input

- `(button n)` — returns `t` if button n (1–3) is pressed, `nil` otherwise
- BUTTON2 (GPIO4) and BUTTON3 (GPIO5) are the primary buttons for user programs
- BUTTON1 (GPIO0) is readable but not practically useful from Lisp — its escape interrupt fires on press before the result can be used

### SD Card & Package Management

- `save-image` / `load-image` save and restore the entire workspace to MicroSD
- `with-sd-card` provides low-level text file I/O for reading/writing files
- **Package system** for file-based Lisp development — load `.lsp` source files, track which symbols they define, save modified code back, unload cleanly
- All package functions have docstrings — use `(documentation 'package-load)` etc. at the REPL for help

```lisp
;; Load a file, creating a package that tracks its definitions
(package-load "game.lsp")       ; → (main draw-frame init)

;; Run the program
(main)

;; Edit a function with the built-in tree editor, then save back
(edit 'draw-frame)
(package-save "game.lsp")

;; Define something new at the REPL, add it to the package
(defun new-helper (x) (* x x))
(package-add "game.lsp" 'new-helper)
(package-save "game.lsp")

;; Create a brand-new package from scratch (no file needed)
(defun greet () (princ "hello"))
(defun farewell () (princ "bye"))
(package-add "utils.lsp" 'greet)     ; auto-creates the package
(package-add "utils.lsp" 'farewell)  ; adds to existing package
(package-save "utils.lsp")           ; writes to SD card

;; Reload after editing the file externally
(package-load "game.lsp")       ; unloads old, reloads fresh

;; Inspect and manage packages
(package-list)                   ; → ("utils.lsp" "game.lsp")
(package-symbols "game.lsp")    ; → (new-helper main draw-frame init)
(package-remove "game.lsp" 'old-fn)   ; stop tracking
(package-remove "game.lsp" 'old-fn t) ; also remove from workspace
(package-unload "game.lsp")     ; remove all symbols, drop package
```

**7 Lisp functions:** `package-load`, `package-save`, `package-unload`, `package-add`, `package-remove`, `package-symbols`, `package-list`

### Wi-Fi (via ESP32-C6)

- Onboard ESP32-C6 coprocessor over SPI, using the `WiFiNINA - Adafruit Fork` library
- `(wifi-connect "ssid" "password")` — connect to a network, returns IP address
- `(wifi-localip)` — current IP address
- `(get-time)` — current date/time via NTP (requires Wi-Fi connection)
- `(with-client (str "host" port) ...)` — HTTP requests and TCP connections
- `(wifi-server)`, `(wifi-softap ...)` — run a server or create an access point

## Architecture

```
Core 0: uLisp interpreter + display (DVHSTX8 512×384) + audio synthesis
Core 1: USB host keyboard + mouse (PIO USB via TinyUSB)

Hardware: HSTX → HDMI, PIO 0 → I2S audio, PIO 2 → USB host + NeoPixels
DMA: 0–2 = HSTX video, 3 = PIO USB, 4 = I2S audio
Interrupts: BUTTON1 (GPIO0) → escape, DMA_IRQ_1 → audio
Headphone detect: polled via I2C every 500ms (not GPIO IRQ — single callback per core)
```

The display uses a single `DVHSTX8` object for both terminal and graphics. Text mode renders characters from the unscii-8-thin bitmap font directly into the 8bpp pixel framebuffer, with a character grid stored in RAM for scrolling and restore. Graphics mode clears the framebuffer and enables GFX drawing primitives. Switching between modes is instant — no HSTX reinitialization, no PLL changes, no DMA teardown. The same font is used for `(draw-char)` and `(with-gfx)` text in graphics mode.

Audio synthesis runs on core0 in the `testescape()` idle loop, filling a 1024-sample ring buffer that DMA streams to the TLV320DAC3100 via PIO I2S. The synth mixes 5 voices (wavetable lookup + ADSR envelope) per sample — about 0.5% CPU at 22050 Hz.

All Fruit Jam-specific code lives in separate files. Hardware drivers are in `.h` files included from the board config block. The 33 Fruit Jam Lisp functions (graphics-mode, audio-\*, mouse-\*, keyboard, etc.) are in `fruitjam-extensions.ino`, a standard [uLisp Extensions File](http://www.ulisp.com/show?19Q4) with its own `lookup_table2[]`. The Lisp Library (keyboard constants, package system, demo) is in `fruitjam_library.h` as a C++ raw string literal. The main `.ino` requires only 2 changed lines (`#define extensions` and `#include "fruitjam_library.h"`) plus 7 small board-config hooks, keeping the diff against upstream uLisp minimal for easy merging of future releases.

### Workspace — PSRAM Enabled (1,000,000 Objects)

The Fruit Jam's 8MB PSRAM is **enabled**, providing a workspace of **~1,000,000 Lisp objects** (~8MB). This is a ~28× increase over the SRAM-only workspace (~36,000 objects) and makes object count essentially a non-concern for any realistic program.

The PSRAM required a timing fix: the Arduino core initializes PSRAM at boot (125 MHz), but the DVHSTX display library then changes the system clock to 240 MHz via a global constructor, leaving the PSRAM QMI timing registers misconfigured (PSRAM SCK = 120 MHz, exceeding the 109 MHz max). A `psram_reinit_timing()` call in `setup()` recalculates the correct timing for 240 MHz (divisor 3, PSRAM SCK = 80 MHz). This was the root cause of the upstream "HSTX + PSRAM don't work together" issue.

The GC (mark-sweep over all 1M objects) takes several seconds. The `printfreespace` compile option is disabled to avoid a GC pass on every REPL prompt. Use `(room)` to check free objects on demand.

A PSRAM stress test (`tools/stress-test.lsp`) validates data integrity under concurrent HSTX video DMA, I2S audio DMA, USB host activity, and heavy GC pressure. Tested: 500+ rounds, 0 errors.

## Building

Open `ulisp-fruit-jam.ino` in the Arduino IDE with the [Adafruit RP2350 board package](https://github.com/earlephilhower/arduino-pico) installed. Select **Adafruit Fruit Jam** as the board.

### Arduino IDE Settings (Tools menu)

| Setting | Value |
|---------|-------|
| **Board** | Adafruit Fruit Jam |
| **USB Stack** | Adafruit TinyUSB |
| **Flash Size** | 16MB (no FS) |
| **CPU Speed** | 150 MHz (overridden to 240 MHz by DVHSTX at runtime) |
| **Optimize** | Optimize More (-O2) |

⚠️ **Optimize should be "Optimize More (-O2)"** — not the default "Small (-Os)" or "Optimize (-O)". The binary is ~330KB of 16MB flash (~2%), so size is irrelevant. `-O2` meaningfully speeds up the Lisp interpreter's eval loop, graphics rendering, and audio synthesis. Avoid `-O3` and `-Ofast` (aggressive inlining can bloat code, and `-Ofast` enables non-IEEE math).

⚠️ **USB Stack must be "Adafruit TinyUSB"** — not "Adafruit TinyUSB Host (native)". The "Host (native)" option disables USB device mode, which kills serial output and the serial REPL. The correct setting enables dual-mode: native USB = device (serial), PIO USB = host (keyboard).

⚠️ **Remove any user-installed `Adafruit_TinyUSB_Library`** from `~/Arduino/libraries/` — use only the version bundled with the board package. Version mismatches between a user-installed library and the board package can cause USB host init hangs.

## Links

- [uLisp](http://www.ulisp.com/) — the upstream Lisp interpreter
- [uLisp Fruit Jam page](http://www.ulisp.com/show?5CSS)
- [Adafruit Fruit Jam](https://www.adafruit.com/product/6295)
