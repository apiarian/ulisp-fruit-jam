# uLisp for the Adafruit Fruit Jam

A version of the [uLisp](http://www.ulisp.com/) interpreter for the [Adafruit Fruit Jam](https://www.adafruit.com/product/6200) (RP2350B), turning it into a standalone Lisp computer — connect a USB keyboard, an HDMI monitor, power on, and you're in a Lisp REPL.

Based on [uLisp ARM Release 4.9](http://www.ulisp.com/show?5CSS) (9th February 2026).

## What's New

This fork adds USB keyboard + mouse input, an HDMI terminal + graphics display, a sprite sheet with flip/rotate/scale/remap, keyboard input for Lisp programs (with modifier key support), a 5-voice wavetable synthesizer with ADSR envelopes, NeoPixel control, a screensaver with NeoPixel wave animation, Wi-Fi networking, SD card storage, and a hardware escape button — everything needed to use the Fruit Jam as a self-contained Lisp machine without a host computer.

## Features

### Display

- **512×384 @ 8bpp** HDMI output (1024×768 pixel-doubled)
- **Text mode:** 64×48 character terminal with VT100 escape sequences, blinking cursor, 8 ANSI colors, scrolling
- **Graphics mode:** Full 512×384 pixel framebuffer with 256-color palette, accessed via uLisp GFX primitives (`draw-pixel`, `fill-rect`, `draw-circle`, etc.)
- **[unscii-8-thin](https://github.com/viznut/unscii) font** — 8×8 bitmap font used throughout (terminal and graphics mode text)
- **256-color palette:** 8-bit indexed color with a 3-3-2 (RRRGGGBB) layout — 8 levels each for red and green, 4 for blue
- `(graphics-mode)` / `(text-mode)` switch instantly — terminal text persists through graphics mode
- `(demo)` built-in interactive demo (paint + sound + buttons + LEDs)
- `(font-table)` — graphical CP437 font map showing all 256 characters
- `(font-table-text)` — text-mode version of the font table

### Line Editor

- **Tab autocomplete** of both user-defined and built-in symbols (cycles through all matches)
- **Parenthesis matching** — highlights matching `(` in green when `)` is typed
- **8-entry command history** (Up/Down arrows)
- In-line cursor movement (Left/Right/Home/End with insert and delete at any position)
- Ctrl-C abort, Ctrl-U erase line
- **Terminal bell:** BEL character triggers a yellow border flash and audio blip

### Sprite Sheet

- **256×256 pixel sprite sheet** — 8-bit palette indices, stored in PSRAM (64 KB)
- **`sprite-pixel`** — get/set individual pixels on the sheet from Lisp
- **`sprite-draw`** — blit any rectangle from the sheet to the screen with:
  - **Color key transparency** (default: index 0 is transparent, `nil` = draw all)
  - **Flip**: `:none`, `:h` (horizontal), `:v` (vertical), `:hv` (both)
  - **Rotate**: `:none`, `:r90`, `:r180`, `:r270` (clockwise)
  - **Integer scaling**: 1× to arbitrary (each pixel → scale×scale block)
  - **Palette remapping**: 8 remap tables (256 entries each) for recoloring sprites without redrawing
- **`sprite-remap`** — read/write/reset the 8 palette remap tables
- **`sprite-save`** / **`sprite-load`** — stream-based persistence (works with SD card, serial, WiFi, or any uLisp stream)
- **`sprite-remap-save`** / **`sprite-remap-load`** — stream-based persistence for remap tables
- Lazy allocation — no memory used until first sprite function call
- Inspired by PICO-8 and TIC-80 fantasy consoles

```lisp
;; Draw a 16x16 red square on the sprite sheet
(dotimes (y 16) (dotimes (x 16) (sprite-pixel x y 224)))

;; Blit to screen at (100,100), flipped, scaled 2x
(graphics-mode)
(sprite-draw 0 0 16 16 100 100 0 :h :none 2)

;; Recolor: remap red (224) to blue (3), draw with table 0
(sprite-remap 0 224 3)
(sprite-draw 0 0 16 16 200 100 0 :none :none 1 0)

;; Save/load sprite sheet to SD card
(with-sd-card (s "sprites.dat" 2) (sprite-save s))
(with-sd-card (s "sprites.dat") (sprite-load s))

;; Save/load remap tables
(with-sd-card (s "remap0.dat" 2) (sprite-remap-save 0 s))
(with-sd-card (s "remap0.dat") (sprite-remap-load 0 s))
```

**7 Lisp functions:** `sprite-pixel`, `sprite-draw`, `sprite-remap`, `sprite-save`, `sprite-load`, `sprite-remap-save`, `sprite-remap-load`

### USB Keyboard + Mouse

- USB keyboard with US layout, shift, ctrl, caps lock, key repeat
- **USB mouse** with boot-protocol and generic HID report descriptor parsing
- Mouse cursor: 8×8 arrow sprite with save-under buffer, auto-hidden during draw calls
- `(mouse-x)`, `(mouse-y)`, `(mouse-buttons)`, `(mouse-click)` — read mouse state from Lisp
- `(mouse-show)`, `(mouse-hide)` — show/hide the cursor in graphics mode

### Keyboard Input for Lisp Programs

- **`(keyboard)`** — non-blocking, returns next key as an integer or `nil` if buffer is empty
- **`(wait-keyboard)`** — blocking, waits for a keypress (supports escape via button 1)
- **`(keyboard-flush)`** — discards all pending keys
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

### Hardware Escape Button

- BUTTON1 (GPIO0) configured as a GPIO interrupt
- **Short press:** abort a running program and return to the REPL
- **Ctrl-C on keyboard:** equivalent to a short press
- **Long press (≥1s):** also triggers USB host power-cycle recovery and HDMI display reset (recovers from DMA stalls or HSTX corruption)
- Works even if USB host or the Lisp program is hung
- Automatically switches back to text mode, silences audio, clears NeoPixels

### Audio — 5-Voice Wavetable Synthesizer

- **5 uniform voices** (0–4) — any waveform on any voice, inspired by classic sound chips (SID, AY-3-8910, NES APU)
- Built-in waveforms: sine, square, triangle, sawtooth, noise (LFSR)
- **Custom wavetables:** pass a 256-element uLisp array to `audio-wave` for arbitrary waveform shapes
- **ADSR envelopes** per voice: attack/decay/release in milliseconds, sustain level 0–255
- `audio-note` auto-triggers envelope, `audio-release` fades to silence
- **Self-releasing notes:** `(audio-note voice note duration)` — optional duration in ms for fire-and-forget sound effects
- **Headphone detection:** auto-switches between speaker and 3.5mm headphone jack
- `(audio-output mode)` for manual routing: `:auto` (default), `:speaker`, `:headphone`, or `:both`
- **`audio-wave-save`** / **`audio-wave-load`** — stream-based wavetable persistence

```lisp
;; Play a C major chord with sine waves
(audio-wave 0 :sine) (audio-wave 1 :sine) (audio-wave 2 :sine)
(audio-vol 0 120) (audio-vol 1 120) (audio-vol 2 120)
(audio-note 0 60) (audio-note 1 64) (audio-note 2 67)

;; Fire-and-forget sound effect (150ms square blip, no envelope needed)
(audio-wave 0 :square) (audio-vol 0 150)
(audio-note 0 72 150)

;; Plucky arpeggio with envelope
(audio-wave 0 :square)
(audio-vol 0 180)
(audio-envelope 0 5 50 0 100)            ; quick attack, short decay, no sustain
(dolist (n '(48 55 52 60 48 55 52 60))
  (audio-note 0 n) (delay 120))

;; Custom wavetable from Lisp
(let ((wt (make-array 256)))
  (dotimes (i 256)
    (setf (aref wt i) (truncate (* 127 (sin (* 6.283 (/ i 256.0)))) 1)))
  (audio-wave 0 wt))

;; Save/load custom wavetables to SD card
(with-sd-card (s "wave0.dat" 2) (audio-wave-save 0 s))
(with-sd-card (s "wave0.dat") (audio-wave-load 0 s))

(audio-output :speaker)                    ; force speaker output
(audio-output :auto)                      ; restore auto-switching
(audio-stop-all)                          ; silence everything
```

**Built-in demo:** `(demo)` — an interactive paint app that showcases audio (UI click/blip sound effects, startup jingle), mouse drawing with **5 sprite-based brush shapes** (circle, square, diamond, star, spray — procedurally generated on the sprite sheet at startup, drawn via `sprite-draw` with remap for recoloring), 3 brush scales using integer sprite scaling, button input (color/size cycling), keyboard shortcuts (Escape to quit, b/c/s/x for brush/color/size/clear), and NeoPixels (reflecting current brush color).

**14 Lisp functions:** `audio-wave`, `audio-freq`, `audio-note`, `audio-vol`, `audio-master-vol`, `audio-stop`, `audio-stop-all`, `audio-playing`, `audio-envelope`, `audio-trigger`, `audio-release`, `audio-output`, `audio-wave-save`, `audio-wave-load`

### NeoPixel Control

- 5 onboard NeoPixels (WS2812, GPIO 32) accessible from Lisp
- API matches the [official uLisp NeoPixel extension](http://www.ulisp.com/show?4GMV)
- HSV colors, gamma correction, rainbow fills

```lisp
(pixels-set-pixel-color 0 32 0 0)   ; pixel 0 = red
(pixels-set-pixel-color 1 0 32 0)   ; pixel 1 = green
(pixels-show)                         ; transmit to hardware

(pixels-rainbow)                      ; fill with rainbow
(pixels-show)
```

**8 Lisp functions:** `pixels-begin`, `pixels-clear`, `pixels-fill`, `pixels-set-pixel-color`, `pixels-color`, `pixels-color-hsv`, `pixels-show`, `pixels-rainbow`

### Screensaver

- Activates after **5 minutes** of no keyboard or serial input at the REPL
- Blanks the HDMI screen to black (prevents burn-in on static text)
- If the NeoPixels are all off, starts a gentle **rolling wave** animation
- Any keypress or hardware button instantly wakes: restores the terminal and turns off the wave
- `(set-screensaver seconds)` — configure timeout (default 300); `(set-screensaver 0)` to disable; `(set-screensaver)` returns current timeout

### Button Input

- `(button n)` — returns `t` if button n (1–3) is pressed, `nil` otherwise
- BUTTON2 (GPIO4) and BUTTON3 (GPIO5) are the primary buttons for user programs
- BUTTON1 (GPIO0) is readable but its escape interrupt fires on press before the result can be used

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

### Workspace — 1,000,000 Lisp Objects

The Fruit Jam's 8MB PSRAM provides a workspace of **~1,000,000 Lisp objects** (~8MB). This is a ~28× increase over the SRAM-only workspace (~36,000 objects) and makes object count essentially a non-concern for any realistic program.

The GC (mark-sweep over all 1M objects) takes several seconds. Use `(room)` to check free objects on demand.

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
- [Adafruit Fruit Jam](https://www.adafruit.com/product/6200)
