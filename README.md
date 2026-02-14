# uLisp for the Adafruit Fruit Jam

A version of the [uLisp](http://www.ulisp.com/) interpreter for the [Adafruit Fruit Jam](https://www.adafruit.com/product/6295) (RP2350B), turning it into a standalone Lisp computer — connect a USB keyboard, an HDMI monitor, power on, and you're in a Lisp REPL.

Based on [uLisp ARM Release 4.9](http://www.ulisp.com/show?5CSS) (9th February 2026).

## What's New

This fork adds USB keyboard input, an HDMI terminal + graphics display, Wi-Fi networking, SD card storage, and a hardware escape button — everything needed to use the Fruit Jam as a self-contained Lisp machine without a host computer.

### Display (fruitjam_terminal.h + fruitjam_graphics.h)

- Single `DVHSTX8` display at **400×300 @ 8bpp** (800×600 HDMI, pixel-doubled)
- **Text mode:** 66×37 character terminal with VT100 escape sequences, blinking cursor, 8 ANSI colors, scrolling
- **Graphics mode:** Full 400×300 pixel framebuffer with 256-color palette, accessed via uLisp GFX primitives (`draw-pixel`, `fill-rect`, `draw-circle`, etc.)
- `(graphics-mode)` / `(text-mode)` switch instantly — no hardware reconfiguration, just clear and redraw
- `(gfx-test)` built-in demo function
- Terminal text persists through graphics mode — returns exactly where you left off

### USB Host Keyboard (fruitjam_usbhost.h)

- PIO USB host on core1 (PIO 2 / DMA channel 3 to avoid HSTX conflicts)
- HID keyboard → 256-byte ring buffer → `gserial()` on core0
- US keyboard layout with shift, ctrl, caps lock
- Key repeat (500ms delay, 50ms rate)
- Automatic recovery from PIO USB deaf states and core1 lockups

### Hardware Escape Button (fruitjam_escape.h)

- BUTTON1 (GPIO0) configured as a GPIO interrupt
- Press it at any time to abort a running program and return to the REPL
- Works even if USB host or the Lisp program is hung
- If in graphics mode, automatically switches back to text mode
- Checked in `testescape()` and the `gserial()` input wait loop for immediate response

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
Core 0: uLisp interpreter + display (DVHSTX8 400×300)
Core 1: USB host keyboard (PIO USB via TinyUSB)

Hardware interrupt: BUTTON1 (GPIO0) → escape to REPL
```

The display uses a single `DVHSTX8` object for both terminal and graphics. Text mode renders characters via `Adafruit_GFX::drawChar()` into the pixel framebuffer with a character grid stored in RAM for scrolling and restore. Graphics mode clears the framebuffer and enables GFX drawing primitives. Switching between modes is instant — no HSTX reinitialization, no PLL changes, no DMA teardown.

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
- **Audio** — TLV320DAC3100 I2S DAC for sound output
- **Better terminal font** — replace the 6×8 bitmap with a more readable font (8×16 VGA, Terminus, or converted Intel One Mono)
- **Line editor** — enable uLisp's built-in tab completion, paren highlighting, and command recall on HDMI
- **Autorun** — boot directly into a saved program from SD card

## Links

- [uLisp](http://www.ulisp.com/) — the upstream Lisp interpreter
- [uLisp Fruit Jam page](http://www.ulisp.com/show?5CSS)
- [Adafruit Fruit Jam](https://www.adafruit.com/product/6295)
