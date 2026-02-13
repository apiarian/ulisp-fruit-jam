# uLisp for the Adafruit Fruit Jam

A version of the [uLisp](http://www.ulisp.com/) interpreter for the [Adafruit Fruit Jam](https://www.adafruit.com/product/6295) (RP2350B), turning it into a standalone Lisp computer — connect a USB keyboard, an HDMI monitor, power on, and you're in a Lisp REPL.

Based on [uLisp ARM Release 4.9](http://www.ulisp.com/show?5CSS) (9th February 2026).

## What's New

This fork adds USB keyboard input, an HDMI text-mode terminal, and a hardware escape button — everything needed to use the Fruit Jam as a self-contained Lisp machine without a host computer.

### USB Host Keyboard (fruitjam_usbhost.h)

- PIO USB host on core1 (PIO 2 / DMA channel 3 to avoid HSTX conflicts)
- HID keyboard → 256-byte ring buffer → `gserial()` on core0
- US keyboard layout with shift, ctrl, caps lock
- Key repeat (500ms delay, 50ms rate)
- Core1 waits for display clock init before PIO USB init (DVHSTX reconfigures PLL_USB to 480 MHz)

### Text-Mode Terminal (fruitjam_terminal.h)

- `DVHSTXText` at **91×30 characters / 1280×720** — high-resolution text with ~5.5KB buffer (vs ~188KB for a pixel framebuffer)
- VT100 escape sequence parser: cursor movement (CSI A/B/C/D/H/f), SGR colors (30-37, 40-47, bold, dim, reset), erase in display/line (J/K), save/restore cursor, terminal reset
- Hardware block cursor via `showCursor()`
- `pserial()` writes to both the HDMI terminal and USB serial (mirror)
- Line-buffered input with local backspace, Ctrl-C, and Ctrl-U handling — prevents raw control characters from reaching the uLisp reader

### Hardware Escape Button (fruitjam_escape.h)

- BUTTON1 (GPIO0) configured as a GPIO interrupt
- Press it at any time to abort a running program and return to the REPL
- Works even if USB host or the Lisp program is hung
- Checked in `testescape()` before the 500ms-throttled serial check, so response is immediate

## Architecture

```
Core 0: uLisp interpreter + text-mode display
Core 1: USB host keyboard (PIO USB via TinyUSB)

Hardware interrupt: BUTTON1 (GPIO0) → escape to REPL
```

All Fruit Jam-specific code lives in three `.h` files, included from the board config block. The main `.ino` has only 6 small `#if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)` blocks outside the board config section, keeping the diff against upstream uLisp minimal for easy merging of future releases.

### SD Card Support

- `save-image` / `load-image` save and restore the entire workspace as a binary snapshot to the MicroSD card
- `with-sd-card` provides text file I/O for reading/writing `.lsp` source files
- SD card on SPI0 (CS = GPIO39), works with standard FAT-formatted MicroSD cards

### Workspace

With text-mode display instead of a pixel framebuffer, the workspace is **42,280 objects** (ARM) — nearly double the old GFX configuration (22,500). PSRAM support (1,000,000 objects) is a future goal pending resolution of the HSTX+PSRAM coexistence issue.

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

### Patched Library

The installed `Adafruit_DVI_HSTX` library (v1.2.5) has a bug where the `%` character renders as blank. A patched copy is symlinked at `~/Arduino/libraries/Adafruit_DVI_HSTX` → `~/code/Adafruit-DVI-HSTX`. See REFERENCE.md for details and revert instructions.

## Future Work

- **Graphics mode** — `(graphics-mode)` / `(text-mode)` switching between text REPL and pixel framebuffer
- **Wi-Fi** — ESP32-C6 via SPI (WiFiNINA pattern, same as PyPortal)
- **PSRAM** — 8MB / 1M objects (blocked on HSTX coexistence)
- **Audio** — TLV320DAC3100 I2S DAC for sound output
- **Line editor** — enable uLisp's built-in tab completion, paren highlighting, and command recall on HDMI
- **Autorun** — boot directly into a saved program from SD card

## Links

- [uLisp](http://www.ulisp.com/) — the upstream Lisp interpreter
- [uLisp Fruit Jam page](http://www.ulisp.com/show?5CSS)
- [Adafruit Fruit Jam](https://www.adafruit.com/product/6295)
