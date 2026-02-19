# Architecture

Internal technical reference for developers working on the uLisp Fruit Jam codebase.

## Design Principle: Minimal Core Modifications

The upstream `ulisp-arm` codebase is a single ~10K-line `.ino` file that is actively maintained. The primary design constraint is to **minimize changes to the uLisp core** so that:

- **Upstream upgrades are easy** — when a new uLisp ARM release comes out, we can diff and merge cleanly rather than untangling deep modifications.
- **Upstream adoption is possible** — if the uLisp developer wants to integrate Fruit Jam support, our changes should be small and self-contained enough to accept.
- **Fruit Jam-specific code lives in separate files** — USB host, terminal emulator, audio driver, etc. are each in their own `.h` files, included from the main sketch with minimal glue.

In practice:
- **Hook over patch.** uLisp has board-specific `#if` blocks for configuration, and the I/O functions `gserial`/`pserial` are regular functions modifiable per-board. Use these patterns rather than rewriting core loops.
- **New `.h` files for new subsystems.** Each gets its own file, `#include`'d from the board-specific config block.
- **Narrow board config changes.** The `#if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)` block is the natural place for Fruit Jam additions.
- **Avoid modifying core Lisp primitives.** Wrap or redirect at the board level.

## Hardware Overview

| Component | Details |
|-----------|---------|
| MCU | RP2350B dual Cortex-M33 (also RISC-V Hazard3 capable), up to 150MHz rated |
| Flash | 16 MB |
| PSRAM | 8 MB (QSPI, CS on GPIO47) |
| Video | HSTX DVI/HDMI output (GPIO 12-19) via `Adafruit_dvhstx` library |
| Audio | TLV320DAC3100 I2S codec — stereo headphone + mono speaker |
| USB Host | 2-port USB Type-A hub (PIO USB host, DP=GPIO1, DM=GPIO2) |
| USB Client | USB Type-C (bootloading/serial) |
| Wi-Fi | ESP32-C6 coprocessor via SPI |
| Storage | MicroSD card (SPI, CS on GPIO39) |
| I2C | Stemma QT port (SDA=GPIO20, SCL=GPIO21, i2c0) |
| GPIO | 16-pin socket header: 10 A/D GPIO + power |
| Buttons | 3 tactile switches: BUTTON1 (GPIO0), BUTTON2 (GPIO4), BUTTON3 (GPIO5) |
| LEDs | 5× NeoPixels (GPIO 32), 1× onboard LED (GPIO 29, **active-low**) |

### Key Pin Assignments

- **HSTX DVI**: D0+=GPIO15, D1+=GPIO17, D2+=GPIO19, CK+=GPIO13 (D-=D+−1)
- **I2S Audio**: DIN=GPIO24, MCLK=GPIO25, BCLK=GPIO26, LRCLK/WS=GPIO27
- **Peripheral Reset**: GPIO22 (shared reset for audio codec and ESP32-C6)
- **TLV320 I2C**: address 0x18 on i2c0
- **ESP32-C6 SPI**: CS=GPIO46, BUSY/ACK=GPIO3, RESET=GPIO22 (shared), on SPI1
- **SD Card**: CS=GPIO39
- **PSRAM**: CS=GPIO47
- **USB Host**: DP=GPIO1, DM=GPIO2, 5V enable=GPIO11

## System Architecture

```
Core 0: uLisp interpreter + display (DVHSTX8 512×384) + audio synthesis
Core 1: USB host keyboard + mouse (PIO USB via TinyUSB)

Hardware: HSTX → HDMI, PIO 0 → I2S audio, PIO 2 → USB host + NeoPixels
Memory:  SRAM = framebuffer + DMA + remap tables; PSRAM = Lisp workspace + sprite sheet
DMA: 0–2 = HSTX video, 3 = PIO USB, 4 = I2S audio
Interrupts: BUTTON1 (GPIO0) → escape, DMA_IRQ_1 → audio
Headphone detect: polled via I2C every 500ms (not GPIO IRQ — single callback per core)
```

### PIO / DMA / Peripheral Map

| Resource | Assignment | Notes |
|----------|-----------|-------|
| PIO 0, SM 0 | I2S audio output | 1 out pin (DIN) + 2 side-set pins (BCLK, WS) |
| PIO 2, SM 0-2 | PIO USB host | DP=GPIO1, DM=GPIO2 |
| PIO 2, SM 3 | NeoPixels (WS2812) | GPIO 32 requires PIO 2 (PIO 0/1 access GPIO 0–31 only) |
| DMA 0-2 | HSTX video | Hardcoded by DVHSTX library |
| DMA 3 | PIO USB TX | Claimed by Pico_PIO_USB |
| DMA 4 | Audio I2S ring buffer | Explicitly claimed (`dma_channel_claim(4)`) |
| DMA_IRQ_1 | Audio DMA completion | HSTX uses DMA_IRQ_2 |
| PWM (GPIO25) | MCLK 15 MHz | 50% duty, wrap=15 at 240 MHz clk_sys |
| I2C 0 | TLV320 config + Stemma QT | SDA=GPIO20, SCL=GPIO21, addr 0x18 |
| SPI 0 | SD card | GPIO 34/35/36/39 |
| SPI 1 | ESP32-C6 Wi-Fi | GPIO 28/30/31/46 |

### Memory Layout

```
SRAM (~520KB total, ~456KB heap):
  - DVHSTX8 display (~200KB: 192KB framebuffer + ~8KB line buffers)
  - Terminal character grid (~9KB: term_grid[48][64] × 3 bytes)
  - PIO USB buffers (~5–10KB), I2S ring buffer + DMA (~5.5KB)
  - Sprite remap tables (~2KB, heap-allocated on first use)
  - ~251KB free for stack growth and future features

PSRAM (8MB):
  - uLisp workspace: 1,000,000 objects × 8 bytes
  - Sprite sheet: 64KB (256×256, allocated lazily on first use)
  - Memory-mapped at 0x11000000 via QMI/XIP CS1, QSPI at 80 MHz

Flash (16MB):
  - Program code. SD card used for all persistent storage.
  - Arduino IDE set to "16MB Flash, 0MB FS"
```

## Display Subsystem

The display uses a single `DVHSTX8` object for both terminal and graphics. Text mode renders characters from the unscii-8-thin bitmap font directly into the 8bpp pixel framebuffer, with a character grid stored in RAM for scrolling and restore. Graphics mode clears the framebuffer and enables GFX drawing primitives. Switching between modes is instant — no HSTX reinitialization, no PLL changes, no DMA teardown. The same font is used for `(draw-char)` and `(with-gfx)` text in graphics mode.

### 256-Color Palette

8-bit indexed color with a 3-3-2 (RRRGGGBB) layout. The upstream `Adafruit_dvhstx` library ships with a broken 2-3-2 default (128 unique colors, bit 5 wasted); we overwrite it at init with the correct 3-3-2 mapping. The library's palette system is fully encoding-agnostic — `setColor()` stores arbitrary RGB888 values, and the scanline renderer does a straight `palette[byte]` lookup.

## Audio Subsystem

Audio synthesis runs on core0 in the `testescape()` idle loop, filling a 1024-sample ring buffer that DMA streams to the TLV320DAC3100 via PIO I2S. The synth mixes 5 voices (wavetable lookup + ADSR envelope) per sample — about 0.5% CPU at 22050 Hz.

## Sprite System

The sprite sheet (256×256, 64 KB) is allocated from PSRAM via `__psram_malloc()` on first use. Eight remap tables (256 bytes each, 2 KB total) are allocated from SRAM heap. Both are lazily initialized — no memory cost at boot.

The blit inner loop applies remap before the transparency check, so remapping a color *to* the key value makes those pixels transparent, and remapping the key color to something else makes it visible.

Flip is applied before rotation in `sprite-draw`. The combination of 4 flips × 4 rotations covers all 8 unique orientations (dihedral group D4) with some intentional redundancy (e.g., flip `:hv` = rotate `:r180`).

All bulk data — the sprite sheet (64 KB), remap tables (256 bytes each), and audio wavetables (256 bytes each) — use stream-based save/load via `pstreamfun`/`gstreamfun`. This means they work with any uLisp stream: SD card (`with-sd-card`), serial, WiFi (`with-client`), etc.

## File Organization

All Fruit Jam-specific code lives in separate files:

| File | Purpose |
|------|---------|
| `fruitjam-extensions.ino` | 47 Lisp functions (graphics-mode, audio-\*, mouse-\*, sprite-\*, keyboard, etc.) — standard [uLisp Extensions File](http://www.ulisp.com/show?19Q4) with its own `lookup_table2[]` |
| `fruitjam_library.h` | Lisp Library as a C++ raw string literal (keyboard constants, package system, font table, demo) |
| `fruitjam_terminal.h` | Terminal emulator — VT100, character grid, cursor, scrolling |
| `fruitjam_graphics.h` | Graphics mode, GFX integration, mouse cursor rendering |
| `fruitjam_font.h` | unscii-8-thin CP437 bitmap font data |
| `fruitjam_lineedit.h` | Line editor — autocomplete, history, paren matching |
| `fruitjam_autocomplete.h` | Tab completion engine (user + built-in symbols) |
| `fruitjam_usbhost.h` | PIO USB host — keyboard + mouse HID, recovery logic |
| `fruitjam_mouse.h` | Mouse state tracking, HID report parsing |
| `fruitjam_audio.h` | 5-voice wavetable synth, I2S DMA, TLV320 codec driver |
| `fruitjam_bell.h` | Terminal bell — border flash + audio blip |
| `fruitjam_neopixel.h` | NeoPixel PIO driver and Lisp wrappers |
| `fruitjam_sprites.h` | Sprite sheet, remap tables, blit engine |
| `fruitjam_escape.h` | Hardware escape button (GPIO interrupt) |
| `fruitjam_screensaver.h` | Idle screen blanking + NeoPixel wave animation |
| `fruitjam_hooks.h` | Hook functions included after `testescape()` dependencies are defined |

### Changes to the .ino

The .ino has **2 unconditional changes** to the upstream uLisp ARM source:

1. **`#include "fruitjam_library.h"`** — replaces the empty `LispLibrary[]` with Fruit Jam Lisp definitions
2. **`#define extensions`** — enables the official uLisp Extensions File mechanism

Plus **~22 `#if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)` blocks** for:
- **Board config block** (~line 359): defines, includes, all `.h` file inclusion
- **I/O dispatch hooks**: `testescape()`, `pserial()`, `gserial()`, `gserial flush`, `gfxwrite()`, `initgfx()`
- **Function replacements**: `fn_drawchar`, `fn_settextcolor`, `fn_settextsize`, `fn_settextwrap`, `fn_displaysize` (guarded out with `#if !defined`, replaced by versions in `fruitjam-extensions.ino`)
- **Utility hooks**: `errorend()` (auto-exit graphics on error), `line_mark_input_start()`, `psram_reinit_timing()`, `GFXPPWIDTH`, analog pin range, `read-pixel` delegation, alternative docstrings
- **Core1 entry points**: `setup1()`, `loop1()`
- **`#include "fruitjam_hooks.h"`** — included after `testescape()` dependencies are defined

### GFX Text State Shadows

`gfxwrite()` needs text color/size/wrap, but these are `protected` in `Adafruit_GFX`. Shadow variables (`fruitjam_text_fg`, `fruitjam_text_bg`, `fruitjam_text_size`, `fruitjam_text_wrap`) track the values. Any code that calls `tft.setTextColor()` etc. must also update the shadow. Grep for the shadow names before adding new call sites.

### Header Include Order Constraint

The `.h` files are included from the board config block (~line 378), *before* `tstflag`/`setflag`/`clrflag` macro definitions (~line 443). Headers **cannot use these macros**.

### Lisp Library Raw String Warning

**⚠️ No `;` comments in `fruitjam_library.h` raw string.** uLisp's reader does NOT treat `;` as a line comment — it skips from `;` to the next `(` character. Any comment text containing parentheses will confuse the reader. Put documentation in C++ comments outside the raw string.

## PSRAM

The Fruit Jam's 8MB PSRAM provides a workspace of ~1,000,000 Lisp objects (~8 bytes each). The PSRAM required a timing fix: the Arduino core initializes PSRAM at boot (150 MHz), but the DVHSTX display library then changes the system clock to 240 MHz via a global constructor, leaving the PSRAM QMI timing registers misconfigured (PSRAM SCK = 120 MHz, exceeding the 109 MHz max). A `psram_reinit_timing()` call in `setup()` recalculates the correct timing for 240 MHz (divisor 3, PSRAM SCK = 80 MHz). This was the root cause of the upstream "HSTX + PSRAM don't work together" issue.

The GC (mark-sweep over all 1M objects) takes ~7 seconds — PSRAM ~3× slower than SRAM. The `printfreespace` compile option is disabled to avoid a GC pass on every REPL prompt. Use `(room)` to check free objects on demand.

### PSRAM Dynamic Allocation

The arduino-pico core provides PSRAM heap functions in `psram.h`:

```cpp
extern void *__psram_malloc(size_t size);
extern void __psram_free(void *ptr);
extern void *__psram_realloc(void *ptr, size_t size);
extern void *__psram_calloc(size_t num, size_t size);
```

**Note:** The `psram.h` header may not be auto-included when compiling `.h` files from the board config block. Use `extern` declarations directly inside the function that needs them (see `fruitjam_sprites.h` for an example).

### SRAM Heap Pressure

The DVHSTX8 library allocates its 192KB framebuffer and ~8KB of DMA line buffers from the SRAM heap. After that, large `malloc()` calls (e.g., 64KB) can cause the HSTX DVI output to lose signal entirely. Allocate large buffers from **PSRAM** instead. Prefer **lazy allocation** (on first use) over boot-time allocation.

**Rule of thumb:** After `display8.begin()`, treat SRAM heap as reserved for small allocations only (stack, small buffers, remap tables). Anything ≥4KB should go to PSRAM.

## Runtime Memory Diagnostics

```cpp
rp2040.getFreeHeap()       // SRAM heap free (bytes)
rp2040.getTotalHeap()      // SRAM heap total (bytes)
rp2040.getFreePSRAMHeap()  // PSRAM heap free (bytes)
rp2040.getPSRAMSize()      // PSRAM chip size (bytes)
rp2040.getFreeStack()      // Stack headroom (bytes)
```

**Note:** The `PSRAM` attribute places `Workspace[]` directly in PSRAM at link time (not through `malloc`), so `getFreePSRAMHeap()` reports only the remaining PSRAM after the static allocation.

## Object Count Quick Reference

Each uLisp object = 8 bytes. Key costs:

| Construct | Cost |
|-----------|------|
| Cons cell | 1 object |
| Integer / float literal | 1 object |
| Built-in symbol (`+`, `car`, etc.) | 0 (encoded as lookup index) |
| User symbol (≤6 chars) | 1 object (radix-40 packed, interned) |
| User symbol (>6 chars) | 1 + ⌈len/4⌉ objects |
| String | 1 + ⌈len/4⌉ objects |
| Array of N elements | ~2N objects (balanced binary tree) |
| Bit array of N bits | ~N/16 objects |
| `defun` | ~2 + params + body objects |
| `defvar` | 1 + value objects |

**Rule of thumb:** A line of Lisp ≈ 5–15 objects. A `defun` of N body forms ≈ 8–12 objects per form.

| Configuration | Objects |
|--------------|--------:|
| **ARM + PSRAM (current)** | **~1,000,000** |
| ARM, SRAM-only | ~36,000 |
| RISC-V, SRAM-only | ~35,500 |
