# Development Notes

Hard-won implementation learnings, gotchas, and reference material
accumulated during development.

This project was developed with extensive assistance from Anthropic's
Claude.

## Clock Architecture with DVHSTX

The `Adafruit_dvhstx` library runs a C++ global constructor (priority 101) **before `setup()`** that sets CLK_SYS to 240 MHz. The Arduino IDE CPU speed setting is effectively overridden. PIO USB works because 240/48 = 5 (clean divider for 48 MHz USB).

## DVHSTX8 Default Palette Bug (2-3-2 instead of 3-3-2)

The `DVHSTX8::begin()` palette init uses a 2-3-2 bit layout (`RR_GGG_BB`) that wastes bit 5, giving only 128 unique colors (each duplicated) instead of the expected 256. The loop also uses `i < 255`, skipping index 255. No upstream issue has been filed as of Feb 2026.

**Fix:** After `display8.begin()`, overwrite all 256 entries with a proper 3-3-2 (`RRRGGGBB`) encoding. No library fork needed.

## PSRAM + HSTX Clock Timing

Arduino's `psram_init(47)` calculates QMI timing for 125 MHz in `.preinit_array`. DVHSTX then changes clk_sys to 240 MHz, but the Arduino core skips `psram_reinit_timing` because Fruit Jam's `F_CPU` is 150 MHz (matching the skip condition). Result: PSRAM SCK = 240/2 = 120 MHz, exceeding the 109 MHz max.

**Fix:** `psram_reinit_timing(0)` at the top of `setup()`. This bug affects any Arduino RP2350 board with PSRAM + DVHSTX.

## DMA Channel Conflicts — Use Explicit Claims

`dma_claim_unused_channel()` returned channel 3 during audio init; PIO USB later claims channel 3 → panic. **Fix:** explicitly claim channel 4 with `dma_channel_claim(4)`. Dynamic allocation is unsafe when multiple subsystems share DMA with different init timing.

## GPIO IRQ: One Callback Per Core

The RP2350 allows only one GPIO IRQ callback per core via `gpio_set_irq_enabled_with_callback`. The escape button (GPIO0) registers one; adding another **replaces** it. Solution: poll instead.

## Shared Reset Pin (GPIO22)

WiFiNINA's `SpiDrv::begin()` toggles GPIO22, resetting both ESP32-C6 and TLV320. Fix: call `SpiDrv::begin()` early in `initgfx()` (before `fruitjam_audio_init()`) so the reset happens before DAC configuration. Once `SpiDrv::initialized` is set, subsequent WiFi calls skip `begin()`.

## TLV320DAC3100 Gotchas

- **DAC routing register** (page 1, reg 0x23): Must write `0x44` to route to BOTH headphone and speaker mixer paths.
- **HSDETECT bitmask**: Use bits [6:5] (`hs & 0x60`), not retroJam's bits [5:4] (`hs & 0x30`). With 256ms debounce enabled, bit 4 is always 1, making `0x30` always true.
- **Headphone gain staging**: DAC digital 0 dB; headphone -16 dB analog; speaker +6 dB Class-D, -15 dB analog.
- **Headphone detection**: Polled via I2C (not GPIO interrupt — see GPIO IRQ limitation above). Must read flag registers 0x2C-0x2F to clear latched interrupt state.

## uLisp `/` Produces Rationals

`(/ (* 250 3) 4)` → `375/2` (not `187`). Use `(truncate (* v 3) 4)` for integer division, and `checkintfloat()` instead of `checkinteger()` in C functions that accept numeric values.

## NeoPixels and PIO on RP2350

PIO 0/1 access GPIOs 0–31 only; PIO 2 accesses GPIOs 16–47. NeoPixels on GPIO 32 must use PIO 2 SM 3. Bit-banging was tried first but failed due to HSTX DMA IRQs every ~8.8µs corrupting WS2812 timing.

## PSRAM Performance

With 1M objects, GC (mark-sweep) takes **~7 seconds** — PSRAM ~3× slower than SRAM. `printfreespace` is disabled (triggers GC per prompt). `(room)` returns free count without GC. uLisp arrays (balanced binary trees) through PSRAM are the main perf bottleneck.

## SRAM Heap Pressure — Large Allocations Kill HSTX

The DVHSTX8 library allocates its 192KB framebuffer and ~8KB of DMA line buffers from the SRAM heap. After that, a large `malloc()` (e.g., 64KB for the sprite sheet) can cause the HSTX DVI output to lose signal entirely ("no signal" on the monitor), even though the allocation appears to succeed and the rest of the system continues working.

The exact mechanism is unclear — possibly the new allocation overlaps with HSTX DMA descriptor memory or line buffers, or heap fragmentation forces a future internal allocation to fail silently. The symptom is immediate: the first framebuffer write after the large allocation causes the monitor to go dark.

**Fix:** Allocate large buffers from **PSRAM** instead of SRAM. The arduino-pico core provides `__psram_malloc()`. PSRAM reads are ~3× slower than SRAM but this is acceptable for sprite data that is read once per pixel during blit.

Also: prefer **lazy allocation** (on first use) over boot-time allocation. Even a 2KB static array in `.bss` was initially suspected of interfering — moving everything to heap-allocated and lazy eliminated the issue. Boot-time allocation competes with the display library's init sequence.

## Display Reset (HSTX + DMA Recovery)

The HSTX DVI output occasionally loses signal — DMA stalls or HSTX block corruption cause the monitor to go "no signal" while the system otherwise continues running. Root cause is unclear (possibly DMA descriptor corruption from heap pressure or a library bug).

**Recovery:** `fruitjam_display_reset()` in `fruitjam_terminal.h` does a full teardown and reinit:
1. Save the 192KB framebuffer to PSRAM via `__psram_malloc()`
2. `display8.end()` — stops DMA channels 0–2, resets HSTX block, frees SRAM buffers
3. `display8.begin()` — re-allocates framebuffer + line buffers, restarts DMA chain
4. Re-apply the correct 3-3-2 palette (because `begin()` installs the broken 2-3-2 default)
5. Restore the framebuffer from the PSRAM copy
6. Free the PSRAM copy

**Trigger:** Long-press BUTTON1 (≥1s) sets `fruitjam_display_reset_requested` from core1. The flag is checked in `testescape()` on core0 (DMA/IRQ reconfiguration is not core-safe across cores).

**Fallback:** If the PSRAM save fails, `term_restore_from_grid()` redraws the terminal from the character grid (lossy in graphics mode — only terminal content survives).

## USB Host Stability

- **PIO USB silent failure mode:** After transient glitches, TinyUSB never re-enumerates — HID reports arrive with `len=0` indefinitely. Multiple recovery mechanisms in `fruitjam_usbhost.h`.
- **Self-inflicted recovery trap:** An idle-timeout recovery mechanism interpreted no-keys-pressed silence as failure, triggering power-cycles every 15-30s until TinyUSB state corruption bricked the stack. Fix: removed idle timeout entirely.
- **`pio_usb_host_stop()`/`restart()`** are dead code in the library — they set flags nothing checks.
- **`tusb_init` bug:** Must use 2-argument form `tusb_init(1, &host_init)`. The 1-argument macro path ignores the rhport argument (hardcodes rhport=0).
- **Initialization order:** Core1 must wait for core0 to complete `display8.begin()` before PIO USB init (clock dependency). Synchronized via `volatile bool fruitjam_clocks_ready`.
- **Boot-protocol mice:** Many send 3-byte reports, not TinyUSB's expected 5. Check `len >= 3`.
- Uses stock `Pico_PIO_USB` v0.7.2 (no patches). Library has infinite busy-wait loops (upstream issues #197, #192) but our recovery mechanisms break core1 out within 5 seconds.

## Audio Ring Buffer Fill Strategy

Fill all available ring buffer space per `fruitjam_audio_fill()` call, not just one block. With a 1024-sample buffer (46ms), this tolerates irregular call timing. Pre-fill entire buffer with silence at init.

## Self-Releasing Notes vs. audio-stop

`audio-note` with duration: on timer expiry, `phase_inc` zeroed but `volume` preserved (for reuse). `audio-stop` zeros both — callers must restore volume afterward.

## Arduino Build Notes

- **USB Stack setting:** Must be "Adafruit TinyUSB" (not "Host (native)"). "Host (native)" disables USB device mode — no serial.
- **Library discovery:** `#include <pio_usb.h>` must appear in the `.ino` file (angle brackets) to force Arduino's library resolver to add include paths. `#include "pio_usb.h"` inside a `.h` file isn't scanned.
- **Duplicate libraries:** A user-installed `Adafruit_TinyUSB_Library` alongside the board-bundled version can cause USB host hangs. Remove user-installed copy.

## Screensaver vs. Long-Running Programs

The screensaver idle timeout was only reset by keyboard/serial input events. Long-running Lisp programs that print output without reading input (e.g., compute loops with `print`) would trip the 5-minute timeout and blank the screen mid-execution. **Fix:** `screensaver_poke()` is now called from `testescape()` every 500ms, keeping the screensaver at bay whenever the interpreter is actively running.

## Cooperative Multitasking

uLisp is single-threaded; `testescape()` is checked every `eval()` call. No ISR callbacks into the Lisp evaluator. Users write polling-based event loops for interactive programs (same as Lisp Badge, IchigoJam, classic home computers).

## Future Enhancement Ideas

### Keyboard

- **Key-up/release events:** HID callback has full 6-key rollover state. Push release events with flag bit (e.g., bit 15). Refs: PicoEdit, M5Cardputer sedit fork.
- **Instantaneous key-state polling `(key-pressed? code)`:** Maintain `keys_currently_held[6]` from HID callback. Ref: SDL's `SDL_GetKeyboardState()`.

### Sprite Enhancements

- **Tilemap function** — draw a grid of tiles from the sheet efficiently (PICO-8 `map()` equivalent), with per-tile flip/rotate
- **Sprite editor** — built entirely in Lisp using mouse, keyboard, and the existing sprite/graphics primitives
- **BMP file loading** — parse BMP headers to load standard images into the sheet from SD card
- **Lisp helpers** — `sprite-clear`, `sprite-fill`, `sprite-tile` (trivial wrappers definable in `fruitjam_library.h`)
- **Second sprite sheet** — for games needing more than 64 KB of art

## Reference Projects & Prior Art

### Fruit Jam Projects

| Project | Description | Link |
|---------|-------------|------|
| **retroJam** | Multi-console emulator with HSTX video, I2S audio via TLV320, USB host, PSRAM, SD | https://github.com/fhoedemakers/retroJam |
| **Fruit-Jam-OS** | CircuitPython "OS" with launcher, apps, full peripheral support | https://github.com/adafruit/Fruit-Jam-OS |
| **Pico2MSX** | MSX emulator adapted for Fruit Jam | https://github.com/RafaGS/Pico2MSX |

### Display / Video Libraries

| Project | Description | Link |
|---------|-------------|------|
| **DispHSTX** | Comprehensive DVI/VGA driver for RP2350 HSTX — 40 formats, up to 1440×600 | https://github.com/Panda381/DispHSTX |

### uLisp Ecosystem

| Project | Description | Link |
|---------|-------------|------|
| **ulisp-arm** | Upstream uLisp ARM (our base) | https://github.com/technoblogy/ulisp-arm |
| **lisp-badge** | Handheld Lisp computer with keyboard + display (inspiration) | https://github.com/technoblogy/lisp-badge |

### Hardware References

| Resource | Link |
|----------|------|
| **Fruit Jam PCB files** | https://github.com/adafruit/Adafruit-Fruit-Jam-PCB |
| **CircuitPython FruitJam helper** | https://github.com/adafruit/Adafruit_CircuitPython_FruitJam |
| **TLV320DAC3100 datasheet** | https://www.ti.com/lit/ds/symlink/tlv320dac3100.pdf |


