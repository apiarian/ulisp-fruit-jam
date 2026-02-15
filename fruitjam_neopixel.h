// fruitjam_neopixel.h — PIO-based WS2812 NeoPixel driver for Fruit Jam
//
// The 5 NeoPixels are on GPIO 32, which is in the "HI" GPIO bank (32-47).
// Only PIO 2 can reach GPIOs 16-47. PIO USB also uses PIO 2 (SM 0-2),
// but the 4th state machine (SM 3) is free for NeoPixels.
// PIO 0 is used by I2S audio. PIO 1 can only reach GPIOs 0-31.
//
// pio_claim_free_sm_and_add_program_for_gpio_range() automatically finds
// PIO 2 SM 3 and loads the WS2812 program. The PIO state machine handles
// all bit timing autonomously, immune to CPU interrupts. We just write
// pixel data to the PIO TX FIFO and it handles the rest.
//
// Bit-banging was attempted first but proved unreliable — HSTX DMA IRQs
// fire every ~8.8µs, corrupting the timing-critical WS2812 protocol
// regardless of vblank sync, BASEPRI, or assembly optimization.
//
// API modeled on the official uLisp NeoPixel extension (http://www.ulisp.com/show?4GMV)
// but with a PIO backend instead of the Adafruit_NeoPixel library.

#ifndef FRUITJAM_NEOPIXEL_H
#define FRUITJAM_NEOPIXEL_H

#include "hardware/pio.h"
#include "hardware/clocks.h"
#include "hardware/gpio.h"

// ---- Configuration ----
#define NEOPIXEL_PIN      32
#define NEOPIXEL_COUNT    5

// ---- WS2812 PIO program (from pioasm output) ----
// Standard WS2812 PIO program with side-set for timing.
// T1=2, T2=5, T3=3 → 10 cycles per bit at 800kHz.
#define FJ_WS2812_T1 2
#define FJ_WS2812_T2 5
#define FJ_WS2812_T3 3

static const uint16_t fj_ws2812_program_instructions[] = {
    //     .wrap_target
    0x6221, //  0: out    x, 1            side 0 [2]
    0x1123, //  1: jmp    !x, 3           side 1 [1]
    0x1400, //  2: jmp    0               side 1 [4]
    0xa442, //  3: nop                    side 0 [4]
            //     .wrap
};

static const struct pio_program fj_ws2812_program = {
    .instructions = fj_ws2812_program_instructions,
    .length = 4,
    .origin = -1,
    .pio_version = 0,
#if PICO_PIO_VERSION > 0
    .used_gpio_ranges = 0x0,
#endif
};

// ---- Pixel data buffer (GRB order, 3 bytes per pixel) ----
static uint8_t neopixel_data[NEOPIXEL_COUNT * 3];
static bool neopixel_initialized = false;
static PIO neopixel_pio = NULL;
static uint neopixel_sm = 0;

// Forward declaration
static void neopixel_init();

// ---- Send a single 24-bit GRB value to the PIO ----
static inline void neopixel_put_pixel(uint32_t pixel_grb) {
  pio_sm_put_blocking(neopixel_pio, neopixel_sm, pixel_grb << 8u);
}

// ---- Transmit pixel data to NeoPixels ----
static void neopixel_show() {
  if (!neopixel_initialized) neopixel_init();
  if (!neopixel_pio) return;  // init failed

  for (int i = 0; i < NEOPIXEL_COUNT; i++) {
    uint32_t g = neopixel_data[i * 3 + 0];
    uint32_t r = neopixel_data[i * 3 + 1];
    uint32_t b = neopixel_data[i * 3 + 2];
    uint32_t grb = (g << 16) | (r << 8) | b;
    neopixel_put_pixel(grb);
  }
  // The PIO handles timing automatically. The WS2812 reset pulse (>50µs)
  // happens naturally — by the time the next show() call comes, enough
  // time has elapsed. For safety, add a small delay.
  delayMicroseconds(80);
}

// ---- Initialize NeoPixel PIO ----
static void neopixel_init() {
  if (neopixel_initialized) return;
  neopixel_initialized = true;  // prevent re-entry even on failure

  // Clear pixel buffer
  memset(neopixel_data, 0, sizeof(neopixel_data));

  // Find a free PIO + SM that can reach GPIO 32
  uint offset;
  bool ok = pio_claim_free_sm_and_add_program_for_gpio_range(
      &fj_ws2812_program, &neopixel_pio, &neopixel_sm, &offset,
      NEOPIXEL_PIN, 1, true);
  if (!ok) {
    neopixel_pio = NULL;
    return;
  }

  // Configure the state machine
  pio_gpio_init(neopixel_pio, NEOPIXEL_PIN);
  pio_sm_set_consecutive_pindirs(neopixel_pio, neopixel_sm, NEOPIXEL_PIN, 1, true);

  pio_sm_config c = pio_get_default_sm_config();
  sm_config_set_wrap(&c, offset + 0, offset + 3);  // wrap_target=0, wrap=3
  sm_config_set_sideset(&c, 1, false, false);
  sm_config_set_sideset_pins(&c, NEOPIXEL_PIN);
  sm_config_set_out_shift(&c, false, true, 24);     // MSB first, autopull, 24 bits
  sm_config_set_fifo_join(&c, PIO_FIFO_JOIN_TX);

  int cycles_per_bit = FJ_WS2812_T1 + FJ_WS2812_T2 + FJ_WS2812_T3;
  float div = (float)clock_get_hz(clk_sys) / (800000.0f * cycles_per_bit);
  sm_config_set_clkdiv(&c, div);

  pio_sm_init(neopixel_pio, neopixel_sm, offset, &c);
  pio_sm_set_enabled(neopixel_pio, neopixel_sm, true);
}

// ---- Color utility functions ----

// Pack R, G, B (and optional W) into a 32-bit value
// Format: 0x00RRGGBB (or 0xWWRRGGBB with white)
static uint32_t neopixel_color(uint8_t r, uint8_t g, uint8_t b, uint8_t w = 0) {
  return ((uint32_t)w << 24) | ((uint32_t)r << 16) | ((uint32_t)g << 8) | b;
}

// Unpack 32-bit color to GRB and store in pixel buffer
static void neopixel_set_pixel(int index, uint32_t color) {
  if (index < 0 || index >= NEOPIXEL_COUNT) return;
  uint8_t r = (color >> 16) & 0xFF;
  uint8_t g = (color >> 8) & 0xFF;
  uint8_t b = color & 0xFF;
  neopixel_data[index * 3 + 0] = g;  // GRB order
  neopixel_data[index * 3 + 1] = r;
  neopixel_data[index * 3 + 2] = b;
}

// Set pixel with separate R, G, B components
static void neopixel_set_pixel_rgb(int index, uint8_t r, uint8_t g, uint8_t b) {
  if (index < 0 || index >= NEOPIXEL_COUNT) return;
  neopixel_data[index * 3 + 0] = g;  // GRB order
  neopixel_data[index * 3 + 1] = r;
  neopixel_data[index * 3 + 2] = b;
}

// Clear all pixels (in buffer only — call neopixel_show() to update)
static void neopixel_clear() {
  memset(neopixel_data, 0, sizeof(neopixel_data));
}

// Fill a range of pixels with a color
static void neopixel_fill(uint32_t color, int first_pixel, int count) {
  for (int i = first_pixel; i < first_pixel + count && i < NEOPIXEL_COUNT; i++) {
    neopixel_set_pixel(i, color);
  }
}

// HSV to RGB conversion (matching Adafruit_NeoPixel::ColorHSV)
// hue: 0-65535, sat: 0-255, val: 0-255
// Returns packed 0x00RRGGBB
static uint32_t neopixel_color_hsv(uint16_t hue, uint8_t sat, uint8_t val) {
  uint8_t r, g, b;

  // Adapted from Adafruit_NeoPixel::ColorHSV
  // hue is 0-65535 mapping to 0-360 degrees
  uint8_t region = hue / 10923;  // 65536 / 6 ≈ 10923 per region
  uint16_t remainder = (hue - (region * 10923)) * 6;

  uint8_t p = (val * (255 - sat)) >> 8;
  uint8_t q = (val * (255 - ((sat * remainder) >> 16))) >> 8;
  uint8_t t = (val * (255 - ((sat * (65535 - remainder)) >> 16))) >> 8;

  switch (region) {
    case 0:  r = val; g = t;   b = p;   break;
    case 1:  r = q;   g = val; b = p;   break;
    case 2:  r = p;   g = val; b = t;   break;
    case 3:  r = p;   g = q;   b = val; break;
    case 4:  r = t;   g = p;   b = val; break;
    default: r = val; g = p;   b = q;   break;
  }

  return ((uint32_t)r << 16) | ((uint32_t)g << 8) | b;
}

// Gamma correction table (same as Adafruit_NeoPixel::gamma8)
static const uint8_t neopixel_gamma8[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,
    1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,
    2,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  5,  5,  5,
    5,  6,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  9,  9,  9, 10,
   10, 10, 11, 11, 11, 12, 12, 13, 13, 13, 14, 14, 15, 15, 16, 16,
   17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 24, 24, 25,
   25, 26, 27, 27, 28, 29, 29, 30, 31, 32, 32, 33, 34, 35, 35, 36,
   37, 38, 39, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 50,
   51, 52, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 66, 67, 68,
   69, 70, 72, 73, 74, 75, 77, 78, 79, 81, 82, 83, 85, 86, 87, 89,
   90, 92, 93, 95, 96, 98, 99,101,102,104,105,107,109,110,112,114,
  115,117,119,120,122,124,126,127,129,131,133,135,137,138,140,142,
  144,146,148,150,152,154,156,158,160,162,164,167,169,171,173,175,
  177,180,182,184,186,189,191,193,196,198,200,203,205,208,210,213,
  215,218,220,223,225,228,231,233,236,239,241,244,247,249,252,255
};

// Apply gamma correction to a packed color
static uint32_t neopixel_gamma32(uint32_t color) {
  uint8_t r = neopixel_gamma8[(color >> 16) & 0xFF];
  uint8_t g = neopixel_gamma8[(color >> 8) & 0xFF];
  uint8_t b = neopixel_gamma8[color & 0xFF];
  return ((uint32_t)r << 16) | ((uint32_t)g << 8) | b;
}

// Fill strip with rainbow pattern (matching Adafruit_NeoPixel::rainbow)
static void neopixel_rainbow(uint16_t first_hue, int cycles, uint8_t sat, uint8_t val, bool gammify) {
  for (int i = 0; i < NEOPIXEL_COUNT; i++) {
    uint16_t hue = first_hue + (i * cycles * 65536) / NEOPIXEL_COUNT;
    uint32_t color = neopixel_color_hsv(hue, sat, val);
    if (gammify) color = neopixel_gamma32(color);
    neopixel_set_pixel(i, color);
  }
}

#endif // FRUITJAM_NEOPIXEL_H
