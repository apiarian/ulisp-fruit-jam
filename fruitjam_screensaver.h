// fruitjam_screensaver.h — Screensaver for Fruit Jam
//
// Activates after a configurable idle timeout at the REPL with no keyboard input.
// Blanks the screen to black and, if the NeoPixels are all off, starts a gentle
// rolling wave animation — a single soft peak of light sweeps slowly back and
// forth across the 5 NeoPixels in a warm white color.
//
// The wave uses brightness values high enough (peak ~30) to be above the
// NeoPixels' low-brightness quantization noise, while the spatial spread
// across pixels makes the motion smooth and forgiving.
//
// Any keypress immediately wakes: restores the terminal and stops the wave.
// The waking keypress is consumed (not passed to the line editor).

#ifndef FRUITJAM_SCREENSAVER_H
#define FRUITJAM_SCREENSAVER_H

// ---- Configuration ----
#define SCREENSAVER_DEFAULT_TIMEOUT_MS  (5UL * 60 * 1000)  // 5 minutes
#define SCREENSAVER_WAVE_PERIOD_MS      4000                // time for one full sweep (left→right or right→left)
#define SCREENSAVER_WAVE_PEAK           30                  // peak brightness (0-255)
#define SCREENSAVER_WAVE_WIDTH          1.8f                // width of gaussian bell in pixel units

// Warm white color tint (R > G > B)
#define SCREENSAVER_WAVE_R_SCALE        255   // full red
#define SCREENSAVER_WAVE_G_SCALE        200   // ~78% green
#define SCREENSAVER_WAVE_B_SCALE        80    // ~31% blue → warm white

// Rate-limit NeoPixel updates to avoid hammering PIO in tight idle loop
#define SCREENSAVER_UPDATE_INTERVAL_MS  30    // ~33 fps

// ---- State ----
static uint32_t screensaver_timeout_ms = SCREENSAVER_DEFAULT_TIMEOUT_MS;  // 0 = disabled
static uint32_t screensaver_last_activity_ms = 0;
static bool     screensaver_active = false;
static uint32_t screensaver_wave_start_ms = 0;
static uint32_t screensaver_last_update_ms = 0;

// Track whether the neopixels were all off when we activated, so we know
// whether we "own" them and should animate.
static bool     screensaver_owns_neopixels = false;

// ---- Activity tracking ----
// Call this whenever there's keyboard or serial input.
static void screensaver_poke() {
  screensaver_last_activity_ms = millis();
}

// ---- Check if neopixels are all off ----
static bool screensaver_neopixels_dark() {
  for (int i = 0; i < NEOPIXEL_COUNT * 3; i++) {
    if (neopixel_data[i] != 0) return false;
  }
  return true;
}

// ---- Activate screensaver ----
static void screensaver_enter() {
  if (screensaver_active) return;
  screensaver_active = true;

  // Erase cursor and suppress terminal drawing
  term_erase_cursor();
  term_draw_suppressed = true;

  // Black out the framebuffer
  uint8_t *fb = display8.getBuffer();
  if (fb) {
    memset(fb, 0, DISPLAY_WIDTH * DISPLAY_HEIGHT);
  }

  // Check if we should animate the neopixels
  screensaver_owns_neopixels = screensaver_neopixels_dark();
  screensaver_wave_start_ms = millis();
  screensaver_last_update_ms = 0;
}

// ---- Deactivate screensaver ----
static void screensaver_exit() {
  if (!screensaver_active) return;
  screensaver_active = false;

  // Turn off neopixels if we were animating them
  if (screensaver_owns_neopixels) {
    neopixel_clear();
    neopixel_show();
    screensaver_owns_neopixels = false;
  }

  // Restore terminal
  term_draw_suppressed = false;
  term_restore_from_grid();

  // Reset activity timer
  screensaver_last_activity_ms = millis();
}

// Forward declaration (screensaver_wake defined below, needed by tick)
static bool screensaver_wake();

// ---- Tick: call from the idle loop ----
static void screensaver_tick() {
  if (screensaver_active) {
    // Any hardware button (BUTTON2/BUTTON3) wakes the screensaver
    if (digitalRead(PIN_BUTTON2) == LOW || digitalRead(PIN_BUTTON3) == LOW) {
      screensaver_poke();
      screensaver_wake();
      return;
    }
    if (screensaver_owns_neopixels) {
      uint32_t now = millis();
      if (now - screensaver_last_update_ms < SCREENSAVER_UPDATE_INTERVAL_MS) return;
      screensaver_last_update_ms = now;

      // Compute wave center position using a triangle wave (ping-pong).
      // The center sweeps from pixel 0 to pixel 4 and back.
      uint32_t elapsed = now - screensaver_wave_start_ms;
      // Full cycle = left→right→left = 2 * period
      uint32_t full_cycle = SCREENSAVER_WAVE_PERIOD_MS * 2;
      uint32_t pos = elapsed % full_cycle;
      // Triangle wave: 0→1→0 over one full cycle
      float t;
      if (pos < (uint32_t)SCREENSAVER_WAVE_PERIOD_MS) {
        t = (float)pos / (float)SCREENSAVER_WAVE_PERIOD_MS;
      } else {
        t = 1.0f - (float)(pos - SCREENSAVER_WAVE_PERIOD_MS) / (float)SCREENSAVER_WAVE_PERIOD_MS;
      }
      // Map to pixel range: center sweeps from 0.0 to 4.0
      float center = t * (float)(NEOPIXEL_COUNT - 1);

      // Gaussian bell for each pixel
      float w = SCREENSAVER_WAVE_WIDTH;
      for (int i = 0; i < NEOPIXEL_COUNT; i++) {
        float d = (float)i - center;
        float v = expf(-(d * d) / (2.0f * w * w));
        uint8_t brightness = (uint8_t)(v * SCREENSAVER_WAVE_PEAK);
        uint8_t r = (uint8_t)((uint16_t)brightness * SCREENSAVER_WAVE_R_SCALE / 255);
        uint8_t g = (uint8_t)((uint16_t)brightness * SCREENSAVER_WAVE_G_SCALE / 255);
        uint8_t b = (uint8_t)((uint16_t)brightness * SCREENSAVER_WAVE_B_SCALE / 255);
        neopixel_set_pixel_rgb(i, r, g, b);
      }
      neopixel_show();
    }
    return;
  }

  // Not active — check if we should activate
  if (screensaver_timeout_ms == 0) return;  // disabled
  if (fruitjam_gfx_active) return;           // don't activate during graphics mode

  uint32_t now = millis();
  if (now - screensaver_last_activity_ms >= screensaver_timeout_ms) {
    screensaver_enter();
  }
}

// ---- Wake on input: returns true if screensaver was active (keypress consumed) ----
static bool screensaver_wake() {
  if (screensaver_active) {
    screensaver_exit();
    return true;
  }
  return false;
}

#endif // FRUITJAM_SCREENSAVER_H
