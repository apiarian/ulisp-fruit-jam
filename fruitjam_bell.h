// fruitjam_bell.h — Terminal bell: visual flash + audio blip
//
// Provides the terminal bell (triggered by '\a' or line buffer full).
// Visual: bright yellow border flash. Audio: short sine blip on a private voice.
//
// Public API:
//   fruitjam_bell_init()     — wire up the hook and initialize bell voice
//   fruitjam_bell_trigger()  — start visual flash + mark audio pending
//   fruitjam_bell_tick()     — fire audio blip, manage flash timeout
//   fruitjam_bell_cancel()   — immediate silence + un-flash + restore bell voice


#ifndef FRUITJAM_BELL_H
#define FRUITJAM_BELL_H

#include "fruitjam_terminal.h"
#include "fruitjam_audio.h"

// ---- Bell audio parameters ----
#define BELL_MIDI_NOTE   84     // C6 (~1047 Hz)
#define BELL_DURATION_MS 60     // short blip
#define BELL_VOLUME      80     // moderate volume (0-255)

// ---- Bell visual flash parameters ----
#define BELL_FLASH_DURATION_MS 80   // how long the visual flash lasts
#define BELL_FLASH_BORDER_W 3       // border width in pixels
#define BELL_FLASH_COLOR PAL232(255, 255, 85)  // bright yellow

// ---- Bell state ----
static volatile bool fruitjam_bell_pending = false;
static bool fruitjam_bell_flash_active = false;
static uint32_t fruitjam_bell_flash_start_ms = 0;

// ---- Bell visual flash ----
// Draws a colored border around the screen edges.

static void fruitjam_bell_flash() {
  if (fruitjam_bell_flash_active || term_draw_suppressed) return;
  uint8_t *buf = display8.getBuffer();
  if (!buf) return;
  int w = BELL_FLASH_BORDER_W;
  uint8_t c = BELL_FLASH_COLOR;
  // Top border
  memset(buf, c, DISPLAY_WIDTH * w);
  // Bottom border
  memset(buf + DISPLAY_WIDTH * (DISPLAY_HEIGHT - w), c, DISPLAY_WIDTH * w);
  // Left and right borders (between top and bottom)
  for (int y = w; y < DISPLAY_HEIGHT - w; y++) {
    uint8_t *row = buf + y * DISPLAY_WIDTH;
    memset(row, c, w);
    memset(row + DISPLAY_WIDTH - w, c, w);
  }
  fruitjam_bell_flash_active = true;
  fruitjam_bell_flash_start_ms = millis();
}

// Erases the border by redrawing affected terminal cells and clearing gutters.
static void fruitjam_bell_unflash() {
  if (!fruitjam_bell_flash_active) return;
  fruitjam_bell_flash_active = false;
  if (term_draw_suppressed) return;
  int border_rows = (BELL_FLASH_BORDER_W + CHAR_H - 1) / CHAR_H;
  int border_cols = (BELL_FLASH_BORDER_W + CHAR_W - 1) / CHAR_W;
  // Top rows (full width)
  for (int r = 0; r < border_rows && r < TERM_ROWS; r++)
    for (int c = 0; c < TERM_COLS; c++)
      term_draw_cell(c, r);
  // Bottom rows (full width)
  for (int r = TERM_ROWS - border_rows; r < TERM_ROWS; r++)
    for (int c = 0; c < TERM_COLS; c++)
      if (r >= border_rows) term_draw_cell(c, r);
  // Left and right columns (middle rows only)
  for (int r = border_rows; r < TERM_ROWS - border_rows; r++) {
    for (int c = 0; c < border_cols; c++) term_draw_cell(c, r);
    for (int c = TERM_COLS - border_cols; c < TERM_COLS; c++) term_draw_cell(c, r);
  }
  // Clear any gutter pixels outside the terminal grid
  uint8_t *buf = display8.getBuffer();
  if (!buf) return;
  int grid_w = TERM_COLS * CHAR_W;
  int grid_h = TERM_ROWS * CHAR_H;
  if (grid_w < DISPLAY_WIDTH) {
    int gutter_w = DISPLAY_WIDTH - grid_w;
    for (int y = 0; y < DISPLAY_HEIGHT; y++)
      memset(buf + y * DISPLAY_WIDTH + grid_w, term_bg_color, gutter_w);
  }
  if (grid_h < DISPLAY_HEIGHT) {
    for (int y = grid_h; y < DISPLAY_HEIGHT; y++)
      memset(buf + y * DISPLAY_WIDTH, term_bg_color, grid_w);
  }
}

// ---- Public API ----

// Start visual flash and mark audio pending.
static void fruitjam_bell_trigger() {
  fruitjam_bell_pending = true;
  fruitjam_bell_flash();
}

// Set up the private bell voice (sine wave, fixed volume).
static void fruitjam_bell_voice_init() {
  audio_set_builtin_waveform(BELL_VOICE, AUDIO_WAVE_SINE);
  audio_voices[BELL_VOICE].volume = BELL_VOLUME;
}

// Called from testescape(). Fires the audio blip when pending, and
// removes the visual flash after its timeout expires.
static void fruitjam_bell_tick() {
  if (fruitjam_bell_pending) {
    fruitjam_bell_pending = false;
    if (audio_initialized) {
      float freq = 440.0f * powf(2.0f, (BELL_MIDI_NOTE - 69) / 12.0f);
      audio_set_freq(BELL_VOICE, freq);
      audio_voices[BELL_VOICE].release_at_ms = millis() + BELL_DURATION_MS;
    }
  }
  if (fruitjam_bell_flash_active) {
    if (millis() - fruitjam_bell_flash_start_ms >= BELL_FLASH_DURATION_MS) {
      fruitjam_bell_unflash();
    }
  }
}

// Immediate silence + un-flash + restore bell voice for next use.
static void fruitjam_bell_cancel() {
  fruitjam_bell_pending = false;
  if (fruitjam_bell_flash_active) fruitjam_bell_unflash();
  fruitjam_bell_voice_init();
}

// Wire up the bell hook in fruitjam_terminal.h and initialize the bell voice.
// Call once from fruitjam_initgfx_impl() after fruitjam_audio_init().
static void fruitjam_bell_init() {
  fruitjam_bell_hook = fruitjam_bell_trigger;
  fruitjam_bell_voice_init();
}

#endif // FRUITJAM_BELL_H
