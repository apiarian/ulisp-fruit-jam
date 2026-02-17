// fruitjam_graphics.h — Graphics mode for Fruit Jam
//
// With the single-display architecture (DVHSTX8 for both text and graphics),
// "graphics mode" is just a logical state: the framebuffer is cleared, drawing
// functions are enabled, and the terminal emulator stops rendering.
// "Text mode" redraws the terminal grid from the saved character data.
// No HSTX reinitialization, no PLL changes, no DMA teardown. Rock solid.
//
// Mouse cursor rendering is in fruitjam_mouse.h.

#ifndef FRUITJAM_GRAPHICS_H
#define FRUITJAM_GRAPHICS_H

#include "fruitjam_terminal.h"
#include "fruitjam_mouse.h"

// ---- GFX text state (shadows for Adafruit_GFX protected members) ----
// Tracked here because gfxwrite() needs them but they're protected in Adafruit_GFX.
// Updated by the Lisp functions set-text-color, set-text-size, set-text-wrap.
static uint8_t  fruitjam_text_fg = 0xFF;   // white
static uint8_t  fruitjam_text_bg = 0xFF;   // same as fg = transparent (GFX convention)
static uint8_t  fruitjam_text_size = 1;
static bool     fruitjam_text_wrap = true;

// ---- Init: wire up hooks ----
static void fruitjam_graphics_init() {
  fruitjam_mouse_init();
}

// ---- Enter graphics mode ----
static bool fruitjam_enter_graphics() {
  if (fruitjam_gfx_active) return true;

  // Erase the terminal cursor
  term_erase_cursor();

  // Suppress terminal drawing (updates grid only, doesn't touch framebuffer)
  term_draw_suppressed = true;

  // Clear framebuffer to black
  display8.fillScreen(0);

  // Reset cursor state
  mouse_cursor_drawn = false;
  mouse_save_x = -1;
  mouse_save_y = -1;

  fruitjam_gfx_active = true;
  return true;
}

// ---- Exit graphics mode ----
static void fruitjam_exit_graphics() {
  if (!fruitjam_gfx_active) return;

  // Hide cursor before switching back
  mouse_erase_cursor();
  mouse_cursor_visible = false;

  fruitjam_gfx_active = false;
  term_draw_suppressed = false;

  // Redraw entire terminal from the saved grid
  term_restore_from_grid();

  // Reset screensaver idle timer so it doesn't activate immediately
  screensaver_poke();
}

#endif // FRUITJAM_GRAPHICS_H
