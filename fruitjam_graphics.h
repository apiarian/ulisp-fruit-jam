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
#include "fruitjam_sprites.h"

// ---- GFX text state (shadows for Adafruit_GFX protected members) ----
// Tracked here because gfxwrite() needs them but they're protected in Adafruit_GFX.
// Updated by the Lisp functions set-text-color, set-text-size, set-text-wrap.
// IMPORTANT: If you add C/C++ code that calls tft.setTextColor(), tft.setTextSize(),
// or tft.setTextWrap() directly, you MUST also update these shadow variables to match.
// Otherwise gfxwrite() will render with stale values.
static uint8_t  fruitjam_text_fg = 0xFF;   // white
static uint8_t  fruitjam_text_bg = 0xFF;   // same as fg = transparent (GFX convention)
static uint8_t  fruitjam_text_size = 1;
static bool     fruitjam_text_wrap = true;

// ---- Init: wire up hooks ----
static void fruitjam_graphics_init() {
  fruitjam_mouse_init();
  // Sprite init deferred — fruitjam_sprites_init() is called on first use
  // to avoid allocating 64KB at boot when it may not be needed.
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

// ---- GFX write: render a character using unscii-8-thin font ----
// Called from gfxwrite() in the main .ino.
// Reads cursor state from display8 and color/size/wrap from shadow variables.
// This replaces tft.write(c) which would use the Adafruit_GFX built-in 5×7 font.
static void fruitjam_gfxwrite_impl (char c) {
  if (!fruitjam_gfx_active) return;
  mouse_hide_for_draw();
  uint8_t *fb = display8.getBuffer();
  if (!fb) return;
  int16_t cx = display8.getCursorX();
  int16_t cy = display8.getCursorY();
  uint8_t sz = fruitjam_text_size;
  int charw = 8 * sz;
  int charh = 8 * sz;
  if (c == '\n') {
    display8.setCursor(0, cy + charh);
  } else if (c != '\r') {
    if (fruitjam_text_wrap && (cx + charw > DISPLAY_WIDTH)) {
      cx = 0;
      cy += charh;
      display8.setCursor(cx, cy);
    }
    uint8_t fg = fruitjam_text_fg;
    uint8_t bg = fruitjam_text_bg;
    if (fg == bg) {
      // Transparent background (GFX convention: setTextColor with one arg)
      if (sz == 1)
        fruitjam_draw_char_8x8_transparent(fb, DISPLAY_WIDTH, DISPLAY_HEIGHT,
                                            cx, cy, c, fg);
      else
        fruitjam_draw_char_8x8_scaled_transparent(fb, DISPLAY_WIDTH, DISPLAY_HEIGHT,
                                                    cx, cy, c, fg, sz);
    } else {
      if (sz == 1)
        fruitjam_draw_char_8x8(fb, DISPLAY_WIDTH, cx, cy, c, fg, bg);
      else
        fruitjam_draw_char_8x8_scaled(fb, DISPLAY_WIDTH, DISPLAY_HEIGHT,
                                       cx, cy, c, fg, bg, sz);
    }
    display8.setCursor(cx + charw, cy);
  }
}

#endif // FRUITJAM_GRAPHICS_H
