// fruitjam_graphics.h — Graphics mode for Fruit Jam
//
// With the single-display architecture (DVHSTX8 for both text and graphics),
// "graphics mode" is just a logical state: the framebuffer is cleared, drawing
// functions are enabled, and the terminal emulator stops rendering.
// "Text mode" redraws the terminal grid from the saved character data.
// No HSTX reinitialization, no PLL changes, no DMA teardown. Rock solid.

#ifndef FRUITJAM_GRAPHICS_H
#define FRUITJAM_GRAPHICS_H

// ---- Graphics state ----
static volatile bool fruitjam_gfx_active = false;

// The tft alias points to display8 (defined in fruitjam_terminal.h)
// It's always valid since display8 is always initialized.
// GFX drawing functions use FRUITJAM_CHECK_GFX() to gate on graphics mode.
#define FRUITJAM_CHECK_GFX() if (!fruitjam_gfx_active) error2("not in graphics mode")

// ---- No init needed ----
static void fruitjam_graphics_init() {
  // display8 is initialized in fruitjam_terminal_begin()
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

  fruitjam_gfx_active = true;
  return true;
}

// ---- Exit graphics mode ----
static void fruitjam_exit_graphics() {
  if (!fruitjam_gfx_active) return;

  fruitjam_gfx_active = false;
  term_draw_suppressed = false;

  // Redraw entire terminal from the saved grid
  display8.fillScreen(term_bg_color);
  for (int r = 0; r < TERM_ROWS; r++) {
    for (int c = 0; c < TERM_COLS; c++) {
      term_draw_cell(c, r);
    }
  }
  term_draw_cursor();
}

#endif // FRUITJAM_GRAPHICS_H
