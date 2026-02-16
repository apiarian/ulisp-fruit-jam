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

// Forward declaration — defined in fruitjam_screensaver.h (included after this file)
static void screensaver_poke();

// ---- GFX text state (shadows for Adafruit_GFX protected members) ----
// Tracked here because gfxwrite() needs them but they're protected in Adafruit_GFX.
// Updated by the Lisp functions set-text-color, set-text-size, set-text-wrap.
static uint8_t  fruitjam_text_fg = 0xFF;   // white
static uint8_t  fruitjam_text_bg = 0xFF;   // same as fg = transparent (GFX convention)
static uint8_t  fruitjam_text_size = 1;
static bool     fruitjam_text_wrap = true;

// ---- Mouse state (shared between cores) ----
// Defined here (before fruitjam_usbhost.h) so both files can see them.
// Updated by core1 (USB host), read by core0 (Lisp API).
static volatile int16_t  mouse_x = 10;
static volatile int16_t  mouse_y = 10;
static volatile uint8_t  mouse_buttons = 0;
static volatile bool     mouse_clicked = false;

// ---- Mouse cursor ----
// 8x8 arrow sprite, black outline (0x00) + white fill (0xFF), transparent (0xFE = skip).
// 0xFE is safe as the transparent marker even though it's a valid palette index: the
// save-under logic saves ALL framebuffer pixels unconditionally, and only overwrites/
// restores at non-0xFE sprite positions. Framebuffer pixels under transparent sprite
// pixels are never touched, regardless of their value.
static const uint8_t mouse_cursor_sprite[64] = {
  0x00, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE,
  0x00, 0x00, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE,
  0x00, 0xFF, 0x00, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE,
  0x00, 0xFF, 0xFF, 0x00, 0xFE, 0xFE, 0xFE, 0xFE,
  0x00, 0xFF, 0xFF, 0xFF, 0x00, 0xFE, 0xFE, 0xFE,
  0x00, 0xFF, 0xFF, 0x00, 0xFE, 0xFE, 0xFE, 0xFE,
  0x00, 0x00, 0xFE, 0x00, 0xFE, 0xFE, 0xFE, 0xFE,
  0xFE, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE, 0xFE,
};

// Save-under buffer: stores the 8x8 pixels behind the cursor
static uint8_t  mouse_save_under[64];
static int16_t  mouse_save_x = -1;   // -1 = cursor not drawn
static int16_t  mouse_save_y = -1;
static bool     mouse_cursor_visible = false;  // user-controlled show/hide
static bool     mouse_cursor_drawn = false;     // is it actually on screen right now?

// Draw the cursor at (cx, cy), saving what's underneath
static void mouse_draw_cursor(int16_t cx, int16_t cy) {
  if (mouse_cursor_drawn) return;  // already drawn
  mouse_save_x = cx;
  mouse_save_y = cy;
  uint8_t *fb = display8.getBuffer();
  int idx = 0;
  for (int dy = 0; dy < 8; dy++) {
    int py = cy + dy;
    for (int dx = 0; dx < 8; dx++) {
      int px = cx + dx;
      if (px >= 0 && px < DISPLAY_WIDTH && py >= 0 && py < DISPLAY_HEIGHT) {
        int fboff = py * DISPLAY_WIDTH + px;
        mouse_save_under[idx] = fb[fboff];
        uint8_t sp = mouse_cursor_sprite[idx];
        if (sp != 0xFE) fb[fboff] = sp;
      } else {
        mouse_save_under[idx] = 0;
      }
      idx++;
    }
  }
  mouse_cursor_drawn = true;
}

// Erase the cursor by restoring saved pixels
static void mouse_erase_cursor() {
  if (!mouse_cursor_drawn) return;
  uint8_t *fb = display8.getBuffer();
  int idx = 0;
  for (int dy = 0; dy < 8; dy++) {
    int py = mouse_save_y + dy;
    for (int dx = 0; dx < 8; dx++) {
      int px = mouse_save_x + dx;
      if (px >= 0 && px < DISPLAY_WIDTH && py >= 0 && py < DISPLAY_HEIGHT) {
        int fboff = py * DISPLAY_WIDTH + px;
        uint8_t sp = mouse_cursor_sprite[idx];
        if (sp != 0xFE) fb[fboff] = mouse_save_under[idx];
      }
      idx++;
    }
  }
  mouse_cursor_drawn = false;
}

// Update cursor position — called periodically (e.g. from testescape).
// Erases old cursor and redraws at new position if visible.
static void mouse_update_cursor() {
  if (!fruitjam_gfx_active || !mouse_cursor_visible) return;
  int16_t cx = mouse_x;
  int16_t cy = mouse_y;
  if (mouse_cursor_drawn && cx == mouse_save_x && cy == mouse_save_y) return;
  mouse_erase_cursor();
  mouse_draw_cursor(cx, cy);
}

// Hide cursor before a draw operation (called from FRUITJAM_CHECK_GFX)
static void mouse_hide_for_draw() {
  if (mouse_cursor_drawn) mouse_erase_cursor();
}

// The tft alias points to display8 (defined in fruitjam_terminal.h)
// It's always valid since display8 is always initialized.
// GFX drawing functions use FRUITJAM_CHECK_GFX() to gate on graphics mode.
// The cursor is hidden before any draw call and redrawn lazily via testescape().
#define FRUITJAM_CHECK_GFX() do { \
  if (!fruitjam_gfx_active) error2("not in graphics mode"); \
  mouse_hide_for_draw(); \
} while(0)

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
  display8.fillScreen(term_bg_color);
  for (int r = 0; r < TERM_ROWS; r++) {
    for (int c = 0; c < TERM_COLS; c++) {
      term_draw_cell(c, r);
    }
  }
  term_draw_cursor();

  // Reset screensaver idle timer so it doesn't activate immediately
  screensaver_poke();
}

#endif // FRUITJAM_GRAPHICS_H
