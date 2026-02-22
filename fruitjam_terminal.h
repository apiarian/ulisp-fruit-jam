// fruitjam_terminal.h — Terminal emulator for Fruit Jam
// Renders text into DVHSTX8 framebuffer at 512×384 using unscii-8-thin 8×8 font.
// Single display object — no mode switching, rock solid.
// Provides VT100/ANSI escape sequence handling, scrolling, cursor.

#ifndef FRUITJAM_TERMINAL_H
#define FRUITJAM_TERMINAL_H

#ifndef FRUITJAM_NO_DISPLAY
#include <Adafruit_dvhstx.h>
#endif

#include "fruitjam_font.h"

// Graphics resolution — 512×384 8bpp, pixel-doubled to 1024×768 HDMI
#define DISPLAY_WIDTH  512
#define DISPLAY_HEIGHT 384

// Character cell dimensions (unscii-8-thin 8×8 font)
#define CHAR_W 8
#define CHAR_H 8

// Terminal grid size
#define TERM_COLS (DISPLAY_WIDTH / CHAR_W)   // 64
#define TERM_ROWS (DISPLAY_HEIGHT / CHAR_H)  // 48

#ifndef FRUITJAM_NO_DISPLAY

// ---- Bell hook ----
// Called by term_putchar on '\a' and by line_insert_char on buffer full.
// Null until fruitjam_bell_init() runs — safe during early terminal init.
static void (*fruitjam_bell_hook)() = nullptr;

// ---- Pre-draw hook for mouse cursor hiding ----
// Called by FruitJamDisplay before any framebuffer write.
// Set by fruitjam_graphics.h to erase the mouse cursor (save-under restore).
// Null until mouse subsystem initializes — safe during early terminal init.
static void (*fruitjam_pre_draw_hook)() = nullptr;

// ---- Peek-pixel hook for reading under the mouse cursor ----
// Called by FruitJamDisplay::getPixel to read the true pixel value when the
// mouse cursor sprite is drawn on top.  Returns true if the coordinate was
// under the cursor (and sets *out to the saved pixel); false otherwise.
// Coordinates are in raw (unrotated) framebuffer space.
// Set by fruitjam_graphics.h; null until mouse subsystem initializes.
static bool (*fruitjam_peek_pixel_hook)(int16_t x, int16_t y, uint8_t *out) = nullptr;

// ---- Display subclass with mouse-cursor-aware drawing ----
// Overrides all GFXcanvas8 framebuffer write entry points to call the
// pre-draw hook (which erases the mouse cursor). The hook check is a
// single null-pointer test + a bool check per call — after the first
// erase, mouse_cursor_drawn is false, making subsequent calls trivial.
// This eliminates mouse artifacts without any changes to the uLisp core.
class FruitJamDisplay : public DVHSTX8 {
public:
  FruitJamDisplay(DVHSTXPinout pinout, DVHSTXResolution res,
                  bool double_buffered = false)
      : DVHSTX8(pinout, res, double_buffered) {}

  void startWrite() override {
    if (fruitjam_pre_draw_hook) fruitjam_pre_draw_hook();
    DVHSTX8::startWrite();
  }
  void drawPixel(int16_t x, int16_t y, uint16_t color) override {
    if (fruitjam_pre_draw_hook) fruitjam_pre_draw_hook();
    DVHSTX8::drawPixel(x, y, color);
  }
  void fillScreen(uint16_t color) override {
    if (fruitjam_pre_draw_hook) fruitjam_pre_draw_hook();
    DVHSTX8::fillScreen(color);
  }
  void drawFastVLine(int16_t x, int16_t y, int16_t h, uint16_t color) override {
    if (fruitjam_pre_draw_hook) fruitjam_pre_draw_hook();
    DVHSTX8::drawFastVLine(x, y, h, color);
  }
  void drawFastHLine(int16_t x, int16_t y, int16_t w, uint16_t color) override {
    if (fruitjam_pre_draw_hook) fruitjam_pre_draw_hook();
    DVHSTX8::drawFastHLine(x, y, w, color);
  }

  // Shadow GFXcanvas8::getPixel (non-virtual) to peek under the mouse cursor.
  // Since tft/display8 is typed as FruitJamDisplay, calls resolve here at
  // compile time.  Applies the same rotation→raw conversion as the parent,
  // then checks the peek hook before falling back to the framebuffer.
  uint8_t getPixel(int16_t x, int16_t y) const {
    // Rotation→raw coordinate conversion (same as GFXcanvas8::getPixel)
    int16_t t;
    switch (rotation) {
      case 1: t = x; x = WIDTH - 1 - y; y = t; break;
      case 2: x = WIDTH - 1 - x; y = HEIGHT - 1 - y; break;
      case 3: t = x; x = y; y = HEIGHT - 1 - t; break;
    }
    // Check if this raw pixel is under the mouse cursor sprite
    uint8_t val;
    if (fruitjam_peek_pixel_hook && fruitjam_peek_pixel_hook(x, y, &val)) {
      return val;
    }
    // Not under cursor — read framebuffer directly
    if ((x < 0) || (y < 0) || (x >= WIDTH) || (y >= HEIGHT)) return 0;
    uint8_t *buf = getBuffer();
    return buf ? buf[x + y * WIDTH] : 0;
  }
};

// ---- Display object — FruitJamDisplay at 512×384 ----
// Third arg = double_buffered (false = single-buffered).
// Note: GFXcanvas8 buffer allocation is always suppressed by a hardcoded
// `false` inside the DVHSTX8 constructor — the actual framebuffer is
// malloc'd by the HSTX driver in begin().
static DVHSTXPinout fruitjamPinConfig = ADAFRUIT_FRUIT_JAM_CFG;
static FruitJamDisplay display8(fruitjamPinConfig, DVHSTX_RESOLUTION_512x384, false);

// ---- Palette color indices for terminal ----
// DVHSTX8 uses a 2-3-2 RGB palette by default.
// We define named indices for the 8 standard VT100 colors.
// Format: RRGGGGBB (2-3-2 bits)
//   R: bits 7-6 (0-3 → 0,85,170,255)
//   G: bits 5-3 (0-7 → 0,36,73,109,146,182,219,255)
//   B: bits 1-0 (0-3 → 0,85,170,255)
//
// Helper to make a 3-3-2 palette index from 0-255 RGB:
//   ((r >> 5) << 5) | ((g >> 5) << 2) | (b >> 6)
#define PAL332(r,g,b) (((r) >> 5) << 5 | ((g) >> 5) << 2 | ((b) >> 6))

// Standard VT100 colors (normal intensity)
static const uint8_t TERM_PAL_BLACK   = PAL332(0,   0,   0);
static const uint8_t TERM_PAL_RED     = PAL332(170, 0,   0);
static const uint8_t TERM_PAL_GREEN   = PAL332(0,   170, 0);
static const uint8_t TERM_PAL_YELLOW  = PAL332(170, 170, 0);
static const uint8_t TERM_PAL_BLUE    = PAL332(0,   0,   170);
static const uint8_t TERM_PAL_MAGENTA = PAL332(170, 0,   170);
static const uint8_t TERM_PAL_CYAN    = PAL332(0,   170, 170);
static const uint8_t TERM_PAL_WHITE   = PAL332(170, 170, 170);

// Bright variants
static const uint8_t TERM_PAL_BR_BLACK   = PAL332(85,  85,  85);
static const uint8_t TERM_PAL_BR_RED     = PAL332(255, 85,  85);
static const uint8_t TERM_PAL_BR_GREEN   = PAL332(85,  255, 85);
static const uint8_t TERM_PAL_BR_YELLOW  = PAL332(255, 255, 85);
static const uint8_t TERM_PAL_BR_BLUE    = PAL332(85,  85,  255);
static const uint8_t TERM_PAL_BR_MAGENTA = PAL332(255, 85,  255);
static const uint8_t TERM_PAL_BR_CYAN    = PAL332(85,  255, 255);
static const uint8_t TERM_PAL_BR_WHITE   = PAL332(255, 255, 255);

// Normal and bright color tables (indexed 0-7)
static const uint8_t term_normal_colors[8] = {
  TERM_PAL_BLACK, TERM_PAL_RED, TERM_PAL_GREEN, TERM_PAL_YELLOW,
  TERM_PAL_BLUE, TERM_PAL_MAGENTA, TERM_PAL_CYAN, TERM_PAL_WHITE
};
static const uint8_t term_bright_colors[8] = {
  TERM_PAL_BR_BLACK, TERM_PAL_BR_RED, TERM_PAL_BR_GREEN, TERM_PAL_BR_YELLOW,
  TERM_PAL_BR_BLUE, TERM_PAL_BR_MAGENTA, TERM_PAL_BR_CYAN, TERM_PAL_BR_WHITE
};

// ---- Terminal state ----
static int term_cx = 0;  // cursor column (0-based)
static int term_cy = 0;  // cursor row (0-based)

// Current foreground/background color indices (into the 8-color table)
static uint8_t term_fg_idx = 7;  // white
static uint8_t term_bg_idx = 0;  // black
static bool term_bright = false;  // bold/bright attribute

// Current palette colors (resolved from indices + bright)
static uint8_t term_fg_color = TERM_PAL_WHITE;
static uint8_t term_bg_color = TERM_PAL_BLACK;

// ---- Drawing suppression (when in graphics mode) ----
// When true, terminal updates the grid but doesn't draw to framebuffer.
// Set by fruitjam_graphics.h when entering/exiting graphics mode.
static bool term_draw_suppressed = false;

// Scroll counter for line editor paren highlighting (incremented by term_scroll_up)
static int line_scroll_count = 0;

// ---- Character grid (for scrolling and screen save/restore) ----
// Each cell stores the character and its fg/bg colors
struct TermCell {
  char ch;
  uint8_t fg;
  uint8_t bg;
};
static TermCell term_grid[TERM_ROWS][TERM_COLS];

// Saved cursor position (for ESC 7/8 or CSI s/u)
static int term_saved_cx = 0;
static int term_saved_cy = 0;

// ---- VT100 parser state ----
enum TermParseState {
  TERM_NORMAL,
  TERM_ESC,
  TERM_CSI,
  TERM_CSI_PARAM,
};
static TermParseState term_parse_state = TERM_NORMAL;
static int  term_csi_params[8];
static int  term_csi_param_count = 0;
static int  term_csi_current_param = 0;
static bool term_csi_has_param = false;

// ---- Cursor rendering ----
static bool term_cursor_visible = true;
static bool term_cursor_drawn = false;  // tracks XOR state
static unsigned long term_cursor_blink_ms = 0;

static void term_resolve_colors() {
  term_fg_color = term_bright ? term_bright_colors[term_fg_idx] : term_normal_colors[term_fg_idx];
  term_bg_color = term_normal_colors[term_bg_idx];  // background never bright
}

// ---- Low-level drawing ----

static void term_draw_cell(int col, int row) {
  if (col < 0 || col >= TERM_COLS || row < 0 || row >= TERM_ROWS) return;
  if (term_draw_suppressed) return;
  TermCell &cell = term_grid[row][col];
  uint8_t *fb = display8.getBuffer();
  if (!fb) return;
  fruitjam_draw_char_8x8(fb, DISPLAY_WIDTH,
                          col * CHAR_W, row * CHAR_H,
                          cell.ch, cell.fg, cell.bg);
}

static void term_draw_cursor() {
  if (!term_cursor_visible || term_cursor_drawn || term_draw_suppressed) return;
  int px = term_cx * CHAR_W;
  int py = term_cy * CHAR_H;
  // Invert the cursor cell
  for (int y = py; y < py + CHAR_H && y < DISPLAY_HEIGHT; y++) {
    for (int x = px; x < px + CHAR_W && x < DISPLAY_WIDTH; x++) {
      uint8_t p = display8.getPixel(x, y);
      display8.drawPixel(x, y, ~p);
    }
  }
  term_cursor_drawn = true;
  term_cursor_blink_ms = millis();  // reset blink timer so cursor stays visible
}

static void term_erase_cursor() {
  if (!term_cursor_drawn || term_draw_suppressed) return;
  // Redraw the cell normally to remove cursor inversion
  bool was_suppressed = term_draw_suppressed;
  term_draw_suppressed = false;  // temporarily unsuppress to redraw cell
  term_draw_cell(term_cx, term_cy);
  term_draw_suppressed = was_suppressed;
  term_cursor_drawn = false;
}

// ---- Restore terminal from grid ----
// Fills framebuffer with background color, redraws all cells, redraws cursor.
// Caller must set term_draw_suppressed = false before calling.
static void term_restore_from_grid() {
  uint8_t *fb = display8.getBuffer();
  if (fb) {
    memset(fb, term_bg_color, DISPLAY_WIDTH * DISPLAY_HEIGHT);
  }
  for (int r = 0; r < TERM_ROWS; r++) {
    for (int c = 0; c < TERM_COLS; c++) {
      term_draw_cell(c, r);
    }
  }
  term_draw_cursor();
}

static void term_blink_cursor() {
  if (!term_cursor_visible) return;
  unsigned long now = millis();
  if (now - term_cursor_blink_ms >= 500) {
    term_cursor_blink_ms = now;
    if (term_cursor_drawn) {
      term_erase_cursor();
    } else {
      term_draw_cursor();
    }
  }
}

// ---- Scrolling ----

static void term_scroll_up() {
  // Track scrolls for parenthesis highlighting position correction
  line_scroll_count++;
  // Shift grid rows up by 1
  memmove(&term_grid[0], &term_grid[1], sizeof(TermCell) * TERM_COLS * (TERM_ROWS - 1));
  // Clear the bottom row
  for (int x = 0; x < TERM_COLS; x++) {
    term_grid[TERM_ROWS - 1][x] = {' ', term_fg_color, term_bg_color};
  }
  if (term_draw_suppressed) return;
  // Shift framebuffer pixels up by CHAR_H
  uint8_t *buf = display8.getBuffer();
  if (buf) {
    memmove(buf, buf + DISPLAY_WIDTH * CHAR_H,
            DISPLAY_WIDTH * (DISPLAY_HEIGHT - CHAR_H));
    // Clear the bottom CHAR_H rows of pixels
    memset(buf + DISPLAY_WIDTH * (DISPLAY_HEIGHT - CHAR_H),
           term_bg_color, DISPLAY_WIDTH * CHAR_H);
  }
}

// ---- Character output ----

static void term_put_char_at(int col, int row, char c) {
  if (col < 0 || col >= TERM_COLS || row < 0 || row >= TERM_ROWS) return;
  term_grid[row][col] = {c, term_fg_color, term_bg_color};
  term_draw_cell(col, row);
}

static void term_clear_row(int row, int from_col, int to_col) {
  for (int x = from_col; x <= to_col && x < TERM_COLS; x++) {
    term_grid[row][x] = {' ', term_fg_color, term_bg_color};
  }
  if (term_draw_suppressed) return;
  int px = from_col * CHAR_W;
  int w = (to_col - from_col + 1) * CHAR_W;
  display8.fillRect(px, row * CHAR_H, w, CHAR_H, term_bg_color);
}

// ---- VT100 SGR (Select Graphic Rendition) ----

static void term_process_sgr() {
  if (term_csi_param_count == 0) {
    term_fg_idx = 7; term_bg_idx = 0; term_bright = false;
  }
  for (int i = 0; i < term_csi_param_count; i++) {
    int p = term_csi_params[i];
    if (p == 0) { term_fg_idx = 7; term_bg_idx = 0; term_bright = false; }
    else if (p == 1) { term_bright = true; }
    else if (p == 2) { term_bright = false; }
    else if (p == 7) { uint8_t tmp = term_fg_idx; term_fg_idx = term_bg_idx; term_bg_idx = tmp; }
    else if (p >= 30 && p <= 37) { term_fg_idx = p - 30; }
    else if (p >= 40 && p <= 47) { term_bg_idx = p - 40; }
  }
  term_resolve_colors();
}

// ---- VT100 CSI sequence processing ----

static void term_process_csi(char final_char) {
  int p0 = (term_csi_param_count > 0) ? term_csi_params[0] : 0;
  int p1 = (term_csi_param_count > 1) ? term_csi_params[1] : 0;

  switch (final_char) {
    case 'A': { int n = (p0 > 0) ? p0 : 1; term_cy = max(0, term_cy - n); } break;
    case 'B': { int n = (p0 > 0) ? p0 : 1; term_cy = min(TERM_ROWS - 1, term_cy + n); } break;
    case 'C': { int n = (p0 > 0) ? p0 : 1; term_cx = min(TERM_COLS - 1, term_cx + n); } break;
    case 'D': { int n = (p0 > 0) ? p0 : 1; term_cx = max(0, term_cx - n); } break;
    case 'H': case 'f': {
      int row = (p0 > 0) ? p0 - 1 : 0;
      int col = (p1 > 0) ? p1 - 1 : 0;
      term_cy = constrain(row, 0, TERM_ROWS - 1);
      term_cx = constrain(col, 0, TERM_COLS - 1);
    } break;
    case 'J': {
      if (p0 == 0) {
        term_clear_row(term_cy, term_cx, TERM_COLS - 1);
        for (int r = term_cy + 1; r < TERM_ROWS; r++) term_clear_row(r, 0, TERM_COLS - 1);
      } else if (p0 == 1) {
        for (int r = 0; r < term_cy; r++) term_clear_row(r, 0, TERM_COLS - 1);
        term_clear_row(term_cy, 0, term_cx);
      } else if (p0 == 2) {
        for (int r = 0; r < TERM_ROWS; r++) term_clear_row(r, 0, TERM_COLS - 1);
        term_cx = 0; term_cy = 0;
      }
    } break;
    case 'K': {
      if (p0 == 0) term_clear_row(term_cy, term_cx, TERM_COLS - 1);
      else if (p0 == 1) term_clear_row(term_cy, 0, term_cx);
      else if (p0 == 2) term_clear_row(term_cy, 0, TERM_COLS - 1);
    } break;
    case 'm': term_process_sgr(); break;
    case 's': term_saved_cx = term_cx; term_saved_cy = term_cy; break;
    case 'u': term_cx = term_saved_cx; term_cy = term_saved_cy; break;
    case 'n': break;  // Device Status Report — ignore
    default: break;
  }
}

// ---- VT100 parser + character rendering ----

static void term_putchar(char c) {
  // Erase cursor before any update
  term_erase_cursor();

  switch (term_parse_state) {
    case TERM_NORMAL:
      if (c == '\033') {
        term_parse_state = TERM_ESC;
      } else if (c == '\r') {
        term_cx = 0;
      } else if (c == '\n') {
        term_cx = 0;  // CR+LF: newline implies carriage return
        term_cy++;
        if (term_cy >= TERM_ROWS) {
          term_cy = TERM_ROWS - 1;
          term_scroll_up();
        }
      } else if (c == '\b' || c == 0x7F) {
        if (term_cx > 0) term_cx--;
      } else if (c == '\a') {
        // Terminal bell — visual flash + audio blip (via hook set by fruitjam_bell.h)
        if (fruitjam_bell_hook) fruitjam_bell_hook();
      } else if (c == '\t') {
        term_cx = min(((term_cx + 8) & ~7), TERM_COLS - 1);
      } else if (c >= 32 || c < 0) {
        // Printable character
        term_put_char_at(term_cx, term_cy, c);
        term_cx++;
        if (term_cx >= TERM_COLS) {
          term_cx = 0;
          term_cy++;
          if (term_cy >= TERM_ROWS) {
            term_cy = TERM_ROWS - 1;
            term_scroll_up();
          }
        }
      }
      break;

    case TERM_ESC:
      if (c == '[') {
        term_parse_state = TERM_CSI;
        term_csi_param_count = 0;
        term_csi_current_param = 0;
        term_csi_has_param = false;
        memset(term_csi_params, 0, sizeof(term_csi_params));
      } else if (c == '7') {
        term_saved_cx = term_cx; term_saved_cy = term_cy;
        term_parse_state = TERM_NORMAL;
      } else if (c == '8') {
        term_cx = term_saved_cx; term_cy = term_saved_cy;
        term_parse_state = TERM_NORMAL;
      } else if (c == 'c') {
        // Reset terminal
        for (int r = 0; r < TERM_ROWS; r++) term_clear_row(r, 0, TERM_COLS - 1);
        term_cx = 0; term_cy = 0;
        term_fg_idx = 7; term_bg_idx = 0; term_bright = false;
        term_resolve_colors();
        term_parse_state = TERM_NORMAL;
      } else {
        term_parse_state = TERM_NORMAL;
      }
      break;

    case TERM_CSI:
    case TERM_CSI_PARAM:
      if (c >= '0' && c <= '9') {
        term_csi_current_param = term_csi_current_param * 10 + (c - '0');
        term_csi_has_param = true;
        term_parse_state = TERM_CSI_PARAM;
      } else if (c == ';') {
        if (term_csi_param_count < 8)
          term_csi_params[term_csi_param_count++] = term_csi_has_param ? term_csi_current_param : 0;
        term_csi_current_param = 0;
        term_csi_has_param = false;
        term_parse_state = TERM_CSI_PARAM;
      } else if (c >= 0x40 && c <= 0x7E) {
        if (term_csi_has_param && term_csi_param_count < 8)
          term_csi_params[term_csi_param_count++] = term_csi_current_param;
        term_process_csi(c);
        term_parse_state = TERM_NORMAL;
      }
      break;
  }

  // Redraw cursor after update
  term_draw_cursor();
}

#endif // !FRUITJAM_NO_DISPLAY

// ---- Terminal initialization ----

extern volatile bool fruitjam_clocks_ready;

static bool fruitjam_terminal_begin() {
  #ifndef FRUITJAM_NO_DISPLAY
  if (!display8.begin()) {
    return false;
  }

  // Fix default palette: DVHSTX8::begin() uses a broken 2-3-2 encoding
  // (bit 5 wasted, only 128 unique colors, skips index 255).
  // Overwrite with proper 3-3-2 (RRRGGGBB) encoding: 256 unique colors.
  for (int i = 0; i < 256; i++) {
    uint8_t r = (i >> 5) * 255 / 7;         // bits 7-5: 3 bits (8 R levels)
    uint8_t g = ((i >> 2) & 7) * 255 / 7;   // bits 4-2: 3 bits (8 G levels)
    uint8_t b = (i & 3) * 255 / 3;          // bits 1-0: 2 bits (4 B levels)
    display8.setColor(i, r, g, b);
  }

  // Clear screen to black
  display8.fillScreen(0);

  // Initialize terminal grid
  for (int r = 0; r < TERM_ROWS; r++)
    for (int c = 0; c < TERM_COLS; c++)
      term_grid[r][c] = {' ', TERM_PAL_WHITE, TERM_PAL_BLACK};

  term_cx = 0;
  term_cy = 0;
  term_fg_idx = 7;
  term_bg_idx = 0;
  term_bright = false;
  term_resolve_colors();
  #endif

  fruitjam_clocks_ready = true;
  return true;
}

// ---- Display reset (HSTX + DMA recovery) ----
// Tears down and re-initializes the HSTX display pipeline.
// Recovers from DMA stalls or HSTX corruption that cause "no signal".
// In terminal mode, redraws from term_grid. In graphics mode, saves the
// framebuffer to PSRAM before teardown and restores it after re-init.
// Callable from core0 only (DMA/IRQ reconfiguration is not core-safe).

static volatile bool fruitjam_display_reset_requested = false;

static void fruitjam_display_reset() {
  #ifndef FRUITJAM_NO_DISPLAY
  // Save framebuffer to PSRAM before teardown (end/begin frees and re-mallocs).
  // This preserves both graphics mode content and terminal mode content.
  uint8_t *fb_save = nullptr;
  uint8_t *fb = display8.getBuffer();
  if (fb) {
    extern void *__psram_malloc(size_t size);
    fb_save = (uint8_t *)__psram_malloc(DISPLAY_WIDTH * DISPLAY_HEIGHT);
    if (fb_save) memcpy(fb_save, fb, DISPLAY_WIDTH * DISPLAY_HEIGHT);
  }

  // Tear down the display (stops DMA ch 0-2, resets HSTX block, frees buffers)
  display8.end();
  delay(10);

  // Re-initialize (resets HSTX, allocates new framebuffer, restarts DMA chain)
  if (!display8.begin()) {
    if (fb_save) { extern void __psram_free(void *ptr); __psram_free(fb_save); }
    return;
  }

  // Re-apply proper 3-3-2 palette (begin() installs broken 2-3-2)
  for (int i = 0; i < 256; i++) {
    uint8_t r = (i >> 5) * 255 / 7;
    uint8_t g = ((i >> 2) & 7) * 255 / 7;
    uint8_t b = (i & 3) * 255 / 3;
    display8.setColor(i, r, g, b);
  }

  // Restore framebuffer content
  fb = display8.getBuffer();
  if (fb_save && fb) {
    memcpy(fb, fb_save, DISPLAY_WIDTH * DISPLAY_HEIGHT);
  } else if (fb) {
    // Couldn't save — best effort: redraw terminal grid
    term_cursor_drawn = false;
    term_restore_from_grid();
  }
  if (fb_save) { extern void __psram_free(void *ptr); __psram_free(fb_save); }
  #endif
}

// ---- Print to terminal (+ mirror to serial) ----

// Filter ANSI escape sequences from serial output.
// The HDMI terminal understands VT100 sequences, but the Arduino IDE
// serial monitor does not, so we strip them before sending to Serial.
enum SerialFilterState { SF_NORMAL, SF_ESC, SF_CSI };
static SerialFilterState serial_filter_state = SF_NORMAL;

static void serial_write_filtered(char c) {
  switch (serial_filter_state) {
    case SF_NORMAL:
      if (c == '\033') {
        serial_filter_state = SF_ESC;  // swallow ESC
      } else {
        if (c == '\n') Serial.write('\r');
        Serial.write(c);
      }
      break;
    case SF_ESC:
      if (c == '[') {
        serial_filter_state = SF_CSI;  // swallow '['
      } else {
        // ESC + single char sequences (e.g. ESC 7, ESC 8) — swallow both
        serial_filter_state = SF_NORMAL;
      }
      break;
    case SF_CSI:
      // CSI parameters are digits, ';', '?', then a letter terminates
      if (c >= 0x40 && c <= 0x7E) {
        serial_filter_state = SF_NORMAL;  // final byte — swallow it
      }
      // else: intermediate/parameter bytes — keep swallowing
      break;
  }
}

static void fruitjam_pserial(char c) {
  #ifndef FRUITJAM_NO_DISPLAY
  term_putchar(c);
  #endif
  serial_write_filtered(c);
}

#endif // FRUITJAM_TERMINAL_H
