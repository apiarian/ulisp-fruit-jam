// fruitjam_terminal.h — Terminal emulator for Fruit Jam
// Renders text into DVHSTX8 framebuffer at 400×300 using Adafruit_GFX drawChar()
// Single display object — no mode switching, rock solid.
// Provides VT100/ANSI escape sequence handling, scrolling, cursor.

#ifndef FRUITJAM_TERMINAL_H
#define FRUITJAM_TERMINAL_H

#ifndef FRUITJAM_NO_DISPLAY
#include <Adafruit_dvhstx.h>
#endif

// Graphics resolution — 400×300 8bpp, pixel-doubled to 800×600 HDMI
#define DISPLAY_WIDTH  400
#define DISPLAY_HEIGHT 300

// Character cell dimensions (built-in 6×8 font from Adafruit_GFX)
#define CHAR_W 6
#define CHAR_H 8

// Terminal grid size
#define TERM_COLS (DISPLAY_WIDTH / CHAR_W)   // 66
#define TERM_ROWS (DISPLAY_HEIGHT / CHAR_H)  // 37

#ifndef FRUITJAM_NO_DISPLAY

// ---- Display object — DVHSTX8 at 400×300 ----
// Third arg = double_buffered (false = single-buffered).
// Note: GFXcanvas8 buffer allocation is always suppressed by a hardcoded
// `false` inside the DVHSTX8 constructor — the actual framebuffer is
// malloc'd by the HSTX driver in begin().
static DVHSTXPinout fruitjamPinConfig = ADAFRUIT_FRUIT_JAM_CFG;
static DVHSTX8 display8(fruitjamPinConfig, DVHSTX_RESOLUTION_400x300, false);

// ---- Palette color indices for terminal ----
// DVHSTX8 uses a 2-3-2 RGB palette by default.
// We define named indices for the 8 standard VT100 colors.
// Format: RRGGGGBB (2-3-2 bits)
//   R: bits 7-6 (0-3 → 0,85,170,255)
//   G: bits 5-3 (0-7 → 0,36,73,109,146,182,219,255)
//   B: bits 1-0 (0-3 → 0,85,170,255)
//
// Helper to make a 2-3-2 palette index from 0-255 RGB:
//   ((r >> 6) << 6) | ((g >> 5) << 2) | (b >> 6)
#define PAL232(r,g,b) (((r) >> 6) << 6 | ((g) >> 5) << 2 | ((b) >> 6))

// Standard VT100 colors (normal intensity)
static const uint8_t TERM_PAL_BLACK   = PAL232(0,   0,   0);
static const uint8_t TERM_PAL_RED     = PAL232(170, 0,   0);
static const uint8_t TERM_PAL_GREEN   = PAL232(0,   170, 0);
static const uint8_t TERM_PAL_YELLOW  = PAL232(170, 170, 0);
static const uint8_t TERM_PAL_BLUE    = PAL232(0,   0,   170);
static const uint8_t TERM_PAL_MAGENTA = PAL232(170, 0,   170);
static const uint8_t TERM_PAL_CYAN    = PAL232(0,   170, 170);
static const uint8_t TERM_PAL_WHITE   = PAL232(170, 170, 170);

// Bright variants
static const uint8_t TERM_PAL_BR_BLACK   = PAL232(85,  85,  85);
static const uint8_t TERM_PAL_BR_RED     = PAL232(255, 85,  85);
static const uint8_t TERM_PAL_BR_GREEN   = PAL232(85,  255, 85);
static const uint8_t TERM_PAL_BR_YELLOW  = PAL232(255, 255, 85);
static const uint8_t TERM_PAL_BR_BLUE    = PAL232(85,  85,  255);
static const uint8_t TERM_PAL_BR_MAGENTA = PAL232(255, 85,  255);
static const uint8_t TERM_PAL_BR_CYAN    = PAL232(85,  255, 255);
static const uint8_t TERM_PAL_BR_WHITE   = PAL232(255, 255, 255);

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
  int px = col * CHAR_W;
  int py = row * CHAR_H;
  // Draw background
  display8.fillRect(px, py, CHAR_W, CHAR_H, cell.bg);
  // Draw character (only printable)
  if (cell.ch >= 32 && cell.ch < 127) {
    display8.drawChar(px, py, cell.ch, cell.fg, cell.bg, 1);
  }
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
        // Bell — ignore
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
  Serial.println("[TERM] Starting DVHSTX8 at 400x300...");
  Serial.flush();

  if (!display8.begin()) {
    Serial.println("[TERM] display8.begin() FAILED");
    Serial.flush();
    return false;
  }

  Serial.println("[TERM] Display started OK");
  Serial.flush();

  // Fix palette index 255 — DVHSTX8::begin() loop is i < 255, skips it
  display8.setColor(255, 255, 255, 255);  // white

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

// ---- Print to terminal (+ mirror to serial) ----

static void fruitjam_pserial(char c) {
  #ifndef FRUITJAM_NO_DISPLAY
  term_putchar(c);
  #endif
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
}

// ---- Line buffer for gserial() ----
// Features: editing, Tab autocomplete, parenthesis matching, Up arrow recall.
// Adapted from PicoCalc/Cardputer uLisp machines.

// Special key codes from USB keyboard (>= 0x80, set in fruitjam_usbhost.h)
#define KEY_UP    0x80
#define KEY_DOWN  0x81
#define KEY_LEFT  0x82
#define KEY_RIGHT 0x83

#define LINEBUF_SIZE 256
static char  linebuf[LINEBUF_SIZE];
static int   linebuf_len = 0;
static int   linebuf_read = 0;
static bool  linebuf_ready = false;

// Command history ring buffer (Up/Down arrow recall)
// linebuf_accum accumulates lines as they're submitted; finalized on next REPL prompt.
#define HISTORY_SIZE 8
static char  linebuf_hist[HISTORY_SIZE][LINEBUF_SIZE];
static int   linebuf_hist_len[HISTORY_SIZE];
static int   hist_head = 0;      // next slot to write (newest + 1)
static int   hist_count = 0;     // number of entries stored (0..HISTORY_SIZE)
static int   hist_browse = -1;   // browsing position (-1 = not browsing)
static char  linebuf_accum[LINEBUF_SIZE];
static int   linebuf_accum_len = 0;

// ---- Autocomplete state ----
static bool line_autocomplete_reset = true;
static int  line_ac_buf_index = 0;
static int  line_ac_match_len = 0;
static int  line_ac_last_extra = 0;    // chars added by last completion
static unsigned int line_ac_i = 0;     // scan position in symbol table

// Helper: erase n characters backward on screen (no linebuf change)
static void line_erase_back(int n) {
  for (int i = 0; i < n; i++) {
    fruitjam_pserial('\b');
    fruitjam_pserial(' ');
    fruitjam_pserial('\b');
  }
}

// Helper: append a char to linebuf and echo it
static void line_append_char(char c) {
  if (linebuf_len < LINEBUF_SIZE - 1) {
    linebuf[linebuf_len++] = c;
    fruitjam_pserial(c);
  } else {
    Serial.write('\a');  // bell on serial terminal — buffer full
  }
}

// Autocomplete: implemented in .ino (needs access to symbol table).
// Declared here, defined after lookup_table in the .ino.
static void line_autocomplete();

// ---- Parenthesis highlighting ----
// Uses the terminal's character grid and absolute cursor positioning to
// highlight the matching '(' in green when ')' is typed.
// Works correctly even when input wraps across multiple terminal lines.

// Saved cursor position from when line editing started (after the prompt)
static int  line_start_cx = 0;
static int  line_start_cy = 0;
// line_scroll_count declared above (near term_draw_suppressed) for use in term_scroll_up()

// Update cursor start position (called after each line drains, for continuation lines)
static void line_update_input_pos() {
  #ifndef FRUITJAM_NO_DISPLAY
  line_start_cx = term_cx;
  line_start_cy = term_cy;
  line_scroll_count = 0;
  #endif
}

// Called from the REPL at prompt time: finalizes the accumulated command as the
// "previous command" for Up arrow recall, and records cursor position.
static void line_mark_input_start() {
  line_update_input_pos();
  // Finalize accumulated lines as the previous command
  if (linebuf_accum_len > 0) {
    memcpy(linebuf_hist[hist_head], linebuf_accum, linebuf_accum_len);
    linebuf_hist_len[hist_head] = linebuf_accum_len;
    hist_head = (hist_head + 1) % HISTORY_SIZE;
    if (hist_count < HISTORY_SIZE) hist_count++;
    linebuf_accum_len = 0;
  }
  hist_browse = -1;
}

// Compute absolute terminal position for a character at offset `idx` in linebuf.
// Returns false if the position has scrolled off screen.
static bool line_pos_for_index(int idx, int *col, int *row) {
  // Characters from the start of input wrap at TERM_COLS
  int abs_offset = line_start_cx + idx;
  *row = line_start_cy - line_scroll_count + abs_offset / TERM_COLS;
  *col = abs_offset % TERM_COLS;
  if (*row < 0 || *row >= TERM_ROWS) return false;
  return true;
}

// Move cursor to absolute position using CSI H (1-based)
static void line_vt100_goto(int col, int row) {
  char buf[16];
  snprintf(buf, sizeof(buf), "\033[%d;%dH", row + 1, col + 1);
  for (char *p = buf; *p; p++) fruitjam_pserial(*p);
}

// Highlight or unhighlight the '(' at buffer index `idx`
static void line_highlight_at(int idx, bool highlight) {
  int col, row;
  if (!line_pos_for_index(idx, &col, &row)) return;  // scrolled off screen
  // Save cursor
  fruitjam_pserial('\033'); fruitjam_pserial('7');
  // Move to target
  line_vt100_goto(col, row);
  if (highlight) {
    const char *seq = "\033[32;1m(\033[0m";
    for (const char *p = seq; *p; p++) fruitjam_pserial(*p);
  } else {
    fruitjam_pserial('(');
  }
  // Restore cursor
  fruitjam_pserial('\033'); fruitjam_pserial('8');
}

// Index of the currently highlighted '(' in linebuf, or -1
static int line_paren_idx = -1;

// Find and highlight matching parenthesis after ')' is typed.
// Searches the current linebuf only. Matches in previous lines (linebuf_accum)
// are not highlighted because the cursor origin differs across input lines.
static void line_check_paren_match() {
  // Undo previous highlight
  if (line_paren_idx >= 0) {
    line_highlight_at(line_paren_idx, false);
    line_paren_idx = -1;
  }

  // Check if last char is ')'
  if (linebuf_len == 0 || linebuf[linebuf_len - 1] != ')') return;

  // Scan backward for matching '(' — first in current linebuf
  int level = 0;
  bool in_string = false;
  for (int i = linebuf_len - 1; i >= 0; i--) {
    char ch = linebuf[i];
    if (ch == '"') in_string = !in_string;
    if (!in_string) {
      if (ch == ')') level++;
      else if (ch == '(') {
        level--;
        if (level == 0) {
          line_paren_idx = i;
          line_highlight_at(i, true);
          return;
        }
      }
    }
  }

  // Not found in current line — continue scanning into linebuf_accum (previous lines).
  // We can't highlight across lines (different cursor origin), so just skip highlighting.
  // But we don't need to — the visual feedback that the match was NOT found (no highlight)
  // already tells the user their parens span previous lines, which is useful info too.
}

static int fruitjam_line_getchar(int raw_c) {
  // Drain completed line first
  if (linebuf_ready) {
    if (linebuf_read < linebuf_len) return linebuf[linebuf_read++];
    linebuf_ready = false;
    linebuf_len = 0;
    linebuf_read = 0;
    // Update cursor position for continuation lines (don't finalize command yet)
    line_update_input_pos();
    return '\n';
  }

  if (raw_c < 0) return -1;
  uint8_t uc = (uint8_t)raw_c;

  // ---- Undo previous paren highlight before any editing ----
  if (line_paren_idx >= 0) {
    line_highlight_at(line_paren_idx, false);
    line_paren_idx = -1;
  }

  // Reset history browsing on any non-arrow key
  if (uc != KEY_UP && uc != KEY_DOWN) hist_browse = -1;

  // ---- Enter: submit line ----
  if (uc == '\n' || uc == '\r') {
    fruitjam_pserial('\n');
    // Accumulate this line into the command buffer (joined with spaces)
    if (linebuf_len > 0) {
      if (linebuf_accum_len > 0 && linebuf_accum_len < LINEBUF_SIZE - 1) {
        linebuf_accum[linebuf_accum_len++] = ' ';  // space separator between lines
      }
      int copy_len = linebuf_len;
      if (linebuf_accum_len + copy_len > LINEBUF_SIZE - 1) {
        copy_len = LINEBUF_SIZE - 1 - linebuf_accum_len;
      }
      if (copy_len > 0) {
        memcpy(linebuf_accum + linebuf_accum_len, linebuf, copy_len);
        linebuf_accum_len += copy_len;
      }
    }
    linebuf_ready = true;
    linebuf_read = 0;
    line_autocomplete_reset = true;
    if (linebuf_len == 0) { linebuf_ready = false; return '\n'; }
    return linebuf[linebuf_read++];
  }

  // ---- Backspace / Delete ----
  if (uc == '\b' || uc == 0x7F) {
    if (linebuf_len > 0) {
      linebuf_len--;
      fruitjam_pserial('\b'); fruitjam_pserial(' '); fruitjam_pserial('\b');
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Ctrl-C: cancel line ----
  if (uc == 0x03) {
    linebuf_len = 0;
    fruitjam_pserial('\n');
    line_autocomplete_reset = true;
    return '\n';
  }

  // ---- Ctrl-U: erase line ----
  if (uc == 0x15) {
    line_erase_back(linebuf_len);
    linebuf_len = 0;
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Tab: autocomplete ----
  if (uc == '\t') {
    if (linebuf_len > 0) {
      line_autocomplete();
    }
    return -1;
  }

  // ---- Up arrow: browse history backward ----
  if (uc == KEY_UP) {
    if (hist_count > 0) {
      int next = (hist_browse < 0) ? 1 : hist_browse + 1;
      if (next <= hist_count) {
        line_erase_back(linebuf_len);
        linebuf_len = 0;
        hist_browse = next;
        int idx = (hist_head - hist_browse + HISTORY_SIZE) % HISTORY_SIZE;
        for (int i = 0; i < linebuf_hist_len[idx]; i++) {
          line_append_char(linebuf_hist[idx][i]);
        }
      }
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Down arrow: browse history forward (or clear line) ----
  if (uc == KEY_DOWN) {
    if (hist_browse > 1) {
      line_erase_back(linebuf_len);
      linebuf_len = 0;
      hist_browse--;
      int idx = (hist_head - hist_browse + HISTORY_SIZE) % HISTORY_SIZE;
      for (int i = 0; i < linebuf_hist_len[idx]; i++) {
        line_append_char(linebuf_hist[idx][i]);
      }
    } else if (hist_browse >= 0) {
      line_erase_back(linebuf_len);
      linebuf_len = 0;
      hist_browse = -1;
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Ignore other special keys (arrows, etc.) for now ----
  if (uc >= 0x80) return -1;

  // ---- Ignore other control chars ----
  if (uc < 32) return -1;

  // ---- Normal printable character ----
  line_autocomplete_reset = true;
  line_append_char((char)uc);

  // Check for parenthesis match after typing ')'
  if (uc == ')') {
    line_check_paren_match();
  }

  return -1;
}

#endif // FRUITJAM_TERMINAL_H
