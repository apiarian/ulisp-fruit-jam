// fruitjam_terminal.h — VT100 terminal emulator for Fruit Jam
// Wraps DVHSTXText to interpret VT100/ANSI escape sequences
// and provides pserial/gserial hooks for uLisp

#ifndef FRUITJAM_TERMINAL_H
#define FRUITJAM_TERMINAL_H

#ifndef FRUITJAM_NO_DISPLAY
#include <Adafruit_dvhstx.h>
#endif

#ifndef FRUITJAM_NO_DISPLAY
// ---- Text mode display ----
static DVHSTXPinout textPinConfig = ADAFRUIT_FRUIT_JAM_CFG;
static DVHSTXText textDisplay(textPinConfig);
#endif

#define TERM_COLS 91
#define TERM_ROWS 30

#ifndef FRUITJAM_NO_DISPLAY
// ---- VT100 escape sequence parser ----

// Parser states
enum TermParseState {
  TERM_NORMAL,
  TERM_ESC,        // Received ESC
  TERM_CSI,        // Received ESC [
  TERM_CSI_PARAM,  // Collecting CSI parameters
};

static TermParseState term_parse_state = TERM_NORMAL;
static int  term_csi_params[8];
static int  term_csi_param_count = 0;
static int  term_csi_current_param = 0;
static bool term_csi_has_param = false;

// Current text attributes
static uint8_t term_fg = TextColor::TEXT_WHITE;
static uint8_t term_bg = TextColor::BG_BLACK;
static uint8_t term_intensity = TextColor::ATTR_NORMAL_INTEN;

// Saved cursor position (for ESC 7 / ESC 8 or CSI s / CSI u)
static int term_saved_cx = 0;
static int term_saved_cy = 0;

static void term_update_color() {
  textDisplay.setColor(term_fg | term_bg | term_intensity);
}

// Map ANSI color code (30-37) to DVHSTXText foreground color
static uint8_t ansi_fg_to_textcolor(int code) {
  switch (code) {
    case 30: return TextColor::TEXT_BLACK;
    case 31: return TextColor::TEXT_RED;
    case 32: return TextColor::TEXT_GREEN;
    case 33: return TextColor::TEXT_YELLOW;
    case 34: return TextColor::TEXT_BLUE;
    case 35: return TextColor::TEXT_MAGENTA;
    case 36: return TextColor::TEXT_CYAN;
    case 37: return TextColor::TEXT_WHITE;
    default: return TextColor::TEXT_WHITE;
  }
}

// Map ANSI color code (40-47) to DVHSTXText background color
static uint8_t ansi_bg_to_textcolor(int code) {
  switch (code) {
    case 40: return TextColor::BG_BLACK;
    case 41: return TextColor::BG_RED;
    case 42: return TextColor::BG_GREEN;
    case 43: return TextColor::BG_YELLOW;
    case 44: return TextColor::BG_BLUE;
    case 45: return TextColor::BG_MAGENTA;
    case 46: return TextColor::BG_CYAN;
    case 47: return TextColor::BG_WHITE;
    default: return TextColor::BG_BLACK;
  }
}

// Process SGR (Select Graphic Rendition) — ESC [ ... m
static void term_process_sgr() {
  if (term_csi_param_count == 0) {
    // ESC[m with no params = reset
    term_fg = TextColor::TEXT_WHITE;
    term_bg = TextColor::BG_BLACK;
    term_intensity = TextColor::ATTR_NORMAL_INTEN;
  }
  for (int i = 0; i < term_csi_param_count; i++) {
    int p = term_csi_params[i];
    if (p == 0) {
      // Reset all attributes
      term_fg = TextColor::TEXT_WHITE;
      term_bg = TextColor::BG_BLACK;
      term_intensity = TextColor::ATTR_NORMAL_INTEN;
    } else if (p == 1) {
      // Bold / bright
      term_intensity = TextColor::ATTR_NORMAL_INTEN;
    } else if (p == 2) {
      // Dim
      term_intensity = TextColor::ATTR_LOW_INTEN;
    } else if (p == 7) {
      // Reverse video — swap fg/bg
      // Simple approach: swap the current fg and bg
      uint8_t tmp = term_fg;
      // Convert fg color index to bg and vice versa
      // fg colors are 0x00-0x07, bg colors are 0x08-0x78 (shifted)
      // This is a simplified swap
      term_fg = TextColor::TEXT_BLACK;
      term_bg = TextColor::BG_WHITE;
    } else if (p >= 30 && p <= 37) {
      term_fg = ansi_fg_to_textcolor(p);
    } else if (p >= 40 && p <= 47) {
      term_bg = ansi_bg_to_textcolor(p);
    }
  }
  term_update_color();
}

// Clear part of the screen
static void term_clear_region(int fromX, int fromY, int toX, int toY) {
  uint8_t savedAttr = term_fg | term_bg | term_intensity;
  textDisplay.setColor(savedAttr);

  // Clear character cells by writing spaces
  uint16_t *buf = textDisplay.getBuffer();
  if (!buf) return;

  for (int y = fromY; y <= toY && y < TERM_ROWS; y++) {
    int startX = (y == fromY) ? fromX : 0;
    int endX = (y == toY) ? toX : TERM_COLS - 1;
    for (int x = startX; x <= endX && x < TERM_COLS; x++) {
      buf[y * TERM_COLS + x] = ' ' | (savedAttr << 8);
    }
  }
}

// Process a completed CSI sequence
static void term_process_csi(char final_char) {
  // Default params
  int p0 = (term_csi_param_count > 0) ? term_csi_params[0] : 0;
  int p1 = (term_csi_param_count > 1) ? term_csi_params[1] : 0;

  int cx = textDisplay.getCursorX();
  int cy = textDisplay.getCursorY();

  switch (final_char) {
    case 'A': // Cursor Up
      {
        int n = (p0 > 0) ? p0 : 1;
        textDisplay.setCursor(cx, cy - n);
      }
      break;

    case 'B': // Cursor Down
      {
        int n = (p0 > 0) ? p0 : 1;
        textDisplay.setCursor(cx, cy + n);
      }
      break;

    case 'C': // Cursor Forward (Right)
      {
        int n = (p0 > 0) ? p0 : 1;
        textDisplay.setCursor(cx + n, cy);
      }
      break;

    case 'D': // Cursor Back (Left)
      {
        int n = (p0 > 0) ? p0 : 1;
        textDisplay.setCursor(cx - n, cy);
      }
      break;

    case 'H': // Cursor Position (row;col — 1-based)
    case 'f':
      {
        int row = (p0 > 0) ? p0 - 1 : 0;
        int col = (p1 > 0) ? p1 - 1 : 0;
        textDisplay.setCursor(col, row);
      }
      break;

    case 'J': // Erase in Display
      if (p0 == 0) {
        // Clear from cursor to end of screen
        term_clear_region(cx, cy, TERM_COLS - 1, TERM_ROWS - 1);
      } else if (p0 == 1) {
        // Clear from start to cursor
        term_clear_region(0, 0, cx, cy);
      } else if (p0 == 2) {
        // Clear entire screen
        textDisplay.clear();
      }
      break;

    case 'K': // Erase in Line
      if (p0 == 0) {
        // Clear from cursor to end of line
        term_clear_region(cx, cy, TERM_COLS - 1, cy);
      } else if (p0 == 1) {
        // Clear from start of line to cursor
        term_clear_region(0, cy, cx, cy);
      } else if (p0 == 2) {
        // Clear entire line
        term_clear_region(0, cy, TERM_COLS - 1, cy);
      }
      break;

    case 'm': // SGR - Select Graphic Rendition
      term_process_sgr();
      break;

    case 's': // Save cursor position
      term_saved_cx = cx;
      term_saved_cy = cy;
      break;

    case 'u': // Restore cursor position
      textDisplay.setCursor(term_saved_cx, term_saved_cy);
      break;

    case 'n': // Device Status Report
      // We just ignore these
      break;

    default:
      // Unknown CSI sequence — ignore
      break;
  }
}

// Feed a single character through the VT100 parser and render
static void term_putchar(char c) {
  switch (term_parse_state) {
    case TERM_NORMAL:
      if (c == '\033') {  // ESC
        term_parse_state = TERM_ESC;
      } else if (c == '\r') {
        textDisplay.setCursor(0, textDisplay.getCursorY());
      } else if (c == '\n') {
        textDisplay.write('\n');
      } else if (c == '\b' || c == 0x7F) {
        // Backspace — move cursor left
        int cx = textDisplay.getCursorX();
        int cy = textDisplay.getCursorY();
        if (cx > 0) {
          textDisplay.setCursor(cx - 1, cy);
        }
      } else if (c == '\a') {
        // Bell — ignore for now
      } else if (c == '\t') {
        // Tab — advance to next 8-column boundary
        int cx = textDisplay.getCursorX();
        int newcx = (cx + 8) & ~7;
        if (newcx >= TERM_COLS) newcx = TERM_COLS - 1;
        textDisplay.setCursor(newcx, textDisplay.getCursorY());
      } else if (c >= 32 || c < 0) {
        // Printable character
        textDisplay.write((uint8_t)c);
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
        // Save cursor
        term_saved_cx = textDisplay.getCursorX();
        term_saved_cy = textDisplay.getCursorY();
        term_parse_state = TERM_NORMAL;
      } else if (c == '8') {
        // Restore cursor
        textDisplay.setCursor(term_saved_cx, term_saved_cy);
        term_parse_state = TERM_NORMAL;
      } else if (c == 'c') {
        // Reset terminal
        textDisplay.clear();
        textDisplay.setCursor(0, 0);
        term_fg = TextColor::TEXT_WHITE;
        term_bg = TextColor::BG_BLACK;
        term_intensity = TextColor::ATTR_NORMAL_INTEN;
        term_update_color();
        term_parse_state = TERM_NORMAL;
      } else {
        // Unknown escape — ignore and go back to normal
        term_parse_state = TERM_NORMAL;
      }
      break;

    case TERM_CSI:
    case TERM_CSI_PARAM:
      if (c >= '0' && c <= '9') {
        // Accumulate parameter digit
        term_csi_current_param = term_csi_current_param * 10 + (c - '0');
        term_csi_has_param = true;
        term_parse_state = TERM_CSI_PARAM;
      } else if (c == ';') {
        // Parameter separator
        if (term_csi_param_count < 8) {
          term_csi_params[term_csi_param_count++] = term_csi_has_param ? term_csi_current_param : 0;
        }
        term_csi_current_param = 0;
        term_csi_has_param = false;
        term_parse_state = TERM_CSI_PARAM;
      } else if (c >= 0x40 && c <= 0x7E) {
        // Final character — end of CSI sequence
        if (term_csi_has_param && term_csi_param_count < 8) {
          term_csi_params[term_csi_param_count++] = term_csi_current_param;
        }
        term_process_csi(c);
        term_parse_state = TERM_NORMAL;
      } else {
        // Intermediate character or unknown — for now, ignore
        // Stay in CSI_PARAM to continue collecting
      }
      break;
  }
}

#endif // !FRUITJAM_NO_DISPLAY

// ---- Terminal initialization ----

// Declared in fruitjam_usbhost.h — signals core1 that clocks are stable
extern volatile bool fruitjam_clocks_ready;

static bool fruitjam_terminal_begin() {
  #ifndef FRUITJAM_NO_DISPLAY
  if (!textDisplay.begin()) {
    return false;
  }

  textDisplay.clear();
  textDisplay.setCursor(0, 0);
  textDisplay.showCursor();
  term_update_color();
  #endif

  // Signal core1 that clocks are reconfigured and stable
  fruitjam_clocks_ready = true;

  return true;
}

// ---- Print to terminal (+ mirror to serial) ----

static void fruitjam_pserial(char c) {
  #ifndef FRUITJAM_NO_DISPLAY
  // Write to VT100 terminal
  term_putchar(c);
  #endif

  // Mirror to serial for debugging
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
}

// ---- Line buffer for gserial() ----
// Accumulates typed characters, handles backspace locally,
// and submits to the reader on Enter. This is needed because
// without the lineeditor, gserial() returns raw chars to the
// uLisp reader which can't handle backspace/control chars.

#define LINEBUF_SIZE 256

static char  linebuf[LINEBUF_SIZE];
static int   linebuf_len = 0;    // chars in buffer (editing)
static int   linebuf_read = 0;   // read position (draining)
static bool  linebuf_ready = false; // true when Enter pressed, draining

// Get next char for gserial(). Returns -1 if nothing available yet.
// Handles line editing (backspace) and submits on Enter.
// Echo is always on — gserial() is only called for interactive input;
// file/library loading uses different gfun functions.
static int fruitjam_line_getchar(int raw_c) {
  // If we're draining a completed line, return next char
  if (linebuf_ready) {
    if (linebuf_read < linebuf_len) {
      return linebuf[linebuf_read++];
    }
    // Line fully consumed — return the newline and reset
    linebuf_ready = false;
    linebuf_len = 0;
    linebuf_read = 0;
    return '\n';
  }

  // No raw character available — nothing to do
  if (raw_c < 0) return -1;

  char c = (char)raw_c;

  if (c == '\n' || c == '\r') {
    // Submit the line
    fruitjam_pserial('\n');
    linebuf_ready = true;
    linebuf_read = 0;
    if (linebuf_len == 0) {
      // Empty line — just return newline immediately
      linebuf_ready = false;
      return '\n';
    }
    return linebuf[linebuf_read++];
  }

  if (c == '\b' || c == 0x7F) {
    // Backspace — remove last char
    if (linebuf_len > 0) {
      linebuf_len--;
      // Echo: move cursor left, overwrite with space, move left again
      fruitjam_pserial('\b');
      fruitjam_pserial(' ');
      fruitjam_pserial('\b');
    }
    return -1;  // consumed, nothing to return yet
  }

  if (c == 0x03) {
    // Ctrl-C — discard line, return newline to reset reader
    linebuf_len = 0;
    fruitjam_pserial('\n');
    return '\n';
  }

  if (c == 0x15) {
    // Ctrl-U — erase entire line
    while (linebuf_len > 0) {
      linebuf_len--;
      fruitjam_pserial('\b');
      fruitjam_pserial(' ');
      fruitjam_pserial('\b');
    }
    return -1;
  }

  // Ignore other control characters (except printable)
  if (c < 32 && c != '\t') {
    return -1;
  }

  // Printable character — add to buffer
  if (linebuf_len < LINEBUF_SIZE - 1) {
    linebuf[linebuf_len++] = c;
    fruitjam_pserial(c);
  }
  return -1;  // buffered, not ready to return yet
}

#endif // FRUITJAM_TERMINAL_H
