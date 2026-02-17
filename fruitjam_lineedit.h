// fruitjam_lineedit.h — Line editor for the Fruit Jam REPL
// Provides line buffer, command history, Tab autocomplete, parenthesis
// highlighting, and cursor movement for fruitjam_line_getchar().
// Consumes fruitjam_terminal.h (fruitjam_pserial, term_cx/cy, TERM_COLS/ROWS,
// line_scroll_count) and fruitjam_escape.h (escape_button_pressed).

#ifndef FRUITJAM_LINEEDIT_H
#define FRUITJAM_LINEEDIT_H

#include "fruitjam_terminal.h"
#include "fruitjam_escape.h"

// Special key codes from USB keyboard (>= 0x80)
// Must match KEY_CODE_* defines in fruitjam_usbhost.h
#define KEY_UP    0x80
#define KEY_DOWN  0x81
#define KEY_LEFT  0x82
#define KEY_RIGHT 0x83
#define KEY_HOME  0x84
#define KEY_END   0x85

#define LINEBUF_SIZE 256
static char  linebuf[LINEBUF_SIZE];
static int   linebuf_len = 0;
static int   linebuf_pos = 0;   // cursor position within linebuf (0..linebuf_len)
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
// Cycles through user-defined symbols (GlobalEnv) then built-in symbols.
// Phase tracking and GlobalEnv pointer live in .ino (need object* type).
static bool line_autocomplete_reset = true;
static int  line_ac_buf_index = 0;
static int  line_ac_match_len = 0;
static int  line_ac_last_extra = 0;    // chars added by last completion
static unsigned int line_ac_i = 0;     // scan position in built-in table

// Helper: erase n characters backward on screen (no linebuf change)
static void line_erase_back(int n) {
  for (int i = 0; i < n; i++) {
    fruitjam_pserial('\b');
    fruitjam_pserial(' ');
    fruitjam_pserial('\b');
  }
}

// Autocomplete: implemented in fruitjam_autocomplete.h (needs access to
// symbol table). Connected via function pointer — same pattern as the
// fruitjam_pre_draw_hook / fruitjam_peek_pixel_hook in terminal.h.
// Set by fruitjam_autocomplete.h when included from extensions.ino.
static void (*line_autocomplete_fn)() = NULL;
static void line_autocomplete() { if (line_autocomplete_fn) line_autocomplete_fn(); }

// ---- Parenthesis highlighting ----
// Uses the terminal's character grid and absolute cursor positioning to
// highlight the matching '(' in green when ')' is typed.
// Works correctly even when input wraps across multiple terminal lines.

// Saved cursor position from when line editing started (after the prompt)
static int  line_start_cx = 0;
static int  line_start_cy = 0;
// line_scroll_count declared in fruitjam_terminal.h (incremented by term_scroll_up)

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

// Helper: redraw linebuf from position `from` to end, then reposition cursor.
// Used after insert/delete in the middle of the line.
static void line_redraw_from(int from) {
  // Move terminal cursor to position `from` in the line
  int col, row;
  if (line_pos_for_index(from, &col, &row)) {
    line_vt100_goto(col, row);
  }
  // Redraw from `from` to end of buffer
  for (int i = from; i < linebuf_len; i++) {
    fruitjam_pserial(linebuf[i]);
  }
  // Erase one char beyond (covers deletion case)
  fruitjam_pserial(' ');
  // Move cursor back to linebuf_pos
  if (line_pos_for_index(linebuf_pos, &col, &row)) {
    line_vt100_goto(col, row);
  }
}

// Helper: insert a char at linebuf_pos and echo it
static void line_insert_char(char c) {
  if (linebuf_len >= LINEBUF_SIZE - 1) {
    Serial.write('\a');  // bell on serial terminal — buffer full
    if (fruitjam_bell_hook) fruitjam_bell_hook();  // trigger HDMI bell (audio + visual)
    return;
  }
  if (linebuf_pos == linebuf_len) {
    // Append at end — simple fast path
    linebuf[linebuf_len++] = c;
    linebuf_pos++;
    fruitjam_pserial(c);
  } else {
    // Insert in middle — shift tail right
    memmove(&linebuf[linebuf_pos + 1], &linebuf[linebuf_pos], linebuf_len - linebuf_pos);
    linebuf[linebuf_pos] = c;
    linebuf_len++;
    linebuf_pos++;
    line_redraw_from(linebuf_pos - 1);
  }
}

// Helper: clear the visible line and reset buffer (used by history, Ctrl-U)
static void line_clear_visible() {
  // Move cursor to end of line on screen, then erase backward
  int col, row;
  if (line_pos_for_index(linebuf_len, &col, &row)) {
    line_vt100_goto(col, row);
  }
  line_erase_back(linebuf_len);
  linebuf_len = 0;
  linebuf_pos = 0;
}

// Helper: replace line content with history entry and position cursor at end
static void line_load_history(int idx) {
  for (int i = 0; i < linebuf_hist_len[idx]; i++) {
    line_insert_char(linebuf_hist[idx][i]);
  }
}

static int fruitjam_line_getchar(int raw_c) {
  // Drain completed line first
  if (linebuf_ready) {
    if (linebuf_read < linebuf_len) return linebuf[linebuf_read++];
    linebuf_ready = false;
    linebuf_len = 0;
    linebuf_pos = 0;
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

  // Reset history browsing on any non-arrow/nav key
  if (uc != KEY_UP && uc != KEY_DOWN && uc != KEY_LEFT && uc != KEY_RIGHT
      && uc != KEY_HOME && uc != KEY_END) {
    hist_browse = -1;
  }

  // ---- Enter: submit line ----
  if (uc == '\n' || uc == '\r') {
    // Empty line: just return newline without repositioning cursor.
    // This avoids jumping back to a stale line_start position when
    // gserial() is called from non-REPL contexts (e.g. the tree editor).
    if (linebuf_len == 0) {
      fruitjam_pserial('\n');
      line_update_input_pos();  // resync start position to current cursor
      return '\n';
    }
    // Move cursor to end of line on screen before newline
    int col, row;
    if (line_pos_for_index(linebuf_len, &col, &row)) {
      line_vt100_goto(col, row);
    }
    linebuf_pos = linebuf_len;
    fruitjam_pserial('\n');
    // Accumulate this line into the command buffer (joined with spaces)
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
    linebuf_ready = true;
    linebuf_read = 0;
    line_autocomplete_reset = true;
    return linebuf[linebuf_read++];
  }

  // ---- Backspace / Delete ----
  if (uc == '\b' || uc == 0x7F) {
    if (linebuf_pos > 0) {
      linebuf_pos--;
      memmove(&linebuf[linebuf_pos], &linebuf[linebuf_pos + 1], linebuf_len - linebuf_pos - 1);
      linebuf_len--;
      if (linebuf_pos == linebuf_len) {
        // Cursor was at end — simple erase
        fruitjam_pserial('\b'); fruitjam_pserial(' '); fruitjam_pserial('\b');
      } else {
        // Deleted in middle — redraw from cursor
        line_redraw_from(linebuf_pos);
      }
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Ctrl-C: abort input (same as hardware escape button) ----
  if (uc == 0x03) {
    linebuf_len = 0;
    linebuf_pos = 0;
    linebuf_ready = false;
    linebuf_accum_len = 0;
    line_autocomplete_reset = true;
    fruitjam_pserial('\n');
    escape_button_pressed = true;  // triggers error2("escape!") on next testescape()
    return -1;  // return to input loop, which will call testescape() and longjmp out
  }

  // ---- Ctrl-U: erase line ----
  if (uc == 0x15) {
    line_clear_visible();
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Tab: autocomplete (only when cursor is at end of line) ----
  if (uc == '\t') {
    if (linebuf_len > 0 && linebuf_pos == linebuf_len) {
      line_autocomplete();
    }
    return -1;
  }

  // ---- Left arrow: move cursor left ----
  if (uc == KEY_LEFT) {
    if (linebuf_pos > 0) {
      linebuf_pos--;
      int col, row;
      if (line_pos_for_index(linebuf_pos, &col, &row)) {
        line_vt100_goto(col, row);
      }
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Right arrow: move cursor right ----
  if (uc == KEY_RIGHT) {
    if (linebuf_pos < linebuf_len) {
      linebuf_pos++;
      int col, row;
      if (line_pos_for_index(linebuf_pos, &col, &row)) {
        line_vt100_goto(col, row);
      }
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Home: move cursor to start of line ----
  if (uc == KEY_HOME) {
    if (linebuf_pos > 0) {
      linebuf_pos = 0;
      int col, row;
      if (line_pos_for_index(0, &col, &row)) {
        line_vt100_goto(col, row);
      }
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- End: move cursor to end of line ----
  if (uc == KEY_END) {
    if (linebuf_pos < linebuf_len) {
      linebuf_pos = linebuf_len;
      int col, row;
      if (line_pos_for_index(linebuf_len, &col, &row)) {
        line_vt100_goto(col, row);
      }
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Up arrow: browse history backward ----
  if (uc == KEY_UP) {
    if (hist_count > 0) {
      int next = (hist_browse < 0) ? 1 : hist_browse + 1;
      if (next <= hist_count) {
        line_clear_visible();
        hist_browse = next;
        int idx = (hist_head - hist_browse + HISTORY_SIZE) % HISTORY_SIZE;
        line_load_history(idx);
      }
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Down arrow: browse history forward (or clear line) ----
  if (uc == KEY_DOWN) {
    if (hist_browse > 1) {
      line_clear_visible();
      hist_browse--;
      int idx = (hist_head - hist_browse + HISTORY_SIZE) % HISTORY_SIZE;
      line_load_history(idx);
    } else if (hist_browse >= 0) {
      line_clear_visible();
      hist_browse = -1;
    }
    line_autocomplete_reset = true;
    return -1;
  }

  // ---- Ignore other special keys ----
  if (uc >= 0x80) return -1;

  // ---- Ignore other control chars ----
  if (uc < 32) return -1;

  // ---- Normal printable character ----
  // Resync start position when beginning a fresh input line, so that
  // cursor positioning (paren highlight, end-of-line goto on Enter) uses
  // the correct origin even if output was printed since the last line.
  if (linebuf_len == 0) line_update_input_pos();
  line_autocomplete_reset = true;
  line_insert_char((char)uc);

  // Check for parenthesis match after typing ')' at end of line
  if (uc == ')' && linebuf_pos == linebuf_len) {
    line_check_paren_match();
  }

  return -1;
}

// ---- Flush: reset all line editor state ----
// Called from gserial_flush() in the main .ino.
static void fruitjam_gserial_flush_impl () {
  linebuf_len = 0;
  linebuf_read = 0;
  linebuf_ready = false;
  line_autocomplete_reset = true;
  line_paren_idx = -1;
  hist_browse = -1;
}

#endif // FRUITJAM_LINEEDIT_H
