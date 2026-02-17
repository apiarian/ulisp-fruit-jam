// fruitjam_autocomplete.h — Tab autocomplete for the Fruit Jam line editor
//
// Cycles through matching symbols on each Tab press.
// Phase 0: user-defined symbols from GlobalEnv
// Phase 1: built-in symbols from lookup tables
// Called from fruitjam_line_getchar() on Tab key via function pointer.
//
// This file is #included from fruitjam-extensions.ino AFTER the table
// cross-reference functions (tablesize, table) are defined, because it
// needs them, plus object*, GlobalEnv, car/cdr, etc.
//
// Wires up line_autocomplete_fn (declared in fruitjam_lineedit.h) via a
// static constructor, so that lineedit can call autocomplete without a
// cross-file forward declaration.
//
// The caller is responsible for the #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
// guard around the #include.

// Additional autocomplete state (needs object* so lives here, not in .h)
static int     line_ac_phase = 0;         // 0 = GlobalEnv, 1 = builtins
static object *line_ac_globals_ptr = NULL; // current position in GlobalEnv walk
static bool    line_ac_looped = false;     // true after wrapping from builtins back to GlobalEnv
static int     line_ac_bi_remaining = 0;   // builtin entries left to check before exhausted

/*
  symbol_to_cstr - writes a symbol's name into a C string buffer.
  Handles radix-40 short symbols, long (linked-cell) symbols, and builtins.
  Returns buf. Result is lowercase (matching how uLisp prints symbols).
*/
static char *symbol_to_cstr (object *sym, char *buf, int buflen) {
  int idx = 0;
  symbol_t name = sym->name;
  if (longnamep(name)) {
    // Long symbol: name is a pointer to linked character cells
    object *form = (object *)name;
    while (form != NULL && idx < buflen - 1) {
      int chars = form->chars;
      for (int i = (sizeof(int)-1)*8; i >= 0 && idx < buflen - 1; i -= 8) {
        char ch = (chars >> i) & 0xFF;
        if (ch) buf[idx++] = ch;
      }
      form = car(form);
    }
  } else {
    uint32_t value = untwist(name);
    if (value >= BUILTINS) {
      // Built-in symbol name (shouldn't normally appear in GlobalEnv, but handle it)
      builtin_t bn = (builtin_t)(value - BUILTINS);
      bool n = bn < tablesize(0);
      const char *k = table(n ? 0 : 1)[n ? bn : bn - tablesize(0)].string;
      while (*k && idx < buflen - 1) buf[idx++] = *k++;
    } else if (value >= PACKEDS) {
      // Radix-40 encoded short symbol
      uint32_t x = value;
      for (int d = 102400000; d > 0; d /= 40) {
        uint32_t j = x / d;
        char c = fromradix40(j);
        if (c == 0) break;
        if (idx < buflen - 1) buf[idx++] = c;
        x -= j * d;
      }
    }
  }
  buf[idx] = '\0';
  return buf;
}

/*
  line_ac_try_match - checks if candidate string matches the prefix in linebuf.
  If so, appends the remaining characters and returns true.
*/
static bool line_ac_try_match (const char *candidate) {
  if (strncmp(candidate, &linebuf[line_ac_buf_index], line_ac_match_len) == 0) {
    int klen = strlen(candidate);
    int before = linebuf_len;
    for (int j = line_ac_match_len; j < klen; j++) {
      line_insert_char(candidate[j]);
    }
    line_ac_last_extra = linebuf_len - before;
    return true;
  }
  return false;
}

/*
  line_ac_scan_globals - scan GlobalEnv from line_ac_globals_ptr for the next match.
  Advances line_ac_globals_ptr past the match. Returns true if a match was found.
*/
static bool line_ac_scan_globals () {
  char symbuf[33];
  while (line_ac_globals_ptr != NULL) {
    object *pair = first(line_ac_globals_ptr);
    object *var = car(pair);
    line_ac_globals_ptr = cdr(line_ac_globals_ptr);
    symbol_to_cstr(var, symbuf, sizeof(symbuf));
    if (line_ac_try_match(symbuf)) return true;
  }
  return false;
}

/*
  line_ac_scan_builtins - scan built-in tables from line_ac_i for the next match.
  Advances line_ac_i past the match. Uses line_ac_bi_remaining to track how many
  entries are left before the table is considered exhausted.
  Returns true if a match was found.
*/
static bool line_ac_scan_builtins () {
  int entries = tablesize(0) + tablesize(1);
  while (line_ac_bi_remaining > 0) {
    bool n = line_ac_i < (unsigned int)tablesize(0);
    const char *k = table(n ? 0 : 1)[n ? line_ac_i : line_ac_i - tablesize(0)].string;
    line_ac_i = (line_ac_i + 1) % entries;
    line_ac_bi_remaining--;
    if (line_ac_try_match(k)) return true;
  }
  return false;
}

static void line_autocomplete_impl () {
  if (line_autocomplete_reset) {
    line_ac_phase = 0;
    line_ac_i = 0;
    line_ac_last_extra = 0;
    line_autocomplete_reset = false;
    line_ac_buf_index = linebuf_len;
    line_ac_match_len = 0;
    line_ac_globals_ptr = GlobalEnv;
    line_ac_looped = false;

    // Scan backward to find start of current word
    for (int m = 0; m < 32 && m < linebuf_len; m++) {
      int pos = linebuf_len - 1 - m;
      char ch = linebuf[pos];
      if (ch == ' ' || ch == '(' || ch == '\n') {
        if (m > 0) {
          line_ac_buf_index = pos + 1;
          line_ac_match_len = m;
        }
        break;
      }
      if (pos == 0) {
        line_ac_buf_index = 0;
        line_ac_match_len = m + 1;
        break;
      }
    }
  }

  if (line_ac_match_len <= 0) return;

  // Erase the extra chars from the previous completion
  for (int n = 0; n < line_ac_last_extra; n++) {
    if (linebuf_len > 0) { linebuf_len--; linebuf_pos--; }
    fruitjam_pserial('\b'); fruitjam_pserial(' '); fruitjam_pserial('\b');
  }
  line_ac_last_extra = 0;

  // Scan from the current position through both phases, wrapping around once.
  // line_ac_looped is set when we wrap and cleared when we find a match,
  // so that a full fruitless cycle stops instead of looping forever.

  for (int attempt = 0; attempt < 2; attempt++) {
    // Phase 0: user-defined symbols
    if (line_ac_phase == 0) {
      if (line_ac_scan_globals()) { line_ac_looped = false; return; }
      // Exhausted GlobalEnv, move to builtins
      line_ac_phase = 1;
      line_ac_i = 0;
      line_ac_bi_remaining = tablesize(0) + tablesize(1);
    }

    // Phase 1: built-in symbols
    if (line_ac_scan_builtins()) { line_ac_looped = false; return; }

    // Exhausted builtins — wrap back to GlobalEnv
    if (line_ac_looped) return;  // Already wrapped with no match; give up
    line_ac_looped = true;
    line_ac_phase = 0;
    line_ac_globals_ptr = GlobalEnv;
  }
}

// Wire up the function pointer so fruitjam_lineedit.h can call autocomplete
// without a forward declaration.
static struct _line_ac_init {
  _line_ac_init() { line_autocomplete_fn = line_autocomplete_impl; }
} _line_ac_init_instance;
