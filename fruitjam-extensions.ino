/*
  Fruit Jam Extensions File for uLisp
  Adds Fruit Jam-specific Lisp functions: graphics mode, audio, mouse,
  keyboard, buttons, NeoPixels, and screensaver.

  This is a standard uLisp Extensions File (see "Adding your own functions"
  on ulisp.com). It requires #define extensions in the main .ino file.
*/

// Impl functions — called from dispatches in the main .ino
// These replace large inline #if blocks with single function calls.

/*
  fruitjam_gserial_flush_impl - resets line editor state
  Called from gserial_flush() in the main .ino.
*/
void fruitjam_gserial_flush_impl () {
  linebuf_len = 0;
  linebuf_read = 0;
  linebuf_ready = false;
  line_autocomplete_reset = true;
  line_paren_idx = -1;
  hist_browse = -1;
}

/*
  fruitjam_gserial_impl - line-buffered input from USB keyboard or serial
  Called from gserial() in the main .ino.
  Reads from kbd ring buffer or Serial, feeds through the line editor,
  handles screensaver wake, cursor blink, and core1 health checks.
*/
int fruitjam_gserial_impl () {
  unsigned long start = millis();
  // Line-buffered input: accumulate chars, handle backspace, submit on Enter
  for (;;) {
    // Try to drain a completed line first
    int ch = fruitjam_line_getchar(-1);
    if (ch >= 0) return (char)ch;
    // Wait for raw input from keyboard or serial
    while (!kbd_available() && !Serial.available()) {
      if (millis() - start > 1000) clrflag(NOECHO);
      testescape();  // check button1 + serial escape
      #ifndef FRUITJAM_NO_DISPLAY
      screensaver_tick();  // check idle timeout, animate if active
      if (screensaver_active) {
        // Any hardware button wakes the screensaver
        if (digitalRead(PIN_BUTTON2) == LOW || digitalRead(PIN_BUTTON3) == LOW) {
          screensaver_poke();
          screensaver_wake();
        }
      } else {
        term_blink_cursor();  // animate cursor while waiting for input
      }
      #endif
      usbh_check_core1_health();  // auto-recover if core1 is stuck
    }
    // Wake screensaver on any input (consumes the keypress)
    screensaver_poke();
    if (screensaver_wake()) {
      // Screensaver was active — discard this keypress, go back to waiting
      if (kbd_available()) kbd_ring_get();
      else if (Serial.available()) Serial.read();
      continue;
    }
    int raw = kbd_available() ? (kbd_ring_get() & 0xFF) : Serial.read();
    ch = fruitjam_line_getchar(raw);
    if (ch >= 0) return (char)ch;
  }
}

/*
  fruitjam_gfxwrite_impl - renders a character using unscii-8-thin font
  Called from gfxwrite() in the main .ino.
  Reads cursor state from display8 and color/size/wrap from shadow variables.
  This replaces tft.write(c) which would use the Adafruit_GFX built-in 5×7 font.
*/
void fruitjam_gfxwrite_impl (char c) {
  if (!fruitjam_gfx_active) return;
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

/*
  fruitjam_testescape_impl - checks for escape (button1, serial ~) and periodic tasks
  Called from testescape() in the main .ino.
  On escape: exits graphics mode, silences audio, clears NeoPixels, raises error.
  Also runs mouse cursor update, audio buffer fill, and bell tick on each call.
  Includes the serial '~' escape check (500ms throttled) that upstream uses.
*/
void fruitjam_testescape_impl () {
  if (fruitjam_escape_check()) {
    // If screensaver is active, just wake it — don't trigger escape error
    #ifndef FRUITJAM_NO_DISPLAY
    if (screensaver_active) {
      screensaver_poke();
      screensaver_wake();
      return;
    }
    #endif
    if (fruitjam_gfx_active) fruitjam_exit_graphics();
    digitalWrite(29, HIGH);  // Turn off onboard LED (active-low on Fruit Jam)
    for (int i = 0; i < AUDIO_TOTAL_VOICES; i++) {
      audio_voices[i].volume = 0;
      audio_voices[i].phase_inc = 0;
      audio_voices[i].env.stage = ADSR_OFF;
      audio_voices[i].release_at_ms = 0;
    }
    // Clear any pending bell state and restore bell voice for next use
    bell_pending = false;
    if (bell_flash_active) term_bell_unflash();
    bell_voice_init();
    if (neopixel_initialized) {
      neopixel_clear();
      neopixel_show();
    }
    error2("escape!");
  }
  mouse_update_cursor();
  fruitjam_audio_fill();
  fruitjam_bell_tick();
  // Serial escape check (500ms throttled) — same as upstream
  static unsigned long n;
  if (millis()-n < 500) return;
  n = millis();
  if (Serial.available() && Serial.read() == '~') error2("escape!");
}

/*
  fruitjam_initgfx_impl - initializes all Fruit Jam peripherals and display
  Called from initgfx() in the main .ino.
  Starts HDMI terminal, escape button, buttons, WiFi SPI (for shared reset
  sequencing), audio, screensaver, and graphics subsystems.
*/
void fruitjam_initgfx_impl () {
  if (!fruitjam_terminal_begin()) {
    pinMode(LED_BUILTIN, OUTPUT);
    for (;;) digitalWrite(LED_BUILTIN, (millis() / 500) & 1);
  }
  fruitjam_escape_setup();
  pinMode(PIN_BUTTON2, INPUT_PULLUP);
  pinMode(PIN_BUTTON3, INPUT_PULLUP);
  #if defined(ULISP_WIFI)
  WiFi.setPins(SPIWIFI_SS, SPIWIFI_ACK, ESP32_RESETN, ESP32_GPIO0, &SPIWIFI);
  // Initialize WiFiNINA SPI now so that the ESP32-C6 reset pulse (which
  // toggles the shared GPIO22 peripheral reset, also resetting the TLV320
  // audio codec) happens BEFORE audio init.  SpiDrv::begin() sets its
  // internal initialized flag, so subsequent WiFi calls (wifi-connect etc.)
  // won't re-trigger the reset.  WiFi.init() is private, so call directly.
  SpiDrv::begin();
  #endif
  fruitjam_audio_init();
  screensaver_poke();  // initialize activity timer
  fruitjam_graphics_init(); // No-op — single HSTX instance, initialized on demand
}

// Definitions — Lisp-exposed functions

/*
  (graphics-mode)
  Switches the display to graphics mode (512x384 8bpp). Returns t on success, or signals an error on failure.
*/
object *fn_graphicsmode (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  if (!fruitjam_enter_graphics()) error2("graphics mode init failed");
  return tee;
  #else
  error2("not supported");
  return nil;
  #endif
}

/*
  (text-mode)
  Switches the display back to text mode (64x48 terminal). Returns t.
*/
object *fn_textmode (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  fruitjam_exit_graphics();
  return tee;
  #else
  error2("not supported");
  return nil;
  #endif
}

/*
  (mouse-x)
  Returns the current mouse cursor X position (0 to 511).
*/
object *fn_mousex (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  return number(mouse_x);
  #else
  return number(0);
  #endif
}

/*
  (mouse-y)
  Returns the current mouse cursor Y position (0 to 383).
*/
object *fn_mousey (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  return number(mouse_y);
  #else
  return number(0);
  #endif
}

/*
  (mouse-buttons)
  Returns the mouse button state as a bitmask: bit 0 = left, bit 1 = right, bit 2 = middle.
*/
object *fn_mousebuttons (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  return number(mouse_buttons);
  #else
  return number(0);
  #endif
}

/*
  (mouse-click)
  Returns t if a mouse button was clicked since last call, nil otherwise. Clears the flag.
*/
object *fn_mouseclick (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  if (mouse_clicked) {
    mouse_clicked = false;
    return tee;
  }
  #endif
  return nil;
}

/*
  (mouse-show)
  Shows the mouse cursor in graphics mode. Returns nil.
*/
object *fn_mouseshow (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  mouse_cursor_visible = true;
  #endif
  return nil;
}

/*
  (mouse-hide)
  Hides the mouse cursor. Returns nil.
*/
object *fn_mousehide (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  mouse_erase_cursor();
  mouse_cursor_visible = false;
  #endif
  return nil;
}

/*
  (audio-wave voice waveform)
  Sets the waveform for a voice (0-4).
  waveform can be: :sine, :square, :triangle, :sawtooth, :noise, :silence,
  or a 1D array of 256 integers (-128 to 127) for a custom wavetable.
*/
object *fn_audiowave (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  object *wf = second(args);
  if (integerp(wf)) {
    // Accept integer waveform IDs directly: 0=silence, 1=sine, 2=square, 3=triangle, 4=sawtooth, 5=noise
    int id = wf->integer;
    if (id < 0 || id > 5) error("waveform id 0-5", wf);
    audio_set_builtin_waveform(voice, (uint8_t)id);
  } else if (arrayp(wf)) {
    // Copy 256 values from uLisp array into C wavetable buffer
    int bit;
    audio_voice_t *v = &audio_voices[voice];
    for (int i = 0; i < 256; i++) {
      object *index = cons(number(i), NULL);
      object *val = *getarray(wf, index, NULL, &bit);
      int ival = checkinteger(val);
      if (ival < -128) ival = -128;
      if (ival > 127) ival = 127;
      v->wavetable[i] = (int8_t)ival;
    }
    v->waveform_id = AUDIO_WAVE_CUSTOM;
  } else {
    error("expected integer (0-5) or array", wf);
  }
  #endif
  return nil;
}

/*
  (audio-freq voice frequency)
  Sets the frequency in Hz for a voice (0-4). frequency can be integer or float.
*/
object *fn_audiofreq (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  float freq = checkintfloat(second(args));
  audio_set_freq(voice, freq);
  #endif
  return nil;
}

/*
  (audio-note voice midi-note [duration])
  Plays a MIDI note number on a voice (0-4). Middle C = 60, A4 = 69.
  Triggers the envelope if one is set.
  Optional duration in ms: time from note start until auto-release.
  With an envelope, starts the release phase; without, stops the oscillator.
*/
object *fn_audionote (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  int note = checkinteger(second(args));
  // MIDI note to Hz: freq = 440 * 2^((note - 69) / 12)
  float freq = 440.0f * powf(2.0f, (note - 69) / 12.0f);
  audio_set_freq(voice, freq);
  // Auto-trigger envelope if configured
  audio_trigger(voice);
  // Optional duration → schedule auto-release
  args = cddr(args);
  if (args != NULL) {
    int duration = checkinteger(first(args));
    if (duration > 0) {
      audio_voices[voice].release_at_ms = millis() + duration;
    } else {
      audio_voices[voice].release_at_ms = 0;
    }
  } else {
    audio_voices[voice].release_at_ms = 0;
  }
  #endif
  return nil;
}

/*
  (audio-vol voice level)
  Sets the volume for a voice (0-4). level is 0-255.
*/
object *fn_audiovol (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  int vol = (int)checkintfloat(second(args));
  if (vol < 0) vol = 0;
  if (vol > 255) vol = 255;
  audio_voices[voice].volume = (uint8_t)vol;
  #endif
  return nil;
}

/*
  (audio-master-vol level)
  Sets the master volume (0-255). Affects all voices.
*/
object *fn_audiomastervol (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int vol = (int)checkintfloat(first(args));
  if (vol < 0) vol = 0;
  if (vol > 255) vol = 255;
  audio_master_vol = (uint8_t)vol;
  #endif
  return nil;
}

/*
  (audio-stop voice)
  Stops a voice (0-4): sets volume to 0 and resets phase.
*/
object *fn_audiostop (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  audio_voices[voice].volume = 0;
  audio_voices[voice].phase = 0;
  audio_voices[voice].phase_inc = 0;
  audio_voices[voice].env.stage = ADSR_OFF;
  audio_voices[voice].env.env_level = 0;
  audio_voices[voice].release_at_ms = 0;
  #endif
  return nil;
}

/*
  (audio-stop-all)
  Stops all voices: sets all volumes to 0 and resets phase.
*/
object *fn_audiostopall (object *args, object *env) {
  (void) args, (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  for (int i = 0; i < AUDIO_NUM_VOICES; i++) {
    audio_voices[i].volume = 0;
    audio_voices[i].phase = 0;
    audio_voices[i].phase_inc = 0;
    audio_voices[i].env.stage = ADSR_OFF;
    audio_voices[i].env.env_level = 0;
    audio_voices[i].release_at_ms = 0;
  }
  #endif
  return nil;
}

/*
  (audio-playing voice)
  Returns t if a voice (0-4) is active (has non-zero volume and frequency).
*/
object *fn_audioplaying (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  audio_voice_t *av = &audio_voices[voice];
  if (av->phase_inc > 0 && av->volume > 0) {
    // If envelope is enabled, check it's not done
    if (av->env.enabled && av->env.stage == ADSR_DONE) return nil;
    return tee;
  }
  #endif
  return nil;
}

/*
  (audio-envelope voice attack decay sustain release)
  Sets ADSR envelope for voice (0-4). attack, decay, release in ms.
  sustain is level 0-255. Pass nil as attack to clear the envelope.
*/
object *fn_audioenvelope (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  object *atk = second(args);
  if (atk == nil) {
    audio_clear_envelope(voice);
  } else {
    int a = checkinteger(atk);
    object *rest = cddr(args);            // skip voice, attack
    int d = checkinteger(car(rest));
    int s = checkinteger(car(cdr(rest)));
    int r = checkinteger(car(cdr(cdr(rest))));
    if (s < 0) s = 0; if (s > 255) s = 255;
    if (a < 0) a = 0; if (d < 0) d = 0; if (r < 0) r = 0;
    audio_set_envelope(voice, (uint32_t)a, (uint32_t)d, (uint8_t)s, (uint32_t)r);
  }
  #endif
  return nil;
}

/*
  (audio-trigger voice)
  Triggers the attack phase of the envelope on voice (0-4).
  Called automatically by audio-note if an envelope is set.
*/
object *fn_audiotrigger (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  audio_trigger(voice);
  #endif
  return nil;
}

/*
  (audio-release voice)
  Starts the release phase of the envelope on voice (0-4).
  Voice will fade to silence based on the release time.
*/
object *fn_audiorelease (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int voice = checkinteger(first(args));
  if (voice < 0 || voice >= AUDIO_NUM_VOICES) error("voice out of range", first(args));
  audio_release(voice);
  audio_voices[voice].release_at_ms = 0;
  #endif
  return nil;
}

/*
  (audio-output mode)
  Sets audio output routing. mode: 0=auto (switch on headphone detect),
  1=speaker, 2=headphone, 3=both. Returns nil.
*/
object *fn_audiooutput (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int mode = checkinteger(first(args));
  if (mode < 0 || mode > 3) error("mode 0-3", first(args));
  audio_output_mode = (uint8_t)mode;
  audio_apply_output_routing();
  #endif
  return nil;
}

object *fn_button (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int n = checkinteger(first(args));
  uint8_t pin;
  switch (n) {
    case 1: pin = PIN_BUTTON1; break;
    case 2: pin = PIN_BUTTON2; break;
    case 3: pin = PIN_BUTTON3; break;
    default: error("button 1-3", first(args)); return nil;
  }
  return digitalRead(pin) == LOW ? tee : nil;
  #else
  return nil;
  #endif
}

object *fn_keyboard (object *args, object *env) {
  (void) args; (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int ch = kbd_ring_get();
  if (ch < 0) return nil;
  return number(ch);
  #else
  return nil;
  #endif
}

object *fn_waitkeyboard (object *args, object *env) {
  (void) args; (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  while (!kbd_available()) {
    testescape();
    usbh_check_core1_health();
  }
  return number(kbd_ring_get());
  #else
  return nil;
  #endif
}

object *fn_keyboardflush (object *args, object *env) {
  (void) args; (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  kbd_ring_tail = kbd_ring_head;
  #endif
  return nil;
}

object *fn_pixelsbegin (object *args, object *env) {
  (void) args; (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  neopixel_init();
  #endif
  return nil;
}

object *fn_pixelsclear (object *args, object *env) {
  (void) args; (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  neopixel_clear();
  #endif
  return nil;
}

object *fn_pixelsfill (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  uint32_t color = 0;
  int first_pixel = 0, count = NEOPIXEL_COUNT;
  if (args != NULL) {
    color = checkinteger(first(args));
    args = cdr(args);
    if (args != NULL) {
      first_pixel = checkinteger(first(args));
      args = cdr(args);
      if (args != NULL) {
        count = checkinteger(first(args));
      }
    }
  }
  neopixel_fill(color, first_pixel, count);
  #endif
  return nil;
}

object *fn_pixelssetpixelcolor (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int nargs = listlength(args);
  int i = checkinteger(first(args));
  if (nargs == 2) {
    neopixel_set_pixel(i, checkinteger(second(args)));
  } else {
    args = cdr(args);
    int r = checkinteger(first(args));
    int g = checkinteger(second(args));
    int b = checkinteger(third(args));
    neopixel_set_pixel_rgb(i, r, g, b);
  }
  #endif
  return nil;
}

object *fn_pixelscolor (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int r = checkinteger(first(args));
  int g = checkinteger(second(args));
  int b = checkinteger(third(args));
  int w = 0;
  args = cddr(cdr(args));
  if (args != NULL) w = checkinteger(first(args));
  return number(neopixel_color(r, g, b, w));
  #else
  return number(0);
  #endif
}

object *fn_pixelscolorhsv (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  int hue = checkinteger(first(args));
  int sat = checkinteger(second(args));
  int val = checkinteger(third(args));
  return number(neopixel_color_hsv(hue, sat, val));
  #else
  return number(0);
  #endif
}

object *fn_pixelsshow (object *args, object *env) {
  (void) args; (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  neopixel_show();
  #endif
  return nil;
}

object *fn_pixelsrainbow (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  uint16_t first_hue = 0;
  int cycles = 1;
  uint8_t sat = 255, val = 255;
  bool gammify = true;
  if (args != NULL) {
    first_hue = checkinteger(first(args));
    args = cdr(args);
    if (args != NULL) {
      cycles = checkinteger(first(args));
      args = cdr(args);
      if (args != NULL) {
        sat = checkinteger(first(args));
        args = cdr(args);
        if (args != NULL) {
          val = checkinteger(first(args));
          args = cdr(args);
          if (args != NULL) {
            gammify = (first(args) != nil);
          }
        }
      }
    }
  }
  neopixel_rainbow(first_hue, cycles, sat, val, gammify);
  #endif
  return nil;
}

object *fn_setscreensaver (object *args, object *env) {
  (void) env;
  #if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
  if (args == NULL) {
    // No args: return current timeout in seconds
    return number(screensaver_timeout_ms / 1000);
  }
  int secs = checkinteger(first(args));
  if (secs < 0) error2("timeout must be >= 0");
  screensaver_timeout_ms = (uint32_t)secs * 1000UL;
  screensaver_poke();  // reset timer when changing timeout
  #endif
  return nil;
}

// Symbol names
const char stringGraphicsMode[] = "graphics-mode";
const char stringTextMode[] = "text-mode";
const char stringMouseX[] = "mouse-x";
const char stringMouseY[] = "mouse-y";
const char stringMouseButtons[] = "mouse-buttons";
const char stringMouseClick[] = "mouse-click";
const char stringMouseShow[] = "mouse-show";
const char stringMouseHide[] = "mouse-hide";
const char stringAudioWave[] = "audio-wave";
const char stringAudioFreq[] = "audio-freq";
const char stringAudioNote[] = "audio-note";
const char stringAudioVol[] = "audio-vol";
const char stringAudioMasterVol[] = "audio-master-vol";
const char stringAudioStop[] = "audio-stop";
const char stringAudioStopAll[] = "audio-stop-all";
const char stringAudioPlaying[] = "audio-playing";
const char stringAudioEnvelope[] = "audio-envelope";
const char stringAudioTrigger[] = "audio-trigger";
const char stringAudioRelease[] = "audio-release";
const char stringAudioOutput[] = "audio-output";
const char stringButton[] = "button";
const char stringKeyboard[] = "keyboard";
const char stringWaitKeyboard[] = "wait-keyboard";
const char stringKeyboardFlush[] = "keyboard-flush";
const char stringPixelsBegin[] = "pixels-begin";
const char stringPixelsClear[] = "pixels-clear";
const char stringPixelsFill[] = "pixels-fill";
const char stringPixelsSetPixelColor[] = "pixels-set-pixel-color";
const char stringPixelsColor[] = "pixels-color";
const char stringPixelsColorHSV[] = "pixels-color-hsv";
const char stringPixelsShow[] = "pixels-show";
const char stringPixelsRainbow[] = "pixels-rainbow";
const char stringSetScreensaver[] = "set-screensaver";

// Documentation strings
const char docGraphicsMode[] = "(graphics-mode)\n"
"Switches the display to graphics mode (512x384, 256 colours). Returns t on success.";
const char docTextMode[] = "(text-mode)\n"
"Switches the display back to text mode (64x48 terminal). Returns t.";
const char docMouseX[] = "(mouse-x)\n"
"Returns the current mouse cursor X position (0 to 511).";
const char docMouseY[] = "(mouse-y)\n"
"Returns the current mouse cursor Y position (0 to 383).";
const char docMouseButtons[] = "(mouse-buttons)\n"
"Returns the mouse button state as a bitmask: bit 0 = left, bit 1 = right, bit 2 = middle.";
const char docMouseClick[] = "(mouse-click)\n"
"Returns t if a mouse button was clicked since last call, nil otherwise. Clears the flag.";
const char docMouseShow[] = "(mouse-show)\n"
"Shows the mouse cursor in graphics mode. Returns nil.";
const char docMouseHide[] = "(mouse-hide)\n"
"Hides the mouse cursor. Returns nil.";
const char docAudioWave[] = "(audio-wave voice waveform)\n"
"Sets the waveform for voice (0-4). waveform: 0=silence, 1=sine, 2=square,\n"
"3=triangle, 4=sawtooth, 5=noise, or a 256-element array of values -128 to 127.";
const char docAudioFreq[] = "(audio-freq voice frequency)\n"
"Sets the frequency in Hz for voice (0-4). Accepts integer or float.";
const char docAudioNote[] = "(audio-note voice midi-note [duration])\n"
"Plays a MIDI note on voice (0-4). Middle C = 60, A4 = 69.\n"
"Optional duration: ms from note start until auto-release.";
const char docAudioVol[] = "(audio-vol voice level)\n"
"Sets volume for voice (0-4). level is 0-255.";
const char docAudioMasterVol[] = "(audio-master-vol level)\n"
"Sets the master volume (0-255) affecting all voices.";
const char docAudioStop[] = "(audio-stop voice)\n"
"Stops voice (0-4): sets volume to 0 and resets phase.";
const char docAudioStopAll[] = "(audio-stop-all)\n"
"Stops all voices.";
const char docAudioPlaying[] = "(audio-playing voice)\n"
"Returns t if voice (0-4) is active (non-zero volume and frequency).";
const char docAudioEnvelope[] = "(audio-envelope voice attack decay sustain release)\n"
"Sets ADSR envelope for voice (0-4). attack/decay/release in ms, sustain 0-255.\n"
"Pass nil as attack to clear the envelope.";
const char docAudioTrigger[] = "(audio-trigger voice)\n"
"Triggers the attack phase of the envelope on voice (0-4).";
const char docAudioRelease[] = "(audio-release voice)\n"
"Starts the release phase on voice (0-4), fading to silence.";
const char docAudioOutput[] = "(audio-output mode)\n"
"Sets output routing: 0=auto (headphone detect), 1=speaker, 2=headphone, 3=both.";
const char docButton[] = "(button n)\n"
"Returns t if button n (1-3) is currently pressed, or nil if not.\n"
"Note: button 1 is the escape button. Its interrupt fires on press,\n"
"so polling it in a loop will trigger an escape before the result\n"
"can be used. Use buttons 2 and 3 for interactive input.";
const char docKeyboard[] = "(keyboard)\n"
"Returns the next key from the keyboard buffer as an integer, or nil if no key\n"
"is available. Non-blocking. The result encodes both the key and modifier state:\n"
"  (key-code k) = key code (ASCII or *key-...* constant)\n"
"  (key-mod k) = modifier bitmask\n"
"Test modifiers with (> (logand (key-mod k) *mod-ctrl*) 0) etc.";
const char docWaitKeyboard[] = "(wait-keyboard)\n"
"Waits for a keypress and returns it as an integer. Blocks until a key is\n"
"available. Supports escape (button 1) to abort. Returns the same encoding\n"
"as (keyboard): low byte = key code, high byte = modifiers.";
const char docKeyboardFlush[] = "(keyboard-flush)\n"
"Discards all pending keys in the keyboard buffer. Useful when entering a\n"
"game loop or switching modes to prevent stale keystrokes from leaking in.";
const char docPixelsBegin[] = "(pixels-begin)\n"
"Configures the NeoPixel pin for output. Called automatically at boot.";
const char docPixelsClear[] = "(pixels-clear)\n"
"Sets all pixel colors to off in the buffer. Call (pixels-show) to update.";
const char docPixelsFill[] = "(pixels-fill [color] [first] [count])\n"
"Fills all or part of the NeoPixel strip with a packed 32-bit color (default 0).\n"
"first, default 0, the first NeoPixel to fill.\n"
"count, default all, the number of NeoPixels to fill.";
const char docPixelsSetPixelColor[] = "(pixels-set-pixel-color index color)\n"
"(pixels-set-pixel-color index red green blue)\n"
"Sets a pixel's color using either a packed 32-bit RGB value,\n"
"or separate red, green, blue components (0-255 each).";
const char docPixelsColor[] = "(pixels-color red green blue [white])\n"
"Packs separate red, green, blue, and optional white values (0-255)\n"
"into a single 32-bit color value.";
const char docPixelsColorHSV[] = "(pixels-color-hsv hue sat val)\n"
"Converts hue (0-65535), saturation (0-255), and value (0-255)\n"
"into a packed 32-bit RGB color.";
const char docPixelsShow[] = "(pixels-show)\n"
"Transmits the pixel data buffer to the NeoPixels.";
const char docPixelsRainbow[] = "(pixels-rainbow [first-hue] [cycles] [sat] [val] [gammify])\n"
"Fills the NeoPixel strip with one or more cycles of hues.\n"
"first-hue (0-65535), cycles (default 1), sat (0-255, default 255),\n"
"val (0-255, default 255), gammify (default t). Call (pixels-show) to update.";
const char docSetScreensaver[] = "(set-screensaver [timeout])\n"
"Sets the screensaver idle timeout in seconds. 0 disables the screensaver.\n"
"With no argument, returns the current timeout in seconds.\n"
"Default is 300 (5 minutes). The screensaver blanks the screen and\n"
"breathes the NeoPixels if they are off. Any keypress wakes.";

// Symbol lookup table
const tbl_entry_t lookup_table2[] = {
  { stringGraphicsMode, fn_graphicsmode, 0200, docGraphicsMode },
  { stringTextMode, fn_textmode, 0200, docTextMode },
  { stringMouseX, fn_mousex, 0200, docMouseX },
  { stringMouseY, fn_mousey, 0200, docMouseY },
  { stringMouseButtons, fn_mousebuttons, 0200, docMouseButtons },
  { stringMouseClick, fn_mouseclick, 0200, docMouseClick },
  { stringMouseShow, fn_mouseshow, 0200, docMouseShow },
  { stringMouseHide, fn_mousehide, 0200, docMouseHide },
  { stringAudioWave, fn_audiowave, 0222, docAudioWave },
  { stringAudioFreq, fn_audiofreq, 0222, docAudioFreq },
  { stringAudioNote, fn_audionote, 0223, docAudioNote },
  { stringAudioVol, fn_audiovol, 0222, docAudioVol },
  { stringAudioMasterVol, fn_audiomastervol, 0211, docAudioMasterVol },
  { stringAudioStop, fn_audiostop, 0211, docAudioStop },
  { stringAudioStopAll, fn_audiostopall, 0200, docAudioStopAll },
  { stringAudioPlaying, fn_audioplaying, 0211, docAudioPlaying },
  { stringAudioEnvelope, fn_audioenvelope, 0225, docAudioEnvelope },
  { stringAudioTrigger, fn_audiotrigger, 0211, docAudioTrigger },
  { stringAudioRelease, fn_audiorelease, 0211, docAudioRelease },
  { stringAudioOutput, fn_audiooutput, 0211, docAudioOutput },
  { stringButton, fn_button, 0211, docButton },
  { stringKeyboard, fn_keyboard, 0200, docKeyboard },
  { stringWaitKeyboard, fn_waitkeyboard, 0200, docWaitKeyboard },
  { stringKeyboardFlush, fn_keyboardflush, 0200, docKeyboardFlush },
  { stringPixelsBegin, fn_pixelsbegin, 0200, docPixelsBegin },
  { stringPixelsClear, fn_pixelsclear, 0200, docPixelsClear },
  { stringPixelsFill, fn_pixelsfill, 0203, docPixelsFill },
  { stringPixelsSetPixelColor, fn_pixelssetpixelcolor, 0224, docPixelsSetPixelColor },
  { stringPixelsColor, fn_pixelscolor, 0234, docPixelsColor },
  { stringPixelsColorHSV, fn_pixelscolorhsv, 0233, docPixelsColorHSV },
  { stringPixelsShow, fn_pixelsshow, 0200, docPixelsShow },
  { stringPixelsRainbow, fn_pixelsrainbow, 0205, docPixelsRainbow },
  { stringSetScreensaver, fn_setscreensaver, 0201, docSetScreensaver },
};

// Table cross-reference functions - do not edit below this line

tbl_entry_t *tables[] = {lookup_table, lookup_table2};
const unsigned int tablesizes[] = { arraysize(lookup_table), arraysize(lookup_table2) };

const tbl_entry_t *table (int n) {
  return tables[n];
}

unsigned int tablesize (int n) {
  return tablesizes[n];
}

// Fruit Jam Tab autocomplete
// Cycles through matching symbols on each Tab press.
// Phase 0: user-defined symbols from GlobalEnv
// Phase 1: built-in symbols from lookup tables
// Called from fruitjam_line_getchar() on Tab key.
#if defined(ARDUINO_ADAFRUIT_FRUITJAM_RP2350)
#include "fruitjam_autocomplete.h"
#endif
