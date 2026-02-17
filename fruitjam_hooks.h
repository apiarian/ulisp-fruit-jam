// fruitjam_hooks.h — Hook implementations for uLisp I/O dispatch points
//
// Contains the Fruit Jam implementations of testescape, gserial, and initgfx.
// These functions depend on uLisp core symbols (clrflag, error2, testescape)
// that are defined in the .ino after the board config block, so this header
// must be #included later in the .ino — after testescape() is defined but
// before gserial() / initgfx().
//
// The other two impl functions (fruitjam_gfxwrite_impl and
// fruitjam_gserial_flush_impl) have no .ino dependencies and live in
// fruitjam_graphics.h and fruitjam_lineedit.h respectively.

#ifndef FRUITJAM_HOOKS_H
#define FRUITJAM_HOOKS_H

/*
  fruitjam_testescape_impl - checks for escape (button1, serial ~) and periodic tasks
  Called from testescape() in the main .ino.
  On escape: exits graphics mode, silences audio, clears NeoPixels, raises error.
  Also runs mouse cursor update, audio buffer fill, and bell tick on each call.
  Includes the serial '~' escape check (500ms throttled) that upstream uses.
*/
static void fruitjam_testescape_impl () {
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
    fruitjam_audio_silence(true);
    fruitjam_bell_cancel();
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
  fruitjam_gserial_impl - line-buffered input from USB keyboard or serial
  Called from gserial() in the main .ino.
  Reads from kbd ring buffer or Serial, feeds through the line editor,
  handles screensaver wake, cursor blink, and core1 health checks.
*/
static int fruitjam_gserial_impl () {
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
      screensaver_tick();  // check idle timeout, animate if active, button wake
      if (!screensaver_active) {
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
  fruitjam_initgfx_impl - initializes all Fruit Jam peripherals and display
  Called from initgfx() in the main .ino.
  Starts HDMI terminal, escape button, buttons, WiFi SPI (for shared reset
  sequencing), audio, screensaver, and graphics subsystems.
*/
static void fruitjam_initgfx_impl () {
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
  fruitjam_bell_init();
  screensaver_poke();  // initialize activity timer
  fruitjam_graphics_init(); // Wires up pre-draw hook for mouse cursor hiding
}

#endif // FRUITJAM_HOOKS_H
