// fruitjam_escape.h — Hardware escape button for Fruit Jam
// BUTTON1 (GPIO0) as GPIO interrupt to abort running Lisp programs
// Sets the uLisp ESCAPE flag, checked by the eval loop
//
// Short press: triggers Lisp escape (abort running program)
// Long press (>=1s): also triggers USB host power-cycle recovery

#ifndef FRUITJAM_ESCAPE_H
#define FRUITJAM_ESCAPE_H

// Forward declaration — ESCAPE flag enum value is defined in the .ino
// We access it via setflag() macro which is also defined there

static volatile bool     escape_button_pressed = false;
static volatile uint32_t escape_button_press_time = 0;  // millis() when button went down

#define ESCAPE_LONG_PRESS_MS 1000  // hold >=1s to trigger USB reset

static void fruitjam_escape_isr() {
  escape_button_pressed = true;
  escape_button_press_time = millis();
  // USB reset is NOT triggered here — it requires a long press (>=1s).
  // Long-press detection runs in loop1() in the .ino, which polls the GPIO.
}

static void fruitjam_escape_setup() {
  pinMode(PIN_BUTTON1, INPUT_PULLUP);
  attachInterrupt(digitalPinToInterrupt(PIN_BUTTON1), fruitjam_escape_isr, FALLING);
}

// Call this from testescape() to check the button flag
// Returns true if the escape button was pressed (and clears the flag)
static bool fruitjam_escape_check() {
  if (escape_button_pressed) {
    escape_button_pressed = false;
    return true;
  }
  return false;
}

#endif // FRUITJAM_ESCAPE_H
