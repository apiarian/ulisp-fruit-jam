// fruitjam_escape.h — Hardware escape button for Fruit Jam
// BUTTON1 (GPIO0) as GPIO interrupt to abort running Lisp programs
// Sets the uLisp ESCAPE flag, checked by the eval loop

#ifndef FRUITJAM_ESCAPE_H
#define FRUITJAM_ESCAPE_H

// Forward declaration — ESCAPE flag enum value is defined in the .ino
// We access it via setflag() macro which is also defined there

static volatile bool escape_button_pressed = false;

static void fruitjam_escape_isr() {
  escape_button_pressed = true;
  // Request USB recovery via flag. If core1 is alive, it picks this up
  // in loop1() and does a normal power-cycle. If core1 is stuck, the
  // heartbeat auto-recovery in gserial()'s wait loop (mechanism 5) will
  // trigger the full emergency PIO unstick after 5s — no need to do it
  // here, where it would corrupt working PIO USB state on normal presses.
  usbh_manual_reset_requested = true;
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
