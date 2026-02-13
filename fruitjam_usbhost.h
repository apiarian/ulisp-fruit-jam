// fruitjam_usbhost.h — USB Host keyboard support for Fruit Jam
// Runs TinyUSB host via PIO USB on core1 (setup1/loop1)
// Provides a ring buffer of ASCII characters for gserial() on core0
//
// USB recovery mechanisms (PIO USB is prone to going deaf):
//
//   1. Zero-length reports: PIO USB can resume low-level polling after a glitch
//      but TinyUSB doesn't re-enumerate, producing len=0 HID reports. Detected
//      after 10 consecutive len=0 reports; recovered by power-cycling USB 5V.
//
//   2. Re-arm failure: tuh_hid_receive_report() can fail silently after a
//      callback, killing the HID pipeline. Detected by checking the return
//      value and retrying every loop1() iteration. If retries fail for 5s,
//      power-cycle.
//
//   3. PIO USB deaf state: the PIO state machine goes completely unresponsive.
//      Re-arm "succeeds" (returns true) but callbacks never fire. Physical
//      disconnect is not detected. Caught by a 60s last-resort timeout on
//      HID callback activity. Power-cycles to recover.
//
//   4. Manual recovery: BUTTON1 (escape button) also triggers an immediate USB
//      5V power-cycle via shared flag, so pressing escape recovers both a hung
//      Lisp program and a hung keyboard in one action.

#ifndef FRUITJAM_USBHOST_H
#define FRUITJAM_USBHOST_H

#include "pio_usb.h"
#include "Adafruit_TinyUSB.h"
#include "hardware/dma.h"

// ---- Synchronization: core1 waits for display init (clock reconfiguration) ----
volatile bool fruitjam_clocks_ready = false;

// ---- USB host state tracking ----
static volatile bool     usbh_mounted = false;
static volatile uint8_t  usbh_hid_dev_addr = 0;       // saved from HID mount for re-arm
static volatile uint8_t  usbh_hid_instance = 0;

// ---- USB recovery: detect broken connection and power-cycle ----
static volatile uint32_t usbh_zero_len_count = 0;    // consecutive zero-length reports
static volatile bool     usbh_resetting = false;      // true during power-cycle
static volatile uint32_t usbh_reset_start = 0;        // millis() when reset began
static volatile bool     usbh_power_restored = false;  // phase tracking for reset

#define USBH_ZERO_LEN_THRESHOLD  10   // trigger reset after this many consecutive len=0
#define USBH_RESET_OFF_MS       200   // how long to hold 5V off
#define USBH_RESET_SETTLE_MS    500   // wait after re-enabling 5V before resuming

// ---- HID pipeline health ----
static volatile uint32_t usbh_last_hid_cb_ms = 0;    // millis() of last HID callback (any len)
static volatile bool     usbh_hid_armed = false;      // true when receive_report is active
static volatile uint32_t usbh_rearm_fail_since = 0;   // millis() when re-arm first failed (0=ok)

#define USBH_REARM_FAIL_TIMEOUT_MS  5000   // power-cycle after re-arm fails for this long
#define USBH_DEAD_SILENCE_MS       60000   // power-cycle if mounted but no callbacks for 60s

// ---- Manual USB recovery (triggered by BUTTON1 escape, see fruitjam_escape.h) ----
static volatile bool usbh_manual_reset_requested = false;

// ---- Ring buffer for keyboard input (shared between cores) ----

#define KBD_RINGBUF_SIZE 256

static volatile uint8_t kbd_ringbuf[KBD_RINGBUF_SIZE];
static volatile uint16_t kbd_ring_head = 0;  // written by core1 (producer)
static volatile uint16_t kbd_ring_tail = 0;  // read by core0 (consumer)

static inline bool kbd_ring_empty() {
  return kbd_ring_head == kbd_ring_tail;
}

static inline bool kbd_ring_full() {
  return ((kbd_ring_head + 1) % KBD_RINGBUF_SIZE) == kbd_ring_tail;
}

static void kbd_ring_put(uint8_t c) {
  if (!kbd_ring_full()) {
    kbd_ringbuf[kbd_ring_head] = c;
    kbd_ring_head = (kbd_ring_head + 1) % KBD_RINGBUF_SIZE;
  }
}

static int kbd_ring_get() {
  if (kbd_ring_empty()) return -1;
  uint8_t c = kbd_ringbuf[kbd_ring_tail];
  kbd_ring_tail = (kbd_ring_tail + 1) % KBD_RINGBUF_SIZE;
  return c;
}

static inline bool kbd_available() {
  return !kbd_ring_empty();
}

// ---- USB Host object ----
Adafruit_USBH_Host USBHost;

// ---- HID keycode to ASCII translation ----

// US keyboard layout: HID keycode -> ASCII (unshifted)
static const uint8_t hid_keycode_to_ascii_unshift[128] = {
  0,    0,    0,    0,    'a',  'b',  'c',  'd',  // 0x00-0x07
  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  // 0x08-0x0F
  'm',  'n',  'o',  'p',  'q',  'r',  's',  't',  // 0x10-0x17
  'u',  'v',  'w',  'x',  'y',  'z',  '1',  '2',  // 0x18-0x1F
  '3',  '4',  '5',  '6',  '7',  '8',  '9',  '0',  // 0x20-0x27
  '\n', 0x1B, '\b', '\t', ' ',  '-',  '=',  '[',  // 0x28-0x2F (Enter, Esc, BS, Tab, Space, -, =, [)
  ']',  '\\', 0,    ';',  '\'', '`',  ',',  '.',  // 0x30-0x37
  '/',  0,    0,    0,    0,    0,    0,    0,    // 0x38-0x3F (/, CapsLock, F1-F6)
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x40-0x47 (F7-F12, PrtSc, ScrLk)
  0,    0,    0,    0,    0x7F, 0,    0,    0,    // 0x48-0x4F (Pause, Ins, Home, PgUp, Del, End, PgDn, Right)
  0,    0,    0,    0,    '/',  '*',  '-',  '+',  // 0x50-0x57 (Left, Down, Up, NumLk, KP/, KP*, KP-, KP+)
  '\n', '1',  '2',  '3',  '4',  '5',  '6',  '7',  // 0x58-0x5F (KPEnter, KP1-7)
  '8',  '9',  '0',  '.',  0,    0,    0,    0,    // 0x60-0x67
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x68-0x6F
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x70-0x77
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x78-0x7F
};

// US keyboard layout: HID keycode -> ASCII (shifted)
static const uint8_t hid_keycode_to_ascii_shift[128] = {
  0,    0,    0,    0,    'A',  'B',  'C',  'D',  // 0x00-0x07
  'E',  'F',  'G',  'H',  'I',  'J',  'K',  'L',  // 0x08-0x0F
  'M',  'N',  'O',  'P',  'Q',  'R',  'S',  'T',  // 0x10-0x17
  'U',  'V',  'W',  'X',  'Y',  'Z',  '!',  '@',  // 0x18-0x1F
  '#',  '$',  '%',  '^',  '&',  '*',  '(',  ')',  // 0x20-0x27
  '\n', 0x1B, '\b', '\t', ' ',  '_',  '+',  '{',  // 0x28-0x2F
  '}',  '|',  0,    ':',  '"',  '~',  '<',  '>',  // 0x30-0x37
  '?',  0,    0,    0,    0,    0,    0,    0,    // 0x38-0x3F
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x40-0x47
  0,    0,    0,    0,    0x7F, 0,    0,    0,    // 0x48-0x4F
  0,    0,    0,    0,    '/',  '*',  '-',  '+',  // 0x50-0x57
  '\n', '1',  '2',  '3',  '4',  '5',  '6',  '7',  // 0x58-0x5F
  '8',  '9',  '0',  '.',  0,    0,    0,    0,    // 0x60-0x67
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x68-0x6F
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x70-0x77
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x78-0x7F
};

// ---- Key repeat state ----
static uint8_t  kbd_last_keycode = 0;
static uint8_t  kbd_last_modifier = 0;
static uint32_t kbd_repeat_start = 0;
static uint32_t kbd_repeat_next = 0;
static bool     kbd_repeating = false;
static bool     kbd_caps_lock = false;

#define KEY_REPEAT_DELAY_MS  500
#define KEY_REPEAT_RATE_MS   50

// Track previous report for detecting new key presses
static hid_keyboard_report_t kbd_prev_report = { 0, 0, {0} };

// Convert a HID keycode + modifier to ASCII character
static uint8_t hid_to_ascii(uint8_t keycode, uint8_t modifier) {
  if (keycode >= 128) return 0;

  bool shift = (modifier & (KEYBOARD_MODIFIER_LEFTSHIFT | KEYBOARD_MODIFIER_RIGHTSHIFT)) != 0;
  bool ctrl  = (modifier & (KEYBOARD_MODIFIER_LEFTCTRL  | KEYBOARD_MODIFIER_RIGHTCTRL))  != 0;

  // Toggle shift for alpha keys when caps lock is active
  if (kbd_caps_lock && keycode >= HID_KEY_A && keycode <= HID_KEY_Z) {
    shift = !shift;
  }

  uint8_t ch = shift ? hid_keycode_to_ascii_shift[keycode] : hid_keycode_to_ascii_unshift[keycode];

  // Handle Ctrl+letter -> control character (0x01-0x1A)
  if (ctrl && ch >= 'a' && ch <= 'z') {
    ch = ch - 'a' + 1;
  } else if (ctrl && ch >= 'A' && ch <= 'Z') {
    ch = ch - 'A' + 1;
  }

  return ch;
}

// Process a single keycode, pushing the result into the ring buffer
static void process_keycode(uint8_t keycode, uint8_t modifier) {
  if (keycode == HID_KEY_NONE) return;

  // Handle Caps Lock toggle
  if (keycode == HID_KEY_CAPS_LOCK) {
    kbd_caps_lock = !kbd_caps_lock;
    return;
  }

  uint8_t ch = hid_to_ascii(keycode, modifier);
  if (ch) {
    kbd_ring_put(ch);
  }
}

// Process a keyboard report: detect new key presses and initiate repeat
static void process_keyboard_report(hid_keyboard_report_t const *report) {
  // Find newly pressed keys (in current report but not in previous)
  for (int i = 0; i < 6; i++) {
    uint8_t keycode = report->keycode[i];
    if (keycode == HID_KEY_NONE) continue;

    bool found = false;
    for (int j = 0; j < 6; j++) {
      if (keycode == kbd_prev_report.keycode[j]) {
        found = true;
        break;
      }
    }

    if (!found) {
      // New key press
      process_keycode(keycode, report->modifier);
      kbd_last_keycode = keycode;
      kbd_last_modifier = report->modifier;
      kbd_repeat_start = millis();
      kbd_repeating = false;
    }
  }

  // Check if the repeating key is still held
  bool still_held = false;
  for (int i = 0; i < 6; i++) {
    if (report->keycode[i] == kbd_last_keycode && kbd_last_keycode != HID_KEY_NONE) {
      still_held = true;
      break;
    }
  }
  if (!still_held) {
    kbd_last_keycode = 0;
    kbd_repeating = false;
  }

  // Update modifier for repeat (in case shift changed while holding)
  kbd_last_modifier = report->modifier;

  // Save for next comparison
  memcpy(&kbd_prev_report, report, sizeof(hid_keyboard_report_t));
}

// Called periodically from loop1() to handle key repeat
static void kbd_handle_repeat() {
  if (kbd_last_keycode == HID_KEY_NONE || kbd_last_keycode == HID_KEY_CAPS_LOCK) return;

  uint32_t now = millis();
  if (!kbd_repeating) {
    if ((now - kbd_repeat_start) >= KEY_REPEAT_DELAY_MS) {
      kbd_repeating = true;
      kbd_repeat_next = now;
    }
  }

  if (kbd_repeating && (now >= kbd_repeat_next)) {
    process_keycode(kbd_last_keycode, kbd_last_modifier);
    kbd_repeat_next = now + KEY_REPEAT_RATE_MS;
  }
}

// ---- Core1 setup/loop ----

void fruitjam_usbhost_setup1() {
  // Enable 5V for USB host ports
  pinMode(PIN_5V_EN, OUTPUT);
  digitalWrite(PIN_5V_EN, PIN_5V_EN_STATE);

  // Wait for core0 to finish display init (which reconfigures clocks).
  // PIO USB needs CLK_SYS divisible by 48 MHz. After DVHSTX init,
  // CLK_SYS = 240 MHz (480/2), CLK_PERI = 120 MHz (480/4).
  while (!fruitjam_clocks_ready) {
    tight_loop_contents();
  }

  // Configure PIO USB — use PIO 2 (RP2350 has 3 PIOs) to avoid conflicts.
  // DVHSTX claims DMA channels 0-2, so use channel 3.
  pio_usb_configuration_t pio_cfg = PIO_USB_DEFAULT_CONFIG;
  pio_cfg.pin_dp = PIN_USB_HOST_DP;
  pio_cfg.pio_tx_num = 2;
  pio_cfg.pio_rx_num = 2;
  pio_cfg.tx_ch = 3;  // pio_usb_bus_init() claims this internally

  tuh_configure(1, TUH_CFGID_RPI_PIO_USB_CONFIGURATION, &pio_cfg);

  tusb_rhport_init_t host_init = { .role = TUSB_ROLE_HOST, .speed = TUSB_SPEED_AUTO };
  tusb_init(1, &host_init);
}

// Power-cycle the USB 5V rail to force a full re-enumeration.
static void usbh_start_reset() {
  usbh_resetting = true;
  usbh_reset_start = millis();
  usbh_mounted = false;
  usbh_zero_len_count = 0;
  usbh_hid_armed = false;
  usbh_rearm_fail_since = 0;
  usbh_power_restored = false;

  // Kill 5V power to the USB port
  digitalWrite(PIN_5V_EN, !PIN_5V_EN_STATE);
}

void fruitjam_usbhost_loop1() {
  // Handle USB power-cycle reset sequence
  if (usbh_resetting) {
    uint32_t elapsed = millis() - usbh_reset_start;
    if (elapsed < USBH_RESET_OFF_MS) {
      // Phase 1: keep power off
      return;
    } else if (elapsed < (USBH_RESET_OFF_MS + USBH_RESET_SETTLE_MS)) {
      // Phase 2: re-enable power, wait for device to settle
      if (!usbh_power_restored) {
        digitalWrite(PIN_5V_EN, PIN_5V_EN_STATE);
        usbh_power_restored = true;
      }
      // Still run tuh_task so TinyUSB can detect the new connection
      tuh_task_ext(10, false);
      return;
    } else {
      // Phase 3: reset complete, resume normal operation
      usbh_resetting = false;
    }
  }

  // Check for manual recovery request (BUTTON2 press)
  if (usbh_manual_reset_requested && !usbh_resetting) {
    usbh_manual_reset_requested = false;
    usbh_start_reset();
    return;
  }

  tuh_task_ext(10, false);
  kbd_handle_repeat();

  // If re-arm failed in the callback, retry it here every loop iteration.
  // This handles the case where tuh_hid_receive_report() fails after a
  // callback, silently killing the HID pipeline.
  if (usbh_mounted && !usbh_hid_armed && !usbh_resetting && usbh_hid_dev_addr != 0) {
    if (tuh_hid_receive_report(usbh_hid_dev_addr, usbh_hid_instance)) {
      usbh_hid_armed = true;
      usbh_rearm_fail_since = 0;
    } else {
      // Re-arm still failing — track how long
      if (usbh_rearm_fail_since == 0) {
        usbh_rearm_fail_since = millis();
      } else if ((millis() - usbh_rearm_fail_since) >= USBH_REARM_FAIL_TIMEOUT_MS) {
        // Pipeline is dead — power-cycle to recover
        usbh_rearm_fail_since = 0;
        usbh_start_reset();
      }
    }
  }

  // Last-resort auto-recovery: PIO USB is completely deaf.
  // Re-arm "succeeds" (returns true) but callbacks never fire.
  // Physical disconnect not detected. Only detectable by time.
  // 60s is conservative — won't trigger during normal thinking pauses.
  if (usbh_mounted && !usbh_resetting && usbh_hid_armed &&
      usbh_last_hid_cb_ms != 0 &&
      (millis() - usbh_last_hid_cb_ms) >= USBH_DEAD_SILENCE_MS) {
    usbh_start_reset();
  }
}

// ---- TinyUSB HID Host callbacks (called from core1 context) ----

extern "C" {

void tuh_mount_cb(uint8_t daddr) {
  (void)daddr;
  usbh_mounted = true;
}

void tuh_umount_cb(uint8_t daddr) {
  (void)daddr;
  usbh_mounted = false;
}

void tuh_hid_mount_cb(uint8_t dev_addr, uint8_t instance,
                      uint8_t const *desc_report, uint16_t desc_len) {
  (void)desc_report;
  (void)desc_len;

  uint8_t const itf_protocol = tuh_hid_interface_protocol(dev_addr, instance);
  if (itf_protocol == HID_ITF_PROTOCOL_KEYBOARD) {
    usbh_hid_dev_addr = dev_addr;
    usbh_hid_instance = instance;
    usbh_hid_armed = false;
    usbh_rearm_fail_since = 0;
    usbh_last_hid_cb_ms = millis();
    if (tuh_hid_receive_report(dev_addr, instance)) {
      usbh_hid_armed = true;
    }
  }
}

void tuh_hid_umount_cb(uint8_t dev_addr, uint8_t instance) {
  (void)dev_addr;
  (void)instance;
  kbd_last_keycode = 0;
  kbd_repeating = false;
  memset((void*)&kbd_prev_report, 0, sizeof(kbd_prev_report));
}

void tuh_hid_report_received_cb(uint8_t dev_addr, uint8_t instance,
                                uint8_t const *report, uint16_t len) {
  uint8_t const itf_protocol = tuh_hid_interface_protocol(dev_addr, instance);

  // Track that we received a callback — pipeline is alive
  usbh_last_hid_cb_ms = millis();
  usbh_hid_armed = false;       // will be re-armed below
  usbh_rearm_fail_since = 0;    // reset failure tracking

  if (itf_protocol == HID_ITF_PROTOCOL_KEYBOARD && len >= sizeof(hid_keyboard_report_t)) {
    usbh_zero_len_count = 0;  // good report — reset the watchdog
    process_keyboard_report((hid_keyboard_report_t const *)report);
  } else if (itf_protocol == HID_ITF_PROTOCOL_KEYBOARD && len == 0) {
    // Zero-length keyboard report — connection is broken (PIO USB resumed
    // low-level polling after a glitch but TinyUSB didn't re-enumerate).
    // Power-cycle the USB port to force a clean re-enumeration.
    usbh_zero_len_count++;
    if (usbh_zero_len_count == USBH_ZERO_LEN_THRESHOLD) {
      usbh_start_reset();
      return;  // don't re-arm, we're resetting
    }
  }

  // Re-arm for next report (unless we're in the middle of a reset)
  if (!usbh_resetting) {
    if (tuh_hid_receive_report(dev_addr, instance)) {
      usbh_hid_armed = true;
    }
    // If re-arm failed, usbh_hid_armed stays false → loop1 will retry
  }
}

} // extern "C"

#endif // FRUITJAM_USBHOST_H
