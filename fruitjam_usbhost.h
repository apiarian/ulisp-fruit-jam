// fruitjam_usbhost.h — USB Host keyboard/mouse support for Fruit Jam
// Runs TinyUSB host via PIO USB on core1 (setup1/loop1)
// Provides a ring buffer of ASCII characters for gserial() on core0
//
// USB recovery mechanisms:
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
//   3. Manual recovery: BUTTON1 long press (>=1s) sets a flag requesting USB
//      power-cycle. If core1 is alive, it picks this up in loop1() and does
//      a normal 5V power-cycle. If core1 is locked, mechanism 4 handles it.
//      A short press only triggers the Lisp escape (no USB reset).
//
//   4. Core0 auto-recovery (5s heartbeat): Core1 updates a heartbeat timestamp
//      every loop1() iteration. Core0 checks from the gserial() input wait
//      loop. If the heartbeat stalls for 5s (core1 stuck in a PIO busy-wait),
//      core0 triggers emergency PIO unstick — directly cuts 5V, force-sets
//      PIO 2 IRQ flags, force-jumps all SMs, disables PIO output driving on
//      D+/D-. This breaks core1 out of any PIO busy-wait.

#ifndef FRUITJAM_USBHOST_H
#define FRUITJAM_USBHOST_H

#include "pio_usb.h"
#include "Adafruit_TinyUSB.h"
#include "hardware/dma.h"
#include "hardware/sync.h"
#include "hardware/structs/iobank0.h"
#include "hardware/structs/sio.h"
#include "fruitjam_mouse.h"

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
#define USBH_RESET_OFF_MS       500   // how long to hold 5V off (must be long enough for
                                      // device capacitors to fully discharge)
#define USBH_RESET_SETTLE_MS    500   // wait after re-enabling 5V before resuming

// ---- HID pipeline health ----
static volatile bool     usbh_hid_armed = false;      // true when receive_report is active
static volatile uint32_t usbh_rearm_fail_since = 0;   // millis() when re-arm first failed (0=ok)

#define USBH_REARM_FAIL_TIMEOUT_MS  5000   // power-cycle after re-arm fails for this long

// ---- Manual USB recovery (triggered by BUTTON1 long press, see fruitjam_escape.h) ----
static volatile bool usbh_manual_reset_requested = false;

// ---- Core1 heartbeat (for core0 auto-recovery, mechanism 5) ----
// Updated every loop1() iteration. If this stalls, core1 is stuck.
static volatile uint32_t usbh_core1_heartbeat_ms = 0;

#define USBH_HEARTBEAT_TIMEOUT_MS  5000  // core0 triggers emergency reset after this

// ---- Manual SOF frame timing (skip_alarm_pool mode) ----
// Instead of the PIO USB library using a timer alarm IRQ to call the SOF
// handler every 1ms, we call pio_usb_host_frame() ourselves from loop1()
// with interrupts disabled. TinyUSB's blocking delays during enumeration
// are overridden (tusb_time_delay_ms_api) to keep SOF frames running.
static uint32_t usbh_last_frame_us = 0;

// ---- Ring buffer for keyboard input (shared between cores) ----
//
// Each entry is a uint16_t:
//   High byte = HID modifier bitmask at time of keypress
//   Low byte  = key code (ASCII or 0x80+ special key)
//
// The REPL (gserial) only uses the low byte; Lisp programs via (keyboard)
// and (wait-keyboard) get the full 16-bit value.

#define KBD_RINGBUF_SIZE 256

static volatile uint16_t kbd_ringbuf[KBD_RINGBUF_SIZE];
static volatile uint16_t kbd_ring_head = 0;  // written by core1 (producer)
static volatile uint16_t kbd_ring_tail = 0;  // read by core0 (consumer)

static inline bool kbd_ring_empty() {
  return kbd_ring_head == kbd_ring_tail;
}

static inline bool kbd_ring_full() {
  return ((kbd_ring_head + 1) % KBD_RINGBUF_SIZE) == kbd_ring_tail;
}

static void kbd_ring_put(uint16_t c) {
  if (!kbd_ring_full()) {
    kbd_ringbuf[kbd_ring_head] = c;
    kbd_ring_head = (kbd_ring_head + 1) % KBD_RINGBUF_SIZE;
  }
}

static int kbd_ring_get() {
  if (kbd_ring_empty()) return -1;
  uint16_t c = kbd_ringbuf[kbd_ring_tail];
  kbd_ring_tail = (kbd_ring_tail + 1) % KBD_RINGBUF_SIZE;
  return (int)c;
}

static inline bool kbd_available() {
  return !kbd_ring_empty();
}

// ---- Mouse HID tracking ----
static volatile uint8_t  usbh_mouse_dev_addr = 0;
static volatile uint8_t  usbh_mouse_instance = 0;
static volatile bool     usbh_mouse_mounted = false;
static volatile bool     usbh_mouse_armed = false;

// ---- Emergency PIO unstick (callable from core0 or ISR context) ----
//
// When core1 is locked in a PIO busy-wait (e.g., pio_usb_host_frame() waiting
// for PIO IRQ that never fires), flag-based recovery is useless — nobody on
// core1 reads the flags. This function directly manipulates hardware registers
// to break the deadlock:
//
//   1. Cut 5V power — device will disconnect, bus goes SE0
//   2. Force-set all PIO 2 IRQ flags — breaks `while ((irq & mask) == 0)` loops
//   3. Force-jump all PIO 2 SMs to instruction 31 — breaks `while (*pc <= N)` loops
//   4. Disable PIO output driving on D+/D- — so connection_check() reads actual
//      bus state (SE0 from device off), not PIO-driven levels
//
// After this, the SOF timer callback will return, connection_check() will
// detect SE0 (disconnect), and loop1() will resume. The usbh_start_reset()
// function then handles the timed re-enable of 5V power.
//
// Safe to call from any core or ISR — only does GPIO writes and direct
// hardware register writes (no malloc, no SDK calls that take locks).

static void fruitjam_usb_emergency_reset() {
  // 1. Cut 5V power immediately
  sio_hw->gpio_clr = (1u << PIN_5V_EN);   // direct register write, no SDK call

  // 2. Force-set all PIO 2 IRQ flags (bits 0-7)
  //    This breaks any `while ((pio->irq & mask) == 0)` busy-wait
  pio2_hw->irq_force = 0xFF;

  // 3. Force all 4 PIO 2 state machines to jump to instruction 31
  //    This breaks `while (*pc <= N)` busy-waits where N < 31
  //    pio_sm_exec() equivalent: write the JMP 31 instruction to SMx_INSTR
  //    JMP 31 = 0x001F (opcode 000, condition 00, address 11111)
  for (int sm = 0; sm < 4; sm++) {
    pio2_hw->sm[sm].instr = 0x0000 | 31;  // JMP 31
  }

  // 4. Disable PIO output driving on D+ and D- pins
  //    Set output enable override to "force disable" so the PIO can't
  //    drive the pins. connection_check() reads GPIO input, which will
  //    see the actual bus state (SE0 with device powered off).
  //    GPIO_OVERRIDE_LOW (2) = force output enable low (tri-state)
  hw_write_masked(&iobank0_hw->io[PIN_USB_HOST_DP].ctrl,
                  2u << IO_BANK0_GPIO0_CTRL_OEOVER_LSB,
                  IO_BANK0_GPIO0_CTRL_OEOVER_BITS);
  hw_write_masked(&iobank0_hw->io[PIN_USB_HOST_DP + 1].ctrl,  // DM = DP + 1
                  2u << IO_BANK0_GPIO0_CTRL_OEOVER_LSB,
                  IO_BANK0_GPIO0_CTRL_OEOVER_BITS);

  // 5. Request that core1 handle the reset sequence once it's unstuck
  usbh_resetting = true;
  usbh_reset_start = millis();
  usbh_mounted = false;
  usbh_zero_len_count = 0;
  usbh_hid_armed = false;
  usbh_rearm_fail_since = 0;
  usbh_power_restored = false;
  usbh_hid_dev_addr = 0;
  usbh_mouse_dev_addr = 0;
  usbh_mouse_mounted = false;
  usbh_mouse_armed = false;
}

// Called from core0's gserial() wait loop to detect core1 lockups.
// If core1's heartbeat has stalled for USBH_HEARTBEAT_TIMEOUT_MS,
// triggers emergency PIO unstick + power-cycle from core0.
static void usbh_check_core1_health() {
  uint32_t hb = usbh_core1_heartbeat_ms;
  if (hb == 0) return;  // core1 hasn't started yet
  if (usbh_resetting) return;  // already recovering
  uint32_t now = millis();
  if ((now - hb) >= USBH_HEARTBEAT_TIMEOUT_MS) {
    fruitjam_usb_emergency_reset();
  }
}

// ---- Generic HID report descriptor parsing ----
// Non-boot-protocol mice need report descriptor parsing to identify them.
#define MAX_HID_REPORTS 4
#define MAX_HID_INSTANCES 12

struct hid_instance_info_t {
  uint8_t report_count;
  tuh_hid_report_info_t report_info[MAX_HID_REPORTS];
  bool has_mouse;
};
static hid_instance_info_t hid_info[MAX_HID_INSTANCES];

// Process a boot-protocol mouse report
static void process_mouse_report(hid_mouse_report_t const *report) {
  int16_t nx = mouse_x + report->x / 2;
  int16_t ny = mouse_y + report->y / 2;
  if (nx < 0) nx = 0;
  if (nx >= DISPLAY_WIDTH) nx = DISPLAY_WIDTH - 1;
  if (ny < 0) ny = 0;
  if (ny >= DISPLAY_HEIGHT) ny = DISPLAY_HEIGHT - 1;
  mouse_x = nx;
  mouse_y = ny;
  uint8_t prev = mouse_buttons;
  mouse_buttons = report->buttons;
  if ((report->buttons & ~prev) != 0) mouse_clicked = true;
}

// Dispatch a generic (non-boot-protocol) HID report using parsed descriptor info.
// Handles composite reports where byte 0 is report ID.
static void process_generic_report(uint8_t instance,
                                   uint8_t const *report, uint16_t len) {
  if (instance >= MAX_HID_INSTANCES || len == 0) return;
  uint8_t const rpt_count = hid_info[instance].report_count;
  tuh_hid_report_info_t *rpt_info_arr = hid_info[instance].report_info;
  tuh_hid_report_info_t *rpt_info = NULL;

  if (rpt_count == 1 && rpt_info_arr[0].report_id == 0) {
    // Simple report without report ID
    rpt_info = &rpt_info_arr[0];
  } else {
    // Composite: first byte is report ID
    uint8_t const rpt_id = report[0];
    for (uint8_t i = 0; i < rpt_count; i++) {
      if (rpt_id == rpt_info_arr[i].report_id) {
        rpt_info = &rpt_info_arr[i];
        break;
      }
    }
    report++;
    len--;
  }
  if (!rpt_info) return;
  if (rpt_info->usage_page == HID_USAGE_PAGE_DESKTOP &&
      rpt_info->usage == HID_USAGE_DESKTOP_MOUSE &&
      len >= 3) {
    process_mouse_report((hid_mouse_report_t const *)report);
  }
}

// ---- USB Host object ----
Adafruit_USBH_Host USBHost;

// ---- HID keycode to ASCII translation ----

// Special key codes (0x80+) — used in both ASCII tables and exposed to Lisp
#define KEY_CODE_UP      0x80
#define KEY_CODE_DOWN    0x81
#define KEY_CODE_LEFT    0x82
#define KEY_CODE_RIGHT   0x83
#define KEY_CODE_HOME    0x84
#define KEY_CODE_END     0x85
#define KEY_CODE_PGUP    0x86
#define KEY_CODE_PGDN    0x87
#define KEY_CODE_INSERT  0x88
#define KEY_CODE_F1      0x89
#define KEY_CODE_F2      0x8A
#define KEY_CODE_F3      0x8B
#define KEY_CODE_F4      0x8C
#define KEY_CODE_F5      0x8D
#define KEY_CODE_F6      0x8E
#define KEY_CODE_F7      0x8F
#define KEY_CODE_F8      0x90
#define KEY_CODE_F9      0x91
#define KEY_CODE_F10     0x92
#define KEY_CODE_F11     0x93
#define KEY_CODE_F12     0x94

// US keyboard layout: HID keycode -> ASCII (unshifted)
// Special keys (arrows, Home/End, PgUp/PgDn, F-keys) use 0x80+ codes above
static const uint8_t hid_keycode_to_ascii_unshift[128] = {
  0,    0,    0,    0,    'a',  'b',  'c',  'd',  // 0x00-0x07
  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  // 0x08-0x0F
  'm',  'n',  'o',  'p',  'q',  'r',  's',  't',  // 0x10-0x17
  'u',  'v',  'w',  'x',  'y',  'z',  '1',  '2',  // 0x18-0x1F
  '3',  '4',  '5',  '6',  '7',  '8',  '9',  '0',  // 0x20-0x27
  '\n', 0x1B, '\b', '\t', ' ',  '-',  '=',  '[',  // 0x28-0x2F (Enter, Esc, BS, Tab, Space, -, =, [)
  ']',  '\\', 0,    ';',  '\'', '`',  ',',  '.',  // 0x30-0x37
  '/',  0,                                          // 0x38-0x39 (/, CapsLock)
  KEY_CODE_F1,  KEY_CODE_F2,  KEY_CODE_F3,  KEY_CODE_F4,   // 0x3A-0x3D (F1-F4)
  KEY_CODE_F5,  KEY_CODE_F6,                                // 0x3E-0x3F (F5-F6)
  KEY_CODE_F7,  KEY_CODE_F8,  KEY_CODE_F9,  KEY_CODE_F10,  // 0x40-0x43 (F7-F10)
  KEY_CODE_F11, KEY_CODE_F12, 0,    0,                      // 0x44-0x47 (F11-F12, PrtSc, ScrLk)
  0,    KEY_CODE_INSERT, KEY_CODE_HOME, KEY_CODE_PGUP,      // 0x48-0x4B (Pause, Ins, Home, PgUp)
  0x7F, KEY_CODE_END, KEY_CODE_PGDN, KEY_CODE_RIGHT,       // 0x4C-0x4F (Del, End, PgDn, Right)
  KEY_CODE_LEFT, KEY_CODE_DOWN, KEY_CODE_UP, 0,             // 0x50-0x53 (Left, Down, Up, NumLk)
  '/',  '*',  '-',  '+',                                    // 0x54-0x57 (KP/, KP*, KP-, KP+)
  '\n', '1',  '2',  '3',  '4',  '5',  '6',  '7',  // 0x58-0x5F (KPEnter, KP1-7)
  '8',  '9',  '0',  '.',  0,    0,    0,    0,    // 0x60-0x67
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x68-0x6F
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x70-0x77
  0,    0,    0,    0,    0,    0,    0,    0,    // 0x78-0x7F
};

// US keyboard layout: HID keycode -> ASCII (shifted)
// Special keys produce the same codes regardless of shift
static const uint8_t hid_keycode_to_ascii_shift[128] = {
  0,    0,    0,    0,    'A',  'B',  'C',  'D',  // 0x00-0x07
  'E',  'F',  'G',  'H',  'I',  'J',  'K',  'L',  // 0x08-0x0F
  'M',  'N',  'O',  'P',  'Q',  'R',  'S',  'T',  // 0x10-0x17
  'U',  'V',  'W',  'X',  'Y',  'Z',  '!',  '@',  // 0x18-0x1F
  '#',  '$',  '%',  '^',  '&',  '*',  '(',  ')',  // 0x20-0x27
  '\n', 0x1B, '\b', '\t', ' ',  '_',  '+',  '{',  // 0x28-0x2F
  '}',  '|',  0,    ':',  '"',  '~',  '<',  '>',  // 0x30-0x37
  '?',  0,                                          // 0x38-0x39 (?, CapsLock)
  KEY_CODE_F1,  KEY_CODE_F2,  KEY_CODE_F3,  KEY_CODE_F4,   // 0x3A-0x3D
  KEY_CODE_F5,  KEY_CODE_F6,                                // 0x3E-0x3F
  KEY_CODE_F7,  KEY_CODE_F8,  KEY_CODE_F9,  KEY_CODE_F10,  // 0x40-0x43
  KEY_CODE_F11, KEY_CODE_F12, 0,    0,                      // 0x44-0x47
  0,    KEY_CODE_INSERT, KEY_CODE_HOME, KEY_CODE_PGUP,      // 0x48-0x4B
  0x7F, KEY_CODE_END, KEY_CODE_PGDN, KEY_CODE_RIGHT,       // 0x4C-0x4F
  KEY_CODE_LEFT, KEY_CODE_DOWN, KEY_CODE_UP, 0,             // 0x50-0x53
  '/',  '*',  '-',  '+',                                    // 0x54-0x57
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
    kbd_ring_put(((uint16_t)modifier << 8) | ch);
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

// ---- TinyUSB delay override ----
// TinyUSB calls tusb_time_delay_ms_api() during enumeration (bus reset,
// address recovery, etc.) with blocking delays of 10-50ms. With the alarm
// pool disabled (skip_alarm_pool=true), no SOF frames are sent during these
// delays because our manual pio_usb_host_frame() calls in loop1() can't run.
// Override the weak default to keep calling pio_usb_host_frame() during waits.
extern "C" void tusb_time_delay_ms_api(uint32_t ms) {
  uint32_t start = millis();
  while ((millis() - start) < ms) {
    uint32_t now_us = micros();
    if ((now_us - usbh_last_frame_us) >= 1000) {
      usbh_last_frame_us = now_us;
      uint32_t status = save_and_disable_interrupts();
      pio_usb_host_frame();
      restore_interrupts(status);
    }
  }
}

// ---- Core1 setup/loop ----

void fruitjam_usbhost_setup1() {
  // Power-cycle USB host ports to ensure clean device state.
  // On CPU reset, GPIO11 floats and R8 pulls Q1 gate low (5V off), but
  // the 5V rail capacitance may keep devices powered through a fast
  // reset-to-reinit cycle. Explicitly hold 5V off for 500ms to guarantee
  // the device fully resets — this fixes keyboards that get stuck in a
  // state that normal re-enumeration can't recover from.
  pinMode(PIN_5V_EN, OUTPUT);
  digitalWrite(PIN_5V_EN, !PIN_5V_EN_STATE);  // 5V OFF
  delay(500);
  digitalWrite(PIN_5V_EN, PIN_5V_EN_STATE);   // 5V ON
  delay(200);                                  // let device power up

  // Wait for core0 to finish display init (which reconfigures clocks).
  // PIO USB needs CLK_SYS divisible by 48 MHz. After DVHSTX init,
  // CLK_SYS = 240 MHz (480/2), CLK_PERI = 120 MHz (480/4).
  while (!fruitjam_clocks_ready) {
    tight_loop_contents();
  }

  // Configure PIO USB — use PIO 2 (RP2350 has 3 PIOs) to avoid conflicts.
  // DVHSTX claims DMA channels 0-2, so use channel 3.
  // skip_alarm_pool=true: we call pio_usb_host_frame() manually from loop1()
  // every 1ms with interrupts disabled, instead of letting the library use a
  // timer alarm IRQ. This prevents other interrupts from disrupting PIO USB
  // bus timing during transfers.
  pio_usb_configuration_t pio_cfg = PIO_USB_DEFAULT_CONFIG;
  pio_cfg.pin_dp = PIN_USB_HOST_DP;
  pio_cfg.pio_tx_num = 2;
  pio_cfg.pio_rx_num = 2;
  pio_cfg.tx_ch = 3;  // pio_usb_bus_init() claims this internally
  pio_cfg.skip_alarm_pool = true;

  tuh_configure(1, TUH_CFGID_RPI_PIO_USB_CONFIGURATION, &pio_cfg);

  // Must use 2-arg tusb_init(rhport, &init). The 1-arg tusb_init(rhport)
  // macro expands via _tusb_init_arg1 → _tusb_init_arg0(), which calls
  // tusb_rhport_init(0, NULL), silently ignoring the rhport argument.
  // (Note: tuh_init(rhport) is a separate inline function that works correctly.)
  tusb_rhport_init_t host_init = { .role = TUSB_ROLE_HOST, .speed = TUSB_SPEED_AUTO };
  tusb_init(1, &host_init);

  // Give PIO USB's DMA channel high priority over HSTX video (ch 0-2) and
  // audio (ch 4). Without this, all 5 channels are low-priority round-robin,
  // and PIO USB gets only 1-in-5 DMA scheduling slots. USB bit-banging at
  // 12 Mbps is extremely timing-sensitive — any delay in refilling the PIO TX
  // FIFO can corrupt a packet, leading to transaction failures that cascade
  // into device disconnects. HIGH_PRIORITY ensures PIO USB's DMA is serviced
  // before all low-priority channels in every scheduling round.
  hw_set_bits(&dma_hw->ch[pio_cfg.tx_ch].al1_ctrl, DMA_CH0_CTRL_TRIG_HIGH_PRIORITY_BITS);

  // Initialize heartbeat so core0 doesn't false-trigger before first loop1()
  usbh_core1_heartbeat_ms = millis();

  // Initialize manual SOF frame timer
  usbh_last_frame_us = micros();
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
  // Clear device addresses so stale re-arm attempts don't fire on old addresses
  // after the device re-enumerates (it may get a different address)
  usbh_hid_dev_addr = 0;
  usbh_mouse_dev_addr = 0;
  usbh_mouse_mounted = false;
  usbh_mouse_armed = false;

  // Kill 5V power to the USB port
  digitalWrite(PIN_5V_EN, !PIN_5V_EN_STATE);
}

void fruitjam_usbhost_loop1() {
  // Update heartbeat — core0 monitors this to detect core1 lockups
  usbh_core1_heartbeat_ms = millis();

  // Handle USB power-cycle reset sequence
  if (usbh_resetting) {
    uint32_t elapsed = millis() - usbh_reset_start;
    if (elapsed < USBH_RESET_OFF_MS) {
      // Phase 1: keep power off
      return;
    } else if (elapsed < (USBH_RESET_OFF_MS + USBH_RESET_SETTLE_MS)) {
      // Phase 2: re-enable power, wait for device to settle
      if (!usbh_power_restored) {
        // Restore GPIO overrides to normal (in case emergency reset disabled them)
        hw_write_masked(&iobank0_hw->io[PIN_USB_HOST_DP].ctrl,
                        0u << IO_BANK0_GPIO0_CTRL_OEOVER_LSB,
                        IO_BANK0_GPIO0_CTRL_OEOVER_BITS);
        hw_write_masked(&iobank0_hw->io[PIN_USB_HOST_DP + 1].ctrl,
                        0u << IO_BANK0_GPIO0_CTRL_OEOVER_LSB,
                        IO_BANK0_GPIO0_CTRL_OEOVER_BITS);
        digitalWrite(PIN_5V_EN, PIN_5V_EN_STATE);
        usbh_power_restored = true;
      }
    }
    // During reset, still call pio_usb_host_frame() so the library can
    // detect reconnection, and tuh_task so TinyUSB processes events.
    // Fall through to the frame call below.
    if (elapsed < (USBH_RESET_OFF_MS + USBH_RESET_SETTLE_MS)) {
      // Call frame + tuh_task but skip the rest of loop1 during settle
      uint32_t now_us = micros();
      if ((now_us - usbh_last_frame_us) >= 1000) {
        usbh_last_frame_us = now_us;
        uint32_t status = save_and_disable_interrupts();
        pio_usb_host_frame();
        restore_interrupts(status);
      }
      tuh_task_ext(0, false);
      return;
    } else {
      // Phase 3: reset complete, resume normal operation
      usbh_resetting = false;
    }
  }

  // Check for manual recovery request (BUTTON1 long press, if not already
  // handled by emergency reset)
  if (usbh_manual_reset_requested && !usbh_resetting) {
    usbh_manual_reset_requested = false;
    usbh_start_reset();
    return;
  }

  // Manual SOF frame: call pio_usb_host_frame() every 1ms with interrupts
  // disabled. This replaces the library's alarm pool timer.
  uint32_t now_us = micros();
  if ((now_us - usbh_last_frame_us) >= 1000) {
    usbh_last_frame_us = now_us;
    uint32_t status = save_and_disable_interrupts();
    pio_usb_host_frame();
    restore_interrupts(status);
  }

  // Non-blocking: process any pending TinyUSB events without waiting.
  // We must not block here — loop1() needs to spin fast to call
  // pio_usb_host_frame() every 1ms for SOF timing.
  tuh_task_ext(0, false);
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

  // WARNING: Do NOT add a "no HID callbacks for N seconds" timeout here.
  // USB HID interrupt endpoints only report when state changes — an idle
  // keyboard with no keys pressed produces zero callbacks, which is normal.
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
  uint8_t const itf_protocol = tuh_hid_interface_protocol(dev_addr, instance);

  if (itf_protocol == HID_ITF_PROTOCOL_KEYBOARD) {
    usbh_hid_dev_addr = dev_addr;
    usbh_hid_instance = instance;
    usbh_hid_armed = false;
    usbh_rearm_fail_since = 0;
    if (tuh_hid_receive_report(dev_addr, instance)) {
      usbh_hid_armed = true;
    }
  } else if (itf_protocol == HID_ITF_PROTOCOL_MOUSE) {
    usbh_mouse_dev_addr = dev_addr;
    usbh_mouse_instance = instance;
    usbh_mouse_mounted = true;
    usbh_mouse_armed = false;
    if (tuh_hid_receive_report(dev_addr, instance)) {
      usbh_mouse_armed = true;
    }
  } else if (itf_protocol == HID_ITF_PROTOCOL_NONE && instance < MAX_HID_INSTANCES) {
    // Non-boot device — parse report descriptor to detect mouse
    hid_info[instance].report_count = tuh_hid_parse_report_descriptor(
      hid_info[instance].report_info, MAX_HID_REPORTS, desc_report, desc_len);
    hid_info[instance].has_mouse = false;
    for (uint8_t i = 0; i < hid_info[instance].report_count; i++) {
      if (hid_info[instance].report_info[i].usage_page == HID_USAGE_PAGE_DESKTOP &&
          hid_info[instance].report_info[i].usage == HID_USAGE_DESKTOP_MOUSE) {
        hid_info[instance].has_mouse = true;
        usbh_mouse_dev_addr = dev_addr;
        usbh_mouse_instance = instance;
        usbh_mouse_mounted = true;
        usbh_mouse_armed = false;
      }
    }
    // Arm reports for this device
    if (!tuh_hid_receive_report(dev_addr, instance)) {
      usbh_mouse_armed = false;
    } else {
      if (hid_info[instance].has_mouse) usbh_mouse_armed = true;
    }
  }
}

void tuh_hid_umount_cb(uint8_t dev_addr, uint8_t instance) {
  (void)dev_addr;
  (void)instance;
  kbd_last_keycode = 0;
  kbd_repeating = false;
  memset((void*)&kbd_prev_report, 0, sizeof(kbd_prev_report));
  if (dev_addr == usbh_mouse_dev_addr && instance == usbh_mouse_instance) {
    usbh_mouse_mounted = false;
    usbh_mouse_armed = false;
    mouse_buttons = 0;
  }
  if (instance < MAX_HID_INSTANCES) {
    hid_info[instance].report_count = 0;
    hid_info[instance].has_mouse = false;
  }
}

void tuh_hid_report_received_cb(uint8_t dev_addr, uint8_t instance,
                                uint8_t const *report, uint16_t len) {
  uint8_t const itf_protocol = tuh_hid_interface_protocol(dev_addr, instance);

  if (itf_protocol == HID_ITF_PROTOCOL_KEYBOARD) {
    usbh_hid_armed = false;       // will be re-armed below
    usbh_rearm_fail_since = 0;    // reset failure tracking

    if (len >= sizeof(hid_keyboard_report_t)) {
      usbh_zero_len_count = 0;  // good report — reset the watchdog
      process_keyboard_report((hid_keyboard_report_t const *)report);
    } else if (len == 0) {
      usbh_zero_len_count++;
      if (usbh_zero_len_count == USBH_ZERO_LEN_THRESHOLD) {
        usbh_start_reset();
        return;  // don't re-arm, we're resetting
      }
    }

    // Re-arm for next keyboard report
    if (!usbh_resetting) {
      if (tuh_hid_receive_report(dev_addr, instance)) {
        usbh_hid_armed = true;
      }
    }
  } else if (itf_protocol == HID_ITF_PROTOCOL_MOUSE) {
    if (len >= 3) {  // minimum: buttons + x + y (hid_mouse_report_t is 5 but many mice send 3-4)
      process_mouse_report((hid_mouse_report_t const *)report);
    }
    usbh_mouse_armed = false;
    if (!usbh_resetting) {
      if (tuh_hid_receive_report(dev_addr, instance)) {
        usbh_mouse_armed = true;
      }
    }
  } else if (instance < MAX_HID_INSTANCES && hid_info[instance].has_mouse) {
    // Generic (non-boot) mouse — dispatch via parsed report descriptor
    process_generic_report(instance, report, len);
    usbh_mouse_armed = false;
    if (!usbh_resetting) {
      if (tuh_hid_receive_report(dev_addr, instance)) {
        usbh_mouse_armed = true;
      }
    }
  }
}

} // extern "C"

#endif // FRUITJAM_USBHOST_H
