// fruitjam_audio.h — TLV320DAC3100 I2S DAC + wavetable synth for Fruit Jam
//
// Phase 5, Stage 1: DAC initialization + I2S PIO output + DMA ring buffer
// Produces silence — confirms the audio hardware path is alive.
//
// Hardware path:
//   PWM → MCLK (GPIO25, 15 MHz)
//   I2C0 → TLV320 register config (addr 0x18, SDA=20, SCL=21)
//   PIO 0 → I2S output (DIN=GPIO24, BCLK=GPIO26, WS=GPIO27)
//   DMA → ring buffer → PIO TX FIFO
//
// Must be #include'd from the board config block in the .ino AFTER
// the DVHSTX display is initialized (clocks must be stable at 240 MHz).

#ifndef FRUITJAM_AUDIO_H
#define FRUITJAM_AUDIO_H

#include "hardware/pio.h"
#include "hardware/clocks.h"
#include "hardware/dma.h"
#include "hardware/i2c.h"
#include "hardware/pwm.h"
#include "hardware/gpio.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

// ---- Pin assignments ----
#define AUDIO_PIN_DIN      24   // I2S serial data
#define AUDIO_PIN_MCLK     25   // Master clock (PWM output)
#define AUDIO_PIN_BCLK     26   // I2S bit clock (PIO side-set base)
#define AUDIO_PIN_WS       27   // I2S word select / LRCLK (PIO side-set base+1)
#define AUDIO_PIN_RESET    22   // Peripheral reset (shared with ESP32-C6)
#define AUDIO_PIN_HP_DET   23   // TLV320 INT1 — headphone detect

// ---- I2C ----
#define AUDIO_I2C_PORT     i2c0
#define AUDIO_I2C_ADDR     0x18 // TLV320DAC3100

// ---- Audio parameters ----
#define AUDIO_SAMPLE_RATE  22050
#define AUDIO_RING_SIZE    1024  // samples (must be power of 2)
#define AUDIO_RING_MASK    (AUDIO_RING_SIZE - 1)
#define AUDIO_DMA_BLOCK    128   // samples per DMA transfer

// ---- PIO I2S program (inline) ----
// Standard 16-bit stereo I2S transmitter.
// 1 output pin (DIN), 2 side-set pins (BCLK, WS).
// Autopull, 32-bit threshold, shift left (MSB first).
// Each 32-bit word: [31:16] = left sample, [15:0] = right sample.

// PIO program assembled by pioasm from audio_i2s.pio (retroJam/pico-extras):
//   bitloop1:                             ;  /--- LRCLK
//     out pins, 1       side 0b10         ;  |/-- BCLK
//     jmp x-- bitloop1  side 0b11
//     out pins, 1       side 0b00
//     set x, 14         side 0b01
//   bitloop0:
//     out pins, 1       side 0b00
//     jmp x-- bitloop0  side 0b01
//     out pins, 1       side 0b10
//   entry_point:
//     set x, 14         side 0b11

static const uint16_t audio_i2s_program_instructions[] = {
    0x7001, //  0: out    pins, 1         side 2
    0x1840, //  1: jmp    x--, 0          side 3
    0x6001, //  2: out    pins, 1         side 0
    0xe82e, //  3: set    x, 14           side 1
    0x6001, //  4: out    pins, 1         side 0
    0x0844, //  5: jmp    x--, 4          side 1
    0x7001, //  6: out    pins, 1         side 2
    0xf82e, //  7: set    x, 14           side 3  (entry point)
};

#define AUDIO_I2S_WRAP_TARGET 0
#define AUDIO_I2S_WRAP        7
#define AUDIO_I2S_ENTRY_POINT 7

static const struct pio_program audio_i2s_pio_program = {
    .instructions = audio_i2s_program_instructions,
    .length = 8,
    .origin = -1,
    .pio_version = 0,
#if PICO_PIO_VERSION > 0
    .used_gpio_ranges = 0x0,
#endif
};

// ---- Voice system ----
#define AUDIO_NUM_VOICES  5   // 0-4 user-accessible (0-3 = wavetable tone, 4 = noise)
#define BELL_VOICE        5   // private voice for terminal bell (not user-accessible)
#define AUDIO_TOTAL_VOICES 6  // total voices including bell
#define AUDIO_WAVETABLE_SIZE 256

// Built-in waveform IDs
#define AUDIO_WAVE_SILENCE  0
#define AUDIO_WAVE_SINE     1
#define AUDIO_WAVE_SQUARE   2
#define AUDIO_WAVE_TRIANGLE 3
#define AUDIO_WAVE_SAWTOOTH 4
#define AUDIO_WAVE_NOISE    5
#define AUDIO_WAVE_CUSTOM   6

// ADSR envelope states
#define ADSR_OFF     0  // no envelope — constant volume
#define ADSR_ATTACK  1
#define ADSR_DECAY   2
#define ADSR_SUSTAIN 3
#define ADSR_RELEASE 4
#define ADSR_DONE    5  // released to zero, voice silent

typedef struct {
    // Envelope parameters (set by audio-envelope, 0 = no envelope)
    uint32_t attack_inc;      // 16.16 fixed-point level increment per sample
    uint32_t decay_dec;       // 16.16 fixed-point level decrement per sample
    uint8_t sustain_level;    // 0-255
    uint32_t release_dec;     // 16.16 fixed-point level decrement per sample
    bool enabled;             // true if envelope has been configured
    // Envelope runtime state
    uint8_t stage;            // ADSR_OFF / ATTACK / DECAY / SUSTAIN / RELEASE / DONE
    uint32_t env_level;       // 16.16 fixed-point, top 8 bits = 0-255 level
} audio_envelope_t;

typedef struct {
    int8_t wavetable[AUDIO_WAVETABLE_SIZE]; // waveform data (-128 to 127)
    uint32_t phase;           // 32-bit phase accumulator
    uint32_t phase_inc;       // phase increment per sample (frequency)
    uint8_t volume;           // 0-255 (base volume, scaled by envelope if set)
    uint8_t waveform_id;      // which built-in waveform, or CUSTOM
    uint16_t noise_lfsr;      // LFSR state (voice 4 only)
    audio_envelope_t env;     // ADSR envelope
    uint32_t release_at_ms;   // auto-release timestamp (0 = disabled)
} audio_voice_t;

// Output routing modes
#define AUDIO_OUTPUT_AUTO     0  // auto-switch on headphone detect (default)
#define AUDIO_OUTPUT_SPEAKER  1  // force speaker only
#define AUDIO_OUTPUT_HEADPHONE 2 // force headphone only
#define AUDIO_OUTPUT_BOTH     3  // both speaker and headphone

// ---- State ----
static PIO audio_pio = pio0;
static int audio_sm = -1;
static int audio_dma_chan = -1;
static uint32_t *audio_ring_buf = NULL;
static volatile size_t audio_write_idx = 0;
static volatile size_t audio_read_idx = 0;
static bool audio_initialized = false;
static audio_voice_t audio_voices[AUDIO_TOTAL_VOICES];
static uint8_t audio_master_vol = 255; // master volume 0-255
static bool audio_hp_inserted = false;    // current headphone state
static uint8_t audio_output_mode = AUDIO_OUTPUT_AUTO; // routing mode
static uint32_t audio_hp_last_check_ms = 0; // polling interval timestamp

// ---- I2C helpers for TLV320 ----

static void audio_i2c_write_reg(uint8_t reg, uint8_t val) {
    uint8_t buf[2] = { reg, val };
    int res = i2c_write_timeout_us(AUDIO_I2C_PORT, AUDIO_I2C_ADDR, buf, 2, false, 1000);
    if (res != 2) {
        Serial.printf("audio: I2C write failed reg=0x%02x val=0x%02x res=%d\n", reg, val, res);
    }
}

static uint8_t audio_i2c_read_reg(uint8_t reg) {
    uint8_t buf = reg;
    i2c_write_timeout_us(AUDIO_I2C_PORT, AUDIO_I2C_ADDR, &buf, 1, true, 1000);
    i2c_read_timeout_us(AUDIO_I2C_PORT, AUDIO_I2C_ADDR, &buf, 1, false, 1000);
    return buf;
}

static void audio_i2c_modify_reg(uint8_t reg, uint8_t mask, uint8_t val) {
    uint8_t cur = audio_i2c_read_reg(reg);
    audio_i2c_write_reg(reg, (cur & ~mask) | (val & mask));
}

static void audio_i2c_set_page(uint8_t page) {
    audio_i2c_write_reg(0x00, page);
}

// ---- Speaker / Headphone output control ----

static void audio_speaker_mute(void) {
    audio_i2c_set_page(1);
    audio_i2c_modify_reg(0x2A, 0x04, 0x00); // SPK unmute bit → 0
    audio_i2c_set_page(0);
}

static void audio_speaker_unmute(void) {
    audio_i2c_set_page(1);
    audio_i2c_modify_reg(0x2A, 0x04, 0x04); // SPK unmute bit → 1
    audio_i2c_set_page(0);
}

static void audio_headphone_mute(void) {
    audio_i2c_set_page(1);
    audio_i2c_modify_reg(0x28, 0x04, 0x00); // HPL unmute → 0
    audio_i2c_modify_reg(0x29, 0x04, 0x00); // HPR unmute → 0
    audio_i2c_set_page(0);
}

static void audio_headphone_unmute(void) {
    audio_i2c_set_page(1);
    audio_i2c_modify_reg(0x28, 0x04, 0x04); // HPL unmute → 1
    audio_i2c_modify_reg(0x29, 0x04, 0x04); // HPR unmute → 1
    audio_i2c_set_page(0);
}

// Apply output routing based on current mode and headphone state
static void audio_apply_output_routing(void) {
    switch (audio_output_mode) {
        case AUDIO_OUTPUT_AUTO:
            if (audio_hp_inserted) {
                audio_speaker_mute();
                audio_headphone_unmute();
                Serial.println("audio: headphone detected → speaker muted");
            } else {
                audio_headphone_unmute(); // always keep HP enabled (no harm if unplugged)
                audio_speaker_unmute();
                Serial.println("audio: headphone removed → speaker unmuted");
            }
            break;
        case AUDIO_OUTPUT_SPEAKER:
            audio_headphone_mute();
            audio_speaker_unmute();
            break;
        case AUDIO_OUTPUT_HEADPHONE:
            audio_speaker_mute();
            audio_headphone_unmute();
            break;
        case AUDIO_OUTPUT_BOTH:
            audio_speaker_unmute();
            audio_headphone_unmute();
            break;
    }
}

// Check TLV320 HSDETECT register and update headphone state.
// Called from fruitjam_audio_fill() every 500ms by polling — no GPIO
// interrupt needed. This avoids the gpio_set_irq_enabled_with_callback
// conflict where registering a second GPIO callback (for HP detect)
// would replace the escape button's callback (only one per core).
static void audio_hp_detect_check(void) {
    audio_i2c_set_page(0);
    uint8_t hs = audio_i2c_read_reg(0x43); // HSDETECT register
    bool connected = (hs & 0x60) != 0x00;  // bits [6:5] = detection status

    // Clear latched interrupt flags (even though we're not using the IRQ,
    // reading these prevents the TLV320 from holding INT1 asserted)
    (void)audio_i2c_read_reg(0x2C);
    (void)audio_i2c_read_reg(0x2D);
    (void)audio_i2c_read_reg(0x2E);
    (void)audio_i2c_read_reg(0x2F);

    if (connected != audio_hp_inserted) {
        audio_hp_inserted = connected;
        audio_apply_output_routing();
    }
}

// ---- MCLK via PWM on GPIO25 ----
// Target: 15 MHz, 50% duty.
// CLK_SYS = 240 MHz after DVHSTX init.
// PWM wrap = (240 MHz / 15 MHz) - 1 = 15.  duty = 8/16 = 50%.

static void audio_mclk_init(void) {
    gpio_set_function(AUDIO_PIN_MCLK, GPIO_FUNC_PWM);
    uint slice = pwm_gpio_to_slice_num(AUDIO_PIN_MCLK);
    pwm_config cfg = pwm_get_default_config();
    // 240 MHz / 16 (wrap+1) = 15 MHz
    pwm_config_set_wrap(&cfg, 15);
    pwm_config_set_clkdiv(&cfg, 1.0f); // no divider
    pwm_init(slice, &cfg, true);
    pwm_set_gpio_level(AUDIO_PIN_MCLK, 8); // 50% duty
}

// ---- TLV320DAC3100 initialization ----
// Based on retroJam audio_i2s_hp.c (fruitjam-doom reference).
// Configures: PLL from MCLK, I2S slave 16-bit, headphone + speaker output.

static void audio_tlv320_reset(void) {
    gpio_init(AUDIO_PIN_RESET);
    gpio_set_dir(AUDIO_PIN_RESET, GPIO_OUT);
    gpio_put(AUDIO_PIN_RESET, 0);
    sleep_us(20);
    gpio_put(AUDIO_PIN_RESET, 1);
    sleep_ms(10);
}

static void audio_tlv320_init(void) {
    // I2C init — 400 kHz
    i2c_init(AUDIO_I2C_PORT, 400000);
    gpio_set_function(20, GPIO_FUNC_I2C); // SDA
    gpio_set_function(21, GPIO_FUNC_I2C); // SCL
    gpio_pull_up(20);
    gpio_pull_up(21);
    sleep_ms(10);

    // Software reset
    audio_i2c_set_page(0);
    audio_i2c_write_reg(0x01, 0x01);
    sleep_ms(10);

    // Interface: I2S, 16-bit, slave mode
    audio_i2c_write_reg(0x1B, 0x00);
    audio_i2c_write_reg(0x1C, 0x00);

    // Clock MUX: codec_clkin = PLL_CLK, PLL_clkin = BCLK
    // Following retroJam exactly — PLL derives its clock from BCLK generated
    // by PIO I2S. MCLK (GPIO25 PWM) is generated but not used as PLL source;
    // it may be needed by some TLV320 modes but BCLK-based PLL is proven.
    //
    // Clock math (retroJam):
    //   BCLK = fs * 32 = 22050 * 32 = 705,600 Hz
    //   PLL = BCLK * R * J / P = 705600 * 2 * 32 / 1 = 45,158,400 Hz
    //   DAC_CLK = PLL / (NDAC * MDAC) = 45158400 / (8 * 2) = 2,822,400 Hz
    //   fs = DAC_CLK / DOSR = 2822400 / 128 = 22050 Hz ✓
    audio_i2c_modify_reg(0x04, 0x03, 0x03); // codec_clkin = PLL_CLK
    audio_i2c_modify_reg(0x04, 0x0C, 0x04); // PLL_clkin = BCLK (bits 3:2 = 01)

    // PLL J = 32 (integer multiplier)
    audio_i2c_write_reg(0x06, 0x20);
    // PLL D = 0 (fractional part = 0)
    audio_i2c_write_reg(0x07, 0x00); // D MSB
    audio_i2c_write_reg(0x08, 0x00); // D LSB

    // PLL P and R: reg 0x05 bits 6:4 = P, bits 3:0 = R
    audio_i2c_modify_reg(0x05, 0x70, 0x10); // P = 1
    audio_i2c_modify_reg(0x05, 0x0F, 0x02); // R = 2

    // NDAC = 8, powered up
    audio_i2c_modify_reg(0x0B, 0x7F, 0x08);
    audio_i2c_modify_reg(0x0B, 0x80, 0x80);

    // MDAC = 2, powered up
    audio_i2c_modify_reg(0x0C, 0x7F, 0x02);
    audio_i2c_modify_reg(0x0C, 0x80, 0x80);

    // NADC = 8, powered up (for ADC, but set for completeness)
    audio_i2c_modify_reg(0x12, 0x7F, 0x08);
    audio_i2c_modify_reg(0x12, 0x80, 0x80);

    // MADC = 2, powered up
    audio_i2c_modify_reg(0x13, 0x7F, 0x02);
    audio_i2c_modify_reg(0x13, 0x80, 0x80);

    // Power up PLL
    audio_i2c_modify_reg(0x05, 0x80, 0x80);

    // Headset and GPIO config
    audio_i2c_set_page(1);
    audio_i2c_modify_reg(0x2E, 0xFF, 0x0B); // MICBIAS
    audio_i2c_set_page(0);
    // Enable headset detect with 256ms debounce.
    // Reg 0x43 bits: [7]=enable, [6:5]=status(RO), [4:2]=headset debounce, [1:0]=button debounce
    // Debounce [4:2]: 000=0ms, 010=16ms, 100=32ms, 101=64ms, 110=128ms, 111=256ms
    audio_i2c_modify_reg(0x43, 0x9C, 0x9C); // enable + 256ms headset debounce
    audio_i2c_modify_reg(0x30, 0xC0, 0x80); // INT1 control: enable, level mode
    audio_i2c_modify_reg(0x33, 0x3C, 0x14); // GPIO1 → INT1 output

    // DAC setup: power up both channels
    audio_i2c_modify_reg(0x3F, 0xC0, 0xC0);

    // DAC routing (page 1)
    // Reg 0x23 = 0x44: route LDAC to mixer (HPL+SPK) and RDAC to mixer (HPR)
    // This enables both headphone and speaker paths; muting controls which is active
    audio_i2c_set_page(1);
    audio_i2c_write_reg(0x23, 0x44);

    // DAC volume (page 0)
    // Register 0x41/0x42: signed int8, 0.5 dB steps. 0x00 = 0 dB.
    // Set to 0 dB — use analog stage gains to balance HP vs speaker.
    audio_i2c_set_page(0);
    audio_i2c_modify_reg(0x40, 0x0C, 0x00); // Unmute DAC
    audio_i2c_write_reg(0x41, 0x00);         // Left DAC vol = 0 dB
    audio_i2c_write_reg(0x42, 0x00);         // Right DAC vol = 0 dB

    // Headphone setup (page 1)
    // Use lower analog volume to keep headphone levels comfortable.
    // Reg 0x24/0x25: HP analog vol, 0x00 = 0 dB, higher = more attenuation
    //   0x00=0dB, 0x08=-4dB, 0x10=-8dB, 0x20=-16dB, 0x30=-24dB
    // Reg 0x28/0x29 bits[6:3]: HP driver gain
    //   0x00=0dB, 0x08=+6dB, 0x10=reserved, 0x40=see datasheet
    audio_i2c_set_page(1);
    audio_i2c_modify_reg(0x1F, 0xC0, 0xC0); // HP driver power up
    audio_i2c_modify_reg(0x28, 0x04, 0x04); // HPL unmute
    audio_i2c_modify_reg(0x29, 0x04, 0x04); // HPR unmute
    audio_i2c_write_reg(0x24, 0x20);         // Left analog HP vol = -16 dB
    audio_i2c_write_reg(0x25, 0x20);         // Right analog HP vol = -16 dB
    audio_i2c_modify_reg(0x28, 0x78, 0x00); // HPL gain = 0 dB
    audio_i2c_modify_reg(0x29, 0x78, 0x00); // HPR gain = 0 dB

    // Speaker amp
    // SPK path: 0 dB DAC + 6 dB Class-D gain + (-15 dB) analog = net -9 dB
    // Reg 0x2A bits[4:3]: 00=6dB, 01=12dB, 10=18dB, 11=24dB; bit[2]=unmute
    audio_i2c_write_reg(0x2A, 0x04);         // SPK unmute + gain 6 dB
    audio_i2c_write_reg(0x20, 0x86);         // Power up Class-D driver
    audio_i2c_write_reg(0x26, 0x9E);         // SPK analog vol = -15 dB, enabled

    // Return to page 0
    audio_i2c_set_page(0);

    sleep_ms(100);
    Serial.println("audio: TLV320 init complete");
}

// ---- PIO I2S init ----

static void audio_i2s_init(void) {
    // Claim a state machine on PIO 0
    audio_sm = pio_claim_unused_sm(audio_pio, true);

    // Load the I2S program
    uint offset = pio_add_program(audio_pio, &audio_i2s_pio_program);

    // Configure SM — match pioasm-generated default config
    pio_sm_config cfg = pio_get_default_sm_config();
    sm_config_set_wrap(&cfg, offset + AUDIO_I2S_WRAP_TARGET, offset + AUDIO_I2S_WRAP);
    sm_config_set_sideset(&cfg, 2, false, false); // 2 side-set bits, no opt, no pindirs
    sm_config_set_out_pins(&cfg, AUDIO_PIN_DIN, 1);
    sm_config_set_sideset_pins(&cfg, AUDIO_PIN_BCLK);
    sm_config_set_out_shift(&cfg, false, true, 32); // shift left, autopull, 32-bit threshold
    sm_config_set_fifo_join(&cfg, PIO_FIFO_JOIN_TX);

    // Clock divider for sample rate.
    // The PIO program uses 2 instructions per bit (out + jmp/set), 32 bits per stereo frame.
    // PIO cycles per frame = 32 * 2 = 64.  PIO clock = sample_rate * 64.
    // retroJam formula: divider = clk_sys * 4 / sample_rate (in 16.8 fixed-point).
    //   240 MHz * 4 / 22050 = 43537.4 → int=170, frac=17 → fs ≈ 22051 Hz.
    uint32_t divider = (uint32_t)((uint64_t)clock_get_hz(clk_sys) * 4 / AUDIO_SAMPLE_RATE);
    sm_config_set_clkdiv_int_frac(&cfg, divider >> 8, divider & 0xFF);

    // Init SM — start at entry_point (offset + 7)
    pio_sm_init(audio_pio, audio_sm, offset + AUDIO_I2S_ENTRY_POINT, &cfg);

    // Set GPIO functions to PIO 0 before setting pin directions
    gpio_set_function(AUDIO_PIN_DIN, GPIO_FUNC_PIO0);
    gpio_set_function(AUDIO_PIN_BCLK, GPIO_FUNC_PIO0);
    gpio_set_function(AUDIO_PIN_WS, GPIO_FUNC_PIO0);

    // Set pin directions: DIN, BCLK, WS are all outputs
    uint32_t pin_mask = (1u << AUDIO_PIN_DIN) | (1u << AUDIO_PIN_BCLK) | (1u << AUDIO_PIN_WS);
    pio_sm_set_pindirs_with_mask(audio_pio, audio_sm, pin_mask, pin_mask);
    pio_sm_set_pins(audio_pio, audio_sm, 0); // clear all

    // Start SM
    pio_sm_set_enabled(audio_pio, audio_sm, true);

    Serial.printf("audio: PIO 0 SM%d, I2S at %d Hz (div=%lu.%lu)\n",
                  audio_sm, AUDIO_SAMPLE_RATE,
                  (unsigned long)(divider >> 8), (unsigned long)(divider & 0xFF));
}

// ---- DMA ----

static void __isr audio_dma_handler(void) {
    dma_hw->ints1 = 1u << audio_dma_chan; // clear interrupt
    audio_read_idx = (audio_read_idx + AUDIO_DMA_BLOCK) & AUDIO_RING_MASK;

    // Check if there's another block ready
    size_t avail = (audio_write_idx - audio_read_idx) & AUDIO_RING_MASK;
    if (avail >= AUDIO_DMA_BLOCK) {
        dma_channel_set_read_addr(audio_dma_chan, &audio_ring_buf[audio_read_idx], false);
        dma_channel_set_trans_count(audio_dma_chan, AUDIO_DMA_BLOCK, true);
    }
}

static void audio_dma_init(void) {
    // Explicitly claim DMA channel 4 to avoid conflicts:
    //   channels 0-2 = HSTX video (claimed by dvhstx)
    //   channel 3 = PIO USB TX (claimed by pio_usb_bus_init on core1)
    // We must NOT use dma_claim_unused_channel() here because it would
    // return 3 (PIO USB hasn't started yet on core1), causing a conflict.
    audio_dma_chan = 4;
    dma_channel_claim(audio_dma_chan);
    Serial.printf("audio: DMA channel %d\n", audio_dma_chan);

    dma_channel_config c = dma_channel_get_default_config(audio_dma_chan);
    channel_config_set_transfer_data_size(&c, DMA_SIZE_32);
    channel_config_set_read_increment(&c, true);
    channel_config_set_write_increment(&c, false);
    channel_config_set_dreq(&c, pio_get_dreq(audio_pio, audio_sm, true)); // TX dreq

    dma_channel_configure(
        audio_dma_chan, &c,
        &audio_pio->txf[audio_sm],        // write to PIO TX FIFO
        &audio_ring_buf[audio_read_idx],   // read from ring buffer
        AUDIO_DMA_BLOCK,                   // transfer count
        false                              // don't start yet
    );

    // Use DMA_IRQ_1 (DMA_IRQ_0 might conflict with other things)
    dma_channel_set_irq1_enabled(audio_dma_chan, true);
    irq_set_exclusive_handler(DMA_IRQ_1, audio_dma_handler);
    irq_set_enabled(DMA_IRQ_1, true);

    // Start first DMA transfer (silence — buffer is zeroed)
    dma_channel_set_read_addr(audio_dma_chan, &audio_ring_buf[audio_read_idx], false);
    dma_channel_set_trans_count(audio_dma_chan, AUDIO_DMA_BLOCK, true);
}

// ---- Ring buffer management ----

// Enqueue a 32-bit stereo sample (left:16 | right:16) into the ring buffer.
// Called from the synthesis fill routine on core0.
static inline void audio_enqueue_sample(uint32_t sample) {
    size_t next = (audio_write_idx + 1) & AUDIO_RING_MASK;
    if (next != audio_read_idx) {
        audio_ring_buf[audio_write_idx] = sample;
        audio_write_idx = next;
        // Kick DMA if idle
        if (!dma_channel_is_busy(audio_dma_chan)) {
            size_t avail = (audio_write_idx - audio_read_idx) & AUDIO_RING_MASK;
            if (avail >= AUDIO_DMA_BLOCK) {
                dma_channel_set_read_addr(audio_dma_chan, &audio_ring_buf[audio_read_idx], false);
                dma_channel_set_trans_count(audio_dma_chan, AUDIO_DMA_BLOCK, true);
            }
        }
    }
}

// ---- Built-in waveform tables ----
// Pre-computed at init time into each voice's wavetable when selected.

static void audio_generate_sine(int8_t *table) {
    for (int i = 0; i < 256; i++) {
        table[i] = (int8_t)(127.0f * sinf(2.0f * 3.14159265f * i / 256.0f));
    }
}

static void audio_generate_square(int8_t *table) {
    for (int i = 0; i < 256; i++) {
        table[i] = (i < 128) ? 127 : -127;
    }
}

static void audio_generate_triangle(int8_t *table) {
    for (int i = 0; i < 256; i++) {
        if (i < 64)        table[i] = (int8_t)(i * 2);         // 0 → 126
        else if (i < 192)  table[i] = (int8_t)(254 - i * 2);   // 126 → -130 → clamp
        else               table[i] = (int8_t)(i * 2 - 510);   // -126 → 0
    }
    // Fix edges to avoid overflow
    table[0] = 0;
    table[64] = 127;
    table[128] = 0;
    table[192] = -127;
}

static void audio_generate_sawtooth(int8_t *table) {
    for (int i = 0; i < 256; i++) {
        table[i] = (int8_t)(i - 128);
    }
}

// Load a built-in waveform into a voice's wavetable
static void audio_set_builtin_waveform(int voice, uint8_t waveform_id) {
    audio_voice_t *v = &audio_voices[voice];
    v->waveform_id = waveform_id;
    switch (waveform_id) {
        case AUDIO_WAVE_SINE:     audio_generate_sine(v->wavetable); break;
        case AUDIO_WAVE_SQUARE:   audio_generate_square(v->wavetable); break;
        case AUDIO_WAVE_TRIANGLE: audio_generate_triangle(v->wavetable); break;
        case AUDIO_WAVE_SAWTOOTH: audio_generate_sawtooth(v->wavetable); break;
        case AUDIO_WAVE_SILENCE:
        default:
            memset(v->wavetable, 0, AUDIO_WAVETABLE_SIZE);
            break;
    }
}

// Set voice frequency from Hz (float). Computes phase increment.
static void audio_set_freq(int voice, float freq) {
    // phase_inc = freq * 2^32 / sample_rate
    audio_voices[voice].phase_inc = (uint32_t)((double)freq * 4294967296.0 / AUDIO_SAMPLE_RATE);
}

// Convert milliseconds to 16.16 fixed-point increment/decrement per sample.
// For attack: we go from 0 to 255 (0x00FF0000 in 16.16) over ms milliseconds.
// samples = ms * AUDIO_SAMPLE_RATE / 1000
// inc = 0x00FF0000 / samples
static uint32_t audio_ms_to_inc(uint32_t ms) {
    if (ms == 0) return 0x00FF0000; // instant (one sample)
    uint32_t samples = (uint32_t)((uint64_t)ms * AUDIO_SAMPLE_RATE / 1000);
    if (samples == 0) samples = 1;
    return 0x00FF0000 / samples;
}

// Configure ADSR envelope for a voice. Times in ms, sustain 0-255.
static void audio_set_envelope(int voice, uint32_t attack_ms, uint32_t decay_ms,
                                uint8_t sustain, uint32_t release_ms) {
    audio_envelope_t *e = &audio_voices[voice].env;
    e->attack_inc = audio_ms_to_inc(attack_ms);
    // Decay goes from 255 down to sustain_level
    uint32_t decay_range = (255 - sustain);
    if (decay_ms == 0 || decay_range == 0) {
        e->decay_dec = 0x00FF0000; // instant
    } else {
        uint32_t samples = (uint32_t)((uint64_t)decay_ms * AUDIO_SAMPLE_RATE / 1000);
        if (samples == 0) samples = 1;
        e->decay_dec = (decay_range << 16) / samples;
    }
    e->sustain_level = sustain;
    e->release_dec = audio_ms_to_inc(release_ms);
    e->enabled = true;
    e->stage = ADSR_OFF;
    e->env_level = 0;
}

// Clear envelope for a voice (return to constant volume mode)
static void audio_clear_envelope(int voice) {
    memset(&audio_voices[voice].env, 0, sizeof(audio_envelope_t));
}

// Trigger envelope attack phase
static void audio_trigger(int voice) {
    audio_envelope_t *e = &audio_voices[voice].env;
    if (!e->enabled) return;
    e->stage = ADSR_ATTACK;
    e->env_level = 0;
}

// Start envelope release phase
static void audio_release(int voice) {
    audio_envelope_t *e = &audio_voices[voice].env;
    if (!e->enabled) return;
    if (e->stage != ADSR_DONE && e->stage != ADSR_OFF) {
        e->stage = ADSR_RELEASE;
    }
}

// Advance envelope by one sample, return level 0-255
static inline uint8_t audio_envelope_tick(audio_envelope_t *e) {
    switch (e->stage) {
        case ADSR_OFF:
            return 255; // no envelope = full volume
        case ADSR_ATTACK:
            e->env_level += e->attack_inc;
            if (e->env_level >= 0x00FF0000) {
                e->env_level = 0x00FF0000;
                e->stage = ADSR_DECAY;
            }
            break;
        case ADSR_DECAY: {
            uint32_t target = (uint32_t)e->sustain_level << 16;
            if (e->env_level > e->decay_dec + target) {
                e->env_level -= e->decay_dec;
            } else {
                e->env_level = target;
                e->stage = ADSR_SUSTAIN;
            }
            break;
        }
        case ADSR_SUSTAIN:
            // Hold at sustain level — env_level stays constant
            break;
        case ADSR_RELEASE:
            if (e->env_level > e->release_dec) {
                e->env_level -= e->release_dec;
            } else {
                e->env_level = 0;
                e->stage = ADSR_DONE;
            }
            break;
        case ADSR_DONE:
            return 0;
    }
    return (uint8_t)(e->env_level >> 16);
}

// Initialize all voices to silence
static void audio_voices_init(void) {
    memset(audio_voices, 0, sizeof(audio_voices));
    for (int i = 0; i < AUDIO_TOTAL_VOICES; i++) {
        audio_voices[i].noise_lfsr = 0xACE1; // seed LFSR
    }
}

// ---- Terminal bell — audio parameters & voice init ----
// Bell uses BELL_VOICE (voice 5), a private voice not accessible from Lisp.
#define BELL_MIDI_NOTE   84     // C6 (~1047 Hz) — a clear, attention-getting pitch
#define BELL_DURATION_MS 60     // short blip
#define BELL_VOLUME      80     // moderate volume (0-255)

// Set up the private bell voice.  Called from fruitjam_audio_init() and
// the escape handler (to restore after silencing all voices).
static void bell_voice_init() {
    audio_set_builtin_waveform(BELL_VOICE, AUDIO_WAVE_SINE);
    audio_voices[BELL_VOICE].volume = BELL_VOLUME;
}

// ---- Public API ----

// Silence voices: zeros volume, phase, phase_inc, envelope state, and
// release timer. When include_bell is true, silences all AUDIO_TOTAL_VOICES
// (including the private bell voice); when false, only the AUDIO_NUM_VOICES
// user-accessible voices.
static void fruitjam_audio_silence(bool include_bell) {
    int count = include_bell ? AUDIO_TOTAL_VOICES : AUDIO_NUM_VOICES;
    for (int i = 0; i < count; i++) {
        audio_voices[i].volume = 0;
        audio_voices[i].phase = 0;
        audio_voices[i].phase_inc = 0;
        audio_voices[i].env.stage = ADSR_OFF;
        audio_voices[i].env.env_level = 0;
        audio_voices[i].release_at_ms = 0;
    }
}

// Initialize the entire audio subsystem. Call once from initgfx() after
// display and WiFi are initialized.
static void fruitjam_audio_init(void) {
    if (audio_initialized) return;

    Serial.println("audio: initializing...");

    // Initialize voice state
    audio_voices_init();

    // Allocate ring buffer
    audio_ring_buf = (uint32_t *)malloc(AUDIO_RING_SIZE * sizeof(uint32_t));
    if (!audio_ring_buf) {
        Serial.println("audio: ring buffer alloc failed!");
        return;
    }
    memset(audio_ring_buf, 0, AUDIO_RING_SIZE * sizeof(uint32_t));
    // Pre-fill entire buffer with silence so DMA has data from the start
    audio_write_idx = AUDIO_RING_SIZE - 1;
    audio_read_idx = 0;

    // 1. MCLK — start before DAC init so PLL has a reference
    audio_mclk_init();
    Serial.println("audio: MCLK running at 15 MHz on GPIO25");

    // 2. TLV320 DAC — reset and configure
    audio_tlv320_reset();
    audio_tlv320_init();

    // 3. PIO I2S — start generating BCLK/WS/DIN
    audio_i2s_init();

    // 4. DMA — start streaming silence from ring buffer
    audio_dma_init();

    // 5. Headphone detection — polled from fruitjam_audio_fill() every 500ms.
    // We do NOT use gpio_set_irq_enabled_with_callback here because the
    // RP2350 allows only one GPIO IRQ callback per core, and the escape
    // button (GPIO0) already registered one via attachInterrupt().
    Serial.println("audio: headphone detect via polling");

    audio_initialized = true;

    // Set up the private bell voice (sine wave, fixed volume)
    bell_voice_init();

    // Apply initial output routing unconditionally (don't rely on state change detection)
    audio_hp_detect_check();
    audio_apply_output_routing();

    Serial.println("audio: initialization complete");
}

// ---- Synthesis ----

// Fill the ring buffer with mixed audio from all voices.
// Call from testescape() to keep the buffer ahead of DMA consumption.
static void fruitjam_audio_fill(void) {
    if (!audio_initialized) return;

    // Poll headphone detect every 500ms via I2C register read
    uint32_t now = millis();
    if (now - audio_hp_last_check_ms >= 500) {
        audio_hp_last_check_ms = now;
        audio_hp_detect_check();
    }

    // Check auto-release timers for all voices (including bell voice)
    for (int v = 0; v < AUDIO_TOTAL_VOICES; v++) {
        audio_voice_t *voice = &audio_voices[v];
        if (voice->release_at_ms != 0 && now >= voice->release_at_ms) {
            voice->release_at_ms = 0;
            if (voice->env.enabled) {
                // Envelope active: start release phase (fades to silence)
                audio_release(v);
            } else {
                // No envelope: stop oscillator but preserve volume setting
                // so the next audio-note on this voice plays at the same level
                voice->phase_inc = 0;
            }
        }
    }

    // How many samples can we write? Fill as much as possible to stay
    // ahead of DMA consumption and avoid underruns.
    size_t avail = (audio_read_idx - audio_write_idx - 1) & AUDIO_RING_MASK;

    for (size_t i = 0; i < avail; i++) {
        int32_t mix = 0;

        for (int v = 0; v < AUDIO_TOTAL_VOICES; v++) {
            audio_voice_t *voice = &audio_voices[v];
            if (voice->phase_inc == 0) continue;

            // Compute effective volume: base volume × envelope level
            uint8_t env_level = audio_envelope_tick(&voice->env);
            uint8_t eff_vol = (uint8_t)(((uint16_t)voice->volume * env_level) >> 8);
            if (eff_vol == 0) continue;

            int8_t sample;
            if (v == 4 && voice->waveform_id == AUDIO_WAVE_NOISE) {
                // Noise: advance LFSR at rate controlled by phase_inc
                voice->phase += voice->phase_inc;
                // When phase wraps past bit 24, clock the LFSR
                // This gives noise "pitch" — higher freq = more LFSR clocks per sample
                while (voice->phase >= (1u << 24)) {
                    voice->phase -= (1u << 24);
                    // 16-bit Galois LFSR, taps at bits 16,14,13,11
                    uint16_t lsb = voice->noise_lfsr & 1;
                    voice->noise_lfsr >>= 1;
                    if (lsb) voice->noise_lfsr ^= 0xB400;
                }
                sample = (int8_t)(voice->noise_lfsr & 0xFF);
            } else {
                // Wavetable: top 8 bits of phase index the 256-entry table
                sample = voice->wavetable[(voice->phase >> 24) & 0xFF];
                voice->phase += voice->phase_inc;
            }

            // Apply per-voice volume: sample (-128..127) * eff_vol (0..255) → range ±32385
            mix += ((int16_t)sample * eff_vol);
        }

        // Apply master volume: mix is sum of up to 5 voices × ±32385 = ±161925
        // After master vol: ±161925 * 255 / 256 ≈ ±161290
        mix = (mix * audio_master_vol) >> 8;

        // Clamp to 16-bit range
        if (mix > 32767) mix = 32767;
        if (mix < -32768) mix = -32768;

        int16_t sample16 = (int16_t)mix;
        // Pack stereo: left in upper 16 bits, right in lower 16 bits
        uint32_t stereo = ((uint16_t)sample16 << 16) | (uint16_t)sample16;
        audio_enqueue_sample(stereo);
    }
}

// ---- Terminal bell — audio + visual flash management ----
// Called from testescape() to handle bell sound and flash timer.
// The visual flash is started immediately in term_putchar() (in fruitjam_terminal.h),
// and this function handles:
//   1. Triggering the audio blip when bell_pending is set
//   2. Un-flashing the screen after BELL_FLASH_DURATION_MS
//
static void fruitjam_bell_tick() {
    // Handle pending bell trigger (sound)
    if (bell_pending) {
        bell_pending = false;
        if (audio_initialized) {
            // Play a self-releasing note on the private bell voice
            float freq = 440.0f * powf(2.0f, (BELL_MIDI_NOTE - 69) / 12.0f);
            audio_set_freq(BELL_VOICE, freq);
            audio_voices[BELL_VOICE].release_at_ms = millis() + BELL_DURATION_MS;
        }
    }

    // Handle flash timer — unflash after duration expires
    if (bell_flash_active) {
        if (millis() - bell_flash_start_ms >= BELL_FLASH_DURATION_MS) {
            term_bell_unflash();
        }
    }
}

#endif // FRUITJAM_AUDIO_H
