#!/usr/bin/env python3
"""
Generate a 256-glyph CP437-layout font header from unscii-8-thin.hex.

Slots 0x00–0x7F are plain ASCII (taken directly from the font file).
Slots 0x80–0xFF follow the classic IBM Code Page 437 mapping, with the
control-code range 0x01–0x1F also populated with the CP437 graphical
characters (☺ ♥ ♠ ♪ ► ↑ etc.) — just like DOS.

Three glyphs missing from unscii-8-thin are filled with close substitutes:
  CP437 0x0F  ☼  U+263C → U+2609 ☉ (circled dot)
  CP437 0xA9  ⌐  U+2310 → horizontally flipped U+00AC ¬
  CP437 0xF9  ∙  U+2219 → U+00B7 · (middle dot)

Usage:
    python3 gen_cp437_font.py /path/to/unscii-8-thin.hex ../fruitjam_font.h
"""

import sys
import os

# ── CP437 byte → Unicode codepoint mapping ─────────────────────────────
# 0x01–0x1F: graphical characters that CP437 displays instead of control codes
CP437_CTRL = {
    0x01: 0x263A,  # ☺
    0x02: 0x263B,  # ☻
    0x03: 0x2665,  # ♥
    0x04: 0x2666,  # ♦
    0x05: 0x2663,  # ♣
    0x06: 0x2660,  # ♠
    0x07: 0x2022,  # •
    0x08: 0x25D8,  # ◘
    0x09: 0x25CB,  # ○
    0x0A: 0x25D9,  # ◙
    0x0B: 0x2642,  # ♂
    0x0C: 0x2640,  # ♀
    0x0D: 0x266A,  # ♪
    0x0E: 0x266B,  # ♫
    0x0F: 0x263C,  # ☼  (missing — substitute below)
    0x10: 0x25BA,  # ►
    0x11: 0x25C4,  # ◄
    0x12: 0x2195,  # ↕
    0x13: 0x203C,  # ‼
    0x14: 0x00B6,  # ¶
    0x15: 0x00A7,  # §
    0x16: 0x25AC,  # ▬
    0x17: 0x21A8,  # ↨
    0x18: 0x2191,  # ↑
    0x19: 0x2193,  # ↓
    0x1A: 0x2192,  # →
    0x1B: 0x2190,  # ←
    0x1C: 0x221F,  # ∟
    0x1D: 0x2194,  # ↔
    0x1E: 0x25B2,  # ▲
    0x1F: 0x25BC,  # ▼
}

# 0x80–0xFF: the extended CP437 characters
CP437_HIGH = {
    0x80: 0x00C7,  # Ç
    0x81: 0x00FC,  # ü
    0x82: 0x00E9,  # é
    0x83: 0x00E2,  # â
    0x84: 0x00E4,  # ä
    0x85: 0x00E0,  # à
    0x86: 0x00E5,  # å
    0x87: 0x00E7,  # ç
    0x88: 0x00EA,  # ê
    0x89: 0x00EB,  # ë
    0x8A: 0x00E8,  # è
    0x8B: 0x00EF,  # ï
    0x8C: 0x00EE,  # î
    0x8D: 0x00EC,  # ì
    0x8E: 0x00C4,  # Ä
    0x8F: 0x00C5,  # Å
    0x90: 0x00C9,  # É
    0x91: 0x00E6,  # æ
    0x92: 0x00C6,  # Æ
    0x93: 0x00F4,  # ô
    0x94: 0x00F6,  # ö
    0x95: 0x00F2,  # ò
    0x96: 0x00FB,  # û
    0x97: 0x00F9,  # ù
    0x98: 0x00FF,  # ÿ
    0x99: 0x00D6,  # Ö
    0x9A: 0x00DC,  # Ü
    0x9B: 0x00A2,  # ¢
    0x9C: 0x00A3,  # £
    0x9D: 0x00A5,  # ¥
    0x9E: 0x20A7,  # ₧
    0x9F: 0x0192,  # ƒ
    0xA0: 0x00E1,  # á
    0xA1: 0x00ED,  # í
    0xA2: 0x00F3,  # ó
    0xA3: 0x00FA,  # ú
    0xA4: 0x00F1,  # ñ
    0xA5: 0x00D1,  # Ñ
    0xA6: 0x00AA,  # ª
    0xA7: 0x00BA,  # º
    0xA8: 0x00BF,  # ¿
    0xA9: 0x2310,  # ⌐  (missing — substitute below)
    0xAA: 0x00AC,  # ¬
    0xAB: 0x00BD,  # ½
    0xAC: 0x00BC,  # ¼
    0xAD: 0x00A1,  # ¡
    0xAE: 0x00AB,  # «
    0xAF: 0x00BB,  # »
    0xB0: 0x2591,  # ░
    0xB1: 0x2592,  # ▒
    0xB2: 0x2593,  # ▓
    0xB3: 0x2502,  # │
    0xB4: 0x2524,  # ┤
    0xB5: 0x2561,  # ╡
    0xB6: 0x2562,  # ╢
    0xB7: 0x2556,  # ╖
    0xB8: 0x2555,  # ╕
    0xB9: 0x2563,  # ╣
    0xBA: 0x2551,  # ║
    0xBB: 0x2557,  # ╗
    0xBC: 0x255D,  # ╝
    0xBD: 0x255C,  # ╜
    0xBE: 0x255B,  # ╛
    0xBF: 0x2510,  # ┐
    0xC0: 0x2514,  # └
    0xC1: 0x2534,  # ┴
    0xC2: 0x252C,  # ┬
    0xC3: 0x251C,  # ├
    0xC4: 0x2500,  # ─
    0xC5: 0x253C,  # ┼
    0xC6: 0x255E,  # ╞
    0xC7: 0x255F,  # ╟
    0xC8: 0x255A,  # ╚
    0xC9: 0x2554,  # ╔
    0xCA: 0x2569,  # ╩
    0xCB: 0x2566,  # ╦
    0xCC: 0x2560,  # ╠
    0xCD: 0x2550,  # ═
    0xCE: 0x256C,  # ╬
    0xCF: 0x2567,  # ╧
    0xD0: 0x2568,  # ╨
    0xD1: 0x2564,  # ╤
    0xD2: 0x2565,  # ╥
    0xD3: 0x2559,  # ╙
    0xD4: 0x2558,  # ╘
    0xD5: 0x2552,  # ╒
    0xD6: 0x2553,  # ╓
    0xD7: 0x256B,  # ╫
    0xD8: 0x256A,  # ╪
    0xD9: 0x2518,  # ┘
    0xDA: 0x250C,  # ┌
    0xDB: 0x2588,  # █
    0xDC: 0x2584,  # ▄
    0xDD: 0x258C,  # ▌
    0xDE: 0x2590,  # ▐
    0xDF: 0x2580,  # ▀
    0xE0: 0x03B1,  # α
    0xE1: 0x00DF,  # ß
    0xE2: 0x0393,  # Γ
    0xE3: 0x03C0,  # π
    0xE4: 0x03A3,  # Σ
    0xE5: 0x03C3,  # σ
    0xE6: 0x00B5,  # µ
    0xE7: 0x03C4,  # τ
    0xE8: 0x03A6,  # Φ
    0xE9: 0x0398,  # Θ
    0xEA: 0x03A9,  # Ω
    0xEB: 0x03B4,  # δ
    0xEC: 0x221E,  # ∞
    0xED: 0x03C6,  # φ
    0xEE: 0x03B5,  # ε
    0xEF: 0x2229,  # ∩
    0xF0: 0x2261,  # ≡
    0xF1: 0x00B1,  # ±
    0xF2: 0x2265,  # ≥
    0xF3: 0x2264,  # ≤
    0xF4: 0x2320,  # ⌠
    0xF5: 0x2321,  # ⌡
    0xF6: 0x00F7,  # ÷
    0xF7: 0x2248,  # ≈
    0xF8: 0x00B0,  # °
    0xF9: 0x2219,  # ∙  (missing — substitute below)
    0xFA: 0x00B7,  # ·
    0xFB: 0x221A,  # √
    0xFC: 0x207F,  # ⁿ
    0xFD: 0x00B2,  # ²
    0xFE: 0x25A0,  # ■
    0xFF: 0x00A0,  #   (NBSP → blank)
}


def hflip_glyph(rows):
    """Horizontally flip an 8-pixel-wide glyph (list of 8 ints)."""
    flipped = []
    for byte in rows:
        out = 0
        for bit in range(8):
            if byte & (1 << bit):
                out |= (1 << (7 - bit))
        flipped.append(out)
    return flipped


def parse_hex_font(path):
    """Parse unscii .hex → {unicode_codepoint: [8 bytes]}."""
    glyphs = {}
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            cp_str, bmp_str = line.split(':')
            cp = int(cp_str, 16)
            if len(bmp_str) == 16:
                glyphs[cp] = [int(bmp_str[i:i+2], 16) for i in range(0, 16, 2)]
            elif len(bmp_str) == 0:
                glyphs[cp] = [0] * 8
    return glyphs


# ── Friendly names for comments ────────────────────────────────────────

# Names for 0x01–0x1F (CP437 graphical chars in control-code slots)
CTRL_NAMES = {
    0x00: "NUL (blank)",
    0x01: "☺ smiley",
    0x02: "☻ smiley inv",
    0x03: "♥ heart",
    0x04: "♦ diamond",
    0x05: "♣ club",
    0x06: "♠ spade",
    0x07: "• bullet",
    0x08: "◘ inv bullet",
    0x09: "○ circle",
    0x0A: "◙ inv circle",
    0x0B: "♂ male",
    0x0C: "♀ female",
    0x0D: "♪ note",
    0x0E: "♫ notes",
    0x0F: "☼ sun",
    0x10: "► tri right",
    0x11: "◄ tri left",
    0x12: "↕ updown",
    0x13: "‼ dbl exclam",
    0x14: "¶ pilcrow",
    0x15: "§ section",
    0x16: "▬ bar",
    0x17: "↨ updown base",
    0x18: "↑ up",
    0x19: "↓ down",
    0x1A: "→ right",
    0x1B: "← left",
    0x1C: "∟ right angle",
    0x1D: "↔ leftright",
    0x1E: "▲ tri up",
    0x1F: "▼ tri down",
}

HIGH_NAMES = {
    0x80: "Ç c-cedilla",    0x81: "ü u-diaeresis",  0x82: "é e-acute",
    0x83: "â a-circumflex", 0x84: "ä a-diaeresis",  0x85: "à a-grave",
    0x86: "å a-ring",       0x87: "ç c-cedilla",    0x88: "ê e-circumflex",
    0x89: "ë e-diaeresis",  0x8A: "è e-grave",      0x8B: "ï i-diaeresis",
    0x8C: "î i-circumflex", 0x8D: "ì i-grave",      0x8E: "Ä A-diaeresis",
    0x8F: "Å A-ring",       0x90: "É E-acute",      0x91: "æ ae-ligature",
    0x92: "Æ AE-ligature",  0x93: "ô o-circumflex", 0x94: "ö o-diaeresis",
    0x95: "ò o-grave",      0x96: "û u-circumflex",  0x97: "ù u-grave",
    0x98: "ÿ y-diaeresis",  0x99: "Ö O-diaeresis",  0x9A: "Ü U-diaeresis",
    0x9B: "¢ cent",         0x9C: "£ pound",         0x9D: "¥ yen",
    0x9E: "₧ peseta",       0x9F: "ƒ florin",
    0xA0: "á a-acute",      0xA1: "í i-acute",      0xA2: "ó o-acute",
    0xA3: "ú u-acute",      0xA4: "ñ n-tilde",      0xA5: "Ñ N-tilde",
    0xA6: "ª fem ordinal",  0xA7: "º masc ordinal", 0xA8: "¿ inv question",
    0xA9: "⌐ rev not",      0xAA: "¬ not",          0xAB: "½ one-half",
    0xAC: "¼ one-quarter",  0xAD: "¡ inv exclam",
    0xAE: "« left guillemet",  0xAF: "» right guillemet",
    0xB0: "░ light shade",  0xB1: "▒ med shade",    0xB2: "▓ dark shade",
    0xB3: "│ vert",         0xB4: "┤ right tee",    0xB5: "╡ dbl right tee",
    0xB6: "╢ dbl right tee",0xB7: "╖ dbl down-left",0xB8: "╕ dbl down-left",
    0xB9: "╣ dbl right tee",0xBA: "║ dbl vert",     0xBB: "╗ dbl down-left",
    0xBC: "╝ dbl up-left",  0xBD: "╜ dbl up-left",  0xBE: "╛ dbl up-left",
    0xBF: "┐ down-left",    0xC0: "└ up-right",     0xC1: "┴ up tee",
    0xC2: "┬ down tee",     0xC3: "├ left tee",     0xC4: "─ horiz",
    0xC5: "┼ cross",        0xC6: "╞ dbl left tee", 0xC7: "╟ dbl left tee",
    0xC8: "╚ dbl up-right", 0xC9: "╔ dbl down-right",
    0xCA: "╩ dbl up tee",   0xCB: "╦ dbl down tee",
    0xCC: "╠ dbl left tee", 0xCD: "═ dbl horiz",    0xCE: "╬ dbl cross",
    0xCF: "╧ dbl up tee",   0xD0: "╨ dbl up tee",
    0xD1: "╤ dbl down tee", 0xD2: "╥ dbl down tee",
    0xD3: "╙ dbl up-right", 0xD4: "╘ dbl up-right",
    0xD5: "╒ dbl down-right", 0xD6: "╓ dbl down-right",
    0xD7: "╫ dbl cross",    0xD8: "╪ dbl cross",
    0xD9: "┘ up-left",      0xDA: "┌ down-right",
    0xDB: "█ full block",   0xDC: "▄ lower half",   0xDD: "▌ left half",
    0xDE: "▐ right half",   0xDF: "▀ upper half",
    0xE0: "α alpha",        0xE1: "ß beta/eszett",  0xE2: "Γ Gamma",
    0xE3: "π pi",           0xE4: "Σ Sigma",        0xE5: "σ sigma",
    0xE6: "µ mu/micro",     0xE7: "τ tau",          0xE8: "Φ Phi",
    0xE9: "Θ Theta",        0xEA: "Ω Omega",        0xEB: "δ delta",
    0xEC: "∞ infinity",     0xED: "φ phi",           0xEE: "ε epsilon",
    0xEF: "∩ intersection",
    0xF0: "≡ identical",    0xF1: "± plus-minus",   0xF2: "≥ greater-equal",
    0xF3: "≤ less-equal",   0xF4: "⌠ integral top", 0xF5: "⌡ integral bot",
    0xF6: "÷ division",     0xF7: "≈ approx",       0xF8: "° degree",
    0xF9: "∙ bullet op",    0xFA: "· middle dot",   0xFB: "√ sqrt",
    0xFC: "ⁿ superscript n",0xFD: "² superscript 2",0xFE: "■ filled square",
    0xFF: "NBSP",
}


def comment_for(cp):
    """Return a comment string for CP437 byte cp."""
    if cp == 0x00:
        return "NUL (blank)"
    if cp in CTRL_NAMES:
        return CTRL_NAMES[cp]
    if 0x20 <= cp <= 0x7E:
        ch = chr(cp)
        return f"'{ch}'"
    if cp == 0x7F:
        return "DEL"
    if cp in HIGH_NAMES:
        return HIGH_NAMES[cp]
    return ""


def main():
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <unscii-8-thin.hex> <output.h>", file=sys.stderr)
        sys.exit(1)

    hex_path = sys.argv[1]
    out_path = sys.argv[2]

    glyphs = parse_hex_font(hex_path)
    print(f"Loaded {len(glyphs)} glyphs from {os.path.basename(hex_path)}",
          file=sys.stderr)

    # ── Substitutions for the 3 missing glyphs ──
    # CP437 0x0F ☼ (U+263C) → U+2609 ☉ (sun / circled dot)
    subs = {
        0x263C: glyphs.get(0x2609, [0]*8),
    }
    # CP437 0xA9 ⌐ (U+2310) → horizontally flipped ¬ (U+00AC)
    not_glyph = glyphs.get(0x00AC, [0]*8)
    subs[0x2310] = hflip_glyph(not_glyph)
    # CP437 0xF9 ∙ (U+2219) → U+00B7 · (middle dot)
    subs[0x2219] = glyphs.get(0x00B7, [0]*8)

    # ── Build the 256-entry table ──
    table = []  # list of 256 × [8 bytes]

    for byte in range(256):
        if byte == 0x00:
            # NUL → blank
            table.append([0]*8)
        elif byte in CP437_CTRL:
            uni = CP437_CTRL[byte]
            if uni in glyphs:
                table.append(glyphs[uni])
            elif uni in subs:
                table.append(subs[uni])
            else:
                print(f"  WARNING: no glyph for 0x{byte:02X} (U+{uni:04X})",
                      file=sys.stderr)
                table.append([0]*8)
        elif 0x20 <= byte <= 0x7F:
            # Standard ASCII — use the codepoint directly
            table.append(glyphs.get(byte, [0]*8))
        elif byte in CP437_HIGH:
            uni = CP437_HIGH[byte]
            if uni in glyphs:
                table.append(glyphs[uni])
            elif uni in subs:
                table.append(subs[uni])
            else:
                print(f"  WARNING: no glyph for 0x{byte:02X} (U+{uni:04X})",
                      file=sys.stderr)
                table.append([0]*8)
        else:
            table.append([0]*8)

    assert len(table) == 256

    # ── Emit the header ──
    lines = []
    lines.append("// fruitjam_font.h — unscii-8-thin bitmap font + rendering functions")
    lines.append("//")
    lines.append("// Font data auto-generated from unscii-8-thin.hex by tools/gen_cp437_font.py")
    lines.append("// Rendering functions hand-written for direct 8bpp framebuffer access (DVHSTX8).")
    lines.append("//")
    lines.append("// 8×8 row-major bitmap font, 256 glyphs in CP437 layout")
    lines.append("// 8 bytes per glyph, one byte per row, MSB = leftmost pixel")
    lines.append("// Total: 2048 bytes")
    lines.append("//")
    lines.append("// Slots 0x00–0x7F: standard ASCII")
    lines.append("// Slots 0x01–0x1F: CP437 graphical characters (☺♥♠♪► etc.)")
    lines.append("// Slots 0x80–0xFF: CP437 extended (accented, box-drawing, blocks, Greek, math)")
    lines.append("//")
    lines.append("// Used by:")
    lines.append("//   - fruitjam_terminal.h (term_draw_cell) for terminal text")
    lines.append("//   - ulisp-fruit-jam.ino (fn_drawchar, gfxwrite) for graphics-mode text")
    lines.append("")
    lines.append("#ifndef FRUITJAM_FONT_H")
    lines.append("#define FRUITJAM_FONT_H")
    lines.append("")
    lines.append("#include <stdint.h>")
    lines.append("")
    lines.append("static const uint8_t unscii_8_thin[256 * 8] = {")

    for byte in range(256):
        rows = table[byte]
        hex_str = ", ".join(f"0x{b:02X}" for b in rows)
        cmt = comment_for(byte)
        lines.append(f"  {hex_str},  // 0x{byte:02X} {cmt}")

    lines.append("};")
    lines.append("")

    # ── Rendering functions (emitted verbatim) ──
    lines.append("// ---- Rendering functions ----")
    lines.append("// These write directly into an 8bpp framebuffer (uint8_t* buffer, stride = screen width).")
    lines.append("// They are used by both the terminal emulator and graphics-mode text functions.")
    lines.append("// The display8 object (DVHSTX8) is declared in fruitjam_terminal.h and must be")
    lines.append("// available when these functions are called.")
    lines.append("")
    lines.append("// Draw a single 8×8 character at pixel position (x, y) with foreground/background colors.")
    lines.append("// No clipping — caller must ensure (x, y) is within the framebuffer.")
    lines.append("// This is the fast path for terminal rendering (size=1, no clipping needed).")
    lines.append("static void fruitjam_draw_char_8x8(uint8_t *fb, int fb_width,")
    lines.append("                                    int x, int y, unsigned char c,")
    lines.append("                                    uint8_t fg, uint8_t bg) {")
    lines.append("  const uint8_t *glyph = &unscii_8_thin[c * 8];")
    lines.append("  uint8_t *row_ptr = fb + y * fb_width + x;")
    lines.append("  for (int row = 0; row < 8; row++) {")
    lines.append("    uint8_t bits = glyph[row];")
    lines.append("    row_ptr[0] = (bits & 0x80) ? fg : bg;")
    lines.append("    row_ptr[1] = (bits & 0x40) ? fg : bg;")
    lines.append("    row_ptr[2] = (bits & 0x20) ? fg : bg;")
    lines.append("    row_ptr[3] = (bits & 0x10) ? fg : bg;")
    lines.append("    row_ptr[4] = (bits & 0x08) ? fg : bg;")
    lines.append("    row_ptr[5] = (bits & 0x04) ? fg : bg;")
    lines.append("    row_ptr[6] = (bits & 0x02) ? fg : bg;")
    lines.append("    row_ptr[7] = (bits & 0x01) ? fg : bg;")
    lines.append("    row_ptr += fb_width;")
    lines.append("  }")
    lines.append("}")
    lines.append("")
    lines.append("// Draw a single 8×8 character with transparent background (foreground pixels only).")
    lines.append("// Used when set-text-color is called with one argument (GFX convention: fg == bg).")
    lines.append("static void fruitjam_draw_char_8x8_transparent(uint8_t *fb, int fb_width,")
    lines.append("                                                int fb_height,")
    lines.append("                                                int x, int y, unsigned char c,")
    lines.append("                                                uint8_t fg) {")
    lines.append("  if (x >= fb_width || y >= fb_height || (x + 8) <= 0 || (y + 8) <= 0) return;")
    lines.append("  const uint8_t *glyph = &unscii_8_thin[c * 8];")
    lines.append("  for (int row = 0; row < 8; row++) {")
    lines.append("    int py = y + row;")
    lines.append("    if (py < 0 || py >= fb_height) continue;")
    lines.append("    uint8_t bits = glyph[row];")
    lines.append("    uint8_t *row_ptr = fb + py * fb_width + x;")
    lines.append("    for (int col = 0; col < 8; col++) {")
    lines.append("      int px = x + col;")
    lines.append("      if (px >= 0 && px < fb_width && (bits & (0x80 >> col)))")
    lines.append("        row_ptr[col] = fg;")
    lines.append("    }")
    lines.append("  }")
    lines.append("}")
    lines.append("")
    lines.append("// Draw a scaled 8×8 character at pixel position (x, y).")
    lines.append("// Each pixel is expanded to (size × size) pixels.")
    lines.append("// Includes clipping against (clip_w, clip_h) screen bounds.")
    lines.append("// Used by graphics-mode (draw-char x y c [fg bg size]) for size >= 1.")
    lines.append("static void fruitjam_draw_char_8x8_scaled(uint8_t *fb, int fb_width, int fb_height,")
    lines.append("                                           int x, int y, unsigned char c,")
    lines.append("                                           uint8_t fg, uint8_t bg, int size) {")
    lines.append("  if (size < 1) size = 1;")
    lines.append("  // Quick reject if entirely off-screen")
    lines.append("  if (x >= fb_width || y >= fb_height ||")
    lines.append("      (x + 8 * size) <= 0 || (y + 8 * size) <= 0) return;")
    lines.append("")
    lines.append("  const uint8_t *glyph = &unscii_8_thin[c * 8];")
    lines.append("  for (int row = 0; row < 8; row++) {")
    lines.append("    uint8_t bits = glyph[row];")
    lines.append("    for (int col = 0; col < 8; col++) {")
    lines.append("      uint8_t color = (bits & (0x80 >> col)) ? fg : bg;")
    lines.append("      // Fill the size×size block for this pixel")
    lines.append("      for (int sy = 0; sy < size; sy++) {")
    lines.append("        int py = y + row * size + sy;")
    lines.append("        if (py < 0 || py >= fb_height) continue;")
    lines.append("        for (int sx = 0; sx < size; sx++) {")
    lines.append("          int px = x + col * size + sx;")
    lines.append("          if (px < 0 || px >= fb_width) continue;")
    lines.append("          fb[py * fb_width + px] = color;")
    lines.append("        }")
    lines.append("      }")
    lines.append("    }")
    lines.append("  }")
    lines.append("}")
    lines.append("")
    lines.append("// Draw a scaled 8×8 character with transparent background (foreground pixels only).")
    lines.append("static void fruitjam_draw_char_8x8_scaled_transparent(uint8_t *fb, int fb_width,")
    lines.append("                                                       int fb_height,")
    lines.append("                                                       int x, int y, unsigned char c,")
    lines.append("                                                       uint8_t fg, int size) {")
    lines.append("  if (size < 1) size = 1;")
    lines.append("  if (x >= fb_width || y >= fb_height ||")
    lines.append("      (x + 8 * size) <= 0 || (y + 8 * size) <= 0) return;")
    lines.append("  const uint8_t *glyph = &unscii_8_thin[c * 8];")
    lines.append("  for (int row = 0; row < 8; row++) {")
    lines.append("    uint8_t bits = glyph[row];")
    lines.append("    for (int col = 0; col < 8; col++) {")
    lines.append("      if (!(bits & (0x80 >> col))) continue;")
    lines.append("      for (int sy = 0; sy < size; sy++) {")
    lines.append("        int py = y + row * size + sy;")
    lines.append("        if (py < 0 || py >= fb_height) continue;")
    lines.append("        for (int sx = 0; sx < size; sx++) {")
    lines.append("          int px = x + col * size + sx;")
    lines.append("          if (px < 0 || px >= fb_width) continue;")
    lines.append("          fb[py * fb_width + px] = fg;")
    lines.append("        }")
    lines.append("      }")
    lines.append("    }")
    lines.append("  }")
    lines.append("}")
    lines.append("")
    lines.append("#endif // FRUITJAM_FONT_H")
    lines.append("")

    with open(out_path, 'w') as f:
        f.write("\n".join(lines))

    print(f"Wrote {out_path} ({os.path.getsize(out_path)} bytes, 256 glyphs)",
          file=sys.stderr)


if __name__ == "__main__":
    main()
