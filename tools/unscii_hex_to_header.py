#!/usr/bin/env python3
"""
Convert an unscii .hex font file to a C header for use with Adafruit_GFX / DVHSTX8.

Produces a simple 8×8 row-major bitmap font array (8 bytes per glyph, one byte
per row, MSB = leftmost pixel) covering the ASCII range 0x00–0x7F (128 chars).
This is a different format from both the Adafruit_GFX built-in 5×7 column-major
font and the GFXfont proportional-font structure — it's intended for direct
rendering via a custom drawChar that writes 8×8 cells into an 8bpp framebuffer.

Usage:
    python3 unscii_hex_to_header.py <input.hex> <output.h> [--name ARRAY_NAME]

The .hex format (used by unscii and GNU Unifont) is:
    CODEPOINT:BITMAP
where CODEPOINT is 4–5 hex digits and BITMAP is 16 hex digits for an 8×8 glyph
(8 rows × 1 byte each) or 32 hex digits for a 16×8 glyph.  Only 8×8 glyphs
are extracted; wider glyphs are skipped with a warning.

Example:
    python3 unscii_hex_to_header.py \\
        ~/code/unscii/fontfiles/unscii-8-thin.hex \\
        fruitjam_font.h --name unscii_8_thin
"""

import argparse
import sys
import os


def parse_hex_font(path):
    """Parse a .hex font file and return a dict {codepoint_int: [8 bytes]}."""
    glyphs = {}
    with open(path) as f:
        for lineno, line in enumerate(f, 1):
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            parts = line.split(':')
            if len(parts) != 2:
                print(f"Warning: skipping malformed line {lineno}: {line!r}",
                      file=sys.stderr)
                continue
            cp_str, bmp_str = parts
            try:
                cp = int(cp_str, 16)
            except ValueError:
                print(f"Warning: bad codepoint on line {lineno}: {cp_str!r}",
                      file=sys.stderr)
                continue
            if len(bmp_str) == 16:
                # 8×8 glyph — 8 bytes
                rows = [int(bmp_str[i:i+2], 16) for i in range(0, 16, 2)]
                glyphs[cp] = rows
            elif len(bmp_str) == 32:
                # 16×8 glyph — skip (we only handle 8-wide)
                pass  # silently skip wide glyphs
            elif len(bmp_str) == 0:
                # Empty glyph (e.g., U+0000 in some files has all zeros)
                glyphs[cp] = [0] * 8
            else:
                print(f"Warning: unexpected bitmap length {len(bmp_str)} "
                      f"for U+{cp:04X} on line {lineno}, skipping",
                      file=sys.stderr)
    return glyphs


def generate_header(glyphs, array_name, source_name):
    """Generate a C header string with the font bitmap array."""
    lines = []
    lines.append(f"// Auto-generated from {source_name}")
    lines.append(f"// by tools/unscii_hex_to_header.py")
    lines.append(f"//")
    lines.append(f"// 8×8 row-major bitmap font, 128 glyphs (ASCII 0x00–0x7F)")
    lines.append(f"// 8 bytes per glyph, one byte per row, MSB = leftmost pixel")
    lines.append(f"// Total: 1024 bytes")
    lines.append(f"")
    lines.append(f"#ifndef {array_name.upper()}_H")
    lines.append(f"#define {array_name.upper()}_H")
    lines.append(f"")
    lines.append(f"#include <stdint.h>")
    lines.append(f"")
    lines.append(f"static const uint8_t {array_name}[128 * 8] = {{")

    for cp in range(128):
        rows = glyphs.get(cp, [0] * 8)
        hex_bytes = ", ".join(f"0x{b:02X}" for b in rows)
        if cp >= 0x20 and cp <= 0x7E:
            ch = chr(cp)
            if ch == '\\':
                label = "backslash"
            elif ch == '\'':
                label = "apostrophe"
            else:
                label = ch
            comment = f"  // 0x{cp:02X} '{label}'"
        elif cp == 0x00:
            comment = f"  // 0x{cp:02X} NUL"
        elif cp == 0x07:
            comment = f"  // 0x{cp:02X} BEL"
        elif cp == 0x08:
            comment = f"  // 0x{cp:02X} BS"
        elif cp == 0x09:
            comment = f"  // 0x{cp:02X} TAB"
        elif cp == 0x0A:
            comment = f"  // 0x{cp:02X} LF"
        elif cp == 0x0D:
            comment = f"  // 0x{cp:02X} CR"
        elif cp == 0x1B:
            comment = f"  // 0x{cp:02X} ESC"
        elif cp == 0x7F:
            comment = f"  // 0x{cp:02X} DEL"
        else:
            comment = f"  // 0x{cp:02X}"

        lines.append(f"  {hex_bytes},{comment}")

    lines.append(f"}};")
    lines.append(f"")
    lines.append(f"#endif // {array_name.upper()}_H")
    lines.append(f"")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Convert an unscii .hex font to a C header (8×8 bitmap array)")
    parser.add_argument("input", help="Path to .hex font file")
    parser.add_argument("output", help="Path for output .h file")
    parser.add_argument("--name", default="unscii_8_thin",
                        help="C array name (default: unscii_8_thin)")
    args = parser.parse_args()

    glyphs = parse_hex_font(args.input)

    # Check ASCII coverage
    missing = [cp for cp in range(0x20, 0x7F) if cp not in glyphs]
    if missing:
        print(f"Warning: missing {len(missing)} ASCII printable glyphs: "
              f"{', '.join(f'U+{cp:04X}' for cp in missing[:10])}"
              f"{'...' if len(missing) > 10 else ''}",
              file=sys.stderr)

    present = sum(1 for cp in range(0x20, 0x7F) if cp in glyphs)
    print(f"Parsed {len(glyphs)} total glyphs, {present}/95 ASCII printable",
          file=sys.stderr)

    source_name = os.path.basename(args.input)
    header = generate_header(glyphs, args.name, source_name)

    with open(args.output, 'w') as f:
        f.write(header)

    print(f"Wrote {args.output} ({len(header)} bytes, array '{args.name}')",
          file=sys.stderr)


if __name__ == "__main__":
    main()
