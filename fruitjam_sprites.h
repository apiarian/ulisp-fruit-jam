// fruitjam_sprites.h — Sprite sheet for Fruit Jam
//
// A C-managed 256×256 pixel buffer (64 KB) for bitmap graphics.
// Pixels are 8-bit palette indices (same 3-3-2 palette as the display).
// Includes blit-to-screen with clipping, color key transparency,
// flip/rotate, integer scaling, and palette remapping.
//
// Lisp API: sprite-pixel, sprite-draw, sprite-remap, sprite-save, sprite-load,
//           sprite-remap-save, sprite-remap-load
// See SPRITE-PLAN.md for full design rationale.

#ifndef FRUITJAM_SPRITES_H
#define FRUITJAM_SPRITES_H

// ---- Constants ----
#define SPRITE_SHEET_W 256
#define SPRITE_SHEET_H 256
#define SPRITE_SHEET_SIZE (SPRITE_SHEET_W * SPRITE_SHEET_H)  // 65536 bytes
#define SPRITE_REMAP_COUNT 8

// ---- Storage ----
static uint8_t *sprite_sheet = nullptr;
static uint8_t (*sprite_remaps)[256] = nullptr;  // [SPRITE_REMAP_COUNT][256], heap-allocated
static bool sprite_initialized = false;

// ---- Init ----
// Allocates the sprite sheet and remap tables from heap.
// Called lazily on first sprite function use.
// Returns true if the sprite system is ready.
static bool fruitjam_sprites_init () {
  if (sprite_initialized) return (sprite_sheet != nullptr);
  sprite_initialized = true;

  // Allocate sprite sheet — try PSRAM first (preserves scarce SRAM), fall back to SRAM
  #if defined(BOARD_HAS_PSRAM)
  {
    extern void *__psram_malloc(size_t size);
    sprite_sheet = (uint8_t *)__psram_malloc(SPRITE_SHEET_SIZE);
  }
  #else
  sprite_sheet = (uint8_t *)malloc(SPRITE_SHEET_SIZE);
  #endif
  if (!sprite_sheet) return false;
  memset(sprite_sheet, 0, SPRITE_SHEET_SIZE);

  // Allocate remap tables (8 × 256 = 2 KB)
  sprite_remaps = (uint8_t (*)[256])malloc(SPRITE_REMAP_COUNT * 256);
  if (!sprite_remaps) {
    #if defined(BOARD_HAS_PSRAM)
    { extern void __psram_free(void *ptr); __psram_free(sprite_sheet); }
    #else
    free(sprite_sheet);
    #endif
    sprite_sheet = nullptr;
    return false;
  }
  // Initialize all remap tables to identity
  for (int t = 0; t < SPRITE_REMAP_COUNT; t++) {
    for (int i = 0; i < 256; i++) {
      sprite_remaps[t][i] = (uint8_t)i;
    }
  }
  return true;
}

// ---- Pixel access ----

static inline uint8_t sprite_get_pixel (int x, int y) {
  if (x < 0 || x >= SPRITE_SHEET_W || y < 0 || y >= SPRITE_SHEET_H) return 0;
  return sprite_sheet[y * SPRITE_SHEET_W + x];
}

static inline void sprite_set_pixel (int x, int y, uint8_t color) {
  if (x < 0 || x >= SPRITE_SHEET_W || y < 0 || y >= SPRITE_SHEET_H) return;
  sprite_sheet[y * SPRITE_SHEET_W + x] = color;
}

// ---- Blit: copy rectangle from sheet to screen framebuffer ----
//
// sx, sy: source top-left on sprite sheet
// w, h: source rectangle size
// dx, dy: destination top-left on screen
// key: transparent color index (-1 = no transparency)
// flip: 0=none, 1=horizontal, 2=vertical, 3=both
// rotate: 0=none, 1=90°CW, 2=180°, 3=270°CW
// scale: integer scale factor (1+)
// remap: pointer to a 256-byte remap table, or NULL for no remapping
//
// Flip is applied before rotation.
// For 90°/270° rotation, the destination footprint is h×w (scaled).

static void sprite_blit (int sx, int sy, int w, int h,
                          int dx, int dy,
                          int key, int flip, int rotate, int scale,
                          const uint8_t *remap) {
  if (!sprite_sheet) return;
  uint8_t *fb = display8.getBuffer();
  if (!fb) return;
  if (scale < 1) scale = 1;

  // Destination dimensions after rotation
  int dw, dh;
  if (rotate == 1 || rotate == 3) {
    dw = h * scale;
    dh = w * scale;
  } else {
    dw = w * scale;
    dh = h * scale;
  }

  // Iterate over destination pixels
  for (int oy = 0; oy < dh; oy++) {
    int screen_y = dy + oy;
    if (screen_y < 0) continue;
    if (screen_y >= DISPLAY_HEIGHT) break;

    for (int ox = 0; ox < dw; ox++) {
      int screen_x = dx + ox;
      if (screen_x < 0) continue;
      if (screen_x >= DISPLAY_WIDTH) break;

      // Map destination pixel back to source pixel (before scale)
      int src_col = ox / scale;  // column in destination-space (pre-scale)
      int src_row = oy / scale;  // row in destination-space (pre-scale)

      // Undo rotation to get source coordinates
      // rotate maps source → dest:
      //   0: (c,r) → (c,r)
      //   1 (90°CW): (c,r) → (h-1-r, c)  so inverse: (dc,dr) → (dr, h-1-dc)
      //   2 (180°): (c,r) → (w-1-c, h-1-r) so inverse: (dc,dr) → (w-1-dc, h-1-dr)
      //   3 (270°CW): (c,r) → (r, w-1-c) so inverse: (dc,dr) → (w-1-dr, dc)
      int uc, ur;  // un-rotated column, row
      switch (rotate) {
        default:
        case 0: uc = src_col; ur = src_row; break;
        case 1: uc = src_row; ur = h - 1 - src_col; break;
        case 2: uc = w - 1 - src_col; ur = h - 1 - src_row; break;
        case 3: uc = w - 1 - src_row; ur = src_col; break;
      }

      // Undo flip
      int fx = uc, fy = ur;
      if (flip & 1) fx = w - 1 - fx;   // horizontal flip
      if (flip & 2) fy = h - 1 - fy;   // vertical flip

      // Final source coordinates on the sprite sheet
      int final_sx = sx + fx;
      int final_sy = sy + fy;

      // Clip to sprite sheet bounds
      if (final_sx < 0 || final_sx >= SPRITE_SHEET_W) continue;
      if (final_sy < 0 || final_sy >= SPRITE_SHEET_H) continue;

      // Read pixel from sheet
      uint8_t pixel = sprite_sheet[final_sy * SPRITE_SHEET_W + final_sx];

      // Apply remap
      if (remap) pixel = remap[pixel];

      // Transparency check
      if (key >= 0 && pixel == (uint8_t)key) continue;

      // Write to framebuffer
      fb[screen_y * DISPLAY_WIDTH + screen_x] = pixel;
    }
  }
}

#endif // FRUITJAM_SPRITES_H
