#include "resistor_color_duo.h"

uint16_t colorCode(resistor_band_t color[]) {
  return color[0] * 10 + color[1];
}
