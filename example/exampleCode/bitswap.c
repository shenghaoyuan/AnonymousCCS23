#include<stdio.h>
#include <stdint.h>

uint8_t swap_bits(uint8_t value, uint8_t bit1, uint8_t bit2)
{
  uint8_t mask1 = 1 << bit1;
  uint8_t mask2 = 1 << bit2;
  uint8_t result = value & ~(mask1 | mask2);
  result |= ((value & mask1) >> bit1) << bit2;
  result |= ((value & mask2) >> bit2) << bit1;
  return result;
}
