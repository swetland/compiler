// Copyright © 2014 Peter De Wachter
//
// Permission to use, copy, modify, and/or distribute this software for
// any purpose with or without fee is hereby granted, provided that the
// above copyright notice and this permission notice appear in all
// copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
// WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
// AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
// DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
// PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
// TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
// PERFORMANCE OF THIS SOFTWARE.

// based on src/risc-fp.c from git@github.com:pdewacht/oberon-risc-emu.git

#include "risc5emu-fp.h"

uint32_t fp_add(uint32_t x, uint32_t y, bool u, bool v) {
  bool xs = (x & 0x80000000) != 0;
  uint32_t xe;
  int32_t x0;
  if (!u) {
    xe = (x >> 23) & 0xFF;
    uint32_t xm = ((x & 0x7FFFFF) << 1) | 0x1000000;
    x0 = (int32_t)(xs ? -xm : xm);
  } else {
    xe = 150;
    x0 = (int32_t)(x & 0x00FFFFFF) << 8 >> 7;
  }

  bool ys = (y & 0x80000000) != 0;
  uint32_t ye = (y >> 23) & 0xFF;
  uint32_t ym = ((y & 0x7FFFFF) << 1);
  if (!u && !v) ym |= 0x1000000;
  int32_t y0 = (int32_t)(ys ? -ym : ym);

  uint32_t e0;
  int32_t x3, y3;
  if (ye > xe) {
    uint32_t shift = ye - xe;
    e0 = ye;
    x3 = shift > 31 ? x0 >> 31 : x0 >> shift;
    y3 = y0;
  } else {
    uint32_t shift = xe - ye;
    e0 = xe;
    x3 = x0;
    y3 = shift > 31 ? y0 >> 31 : y0 >> shift;
  }

  uint32_t sum = ((xs << 26) | (xs << 25) | (x3 & 0x01FFFFFF))
    + ((ys << 26) | (ys << 25) | (y3 & 0x01FFFFFF));

  uint32_t s = (((sum & (1 << 26)) ? -sum : sum) + 1) & 0x07FFFFFF;

  uint32_t e1 = e0 + 1;
  uint32_t t3 = s >> 1;
  if ((s & 0x3FFFFFC) != 0) {
    while ((t3 & (1<<24)) == 0) {
      t3 <<= 1;
      e1--;
    }
  } else {
    t3 <<= 24;
    e1 -= 24;
  }

  bool xn = (x & 0x7FFFFFFF) == 0;
  bool yn = (y & 0x7FFFFFFF) == 0;

  if (v) {
    return (int32_t)(sum << 5) >> 6;
  } else if (xn) {
    return (u | yn) ? 0 : y;
  } else if (yn) {
    return x;
  } else if ((t3 & 0x01FFFFFF) == 0 || (e1 & 0x100) != 0) {
    return 0;
  } else {
    return ((sum & 0x04000000) << 5) | (e1 << 23) | ((t3 >> 1) & 0x7FFFFF);
  }
}

uint32_t fp_mul(uint32_t x, uint32_t y) {
  uint32_t sign = (x ^ y) & 0x80000000;
  uint32_t xe = (x >> 23) & 0xFF;
  uint32_t ye = (y >> 23) & 0xFF;

  uint32_t xm = (x & 0x7FFFFF) | 0x800000;
  uint32_t ym = (y & 0x7FFFFF) | 0x800000;
  uint64_t m = (uint64_t)xm * ym;

  uint32_t e1 = (xe + ye) - 127;
  uint32_t z0;
  if ((m & (1ULL << 47)) != 0) {
    e1++;
    z0 = ((m >> 23) + 1) & 0xFFFFFF;
  } else {
    z0 = ((m >> 22) + 1) & 0xFFFFFF;
  }

  if (xe == 0 || ye == 0) {
    return 0;
  } else if ((e1 & 0x100) == 0) {
    return sign | ((e1 & 0xFF) << 23) | (z0 >> 1);
  } else if ((e1 & 0x80) == 0) {
    return sign | (0xFF << 23) | (z0 >> 1);
  } else {
    return 0;
  }
}

uint32_t fp_div(uint32_t x, uint32_t y) {
  uint32_t sign = (x ^ y) & 0x80000000;
  uint32_t xe = (x >> 23) & 0xFF;
  uint32_t ye = (y >> 23) & 0xFF;

  uint32_t xm = (x & 0x7FFFFF) | 0x800000;
  uint32_t ym = (y & 0x7FFFFF) | 0x800000;
  uint32_t q1 = (uint32_t)(xm * (1ULL << 25) / ym);

  uint32_t e1 = (xe - ye) + 126;
  uint32_t q2;
  if ((q1 & (1 << 25)) != 0) {
    e1++;
    q2 = (q1 >> 1) & 0xFFFFFF;
  } else {
    q2 = q1 & 0xFFFFFF;
  }
  uint32_t q3 = q2 + 1;

  if (xe == 0) {
    return 0;
  } else if (ye == 0) {
    return sign | (0xFF << 23);
  } else if ((e1 & 0x100) == 0) {
    return sign | ((e1 & 0xFF) << 23) | (q3 >> 1);
  } else if ((e1 & 0x80) == 0) {
    return sign | (0xFF << 23) | (q2 >> 1);
  } else {
    return 0;
  }
}

struct idiv idiv(uint32_t x, uint32_t y, bool signed_div) {
  bool sign = ((int32_t)x < 0) & signed_div;
  uint32_t x0 = sign ? -x : x;

  uint64_t RQ = x0;
  for (int S = 0; S < 32; ++S) {
    uint32_t w0 = (uint32_t)(RQ >> 31);
    uint32_t w1 = w0 - y;
    if ((int32_t)w1 < 0) {
      RQ = ((uint64_t)w0 << 32) | ((RQ & 0x7FFFFFFFU) << 1);
    } else {
      RQ = ((uint64_t)w1 << 32) | ((RQ & 0x7FFFFFFFU) << 1) | 1;
    }
  }

  struct idiv d = { (uint32_t)RQ, (uint32_t)(RQ >> 32) };
  if (sign) {
    d.quot = -d.quot;
    if (d.rem) {
      d.quot -= 1;
      d.rem = y - d.rem;
    }
  }
  return d;
}
