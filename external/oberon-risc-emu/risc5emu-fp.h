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

// based on src/risc-fp.h from git@github.com:pdewacht/oberon-risc-emu.git

#ifndef RISC_FP_H
#define RISC_FP_H

#include <stdint.h>
#include <stdbool.h>

uint32_t fp_add(uint32_t x, uint32_t y, bool u, bool v);
uint32_t fp_mul(uint32_t x, uint32_t y);
uint32_t fp_div(uint32_t x, uint32_t y);

struct idiv { uint32_t quot, rem; };
struct idiv idiv(uint32_t x, uint32_t y, bool signed_div);

#endif  // RISC_FP_H
