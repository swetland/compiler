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

// based on src/risc5.c from git@github.com:pdewacht/oberon-risc-emu.git

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "risc5emu.h"
#include "risc5emu-fp.h"
#include "risc5.h"

static void disasm(uint32_t pc, uint32_t ins) {
  char buf[256];
  risc5dis(pc, ins, buf);
  printf("%08x: %08x  %s\n", pc, ins, buf);
}

// Our memory layout is slightly different from the FPGA implementation:
// The FPGA uses a 20-bit address bus and thus ignores the top 12 bits,
// while we use all 32 bits. This allows us to have more than 1 megabyte
// of RAM.

#define DefaultMemSize 0x00100000
#define IOStart        0xFFFF0000

struct RISC {
  uint32_t PC;
  uint32_t R[16];
  uint32_t H;
  bool     Z, N, C, V;

  uint32_t *RAM;
  uint32_t mem_size;
  bool TRACE;
};

enum {
  MOV, LSL, ASR, ROR,
  AND, ANN, IOR, XOR,
  ADD, SUB, MUL, DIV,
  FAD, FSB, FML, FDV,
};

static uint32_t risc_load_io(struct RISC *risc, uint32_t address);
static void risc_store_io(struct RISC *risc, uint32_t address, uint32_t value);

struct RISC *risc_new(bool trace) {
  struct RISC *risc = calloc(1, sizeof(*risc));
  risc->mem_size = DefaultMemSize;
  risc->RAM = calloc(1, risc->mem_size);
  risc->TRACE = trace;
  risc_reset(risc);
  return risc;
}

void risc_reset(struct RISC *risc) {
  risc->PC = 0;
}

void risc_run(struct RISC *risc, int cycles) {
  // The progress value is used to detect that the RISC cpu is busy
  // waiting on the millisecond counter or on the keyboard ready
  // bit. In that case it's better to just pause emulation until the
  // next frame.
  for (int i = 0; i < cycles; i++) {
    risc_single_step(risc);
  }
}

void risc_single_step(struct RISC *risc) {
  uint32_t ir;
  if (risc->PC < risc->mem_size / 4) {
    ir = risc->RAM[risc->PC];
  } else {
    fprintf(stderr, "Branched into the void (PC=0x%08X), resetting...\n", risc->PC);
    risc_reset(risc);
    return;
  }

  if (risc->TRACE) disasm(risc->PC << 2, ir); 
  risc->PC++;

  const uint32_t pbit = 0x80000000;
  const uint32_t qbit = 0x40000000;
  const uint32_t ubit = 0x20000000;
  const uint32_t vbit = 0x10000000;

  if ((ir & pbit) == 0) {
    // Register instructions
    uint32_t a  = (ir & 0x0F000000) >> 24;
    uint32_t b  = (ir & 0x00F00000) >> 20;
    uint32_t op = (ir & 0x000F0000) >> 16;
    uint32_t im =  ir & 0x0000FFFF;
    uint32_t c  =  ir & 0x0000000F;

    uint32_t a_val, b_val, c_val;
    b_val = risc->R[b];
    if ((ir & qbit) == 0) {
      c_val = risc->R[c];
    } else if ((ir & vbit) == 0) {
      c_val = im;
    } else {
      c_val = 0xFFFF0000 | im;
    }

    switch (op) {
      case MOV: {
        if ((ir & ubit) == 0) {
          a_val = c_val;
        } else if ((ir & qbit) != 0) {
          a_val = c_val << 16;
        } else if ((ir & vbit) != 0) {
          a_val = 0xD0 |   // ???
            (risc->N * 0x80000000U) |
            (risc->Z * 0x40000000U) |
            (risc->C * 0x20000000U) |
            (risc->V * 0x10000000U);
        } else {
          a_val = risc->H;
        }
        break;
      }
      case LSL: {
        a_val = b_val << (c_val & 31);
        break;
      }
      case ASR: {
        a_val = ((int32_t)b_val) >> (c_val & 31);
        break;
      }
      case ROR: {
        a_val = (b_val >> (c_val & 31)) | (b_val << (-c_val & 31));
        break;
      }
      case AND: {
        a_val = b_val & c_val;
        break;
      }
      case ANN: {
        a_val = b_val & ~c_val;
        break;
      }
      case IOR: {
        a_val = b_val | c_val;
        break;
      }
      case XOR: {
        a_val = b_val ^ c_val;
        break;
      }
      case ADD: {
        a_val = b_val + c_val;
        if ((ir & ubit) != 0) {
          a_val += risc->C;
        }
        risc->C = a_val < b_val;
        risc->V = ((a_val ^ c_val) & (a_val ^ b_val)) >> 31;
        break;
      }
      case SUB: {
        a_val = b_val - c_val;
        if ((ir & ubit) != 0) {
          a_val -= risc->C;
        }
        risc->C = a_val > b_val;
        risc->V = ((b_val ^ c_val) & (a_val ^ b_val)) >> 31;
        break;
      }
      case MUL: {
        uint64_t tmp;
        if ((ir & ubit) == 0) {
          tmp = (int64_t)(int32_t)b_val * (int64_t)(int32_t)c_val;
        } else {
          tmp = (uint64_t)b_val * (uint64_t)c_val;
        }
        a_val = (uint32_t)tmp;
        risc->H = (uint32_t)(tmp >> 32);
        break;
      }
      case DIV: {
        if ((int32_t)c_val > 0) {
          if ((ir & ubit) == 0) {
            a_val = (int32_t)b_val / (int32_t)c_val;
            risc->H = (int32_t)b_val % (int32_t)c_val;
            if ((int32_t)risc->H < 0) {
              a_val--;
              risc->H += c_val;
            }
          } else {
            a_val = b_val / c_val;
            risc->H = b_val % c_val;
          }
        } else {
          struct idiv q = idiv(b_val, c_val, ir & ubit);
          a_val = q.quot;
          risc->H = q.rem;
        }
        break;
      }
      case FAD: {
        a_val = fp_add(b_val, c_val, ir & ubit, ir & vbit);
        break;
      }
      case FSB: {
        a_val = fp_add(b_val, c_val ^ 0x80000000, ir & ubit, ir & vbit);
        break;
      }
      case FML: {
        a_val = fp_mul(b_val, c_val);
        break;
      }
      case FDV: {
        a_val = fp_div(b_val, c_val);
        break;
      }
      default: {
        abort();  // unreachable
      }
    }
    risc_set_register(risc, a, a_val);
  }
  else if ((ir & qbit) == 0) {
    // Memory instructions
    uint32_t a = (ir & 0x0F000000) >> 24;
    uint32_t b = (ir & 0x00F00000) >> 20;
    int32_t off = ir & 0x000FFFFF;
    off = (off ^ 0x00080000) - 0x00080000;  // sign-extend

    uint32_t address = risc->R[b] + off;
    if ((ir & ubit) == 0) {
      uint32_t a_val;
      if ((ir & vbit) == 0) {
        a_val = risc_load_word(risc, address);
      } else {
        a_val = risc_load_byte(risc, address);
      }
      risc_set_register(risc, a, a_val);
    } else {
      if ((ir & vbit) == 0) {
        risc_store_word(risc, address, risc->R[a]);
      } else {
        risc_store_byte(risc, address, (uint8_t)risc->R[a]);
      }
    }
  }
  else {
    // Branch instructions
    bool t = (ir >> 27) & 1;
    switch ((ir >> 24) & 7) {
      case 0: t ^= risc->N; break;
      case 1: t ^= risc->Z; break;
      case 2: t ^= risc->C; break;
      case 3: t ^= risc->V; break;
      case 4: t ^= risc->C | risc->Z; break;
      case 5: t ^= risc->N ^ risc->V; break;
      case 6: t ^= (risc->N ^ risc->V) | risc->Z; break;
      case 7: t ^= true; break;
      default: abort();  // unreachable
    }
    if (t) {
      if ((ir & vbit) != 0) {
        risc_set_register(risc, 15, risc->PC * 4);
      }
      if ((ir & ubit) == 0) {
        uint32_t c = ir & 0x0000000F;
        risc->PC = risc->R[c] / 4;
      } else {
        int32_t off = ir & 0x00FFFFFF;
        off = (off ^ 0x00800000) - 0x00800000;  // sign-extend
        risc->PC = risc->PC + off;
      }
    }
  }
}

void risc_set_register(struct RISC *risc, int reg, uint32_t value) {
  if (risc->TRACE) printf("                    R%d = 0x%08x\n", reg, value);
  risc->R[reg] = value;
  risc->Z = value == 0;
  risc->N = (int32_t)value < 0;
}

uint32_t risc_load_word(struct RISC *risc, uint32_t address) {
  if (address < risc->mem_size) {
    return risc->RAM[address/4];
  } else {
    return risc_load_io(risc, address);
  }
}

uint8_t risc_load_byte(struct RISC *risc, uint32_t address) {
  uint32_t w = risc_load_word(risc, address);
  return (uint8_t)(w >> (address % 4 * 8));
}

void risc_store_word(struct RISC *risc, uint32_t address, uint32_t value) {
  if (address < risc->mem_size) {
    if (risc->TRACE) printf("                    mem@ 0x%08x = 0x%08x\n", address, value);
    risc->RAM[address/4] = value;
  } else {
    risc_store_io(risc, address, value);
  }
}

void risc_store_byte(struct RISC *risc, uint32_t address, uint8_t value) {
  if (address < risc->mem_size) {
    uint32_t w = risc_load_word(risc, address);
    uint32_t shift = (address & 3) * 8;
    w &= ~(0xFFu << shift);
    w |= (uint32_t)value << shift;
    risc_store_word(risc, address, w);
  } else {
    risc_store_io(risc, address, (uint32_t)value);
  }
}

static uint32_t risc_load_io(struct RISC *risc, uint32_t address) {
  return 0;
}

static void risc_store_io(struct RISC *risc, uint32_t address, uint32_t value) {
  if (risc->TRACE) printf("                    io@ 0x%08x = 0x%08x\n", address, value);
  switch (address - IOStart) {
  case 0x100: {
    printf("%08x\nEXIT\n", value);
    exit(0);
    break;
  case 0x104:
    printf("%08x\n", value);
    break;
  }
  }
}

