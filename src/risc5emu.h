
#include <stdint.h>

typedef struct RISC risc_t;

struct RISC *risc_new();

void risc_reset(struct RISC *risc);
void risc_run(struct RISC *risc, int cycles);

void risc_single_step(struct RISC *risc);
void risc_set_register(struct RISC *risc, int reg, uint32_t value);
uint32_t risc_load_word(struct RISC *risc, uint32_t address);
uint8_t risc_load_byte(struct RISC *risc, uint32_t address);
void risc_store_word(struct RISC *risc, uint32_t address, uint32_t value);
void risc_store_byte(struct RISC *risc, uint32_t address, uint8_t value);

