#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdint.h>
#include <assert.h>
#include <string.h>

typedef uint64_t rtlreg_t;

typedef uint64_t paddr_t;
typedef uint64_t vaddr_t;

typedef uint16_t ioaddr_t;

#include "macro.h"

// 0~31: GPRs, 32~63 FPRs
enum {
  DIFFTEST_THIS_PC = 64,
  DIFFTEST_MSTATUS,
  DIFFTEST_MCAUSE,
  DIFFTEST_MEPC,
  DIFFTEST_SSTATUS,
  DIFFTEST_SCAUSE,
  DIFFTEST_SEPC,
  DIFFTEST_NR_REG
};

struct DiffState {
  int commit;
  uint64_t *reg_scala;
  uint32_t this_inst;
  int skip;
  int isRVC;
  uint64_t *wpc;
  uint64_t *wdata;
  uint32_t *wdst;
  int wen;
  uint64_t intrNO;
  int priviledgeMode;
};

extern void (*ref_difftest_memcpy_from_dut)(paddr_t dest, void *src, size_t n);
extern void (*ref_difftest_memcpy_from_ref)(void *dest, paddr_t src, size_t n);
extern void (*ref_difftest_getregs)(void *c);
extern void (*ref_difftest_setregs)(const void *c);

void init_difftest();
int difftest_step(DiffState *s);
void difftest_display(uint8_t mode);

#endif
