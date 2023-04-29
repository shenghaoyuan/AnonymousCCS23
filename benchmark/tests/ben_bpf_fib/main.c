#include <inttypes.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include "embUnit.h"
#include "timex.h"
#include "ztimer.h"

#ifdef MODULE_GEN_BPF
#include "interpreter.h"
#elif defined(MODULE_GEN_IBPF)
#include "ibpf_util.h"
#else
#include "bpf.h"
#endif


unsigned char fib_compcert_bpf_bin[] = {
  0xbc, 0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x14, 0x0a, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
  0x63, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x63, 0x9a, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x02, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0xb4, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x15, 0x01, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x15, 0x01, 0x09, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x15, 0x01, 0x08, 0x00, 0x02, 0x00, 0x00, 0x00,
  0xb4, 0x03, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00,
  0x2d, 0x13, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x0c, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x03, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x05, 0x00, 0xf9, 0xff, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x05, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x61, 0xa9, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x0a, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
  0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};
        
#ifdef MODULE_GEN_IBPF
static uint16_t *jitted_thumb_list;

ibpf_full_state_t ibpf_state;

__attribute__ ((noinline)) void _magic_function(unsigned int ofs, struct jit_state* st){
  int res = 0;
  __asm volatile (
    "orr %[input_0], #0x1\n\t"
    "mov r12, sp\n\t"
    "sub sp, sp, #48\n\t"
    "str r12, [sp, #0]\n\t"
    "mov pc, %[input_0]\n\t"
    : [result] "=r" (res)
    : [input_1] "r" (st), [input_0] "r" (jitted_thumb_list + ofs)
    : "cc" //The instruction modifies the condition code flags
  );
  return ;
}
#else
/* ibpf defines this within the struct */
static uint8_t _bpf_stack[512];
#endif

#ifdef MODULE_GEN_BPF
static struct memory_region mr_stack = {.start_addr = (uintptr_t)_bpf_stack,
                                        .block_size = sizeof(_bpf_stack),
                                        .block_perm = Freeable,
                                        .block_ptr = _bpf_stack};
#endif



int main(void){  

#ifdef MODULE_GEN_BPF
  struct memory_region memory_regions[] = { mr_stack };
  struct bpf_state st = {
    .state_pc = 0,
    .regsmap = {0LLU, 10LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, (uintptr_t)_bpf_stack+512},
    .bpf_flag = vBPF_OK,
    .mrs = memory_regions,
    .mrs_num = ARRAY_SIZE(memory_regions),
    .ins = (unsigned long long *) fib_compcert_bpf_bin,
    .ins_len = sizeof(fib_compcert_bpf_bin),
  };
#elif defined(MODULE_GEN_IBPF)
  jitted_thumb_list = ibpf_state.jitted_thumb_list;
  ibpf_full_state_init(&ibpf_state, 1);
  ibpf_set_code(&ibpf_state, fib_compcert_bpf_bin, sizeof(fib_compcert_bpf_bin));
  ibpf_set_input(&ibpf_state, 10LLU, 0LLU, 0LLU, 0LLU, 0LLU);
  jit_alu32(&ibpf_state.st);
#else
  bpf_t bpf = {
    .application = (uint8_t*)&fib_compcert_bpf_bin,
    .application_len = sizeof(fib_compcert_bpf_bin),
    .stack = _bpf_stack,
    .stack_size = sizeof(_bpf_stack),
    .flags = BPF_FLAG_PREFLIGHT_DONE,
  };
  bpf_setup(&bpf);
  int64_t res = 0;
#endif

  uint32_t begin = ztimer_now(ZTIMER_USEC); // unsigned long long -> uint64_t
#ifdef MODULE_GEN_BPF
  int result = bpf_interpreter(&st, 10000);
  
  //printf("CertrBPF C result = 0x:%x\n", (unsigned int)result);
#elif defined(MODULE_GEN_IBPF)
  int result = ibpf_interpreter(&ibpf_state.st, 10000);
  
  //printf("CertrBPF-JIT C result = 0x:%x\n", (unsigned int)result);
#else
  int result = bpf_execute_ctx(&bpf, NULL, 0, &res);
  
  printf("Vanilla-rBPF C result = 0x:%x\n", (unsigned int)result);
#endif
  uint32_t end = ztimer_now(ZTIMER_USEC);
  float duration = (float)(end-begin);
  
  printf("execution time:%f\n", duration);
  return 0;
}