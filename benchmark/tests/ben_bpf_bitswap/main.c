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


unsigned char bitswap_compcert_bpf_bin[] = {
  0xb4, 0x04, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0xb4, 0x05, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x6c, 0x25, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x6c, 0x34, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x4c, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xa4, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
  0x5c, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x5c, 0x15, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x7c, 0x25, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x6c, 0x35, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x4c, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x5c, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x7c, 0x34, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x6c, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x4c, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x54, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00,
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
  float duration = 0;
  for (int loop_size = 0; loop_size < 1000; loop_size++) {

#ifdef MODULE_GEN_BPF
  struct memory_region memory_regions[] = { mr_stack };
  struct bpf_state st = {
    .state_pc = 0,
    .regsmap = {0LLU, 156LLU, 3LLU, 6LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, (uintptr_t)_bpf_stack+512},
    .bpf_flag = vBPF_OK,
    .mrs = memory_regions,
    .mrs_num = ARRAY_SIZE(memory_regions),
    .ins = (unsigned long long *) bitswap_compcert_bpf_bin,
    .ins_len = sizeof(bitswap_compcert_bpf_bin),
  };
#elif defined(MODULE_GEN_IBPF)
  jitted_thumb_list = ibpf_state.jitted_thumb_list;
  ibpf_full_state_init(&ibpf_state, 1);
  ibpf_set_code(&ibpf_state, bitswap_compcert_bpf_bin, sizeof(bitswap_compcert_bpf_bin));
  ibpf_set_input(&ibpf_state, 156LLU, 3LLU, 6LLU, 0LLU, 0LLU);
  jit_alu32(&ibpf_state.st);
#else
  bpf_t bpf = {
    .application = (uint8_t*)&bitswap_compcert_bpf_bin,
    .application_len = sizeof(bitswap_compcert_bpf_bin),
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
  
  //printf("flag=%d\n", st.bpf_flag);
  //printf("CertrBPF C result = 0x:%x\n", (unsigned int)result);
#elif defined(MODULE_GEN_IBPF)
  int result = ibpf_interpreter(&ibpf_state.st, 10000);
  
  //printf("flag=%d\n", ibpf_state.st.flag);
  //printf("CertrBPF-JIT C result = 0x:%x\n", (unsigned int)result);
  //_magic_function(0, &ibpf_state.st);
  //printf("CertrBPF-JIT-Pure C result = 0x:%x\n", (unsigned int)(ibpf_state.st.regs_st[0]));
#else
  int result = bpf_execute_ctx(&bpf, NULL, 0, &res);
  
  //printf("Vanilla-rBPF C result = 0x:%x\n", (unsigned int)result);
#endif
  uint32_t end = ztimer_now(ZTIMER_USEC);
  duration = (float)(end-begin) + duration;
  }
  printf("execution time:%f\n", duration);
  return 0;
}
