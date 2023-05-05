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


unsigned char sock_buffer_compcert_bpf_bin[] = {
  0xb4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x67, 0x05, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x77, 0x05, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x15, 0x05, 0x1c, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x67, 0x05, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x77, 0x05, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x67, 0x02, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x77, 0x02, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x67, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x77, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0xb4, 0x06, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0xbf, 0x37, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x3d, 0x21, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x73, 0x67, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x07, 0x01, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x07, 0x07, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x07, 0x05, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
  0x55, 0x05, 0xfa, 0xff, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x67, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x77, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x15, 0x01, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x67, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x77, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
  0x71, 0x32, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x0c, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x07, 0x03, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x07, 0x01, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
  0x55, 0x01, 0xfb, 0xff, 0x00, 0x00, 0x00, 0x00,
  0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

/*
#define ARRAY_LENGTH 40

struct test_md
{
    uint8_t data_start;
    uint8_t data_end;
    uint8_t array[ARRAY_LENGTH];
};

struct test_md my_test_md = {.data_start = 100, .data_end = 200, .array={0}}; */

uint8_t array[90];
        
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
                              
static struct memory_region mr_array = {.start_addr = (uintptr_t)&array,
                                        .block_size = sizeof(array),
                                        .block_perm = Writable,
                                        .block_ptr = &array};
         /*                               
static struct memory_region mr_md = {.start_addr = (uintptr_t)&my_test_md,
                                        .block_size = sizeof(my_test_md),
                                        .block_perm = Writable,
                                        .block_ptr = &my_test_md}; */
#endif



int main(void){  
  
#ifdef MODULE_GEN_BPF
  struct memory_region memory_regions[] = { mr_stack, mr_array }; //{ mr_stack, mr_md };
  struct bpf_state st = {
    .state_pc = 0,
    //.regsmap = {0LLU, (uintptr_t)&my_test_md, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, (uintptr_t)_bpf_stack+512},
    .regsmap = {0LLU, 100LLU, 200LLU, (uintptr_t)array, 9LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, (uintptr_t)_bpf_stack+512},
    .bpf_flag = vBPF_OK,
    .mrs = memory_regions,
    .mrs_num = ARRAY_SIZE(memory_regions),
    .ins = (unsigned long long *) sock_buffer_compcert_bpf_bin,
    .ins_len = sizeof(sock_buffer_compcert_bpf_bin)/8,
  };
#elif defined(MODULE_GEN_IBPF)

  jitted_thumb_list = ibpf_state.jitted_thumb_list;
  ibpf_full_state_init(&ibpf_state, 2);
  //ibpf_set_mem_region_one(&ibpf_state, (uintptr_t)&my_test_md, sizeof(my_test_md), Writable);
  ibpf_set_mem_region_one(&ibpf_state, (uintptr_t)&array, sizeof(array), Writable);
  ibpf_set_code(&ibpf_state, sock_buffer_compcert_bpf_bin, sizeof(sock_buffer_compcert_bpf_bin));
  ibpf_set_input(&ibpf_state, 100LLU, 200LLU, (uintptr_t)array, 9LLU, 0LLU);

  //print_ibpf(&ibpf_state.st);
  jit_alu32(&ibpf_state.st);
#else
  bpf_t bpf = {
    .application = (uint8_t*)&sock_buffer_compcert_bpf_bin,
    .application_len = sizeof(sock_buffer_compcert_bpf_bin),
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
  //print_ibpf(&ibpf_state.st);
  int result = ibpf_interpreter(&ibpf_state.st, 10000);
  
  //printf("flag=%d\n", ibpf_state.st.flag);
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
