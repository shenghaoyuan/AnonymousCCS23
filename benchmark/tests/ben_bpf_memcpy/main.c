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


unsigned char memcpy_compcert_bpf_bin[] = {
  0xbc, 0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x14, 0x0a, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
  0x63, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x63, 0x9a, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x3d, 0x35, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x0c, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x0c, 0x54, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x71, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x73, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x05, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x05, 0x00, 0xf7, 0xff, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x61, 0xa9, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x0a, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
  0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

unsigned char src_data[] =
        "AD3Awn4kb6FtcsyE0RU25U7f55Yncn3LP3oEx9Gl4qr7iDW7I8L6Pbw9jNnh0sE4DmCKuc"
        "d1J8I34vn31W924y5GMS74vUrZQc08805aj4Tf66HgL1cO94os10V2s2GDQ825yNh9Yuq3"
        /*"QHcA60xl31rdA7WskVtCXI7ruH1A4qaR6Uk454hm401lLmv2cGWt5KTJmr93d3JsGaRRPs"
        "4HqYi4mFGowo8fWv48IcA3N89Z99nf0A0H2R6P0uI4Tir682Of3Rk78DUB2dIGQRRpdqVT"
        "tLhgfET2gUGU65V3edSwADMqRttI9JPVz8JS37g5QZj4Ax56rU1u0m0K8YUs57UYG5645n"
        "byNy4yqxu7"*/;

unsigned char dst_data[520];
        
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
                                        
static struct memory_region mr_dst = {.start_addr = (uintptr_t)dst_data,
                                        .block_size = sizeof(dst_data),
                                        .block_perm = Writable,
                                        .block_ptr = dst_data};
                                        
static struct memory_region mr_src = {.start_addr = (uintptr_t)src_data,
                                        .block_size = sizeof(src_data),
                                        .block_perm = Readable,
                                        .block_ptr = src_data};
#endif



int main(void){  
  float duration = 0;
  for (int loop_size = 0; loop_size < 1000; loop_size++) {

#ifdef MODULE_GEN_BPF
  struct memory_region memory_regions[] = { mr_stack, mr_src, mr_dst };
  struct bpf_state st = {
    .state_pc = 0,
    .regsmap = {0LLU, (intptr_t)src_data, (intptr_t)dst_data, 1LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, (uintptr_t)_bpf_stack+512},
    .bpf_flag = vBPF_OK,
    .mrs = memory_regions,
    .mrs_num = ARRAY_SIZE(memory_regions),
    .ins = (unsigned long long *) memcpy_compcert_bpf_bin,
    .ins_len = sizeof(memcpy_compcert_bpf_bin),
  };
#elif defined(MODULE_GEN_IBPF)
  jitted_thumb_list = ibpf_state.jitted_thumb_list;
  ibpf_full_state_init(&ibpf_state, 3);
  ibpf_set_mem_region_one(&ibpf_state, src_data, sizeof(src_data), Readable);
  ibpf_set_mem_region_two(&ibpf_state, dst_data, sizeof(dst_data), Writable);
  ibpf_set_code(&ibpf_state, memcpy_compcert_bpf_bin, sizeof(memcpy_compcert_bpf_bin));
  ibpf_set_input(&ibpf_state, (intptr_t)src_data, (intptr_t)dst_data, 1LLU, 0LLU, 0LLU);
  jit_alu32(&ibpf_state.st);
#else
  bpf_t bpf = {
    .application = (uint8_t*)&memcpy_compcert_bpf_bin,
    .application_len = sizeof(memcpy_compcert_bpf_bin),
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
#else
  int result = bpf_execute_ctx(&bpf, NULL, 0, &res);
  
  printf("Vanilla-rBPF C result = 0x:%x\n", (unsigned int)result);
#endif
  uint32_t end = ztimer_now(ZTIMER_USEC);
  duration = (float)(end-begin) + duration;
  }
  printf("execution time:%f\n", duration);
  
  printf("%s\n", dst_data);
  return 0;
}
