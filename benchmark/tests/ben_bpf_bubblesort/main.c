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


unsigned char bubblesort_compcert_bpf_bin[] = {
  0xbc, 0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x14, 0x0a, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
  0x63, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x63, 0x9a, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x63, 0x6a, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x04, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
  0x7d, 0x43, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x1c, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
  0x7d, 0x05, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xb4, 0x04, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00,
  0xbc, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x6c, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xbc, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x0c, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x61, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x61, 0x46, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
  0xdd, 0x60, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x63, 0x64, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x63, 0x04, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x05, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x05, 0x00, 0xf0, 0xff, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x03, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
  0x05, 0x00, 0xea, 0xff, 0x00, 0x00, 0x00, 0x00,
  0x61, 0xa6, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x61, 0xa9, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x0a, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
  0x95, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

int unsort_list[] = {5923, 3314, 6281, 2408, 9997, 4393, 772, 3983, 4083, 3212, 9096, 1973, 7792, 1627, 1812, 1683, 4615, 8370, 7379, 1188, 2511, 1115, 9226, 9025, 1898, 5529, 3674, 7868, 750, 2393, 9372, 4370}; //, 8391, 1078, 3337, 7896, 3392, 1269, 7519, 1048, 6186, 1564, 4787, 8984, 9469, 9197, 7108, 3331, 7410, 9487, 4626, 5273, 6575, 8687, 1331, 371, 6691, 2203, 3756, 4793, 7883, 9336, 6382, 975, 2905, 452, 8211, 7959, 5424, 194, 2922, 6789, 1234, 3987, 4225, 2347, 826, 5577, 4747, 1100, 6741, 3577, 783, 2384, 6866, 3328, 157, 1349, 3287, 9884, 8677, 1970, 385, 1299, 5647, 3669, 8766, 7218, 7059, 4740, 8752, 5243, 3566, 5189, 4875, 1296, 1194, 1832, 2347, 6008, 3450, 2553, 7385, 2081, 91, 5498, 9567, 4824, 7587, 9287, 4046, 5164, 7581, 2881, 7998, 6682, 9447, 7937, 7072, 1667, 7712, 884, 4093, 8731, 619, 1139, 336, 1071, 7641, 7084, 1245, 540, 4523, 9873, 144, 4602, 2631, 4015, 2552, 8495, 9162, 337, 8883, 8844, 6357, 8955, 99, 3154, 4132, 3732, 7033, 5482, 9050, 3795, 1461, 426, 5596, 9238, 6466, 7971, 8395, 2423, 9803, 799, 1987, 6867, 6058, 6339, 8264, 4488, 7994, 3619, 5476, 1855, 7028, 6433, 1283, 1218, 5615, 13, 9383, 7192, 1273, 8509, 4953, 8579, 7195, 8836, 3604, 5998, 2064, 6691, 134, 8416, 844, 5940, 783, 3550, 7849, 7457, 9783, 2595, 2499, 8469, 2301, 2522, 9375, 1956, 5777, 1082, 7420, 118, 1599, 9363, 328, 1141, 4360, 416, 7085, 4396, 5708, 2111, 6091, 6792, 2007, 7268, 2123, 4312, 8906, 278, 3924, 129, 7077, 5097, 7784, 3735, 6935, 1262, 7984, 8522, 7141, 5463, 2472, 8502, 6382, 4971, 3541, 8755, 6899, 2606, 2566, 3174, 7013, 2593, 3397, 6135, 4046, 3366, 8768, 2770, 3117, 5247, 7398, 4716, 8100, 7232, 3299, 4123, 9815, 9906, 2298, 4358, 9720, 1929, 5968, 934, 1756, 9239, 1848, 2942, 7644, 6487, 1982, 7213, 3175, 7957, 5528, 8160, 1094, 854};
//{5923, 3314, 6281};

void print_sorted_list (int arr[], int size) {
  for (int i = 0; i < size; i++) {
    if (i%10 == 0) { printf("\n"); }
    printf("%04d, ", arr[i]);
  }
  return ;
}
        
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
                                        
static struct memory_region mr_arr = {.start_addr = (uintptr_t)unsort_list,
                                        .block_size = sizeof(unsort_list),
                                        .block_perm = Freeable,
                                        .block_ptr = unsort_list};
#endif



int main(void){  

#ifdef MODULE_GEN_BPF
  struct memory_region memory_regions[] = { mr_stack, mr_arr };
  struct bpf_state st = {
    .state_pc = 0,
    .regsmap = {0LLU, (intptr_t)unsort_list, (sizeof(unsort_list)/sizeof(unsort_list[0])), 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, (uintptr_t)_bpf_stack+512},
    .bpf_flag = vBPF_OK,
    .mrs = memory_regions,
    .mrs_num = ARRAY_SIZE(memory_regions),
    .ins = (unsigned long long *) bubblesort_compcert_bpf_bin,
    .ins_len = sizeof(bubblesort_compcert_bpf_bin),
  };
#elif defined(MODULE_GEN_IBPF)
  jitted_thumb_list = ibpf_state.jitted_thumb_list;
  ibpf_full_state_init(&ibpf_state, 2);
  ibpf_set_mem_region_one(&ibpf_state, unsort_list, sizeof(unsort_list), Freeable);
  ibpf_set_code(&ibpf_state, bubblesort_compcert_bpf_bin, sizeof(bubblesort_compcert_bpf_bin));
  ibpf_set_input(&ibpf_state, (intptr_t)unsort_list, (sizeof(unsort_list)/sizeof(unsort_list[0])), 0LLU, 0LLU, 0LLU);
  jit_alu32(&ibpf_state.st);
#else
  bpf_t bpf = {
    .application = (uint8_t*)&bubblesort_compcert_bpf_bin,
    .application_len = sizeof(bubblesort_compcert_bpf_bin),
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
  float duration = (float)(end-begin);
  
  printf("execution time:%f\n", duration);
  
  print_sorted_list(unsort_list, (sizeof(unsort_list)/sizeof(unsort_list[0])));
  return 0;
}
