#include <stdio.h>
#include <inttypes.h>
#include "interpreter.h"
#include "fletcher32_compcert_bpf.h"
#include <stdlib.h>
#include <stddef.h>
#include <time.h>

static const unsigned char wrap_around_data[] =
        "AD3Awn4kb6FtcsyE0RU25U7f55Yncn3LP3oEx9Gl4qr7iDW7I8L6Pbw9jNnh0sE4DmCKuc"
        "d1J8I34vn31W924y5GMS74vUrZQc08805aj4Tf66HgL1cO94os10V2s2GDQ825yNh9Yuq3"
        "QHcA60xl31rdA7WskVtCXI7ruH1A4qaR6Uk454hm401lLmv2cGWt5KTJmr93d3JsGaRRPs"
        "4HqYi4mFGowo8fWv48IcA3N89Z99nf0A0H2R6P0uI4Tir682Of3Rk78DUB2dIGQRRpdqVT"
        "tLhgfET2gUGU65V3edSwADMqRttI9JPVz8JS37g5QZj4Ax56rU1u0m0K8YUs57UYG5645n"
        "byNy4yqxu7";
        
static uint8_t _bpf_stack[512];

int main(){  

  printf ("fletcher32 start!!! \n");
  printf ("sizeof(null)=%ld\n", sizeof(""));
  printf ("sizeof(123)=%ld\n", sizeof("123"));
  printf ("sizeof(wrap_around_data)=%ld\n", sizeof(wrap_around_data));
  unsigned long long result;

  const struct memory_region mr_stk = {
  	.start_addr = (uintptr_t) *_bpf_stack,
  	.block_size = 512,
  	.block_perm = Writable,
  	.block_ptr  = (unsigned char *) (uintptr_t) *_bpf_stack
  };
  
  const struct memory_region mr_content ={
  	.start_addr = (uintptr_t) (const uint16_t *)wrap_around_data,
  	.block_size = sizeof(wrap_around_data),
  	.block_perm = Readable,
  	.block_ptr  = (unsigned char *) (uintptr_t) (const uint16_t *)wrap_around_data
  }; 

  struct memory_region my_memory_regions[] = { mr_stk, mr_content};
  printf ("R10=%lld\n", (unsigned long long) (uintptr_t) (_bpf_stack+512));
  struct bpf_state st = {
    .state_pc = 0,
    .bpf_flag = vBPF_OK,
    .regsmap  = { 0LLU,
    	(unsigned long long) (uintptr_t) (const uint16_t *)wrap_around_data,
    	(unsigned long long) sizeof(wrap_around_data)/2,
    	0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 0LLU, 
    	(unsigned long long) (uintptr_t) (_bpf_stack+512) },
    .mrs_num  = 1,
    .mrs      = my_memory_regions,
    .ins_len  = sizeof(bpf_fletcher32_compcert_bpf_bin)/8,
    .ins      = (const unsigned long long *) bpf_fletcher32_compcert_bpf_bin
  };
  
  
  clock_t begin1 = clock();
  result = bpf_interpreter(&st, 10000);
  clock_t end1 = clock();
  printf("execution time:%f\n", (double)(end1-begin1)/CLOCKS_PER_SEC);
  
  printf("rBPF_fletcher32 (dx) C result = 0x:%x\n", (unsigned int)result);
  printf ("fletcher32 end!!! \n");
  return 0;
}



