#include<stdio.h>



unsigned long long bpf_regs_map[11] = {0LLU};

int main(void){
  int x = 100, y = 60;
  
  bpf_regs_map[10] = 100LLU;
  
  if (x - 30 > y + 11) {
    x = 0xefff;
  }
  else {
    y = 0xdffff;
  }
/*
  __asm volatile (
    ".syntax unified\n\t"
    "bal #0x4\n\t"
    "str r7, [sp, #500]\n\t"
    "ldr r0, [sp, #500]\n\t"
    "orr.w   r1, r1, #1\n\t"
    "add r0, r1\n\t"
    "ldr r4, [r10, #0x408]\n\t"
    :
    : 
    : 
  ); */
  return 0;
}
