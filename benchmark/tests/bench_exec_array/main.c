#include <stdio.h>
#include <string.h>
#include <stdint.h>

__attribute__((aligned(4))) unsigned char code[] = {
  //0x4f, 0xf0, 0x2a, 0x00, 0x70, 0x47 //encoding T2
  0x40, 0xf2, 0x2a, 0x00, 0x70, 0x47, 0x40, 0xf2, 0x3a, 0x00, 0x70, 0x47 //encoding T3
};

/*
002af04f mov.w r0, #42; 0x2a
4770     bx lr
*/

/*
002af04f -> adopts MOVW encodeing T2 where 002a is low-16: `0 000 0000 00101010` and f04f is high-16: `11110 0 0 0010 0 1111`
*/

/*
Let's try MOVW encoding T3: 40 F2 2a 00
*/

int main(void) {
    /*
    union {
      uintptr_t as_int;
      int(*fn)(void);
    } helper;

    helper.as_int = ((uintptr_t)&code[0] | 0x1);

    int i = helper.fn();

    printf("get this done. returned: %d\n", i);
    */
    
    int i;
    
    __asm volatile ("orr %[input_0], #0x1\n\t"
    "blx %[input_0]\n\t"
    "mov r1, r0\n\t"
    : [result] "=r" (i)
    : [input_0] "r" (code+6)
    :
    );

    printf("get this done. returned: %d\n", i);
    

    return 0;
}
