
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#include "ibpf_interpreter.h"

#define BPF_INSTRUCTION_CLS_MASK        0x07
#define BPF_INSTRUCTION_CLS_BRANCH      0x05


static __attribute__((always_inline)) inline unsigned int get_dst(unsigned long long ins)
{
  return (unsigned int) ((ins & 4095LLU) >> 8LLU);
}

static __attribute__((always_inline)) inline unsigned int get_src(unsigned long long ins)
{
  return (unsigned int) ((ins & 65535LLU) >> 12LLU);
}

static __attribute__((always_inline)) inline unsigned char get_opcode_ins(unsigned long long ins)
{
  return (unsigned char) (ins & 255LLU);
}

static __attribute__((always_inline)) inline int get_immediate(unsigned long long ins)
{
  return (int) (ins >> 32LLU);
}

static __attribute__((always_inline)) inline int get_offset(unsigned long long ins)
{
  return (int) (short) (ins << 32LLU >> 48LLU);
}


int bpf_verify_preflight(struct jit_state* st)
{
    if (st->ins_len & 0x7) {
        return vBPF_ILLEGAL_LEN;
    }


    for (unsigned i = 0; i < st->ins_len; i++) {
        unsigned long long ins = st->jit_ins[i];
        /* Check if register values are valid */
        if (get_dst(ins) >= 11 || get_dst(ins) >= 11) {
            return vBPF_ILLEGAL_REGISTER;
        }

        /* Double length instruction */
        if (get_opcode_ins(ins) == 0x18) {
            i++;
            continue;
        }

        /* Only instruction-specific checks here */
        if ((get_opcode_ins(ins) & BPF_INSTRUCTION_CLS_MASK) == BPF_INSTRUCTION_CLS_BRANCH) {
           int offset = get_offset(ins);
            intptr_t target = (intptr_t)(st->jit_ins[i+offset]);
            /* Check if the jump target is within bounds. The address is
             * incremented after the jump by the regular PC increase */
            if ((target >= (intptr_t)((uint8_t*)st->jit_ins + st->ins_len))
                || (target < (intptr_t)st->jit_ins)) {
                return vBPF_ILLEGAL_JUMP;
            }
        }
        
        /* check illegal 32-bit shift */
        if (get_opcode_ins(ins) == 0x6c || get_opcode_ins(ins) == 0x7c || get_opcode_ins(ins) == 0xcc) {
          if (i = 0) { return vBPF_ILLEGAL_INSTRUCTION; }
          if (get_opcode_ins(st->jit_ins[i-1]) == 0xb4){
            if (0 >= get_immediate(st->jit_ins[i-1])  || get_immediate(st->jit_ins[i-1]) >= 32) {
              return vBPF_ILLEGAL_SHIFT;
            }
          }
          else {
            return vBPF_ILLEGAL_DIV;
          }
          
        
        }
        
        /* check illegal 32-bit div-by-zero */
        if (get_opcode_ins(ins) == 0x3c) {
          if (i = 0) { return vBPF_ILLEGAL_INSTRUCTION; }
          if (get_opcode_ins(st->jit_ins[i-1]) == 0xb4){
            if (0 == get_immediate(st->jit_ins[i-1]) || get_dst(ins) != 0 || get_src(ins) != 1) {
              return vBPF_ILLEGAL_DIV;
            }
          }
          else {
            return vBPF_ILLEGAL_DIV;
          }
        
        }
        
        /* check illegal 32-bit div/mod/shift */
        if (get_opcode_ins(ins) == 0x34 || get_opcode_ins(ins) == 0x94 || get_opcode_ins(ins) == 0x9c) {
          return vBPF_ILLEGAL_DIV;
        }
        if (get_opcode_ins(ins) == 0x64 || get_opcode_ins(ins) == 0x74 || get_opcode_ins(ins) == 0xc4) {
          return vBPF_ILLEGAL_DIV;
        }

    }


    /* Check if the last instruction is a return instruction */
    if (get_opcode_ins(st->jit_ins[st->ins_len - 1]) != 0x95) {
        return vBPF_NO_RETURN;
    }
    return vBPF_OK;
}
