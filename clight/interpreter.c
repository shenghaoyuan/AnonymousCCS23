/**************************************************************************/
/*  This file is part of CertrBPF,                                        */
/*  a formally verified rBPF verifier + interpreter + JIT in Coq.         */
/*                                                                        */
/*  Copyright (C) 2022 Inria                                              */
/*                                                                        */
/*  This program is free software; you can redistribute it and/or modify  */
/*  it under the terms of the GNU General Public License as published by  */
/*  the Free Software Foundation; either version 2 of the License, or     */
/*  (at your option) any later version.                                   */
/*                                                                        */
/*  This program is distributed in the hope that it will be useful,       */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU General Public License for more details.                          */
/*                                                                        */
/**************************************************************************/

#include "interpreter.h"
#include <stdint.h> /* for uintptr_t */
/*
#include <inttypes.h>
#include<stdlib.h>
#include<stddef.h>

void print_bpf_state(struct bpf_state* st){
  printf("pc= %" PRIu64 "\n", (unsigned long long) (*st).state_pc);
  printf("flag= %d\n", (*st).bpf_flag);
  for(int i = 0; i < 11; i++){
    printf("R%d",i);
    printf("= %" PRIu64 ";", (unsigned long long) (*st).regsmap[i]);
  }
  printf("\n");
}
*/

static __attribute__((always_inline)) inline unsigned int eval_pc (struct bpf_state* st) {
  return (*st).state_pc;
}

static __attribute__((always_inline)) inline void upd_pc(struct bpf_state* st, unsigned int pc) {
  (*st).state_pc = pc;
  return ;
}
static __attribute__((always_inline)) inline void upd_pc_incr(struct bpf_state* st) {
  (*st).state_pc = (*st).state_pc+1;
  return ;
}


static __attribute__((always_inline)) inline unsigned long long eval_reg(struct bpf_state* st, unsigned int i){
  return (*st).regsmap[i];
}

static __attribute__((always_inline)) inline void upd_reg (struct bpf_state* st, unsigned int i, unsigned long long v){
  (*st).regsmap[i] = v;
  return ;
}

static __attribute__((always_inline)) inline unsigned eval_flag(struct bpf_state* st){
  return (*st).bpf_flag;
}

static __attribute__((always_inline)) inline void upd_flag(struct bpf_state* st, unsigned f){
  (*st).bpf_flag = f;
  return ;
}

static __attribute__((always_inline)) inline unsigned int eval_mrs_num(struct bpf_state* st){
  return (*st).mrs_num;
}

static __attribute__((always_inline)) inline struct memory_region *eval_mrs_regions(struct bpf_state* st){
  return (*st).mrs;
}

/*
void add_mem_region(struct bpf_state* st, struct memory_region* mr){
  (*st).mrs[(*st).mem_num] = *mr;
  (*st).mem_num += 1;
  return ;
}

void add_mem_region_ctx(struct bpf_state* st, struct memory_region* mr){
  (*st).mrs[0] = *mr;
  (*st).mem_num = 1;
  return ;
} */

static __attribute__((always_inline)) inline unsigned long long load_mem(struct bpf_state* st, unsigned int chunk, unsigned char* addr){
  /*if (addr == 0U) {
    (*st).bpf_flag = BPF_ILLEGAL_MEM; return ;
  }
  else{*/
    switch (chunk) {
      case 1: return *(unsigned char *) addr;
      case 2: return *(unsigned short *) addr;
      case 4: return *(unsigned int *) addr;
      case 8: return *(unsigned long long *) addr;
      default: /*printf ("load:addr = %" PRIu64 "\n", v); (*st).bpf_flag = BPF_ILLEGAL_MEM;*/ return 0LLU;
    }
  //}
}

static __attribute__((always_inline)) inline void store_mem_reg(struct bpf_state* st, unsigned char* addr, unsigned int chunk, unsigned long long v){
  /*if (addr == 0U) {
    (*st).bpf_flag = BPF_ILLEGAL_MEM; return ;
  }
  else{*/
    switch (chunk) {
      case 1: *(unsigned char *) addr = v; return ;
      case 2: *(unsigned short *) addr = v; return ;
      case 4: *(unsigned int *) addr = v; return ;
      case 8: *(unsigned long long *) addr = v; return ;
      default: /*printf ("store_reg:addr = %" PRIu64 "\n", addr); (*st).bpf_flag = BPF_ILLEGAL_MEM;*/ return ;
    }
  //}
}

static __attribute__((always_inline)) inline void store_mem_imm(struct bpf_state* st, unsigned char* addr, unsigned int chunk, int v){
  /*if (addr == 0U) {
    (*st).bpf_flag = BPF_ILLEGAL_MEM; return ;
  }
  else{*/
    switch (chunk) {
      case 1: *(unsigned char *) addr = v; return ;
      case 2: *(unsigned short *) addr = v; return ;
      case 4: *(unsigned int *) addr = v; return ;
      case 8: *(unsigned long long *) addr = v; return ;
      default: /*printf ("store_imm:addr = %" PRIu64 "\n", addr); (*st).bpf_flag = BPF_ILLEGAL_MEM;*/ return ;
    }
  //}
}

static __attribute__((always_inline)) inline unsigned int eval_ins_len(struct bpf_state* st)
{
  return (*st).ins_len;
}

static __attribute__((always_inline)) inline unsigned long long eval_ins(struct bpf_state* st, unsigned int idx)
{
  return *((*st).ins + idx);
}

static __attribute__((always_inline)) inline _Bool cmp_ptr32_nullM(unsigned char* addr){
   return (addr == 0);
}

static __attribute__((always_inline)) inline unsigned int get_dst(unsigned long long ins)
{
  return (unsigned int) ((ins & 4095LLU) >> 8LLU);
}

static __attribute__((always_inline)) inline unsigned int get_src(unsigned long long ins)
{
  return (unsigned int) ((ins & 65535LLU) >> 12LLU);
}

static __attribute__((always_inline)) inline struct memory_region *get_mem_region(unsigned int n, struct memory_region *mrs)
{
  return mrs + n;
}

static __attribute__((always_inline)) inline unsigned char *_bpf_get_call(int imm) {
  /* deleting `return NULL;` and adding your system APIs
  switch (imm) {
    default: return ...
  }
  */
  return NULL;
}

static __attribute__((always_inline)) inline unsigned int exec_function(struct bpf_state* st, unsigned char * ptr){
  if (ptr == 0){
    (*st).bpf_flag = vBPF_ILLEGAL_CALL;
    return 0U;
  }
  else {
    /**do something e.g. print; */
    return 0U;
  }
}

/*******************below code are automatically generated by dx (after repatch) ***************************/

static __attribute__((always_inline)) inline unsigned int reg64_to_reg32(unsigned long long d)
{
  return (unsigned int) d;
}

static __attribute__((always_inline)) inline int get_offset(unsigned long long ins)
{
  return (int) (short) (ins << 32LLU >> 48LLU);
}

static __attribute__((always_inline)) inline int get_immediate(unsigned long long ins)
{
  return (int) (ins >> 32LLU);
}

static __attribute__((always_inline)) inline long long eval_immediate(int ins)
{
  return (long long) ins;
}

static __attribute__((always_inline)) inline unsigned long long get_src64(struct bpf_state* st, unsigned char x, unsigned long long ins)
{
  int imm;
  long long imm64;
  unsigned int src;
  unsigned long long src64;
  if (0U == (x & 8U)) {
    imm = get_immediate(ins);
    imm64 = eval_immediate(imm);
    return (unsigned long long) imm64;
  } else {
    src = get_src(ins);
    src64 = eval_reg(st, src);
    return src64;
  }
}

static __attribute__((always_inline)) inline unsigned int get_src32(struct bpf_state* st, unsigned char x, unsigned long long ins)
{
  int imm;
  unsigned int src;
  unsigned long long src64;
  unsigned int src32;
  if (0U == (x & 8U)) {
    imm = get_immediate(ins);
    return imm;
  } else {
    src = get_src(ins);
    src64 = eval_reg(st, src);
    src32 = reg64_to_reg32(src64);
    return src32;
  }
}

static __attribute__((always_inline)) inline unsigned char get_opcode_ins(unsigned long long ins)
{
  return (unsigned char) (ins & 255LLU);
}

static __attribute__((always_inline)) inline unsigned char get_opcode_alu64(unsigned char op)
{
  return (unsigned char) (op & 240);
}

static __attribute__((always_inline)) inline unsigned char get_opcode_alu32(unsigned char op)
{
  return (unsigned char) (op & 240);
}

static __attribute__((always_inline)) inline unsigned char get_opcode_branch(unsigned char op)
{
  return (unsigned char) (op & 240);
}

static __attribute__((always_inline)) inline unsigned char get_opcode_mem_ld_imm(unsigned char op)
{
  return (unsigned char) (op & 255);
}

static __attribute__((always_inline)) inline unsigned char get_opcode_mem_ld_reg(unsigned char op)
{
  return (unsigned char) (op & 255);
}

static __attribute__((always_inline)) inline unsigned char get_opcode_mem_st_imm(unsigned char op)
{
  return (unsigned char) (op & 255);
}

static __attribute__((always_inline)) inline unsigned char get_opcode_mem_st_reg(unsigned char op)
{
  return (unsigned char) (op & 255);
}

static __attribute__((always_inline)) inline unsigned char get_opcode(unsigned char op)
{
  return (unsigned char) (op & 7);
}

static __attribute__((always_inline)) inline unsigned int get_add(unsigned int x, unsigned int y)
{
  return x + y;
}

static __attribute__((always_inline)) inline unsigned int get_sub(unsigned int x, unsigned int y)
{
  return x - y;
}

static __attribute__((always_inline)) inline unsigned int get_addr_ofs(unsigned long long x, int ofs)
{
  return (unsigned int) (x + (unsigned long long) ofs);
}

static __attribute__((always_inline)) inline unsigned int get_start_addr(struct memory_region *mr)
{
  return (*mr).start_addr;
}

static __attribute__((always_inline)) inline unsigned int get_block_size(struct memory_region *mr)
{
  return (*mr).block_size;
}

static __attribute__((always_inline)) inline unsigned int get_block_perm(struct memory_region *mr)
{
  return (*mr).block_perm;
}

static __attribute__((always_inline)) inline _Bool is_well_chunk_bool(unsigned int chunk)
{
  switch (chunk) {
    case 1:
      return 1;
    case 2:
      return 1;
    case 4:
      return 1;
    case 8:
      return 1;
    default:
      return 0;
    
  }
}

static __attribute__((always_inline)) inline unsigned char *check_mem_aux2(struct memory_region *mr, unsigned int perm, unsigned int addr, unsigned int chunk)
{
  unsigned int start;
  unsigned int size;
  unsigned int mr_perm;
  unsigned int lo_ofs;
  unsigned int hi_ofs;
  start = get_start_addr(mr);
  size = get_block_size(mr);
  mr_perm = get_block_perm(mr);
  lo_ofs = get_sub(addr, start);
  hi_ofs = get_add(lo_ofs, chunk);
  if (hi_ofs <= size
        && (lo_ofs <= 4294967295U - chunk && 0U == lo_ofs % chunk)
        && mr_perm >= perm) {
    return (*mr).block_ptr + lo_ofs;
  } else {
    return 0;
  }
}

static __attribute__((always_inline)) inline unsigned char *check_mem_aux(struct bpf_state* st, unsigned int num, unsigned int perm, unsigned int chunk, unsigned int addr, struct memory_region *mrs)
{
  unsigned int n;
  struct memory_region *cur_mr;
  unsigned char *check_mem;
  _Bool is_null;
  if (num == 0U) {
    return 0;
  } else {
    n = num - 1U;
    cur_mr = get_mem_region(n, mrs);
    check_mem = check_mem_aux2(cur_mr, perm, addr, chunk);
    is_null = cmp_ptr32_nullM(check_mem);
    if (is_null) {
      return check_mem_aux(st, n, perm, chunk, addr, mrs);
    } else {
      return check_mem;
    }
  }
}

unsigned char *check_mem(struct bpf_state* st, unsigned int perm, unsigned int chunk, unsigned int addr)
{
  _Bool well_chunk;
  unsigned int mem_reg_num;
  struct memory_region *mrs;
  unsigned char *check_mem;
  _Bool is_null;
  well_chunk = is_well_chunk_bool(chunk);
  if (well_chunk) {
    mem_reg_num = eval_mrs_num(st);
    mrs = eval_mrs_regions(st);
    check_mem =
      check_mem_aux(st, mem_reg_num, perm, chunk, addr, mrs);
    is_null = cmp_ptr32_nullM(check_mem);
    if (is_null) {
      return 0;
    } else {
      return check_mem;
    }
  } else {
    return 0;
  }
}

static __attribute__((always_inline)) inline void step_opcode_alu64(struct bpf_state* st, unsigned long long dst64, unsigned long long src64, unsigned int dst, unsigned char op)
{
  unsigned char opcode_alu64;
  unsigned int src32;
  opcode_alu64 = get_opcode_alu64(op);
  switch (opcode_alu64) {
    case 0:
      upd_reg(st, dst, dst64 + src64);
      return;
    case 16:
      upd_reg(st, dst, dst64 - src64);
      return;
    case 32:
      upd_reg(st, dst, dst64 * src64);
      return;
    case 48:
      if (src64 != 0LLU) {
        upd_reg(st, dst, dst64 / src64);
        return;
      } else {
        upd_flag(st, 10U);
        return;
      }
    case 64:
      upd_reg(st, dst, dst64 | src64);
      return;
    case 80:
      upd_reg(st, dst, dst64 & src64);
      return;
    case 96:
      src32 = reg64_to_reg32(src64);
      if (src32 < 64U) {
        upd_reg(st, dst, dst64 << src32);
        return;
      } else {
        upd_flag(st, 11U);
        return;
      }
    case 112:
      src32 = reg64_to_reg32(src64);
      if (src32 < 64U) {
        upd_reg(st, dst, dst64 >> src32);
        return;
      } else {
        upd_flag(st, 11U);
        return;
      }
    case 128:
      if (op == 135) {
        upd_reg(st, dst, -dst64);
        return;
      } else {
        upd_flag(st, 2U);
        return;
      }
    case 144:
      if (src64 != 0LLU) {
        upd_reg(st, dst, dst64 % src64);
        return;
      } else {
        upd_flag(st, 10U);
        return;
      }
    case 160:
      upd_reg(st, dst, dst64 ^ src64);
      return;
    case 176:
      upd_reg(st, dst, src64);
      return;
    case 192:
      src32 = reg64_to_reg32(src64);
      if (src32 < 64U) {
        upd_reg(st, dst, (unsigned long long) ((long long) dst64 >> src32));
        return;
      } else {
        upd_flag(st, 11U);
        return;
      }
    default:
      upd_flag(st, 2U);
      return;
    
  }
}

static __attribute__((always_inline)) inline void step_opcode_alu32(struct bpf_state* st, unsigned int dst32, unsigned int src32, unsigned int dst, unsigned char op)
{
  unsigned char opcode_alu32;
  opcode_alu32 = get_opcode_alu32(op);
  switch (opcode_alu32) {
    case 0:
      upd_reg(st, dst, (unsigned long long) (dst32 + src32));
      return;
    case 16:
      upd_reg(st, dst, (unsigned long long) (dst32 - src32));
      return;
    case 32:
      upd_reg(st, dst, (unsigned long long) (dst32 * src32));
      return;
    case 48:
      if (src32 != 0U) {
        upd_reg(st, dst, (unsigned long long) (dst32 / src32));
        return;
      } else {
        upd_flag(st, 10U);
        return;
      }
    case 64:
      upd_reg(st, dst, (unsigned long long) (dst32 | src32));
      return;
    case 80:
      upd_reg(st, dst, (unsigned long long) (dst32 & src32));
      return;
    case 96:
      if (src32 < 32U) {
        upd_reg(st, dst, (unsigned long long) (dst32 << src32));
        return;
      } else {
        upd_flag(st, 11U);
        return;
      }
    case 112:
      if (src32 < 32U) {
        upd_reg(st, dst, (unsigned long long) (dst32 >> src32));
        return;
      } else {
        upd_flag(st, 11U);
        return;
      }
    case 128:
      if (op == 132) {
        upd_reg(st, dst, (unsigned long long) -dst32);
        return;
      } else {
        upd_flag(st, 2U);
        return;
      }
    case 144:
      if (src32 != 0U) {
        upd_reg(st, dst, (unsigned long long) (dst32 % src32));
        return;
      } else {
        upd_flag(st, 10U);
        return;
      }
    case 160:
      upd_reg(st, dst, (unsigned long long) (dst32 ^ src32));
      return;
    case 176:
      upd_reg(st, dst, (unsigned long long) src32);
      return;
    case 192:
      if (src32 < 32U) {
        upd_reg(st, dst, (unsigned long long) ((int) dst32 >> src32));
        return;
      } else {
        upd_flag(st, 11U);
        return;
      }
    default:
      upd_flag(st, 2U);
      return;
    
  }
}

static __attribute__((always_inline)) inline void step_opcode_branch(struct bpf_state* st, unsigned long long dst64, unsigned long long src64, unsigned int pc, unsigned int ofs, unsigned char op)
{
  unsigned char opcode_jmp;
  unsigned char *f_ptr;
  _Bool is_null;
  unsigned int res;
  opcode_jmp = get_opcode_branch(op);
  switch (opcode_jmp) {
    case 0:
      if (op == 5) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        upd_flag(st, 2U);
        return;
      }
    case 16:
      if (dst64 == src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 32:
      if (dst64 > src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 48:
      if (dst64 >= src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 160:
      if (dst64 < src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 176:
      if (dst64 <= src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 64:
      if ((dst64 & src64) != 0LLU) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 80:
      if (dst64 != src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 96:
      if ((long long) dst64 > (long long) src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 112:
      if ((long long) dst64 >= (long long) src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 192:
      if ((long long) dst64 < (long long) src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 208:
      if ((long long) dst64 <= (long long) src64) {
        upd_pc(st, pc + ofs);
        return;
      } else {
        return;
      }
    case 128:
      if (op == 133) {
        f_ptr = _bpf_get_call((int) src64);
        is_null = cmp_ptr32_nullM(f_ptr);
        if (is_null) {
          upd_flag(st, 5U);
          return;
        } else {
          res = exec_function(st, f_ptr);
          upd_reg(st, 0U, (unsigned long long) res);
          return;
        }
      } else {
        upd_flag(st, 2U);
        return;
      }
    case 144:
      if (op == 149) {
        upd_flag(st, 1U);
        return;
      } else {
        upd_flag(st, 2U);
        return;
      }
    default:
      upd_flag(st, 2U);
      return;
    
  }
}

static __attribute__((always_inline)) inline void step_opcode_mem_ld_imm(struct bpf_state* st, int imm, unsigned long long dst64, unsigned int dst, unsigned char op)
{
  unsigned char opcode_ld;
  opcode_ld = get_opcode_mem_ld_imm(op);
  switch (opcode_ld) {
    case 24:
      upd_reg(st, dst, (unsigned long long) (unsigned int) imm);
      return;
    case 16:
      upd_reg(st, dst,
              dst64 | (unsigned long long) (unsigned int) imm << 32U);
      return;
    default:
      upd_flag(st, 2U);
      return;
    
  }
}

static __attribute__((always_inline)) inline void step_opcode_mem_ld_reg(struct bpf_state* st, unsigned int addr, unsigned int dst, unsigned char op)
{
  unsigned char opcode_ld;
  unsigned char *addr_ptr;
  _Bool is_null;
  unsigned long long v;
  opcode_ld = get_opcode_mem_ld_reg(op);
  switch (opcode_ld) {
    case 97:
      addr_ptr = check_mem(st, 1U, 4U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        v = load_mem(st, 4U, addr_ptr);
        upd_reg(st, dst, v);
        return;
      }
    case 105:
      addr_ptr = check_mem(st, 1U, 2U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        v = load_mem(st, 2U, addr_ptr);
        upd_reg(st, dst, v);
        return;
      }
    case 113:
      addr_ptr = check_mem(st, 1U, 1U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        v = load_mem(st, 1U, addr_ptr);
        upd_reg(st, dst, v);
        return;
      }
    case 121:
      addr_ptr = check_mem(st, 1U, 8U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        v = load_mem(st, 8U, addr_ptr);
        upd_reg(st, dst, v);
        return;
      }
    default:
      upd_flag(st, 2U);
      return;
    
  }
}

static __attribute__((always_inline)) inline void step_opcode_mem_st_imm(struct bpf_state* st, int imm, unsigned int addr, unsigned char op)
{
  unsigned char opcode_st;
  unsigned char *addr_ptr;
  _Bool is_null;
  opcode_st = get_opcode_mem_st_imm(op);
  switch (opcode_st) {
    case 98:
      addr_ptr = check_mem(st, 2U, 4U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        store_mem_imm(st, addr_ptr, 4U, imm);
        return;
      }
    case 106:
      addr_ptr = check_mem(st, 2U, 2U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        store_mem_imm(st, addr_ptr, 2U, imm);
        return;
      }
    case 114:
      addr_ptr = check_mem(st, 2U, 1U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        store_mem_imm(st, addr_ptr, 1U, imm);
        return;
      }
    case 122:
      addr_ptr = check_mem(st, 2U, 8U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        store_mem_imm(st, addr_ptr, 8U, imm);
        return;
      }
    default:
      upd_flag(st, 2U);
      return;
    
  }
}

static __attribute__((always_inline)) inline void step_opcode_mem_st_reg(struct bpf_state* st, unsigned long long src64, unsigned int addr, unsigned char op)
{
  unsigned char opcode_st;
  unsigned char *addr_ptr;
  _Bool is_null;
  opcode_st = get_opcode_mem_st_reg(op);
  switch (opcode_st) {
    case 99:
      addr_ptr = check_mem(st, 2U, 4U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        store_mem_reg(st, addr_ptr, 4U, src64);
        return;
      }
    case 107:
      addr_ptr = check_mem(st, 2U, 2U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        store_mem_reg(st, addr_ptr, 2U, src64);
        return;
      }
    case 115:
      addr_ptr = check_mem(st, 2U, 1U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        store_mem_reg(st, addr_ptr, 1U, src64);
        return;
      }
    case 123:
      addr_ptr = check_mem(st, 2U, 8U, addr);
      is_null = cmp_ptr32_nullM(addr_ptr);
      if (is_null) {
        upd_flag(st, 3U);
        return;
      } else {
        store_mem_reg(st, addr_ptr, 8U, src64);
        return;
      }
    default:
      upd_flag(st, 2U);
      return;
    
  }
}

static __attribute__((always_inline)) inline void step(struct bpf_state* st)
{
  unsigned int pc;
  unsigned long long ins;
  unsigned char op;
  unsigned char opc;
  unsigned int dst;
  unsigned long long dst64;
  unsigned long long src64;
  unsigned int dst32;
  unsigned int src32;
  int ofs;
  int imm;
  unsigned int src;
  unsigned int addr;
  pc = eval_pc(st);
  ins = eval_ins(st, pc);
  op = get_opcode_ins(ins);
  opc = get_opcode(op);
  dst = get_dst(ins);
  switch (opc) {
    case 7:
      dst64 = eval_reg(st, dst);
      src64 = get_src64(st, op, ins);
      step_opcode_alu64(st, dst64, src64, dst, op);
      return;
    case 4:
      dst64 = eval_reg(st, dst);
      dst32 = reg64_to_reg32(dst64);
      src32 = get_src32(st, op, ins);
      step_opcode_alu32(st, dst32, src32, dst, op);
      return;
    case 5:
      dst64 = eval_reg(st, dst);
      ofs = get_offset(ins);
      src64 = get_src64(st, op, ins);
      step_opcode_branch(st, dst64, src64, pc,
                         (unsigned int) ofs, op);
      return;
    case 0:
      dst64 = eval_reg(st, dst);
      imm = get_immediate(ins);
      step_opcode_mem_ld_imm(st, imm, dst64, dst, op);
      return;
    case 1:
      src = get_src(ins);
      src64 = eval_reg(st, src);
      ofs = get_offset(ins);
      addr = get_addr_ofs(src64, ofs);
      step_opcode_mem_ld_reg(st, addr, dst, op);
      return;
    case 2:
      dst64 = eval_reg(st, dst);
      ofs = get_offset(ins);
      imm = get_immediate(ins);
      addr = get_addr_ofs(dst64, ofs);
      step_opcode_mem_st_imm(st, imm, addr, op);
      return;
    case 3:
      dst64 = eval_reg(st, dst);
      src = get_src(ins);
      src64 = eval_reg(st, src);
      ofs = get_offset(ins);
      addr = get_addr_ofs(dst64, ofs);
      step_opcode_mem_st_reg(st, src64, addr, op);
      return;
    default:
      upd_flag(st, 2U);
      return;
    
  }
}

static __attribute__((always_inline)) inline void bpf_interpreter_aux(struct bpf_state* st, unsigned int fuel)
{
  unsigned int fuel0;
  unsigned int len;
  unsigned int pc;
  unsigned int f;
  unsigned int len0;
  unsigned int pc0;
  if (fuel == 0U) {
    upd_flag(st, 6U);
    return;
  } else {
    fuel0 = fuel - 1U;
    len = eval_ins_len(st);
    pc = eval_pc(st);
    if (pc < len) {
      step(st); //print_bpf_state(st);
      f = eval_flag(st);
      if (f == 0U) {
        len0 = eval_ins_len(st);
        pc0 = eval_pc(st);
        if (pc0 + 1U < len0) {
          upd_pc_incr(st);
          bpf_interpreter_aux(st, fuel0);
          return;
        } else {
          upd_flag(st, 6U);
          return;
        }
      } else {
        return;
      }
    } else {
      upd_flag(st, 6U);
      return;
    }
  }
}

unsigned long long bpf_interpreter(struct bpf_state* st, unsigned int fuel)
{
  unsigned int f;
  unsigned long long res;
  bpf_interpreter_aux(st, fuel);
  f = eval_flag(st);
  if (f == 1U) {
    res = eval_reg(st, 0U);
    return res;
  } else {
    return 0LLU;
  }
}

