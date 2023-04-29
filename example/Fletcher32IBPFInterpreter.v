From compcert Require Import Integers Memdata Memory AST Values.
From compcert.arm Require Import AsmSyntax BinSyntax ABinSem BinDecode.

From Coq Require Import Ascii String List ZArith.
Import ListNotations.

From bpf.comm Require Import ListAsArray MemRegion rBPFMonadOp.
From bpf.model Require Import Decode Encode PrintrBPF.
From bpf.jit.thumb Require Import LoadStoreRegs JITState ThumbDecode ThumbJITOpcode ThumbJIT ThumbDecode PrintThumb PrintJIT.
From bpf.jit.iBPF Require Import IDecode ISemantics PrintiBPF.

From bpf.example Require Import DebugExtraction.

Definition wrap_around_data_ascii:string :=  "AD3Awn4kb6FtcsyE0RU25U7f55Yncn3LP3oEx9Gl4qr7iDW7I8L6Pbw9jNnh0sE4DmCKuc" ++
"d1J8I34vn31W924y5GMS74vUrZQc08805aj4Tf66HgL1cO94os10V2s2GDQ825yNh9Yuq3" ++
"QHcA60xl31rdA7WskVtCXI7ruH1A4qaR6Uk454hm401lLmv2cGWt5KTJmr93d3JsGaRRPs" ++
"4HqYi4mFGowo8fWv48IcA3N89Z99nf0A0H2R6P0uI4Tir682Of3Rk78DUB2dIGQRRpdqVT" ++
"tLhgfET2gUGU65V3edSwADMqRttI9JPVz8JS37g5QZj4Ax56rU1u0m0K8YUs57UYG5645n" ++
"byNy4yqxu7".

Definition wrap_around_data_nat: list nat :=
  let ascii_list: list ascii := list_ascii_of_string wrap_around_data_ascii in
  map nat_of_ascii ascii_list.

Compute wrap_around_data_nat.

Definition wrap_around_data_byte: list memval :=
  let ascii_list: list ascii := list_ascii_of_string wrap_around_data_ascii in
  map (fun a => Byte (Byte.repr (Z.of_N (N_of_ascii a)))) ascii_list.

Compute List.length wrap_around_data_byte.

Definition words:int := Int.repr 180. (**r (length data)/2 *)

(** here we have lddw_high and lddw_low *)
Definition test_fletcher32_int64: list int64 :=
[
(Int64.repr 0x000000000000a0bc);
(Int64.repr 0x0000001000000a14);
(Int64.repr 0x0000000000000a63);
(Int64.repr 0x0000000000049a63);
(Int64.repr 0x0000000000086a63);
(Int64.repr 0x0000ffff000005b4);
(Int64.repr 0x00000000000053bc);
(Int64.repr 0x00000000001c0215);
(Int64.repr 0x00000167000202b5);
(Int64.repr 0x00000167000000b4);
(Int64.repr 0x0000000000010005);
(Int64.repr 0x00000000000020bc);
(Int64.repr 0x000000000000021c);
(Int64.repr 0x00000000000014bc);
(Int64.repr 0x00000000000041bc);
(Int64.repr 0x0000000200000104);
(Int64.repr 0x0000000000004469);
(Int64.repr 0x000000000000450c);
(Int64.repr 0x000000000000530c);
(Int64.repr 0xffffffff00000004);
(Int64.repr 0x00000000fff80055);
(Int64.repr 0x00000000000054bc);
(Int64.repr 0x0000ffff00000454);
(Int64.repr 0x00000010000006b4);
(Int64.repr 0x00000000000050bc);
(Int64.repr 0x000000000000607c);
(Int64.repr 0x00000000000045bc);
(Int64.repr 0x000000000000050c);
(Int64.repr 0x00000000000034bc);
(Int64.repr 0x0000ffff00000454);
(Int64.repr 0x00000010000006b4);
(Int64.repr 0x00000000000030bc);
(Int64.repr 0x000000000000607c);
(Int64.repr 0x00000000000043bc);
(Int64.repr 0x000000000000030c);
(Int64.repr 0x00000000ffe30005);
(Int64.repr 0x00000000000051bc);
(Int64.repr 0x0000ffff00000154);
(Int64.repr 0x00000010000004b4);
(Int64.repr 0x000000000000457c);
(Int64.repr 0x000000000000510c);
(Int64.repr 0x00000000000030bc);
(Int64.repr 0x0000ffff00000054);
(Int64.repr 0x00000010000002b4);
(Int64.repr 0x000000000000237c);
(Int64.repr 0x000000000000300c);
(Int64.repr 0x00000010000004b4);
(Int64.repr 0x000000000000406c);
(Int64.repr 0x000000000000104c);
(Int64.repr 0x000000000008a661);
(Int64.repr 0x000000000004a961);
(Int64.repr 0x0000001000000a04);
(Int64.repr 0x0000000000000095)
].
(*
Open Scope string_scope.

Compute print_rBPF_prog (decode_prog test_fletcher32_int64 (List.length test_fletcher32_int64)).

= ["0x0: r0 =.32 r10";
   "0x1: r10 -=.32 0x10";
   "0x2: *(u32 *)(r10 + 0x0) = r0";
   "0x3: *(u32 *)(r10 + 0x4) = r9";
   "0x4: *(u32 *)(r10 + 0x8) = r6";
   "0x5: r5 =.32 0xffff";
   "0x6: r3 =.32 r5";
   "0x7: if r2 == 0x0 goto 0x1c";
   "0x8: if r2 <= 0x167 goto 0x2";
   "0x9: r0 =.32 0x167";
   "0xa: goto 0x1";
   "0xb: r0 =.32 r2";
   "0xc: r2 -=.32 r0";
   "0xd: r4 =.32 r1";
   "0xe: r1 =.32 r4";
   "0xf: r1 +=.32 0x2";
   "0x10: r4 = *(u16 *)(r4 + 0x0)";
   "0x11: r5 +=.32 r4";
   "0x12: r3 +=.32 r5";
   "0x13: r0 +=.32 -0x1";
   "0x14: if r0 != 0x0 goto -0x8";
   "0x15: r4 =.32 r5";
   "0x16: r4 &=.32 0xffff";
   "0x17: r6 =.32 0x10";
   "0x18: r0 =.32 r5";
   "0x19: r0 >>=.32 r6";
   "0x1a: r5 =.32 r4";
   "0x1b: r5 +=.32 r0";
   "0x1c: r4 =.32 r3";
   "0x1d: r4 &=.32 0xffff";
   "0x1e: r6 =.32 0x10";
   "0x1f: r0 =.32 r3";
   "0x20: r0 >>=.32 r6";
   "0x21: r3 =.32 r4";
   "0x22: r3 +=.32 r0";
   "0x23: goto -0x1d";
   "0x24: r1 =.32 r5";
   "0x25: r1 &=.32 0xffff";
   "0x26: r4 =.32 0x10";
   "0x27: r5 >>=.32 r4";
   "0x28: r1 +=.32 r5";
   "0x29: r0 =.32 r3";
   "0x2a: r0 &=.32 0xffff";
   "0x2b: r2 =.32 0x10";
   "0x2c: r3 >>=.32 r2";
   "0x2d: r0 +=.32 r3";
   "0x2e: r4 =.32 0x10";
   "0x2f: r0 <<=.32 r4";
   "0x30: r0 |=.32 r1";
   "0x31: r6 = *(u32 *)(r10 + 0x8)";
   "0x32: r9 = *(u32 *)(r10 + 0x4)";
   "0x33: r10 +=.32 0x10";
   "0x34: exit"]
 : list string

where,
- epl = [0; 5; 36; 11; 9; 12; 17; 13; 21; 51]
*)


(**r Memory info:
   - block 1: [0, 361) input_data

   - block 2: [0, 4) bpf_flag
   - block 3: [0, 88) regsiter_map

   - block 4: [0, 512) rBPF stack
   - block 5: [0, 12) jit_state
   - block 6: [0, ?) jitted_arm_list
*)

Definition init_mem := Mem.alloc Mem.empty 0 361.

Definition input_blk: block := (snd init_mem).

(**r mem1 includes the input_data *)
Definition mem1 :=
  match Mem.storebytes (fst init_mem) input_blk 0%Z wrap_around_data_byte with
  | Some m => m
  | None => Mem.empty
  end.


Definition init_mem2 := Mem.alloc mem1 0 4.

Definition bpf_flag_blk: block := (snd init_mem2).

Definition bpf_flag_memval_list := [ Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero ].

(**r mem2 includes the input_data + bpf_flag *)
Definition mem2 :=
  match Mem.storebytes (fst init_mem2) bpf_flag_blk 0%Z bpf_flag_memval_list with
  | Some m => m
  | None => Mem.empty
  end.


Definition init_mem3 := Mem.alloc mem2 0 88.

Definition bpf_regs_blk: block := (snd init_mem3).

Definition bpf_regs_memval_list : list memval :=
[ Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R0 := 0 *)

  Byte Byte.zero;  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R1 := 0 *)

  Byte (Byte.repr 180); Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R2 := 180 *)

  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R3 := 0 *)

  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R4 := 0 *)

  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R5 := 0 *)

  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R6 := 0 *)

  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R7 := 0 *)

  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R8 := 0 *)

  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r R9 := 0 *)

  Byte (Byte.repr 0xe8); Byte (Byte.repr 0x05); Byte Byte.zero; Byte Byte.zero;
  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero  (**r R10 := 1512, i.e. 0x05e8 *)
].

(**r mem3 includes the input_data + bpf_flag + bpf_regs *)
Definition mem3 :=
  match Mem.storebytes (fst init_mem3) bpf_regs_blk 0%Z bpf_regs_memval_list with
  | Some m => m
  | None => Mem.empty
  end.


Definition init_mem4 := Mem.alloc mem3 0 512.

Definition stk_blk: block := snd init_mem4.

Definition mem4 := fst init_mem4.

Definition fletcher32_init_memory_regions := [{|
  start_addr := Vint (Int.repr 0); (**r assume a memory region *)
  block_size := Vint (Int.repr 361);
  block_perm := Readable;
  block_ptr  := Vptr input_blk Ptrofs.zero;
|}; {|
  start_addr := Vint (Int.repr 1000); (**r assume a memory region *)
  block_size := Vint (Int.repr 512);
  block_perm := Writable;
  block_ptr  := Vptr stk_blk Ptrofs.zero;
|}].


Definition jit_state_memval_list : list memval :=
[ Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r pc := 0 *)

  Fragment (Vptr bpf_flag_blk Ptrofs.zero) Q32 3; (**r bpf_flag *)
  Fragment (Vptr bpf_flag_blk Ptrofs.zero) Q32 2;
  Fragment (Vptr bpf_flag_blk Ptrofs.zero) Q32 1;
  Fragment (Vptr bpf_flag_blk Ptrofs.zero) Q32 0;

  Fragment (Vptr bpf_regs_blk Ptrofs.zero) Q32 3; (**r bpf_regs *)
  Fragment (Vptr bpf_regs_blk Ptrofs.zero) Q32 2;
  Fragment (Vptr bpf_regs_blk Ptrofs.zero) Q32 1;
  Fragment (Vptr bpf_regs_blk Ptrofs.zero) Q32 0
].


Definition init_mem5 := Mem.alloc mem4 0 12.

Definition jit_state_blk: block := (snd init_mem5).

(**r mem1 includes the jit_state *)
Definition mem5 :=
  match Mem.storebytes (fst init_mem5) jit_state_blk 0%Z jit_state_memval_list with
  | Some m => m
  | None => Mem.empty
  end.


Definition init_mem6 := Mem.alloc mem5 0 (Z.of_nat (2 * JITTED_LIST_MAX_LENGTH)).

Definition jitted_arm_blk := snd init_mem6.


Definition fletcher32_init_jit_state: jit_state := {|
  jit_pc          := Int.zero;
  jit_flag        := Vptr bpf_flag_blk Ptrofs.zero;
  jit_regs        := Vptr bpf_regs_blk Ptrofs.zero;
  jit_mrs_num     := 2;
  jit_mrs         := fletcher32_init_memory_regions;
  jit_ins_len     := List.length test_fletcher32_int64;
  jit_ins         := test_fletcher32_int64;

  entry_len       := 0;
  ep_list         := ListNat.create_int_list 20;

  use_IR11        := false;
  load_store_regs := init_LoadStoreRegs;
  offset          := 0;
  thumb_len       := 0;
  thumb           := List16.create_int_list 500;

  jitted_len      := 0;
  jitted_list     := Vptr jitted_arm_blk Ptrofs.zero;
  jit_mem         := fst init_mem6;
|}.
Definition final_jitted_state :=
  let final_state := jit_alu32 fletcher32_init_jit_state in
  match final_state with
  | Some st => st
  | None => empty_jit_state
  end.

Open Scope string_scope.

(*
     = ["0x200009c0: mov r12, r1";
       "0x200009c2: str.w r10, [sp, #0x4]";
       "0x200009c6: ldr.w r0, [r12, #0x8]";
       "0x200009ca: ldr.w r10, [r12, #0x58]";
       "0x200009ce: mov r0, r10";
       "0x200009d0: sub.w r10, r10, #0x10";
       "0x200009d4: str.w r0, [r12, #0x8]";
       "0x200009d8: str.w r10, [r12, #0x58]";
       "0x200009dc: ldr.w r10, [sp, #0x4]";
       "0x200009e0: ldr.w sp, [sp, #0x0]";
       "0x200009e4: bx lr";
       "0x200009e6: mov r12, r1";
       "0x200009e8: str.w r3, [sp, #0x4]";
       "0x200009ec: str.w r5, [sp, #0x8]";
       "0x200009f0: str.w r11, [sp, #0xc]";
       "0x200009f4: ldr.w r3, [r12, #0x20]";
       "0x200009f8: ldr.w r5, [r12, #0x30]";
       "0x200009fc: movw r11, #0xffff";
       "0x20000a00: movt r11, #0x0";
       "0x20000a04: mov r5, r11";
       "0x20000a06: mov r3, r5";
       "0x20000a08: str.w r3, [r12, #0x20]";
       "0x20000a0c: str.w r5, [r12, #0x30]";
       "0x20000a10: ldr.w r11, [sp, #0xc]";
       "0x20000a14: ldr.w r5, [sp, #0x8]";
       "0x20000a18: ldr.w r3, [sp, #0x4]";
       "0x20000a1c: ldr.w sp, [sp, #0x0]";
       "0x20000a20: bx lr";
       "0x20000a22: mov r12, r1";
       "0x20000a24: str.w r1, [sp, #0x4]";
       "0x20000a28: str.w r2, [sp, #0x8]";
       "0x20000a2c: str.w r3, [sp, #0xc]";
       "0x20000a30: str.w r4, [sp, #0x10]";
       "0x20000a34: str.w r5, [sp, #0x14]";
       "0x20000a38: str.w r11, [sp, #0x18]";
       "0x20000a3c: ldr.w r0, [r12, #0x8]";
       "0x20000a40: ldr.w r1, [r12, #0x10]";
       "0x20000a44: ldr.w r2, [r12, #0x18]";
       "0x20000a48: ldr.w r3, [r12, #0x20]";
       "0x20000a4c: ldr.w r4, [r12, #0x28]";
       "0x20000a50: ldr.w r5, [r12, #0x30]";
       "0x20000a54: mov r1, r5";
       "0x20000a56: movw r11, #0xffff";
       "0x20000a5a: movt r11, #0x0";
       "0x20000a5e: and.w r1, r1, r11";
       "0x20000a62: movw r4, #0x10";
       "0x20000a66: cmp.w r4, #0x20";
       "0x20000a6a: b.n 0x20000a76";
       "0x20000a6c: movw r11, #0xb";
       "0x20000a70: str.w r11, [r12, #0x4]";
       "0x20000a74: b.n 0x20000ad8";
       "0x20000a76: lsr.w r5, r5, r4";
       "0x20000a7a: add r1, r1, r5";
       "0x20000a7c: mov r0, r3";
       "0x20000a7e: movw r11, #0xffff";
       "0x20000a82: movt r11, #0x0";
       "0x20000a86: and.w r0, r0, r11";
       "0x20000a8a: movw r2, #0x10";
       "0x20000a8e: cmp.w r2, #0x20";
       "0x20000a92: b.n 0x20000a9e";
       "0x20000a94: movw r11, #0xb";
       "0x20000a98: str.w r11, [r12, #0x4]";
       "0x20000a9c: b.n 0x20000ad8";
       "0x20000a9e: lsr.w r3, r3, r2";
       "0x20000aa2: add r0, r0, r3";
       "0x20000aa4: movw r4, #0x10";
       "0x20000aa8: cmp.w r4, #0x20";
       "0x20000aac: b.n 0x20000ab8";
       "0x20000aae: movw r11, #0xb";
       "0x20000ab2: str.w r11, [r12, #0x4]";
       "0x20000ab6: b.n 0x20000ad8";
       "0x20000ab8: lsl.w r0, r0, r4";
       "0x20000abc: orr.w r0, r0, r1";
       "0x20000ac0: str.w r0, [r12, #0x8]";
       "0x20000ac4: str.w r1, [r12, #0x10]";
       "0x20000ac8: str.w r2, [r12, #0x18]";
       "0x20000acc: str.w r3, [r12, #0x20]";
       "0x20000ad0: str.w r4, [r12, #0x28]";
       "0x20000ad4: str.w r5, [r12, #0x30]";
       "0x20000ad8: ldr.w r11, [sp, #0x18]";
       "0x20000adc: ldr.w r5, [sp, #0x14]";
       "0x20000ae0: ldr.w r4, [sp, #0x10]";
       "0x20000ae4: ldr.w r3, [sp, #0xc]";
       "0x20000ae8: ldr.w r2, [sp, #0x8]";
       "0x20000aec: ldr.w r1, [sp, #0x4]";
       "0x20000af0: ldr.w sp, [sp, #0x0]";
       "0x20000af4: bx lr";
       "0x20000af6: mov r12, r1";
       "0x20000af8: str.w r1, [sp, #0x4]";
       "0x20000afc: str.w r2, [sp, #0x8]";
       "0x20000b00: str.w r4, [sp, #0xc]";
       "0x20000b04: ldr.w r0, [r12, #0x8]";
       "0x20000b08: ldr.w r1, [r12, #0x10]";
       "0x20000b0c: ldr.w r2, [r12, #0x18]";
       "0x20000b10: ldr.w r4, [r12, #0x28]";
       "0x20000b14: mov r0, r2";
       "0x20000b16: sub.w r2, r2, r0";
       "0x20000b1a: mov r4, r1";
       "0x20000b1c: mov r1, r4";
       "0x20000b1e: add r1, r1, #0x2";
       "0x20000b22: str.w r0, [r12, #0x8]";
       "0x20000b26: str.w r1, [r12, #0x10]";
       "0x20000b2a: str.w r2, [r12, #0x18]";
       "0x20000b2e: str.w r4, [r12, #0x28]";
       "0x20000b32: ldr.w r4, [sp, #0xc]";
       "0x20000b36: ldr.w r2, [sp, #0x8]";
       "0x20000b3a: ldr.w r1, [sp, #0x4]";
       "0x20000b3e: ldr.w sp, [sp, #0x0]";
       "0x20000b42: bx lr";
       "0x20000b44: mov r12, r1";
       "0x20000b46: str.w r11, [sp, #0x4]";
       "0x20000b4a: ldr.w r0, [r12, #0x8]";
       "0x20000b4e: movw r11, #0x167";
       "0x20000b52: movt r11, #0x0";
       "0x20000b56: mov r0, r11";
       "0x20000b58: str.w r0, [r12, #0x8]";
       "0x20000b5c: ldr.w r11, [sp, #0x4]";
       "0x20000b60: ldr.w sp, [sp, #0x0]";
       "0x20000b64: bx lr";
       "0x20000b66: mov r12, r1";
       "0x20000b68: str.w r1, [sp, #0x4]";
       "0x20000b6c: str.w r2, [sp, #0x8]";
       "0x20000b70: str.w r4, [sp, #0xc]";
       "0x20000b74: ldr.w r0, [r12, #0x8]";
       "0x20000b78: ldr.w r1, [r12, #0x10]";
       "0x20000b7c: ldr.w r2, [r12, #0x18]";
       "0x20000b80: ldr.w r4, [r12, #0x28]";
       "0x20000b84: sub.w r2, r2, r0";
       "0x20000b88: mov r4, r1";
       "0x20000b8a: mov r1, r4";
       "0x20000b8c: add r1, r1, #0x2";
       "0x20000b90: str.w r1, [r12, #0x10]";
       "0x20000b94: str.w r2, [r12, #0x18]";
       "0x20000b98: str.w r4, [r12, #0x28]";
       "0x20000b9c: ldr.w r4, [sp, #0xc]";
       "0x20000ba0: ldr.w r2, [sp, #0x8]";
       "0x20000ba4: ldr.w r1, [sp, #0x4]";
       "0x20000ba8: ldr.w sp, [sp, #0x0]";
       "0x20000bac: bx lr";
       "0x20000bae: mov r12, r1";
       "0x20000bb0: str.w r3, [sp, #0x4]";
       "0x20000bb4: str.w r4, [sp, #0x8]";
       "0x20000bb8: str.w r5, [sp, #0xc]";
       "0x20000bbc: str.w r11, [sp, #0x10]";
       "0x20000bc0: ldr.w r0, [r12, #0x8]";
       "0x20000bc4: ldr.w r3, [r12, #0x20]";
       "0x20000bc8: ldr.w r4, [r12, #0x28]";
       "0x20000bcc: ldr.w r5, [r12, #0x30]";
       "0x20000bd0: add r5, r5, r4";
       "0x20000bd2: add r3, r3, r5";
       "0x20000bd4: movw r11, #0xffff";
       "0x20000bd8: movt r11, #0xffff";
       "0x20000bdc: add r0, r0, r11";
       "0x20000bde: str.w r0, [r12, #0x8]";
       "0x20000be2: str.w r3, [r12, #0x20]";
       "0x20000be6: str.w r5, [r12, #0x30]";
       "0x20000bea: ldr.w r11, [sp, #0x10]";
       "0x20000bee: ldr.w r5, [sp, #0xc]";
       "0x20000bf2: ldr.w r4, [sp, #0x8]";
       "0x20000bf6: ldr.w r3, [sp, #0x4]";
       "0x20000bfa: ldr.w sp, [sp, #0x0]";
       "0x20000bfe: bx lr";
       "0x20000c00: mov r12, r1";
       "0x20000c02: str.w r1, [sp, #0x4]";
       "0x20000c06: str.w r4, [sp, #0x8]";
       "0x20000c0a: ldr.w r1, [r12, #0x10]";
       "0x20000c0e: ldr.w r4, [r12, #0x28]";
       "0x20000c12: mov r4, r1";
       "0x20000c14: mov r1, r4";
       "0x20000c16: add r1, r1, #0x2";
       "0x20000c1a: str.w r1, [r12, #0x10]";
       "0x20000c1e: str.w r4, [r12, #0x28]";
       "0x20000c22: ldr.w r4, [sp, #0x8]";
       "0x20000c26: ldr.w r1, [sp, #0x4]";
       "0x20000c2a: ldr.w sp, [sp, #0x0]";
       "0x20000c2e: bx lr";
       "0x20000c30: mov r12, r1";
       "0x20000c32: str.w r3, [sp, #0x4]";
       "0x20000c36: str.w r4, [sp, #0x8]";
       "0x20000c3a: str.w r5, [sp, #0xc]";
       "0x20000c3e: str.w r6, [sp, #0x10]";
       "0x20000c42: str.w r11, [sp, #0x14]";
       "0x20000c46: ldr.w r0, [r12, #0x8]";
       "0x20000c4a: ldr.w r3, [r12, #0x20]";
       "0x20000c4e: ldr.w r4, [r12, #0x28]";
       "0x20000c52: ldr.w r5, [r12, #0x30]";
       "0x20000c56: ldr.w r6, [r12, #0x38]";
       "0x20000c5a: mov r4, r5";
       "0x20000c5c: movw r11, #0xffff";
       "0x20000c60: movt r11, #0x0";
       "0x20000c64: and.w r4, r4, r11";
       "0x20000c68: movw r6, #0x10";
       "0x20000c6c: mov r0, r5";
       "0x20000c6e: cmp.w r6, #0x20";
       "0x20000c72: b.n 0x20000c7e";
       "0x20000c74: movw r11, #0xb";
       "0x20000c78: str.w r11, [r12, #0x4]";
       "0x20000c7c: b.n 0x20000cc6";
       "0x20000c7e: lsr.w r0, r0, r6";
       "0x20000c82: mov r5, r4";
       "0x20000c84: add r5, r5, r0";
       "0x20000c86: mov r4, r3";
       "0x20000c88: movw r11, #0xffff";
       "0x20000c8c: movt r11, #0x0";
       "0x20000c90: and.w r4, r4, r11";
       "0x20000c94: movw r6, #0x10";
       "0x20000c98: mov r0, r3";
       "0x20000c9a: cmp.w r6, #0x20";
       "0x20000c9e: b.n 0x20000caa";
       "0x20000ca0: movw r11, #0xb";
       "0x20000ca4: str.w r11, [r12, #0x4]";
       "0x20000ca8: b.n 0x20000cc6";
       "0x20000caa: lsr.w r0, r0, r6";
       "0x20000cae: mov r3, r4";
       "0x20000cb0: add r3, r3, r0";
       "0x20000cb2: str.w r0, [r12, #0x8]";
       "0x20000cb6: str.w r3, [r12, #0x20]";
       "0x20000cba: str.w r4, [r12, #0x28]";
       "0x20000cbe: str.w r5, [r12, #0x30]";
       "0x20000cc2: str.w r6, [r12, #0x38]";
       "0x20000cc6: ldr.w r11, [sp, #0x14]";
       "0x20000cca: ldr.w r6, [sp, #0x10]";
       "0x20000cce: ldr.w r5, [sp, #0xc]";
       "0x20000cd2: ldr.w r4, [sp, #0x8]";
       "0x20000cd6: ldr.w r3, [sp, #0x4]";
       "0x20000cda: ldr.w sp, [sp, #0x0]";
       "0x20000cde: bx lr";
       "0x20000ce0: mov r12, r1";
       "0x20000ce2: str.w r10, [sp, #0x4]";
       "0x20000ce6: ldr.w r10, [r12, #0x58]";
       "0x20000cea: add r10, r10, #0x10";
       "0x20000cee: str.w r10, [r12, #0x58]";
       "0x20000cf2: ldr.w r10, [sp, #0x4]";
       "0x20000cf6: ldr.w sp, [sp, #0x0]";
       "0x20000cfa: bx lr"]
     : list string
*)

Definition fletcher32_ibpf_main :=
  let final_state := ibpf_interpreter 5000%nat final_jitted_state in
  match final_state with
  | Some (v, st) => print_iBPF_prog (ibpf_decode_prog (jit_ins st) (jit_ins_len st))
  | None => []
  end.

Extract Constant IMonadOp._jit_bpf_get_call => "function v -> returnM v".
Extract Constant jit_state_start_address => "Vptr (5, 0)".
Extract Constant IMonadOp.jit_exec_function => "function v -> returnM v".

(*
Extraction "/home/shyuan/GitLab/rbpf-dx/example/fletcher32_ibpf_main6.ml" fletcher32_ibpf_main. *)
