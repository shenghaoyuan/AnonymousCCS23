From compcert Require Import Integers Memdata Memory AST Values.
From compcert.arm Require Import AsmSyntax BinSyntax ABinSem BinDecode.

From Coq Require Import Ascii String List ZArith.
Import ListNotations.

From bpf.comm Require Import ListAsArray Regs MemRegion rBPFMonadOp.
From bpf.model Require Import Decode Encode Syntax PrintrBPF.
From bpf.jit.thumb Require Import LoadStoreRegs  KeyValue2 JITState ThumbDecode ThumbJITOpcode ThumbJIT ThumbDecode PrintThumb PrintJIT.
From bpf.jit.iBPF Require Import ISemantics.

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
Compute (decode_prog test_fletcher32_int64 (List.length test_fletcher32_int64)). *)
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

   - block 2: [0, 512) rBPF stack
   - block 3: [0, 12) jit_state
   - block 4: [0, ?) jitted_arm_list
*)

Definition init_mem := Mem.alloc Mem.empty 0 361.

Definition input_blk: block := (snd init_mem).

(**r mem1 includes the input_data *)
Definition mem1 :=
  match Mem.storebytes (fst init_mem) input_blk 0%Z wrap_around_data_byte with
  | Some m => m
  | None => Mem.empty
  end.


(**r mem2 includes the input_data + bpf_stack *)
Definition init_mem2 := Mem.alloc mem1 0 512.

Definition stk_blk: block := snd init_mem2.

Definition mem2 := fst init_mem2.

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

Definition jit_state_memval_list : list memval :=
[ Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; (**r pc := 0 *)

  Byte Byte.zero; Byte Byte.zero; Byte Byte.zero; Byte Byte.zero (**r bpf_flag *)
] ++ bpf_regs_memval_list.


Definition init_mem3 := Mem.alloc mem2 0 96.

Definition jit_state_blk: block := (snd init_mem3).

(**r mem3 includes the input_data + bpf_stack + bpf_state *)
Definition mem3 :=
  match Mem.storebytes (fst init_mem3) jit_state_blk 0%Z jit_state_memval_list with
  | Some m => m
  | None => Mem.empty
  end.


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


Definition init_mem4 := Mem.alloc mem3 0 (Z.of_nat (2 * JITTED_LIST_MAX_LENGTH)).

Definition jitted_arm_blk := snd init_mem4.


Definition fletcher32_init_jit_state: jit_state := {|
  jit_pc          := Int.zero;
  jit_flag        := Vptr jit_state_blk (Ptrofs.repr 4);
  jit_regs        := Vptr jit_state_blk (Ptrofs.repr 8);
  jit_mrs_num     := 2;
  jit_mrs         := fletcher32_init_memory_regions;
  jit_ins_len     := List.length test_fletcher32_int64;
  jit_ins         := test_fletcher32_int64;

  kv2             := ListKeyV.create_int_list (List.length test_fletcher32_int64);

  use_IR11        := false;
  load_store_regs := init_LoadStoreRegs;
  offset          := 0;
  thumb_len       := 0;
  thumb           := List16.create_int_list 500;

  jitted_len      := 0;
  jitted_list     := Vptr jitted_arm_blk Ptrofs.zero;
  jit_mem         := fst init_mem4;
|}.

Definition final_jitted_state :=
  let final_state := jit_alu32 fletcher32_init_jit_state in
  match final_state with
  | Some st => st
  | None => empty_jit_state
  end.

Open Scope string_scope.

Definition extract_final_jitted_state: list string :=
  let st := final_jitted_state in
    print_arm32_prog (arm32_decode_prog (jitted_list st) (jitted_len st) (jit_mem st)) (Int.repr 0x200009c0).


Definition fletcher32_ibpf_main :=
  let final_state := ibpf_interpreter 5000%nat final_jitted_state in
  match final_state with
  | Some (v, st) => print_rBPF_prog (decode_prog (jit_ins st) (jit_ins_len st))
  | None => []
  end.

Extract Constant IMonadOp._jit_bpf_get_call => "function v -> returnM v".
Extract Constant jit_state_start_address => "Vptr (3, 0)".
Extract Constant IMonadOp.jit_exec_function => "function v -> returnM v".

(*
Extraction "/home/shyuan/GitLab/rbpf-dx/example/fletcher32_ibpf1.ml" extract_final_jitted_state fletcher32_ibpf_main. *)
