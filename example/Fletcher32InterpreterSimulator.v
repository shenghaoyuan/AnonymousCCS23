From compcert Require Import Integers Memdata Memory AST Values.
From compcert.arm Require Import AsmSyntax BinSyntax ABinSem.

From Coq Require Import Ascii String List ZArith.
Import ListNotations.

From bpf.comm Require Import ListAsArray Flag Regs MemRegion State Monad.
From bpf.model Require Import Decode Encode PrintrBPF Syntax Semantics.

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
(Int64.repr 0xffff0000000000b4);
(Int64.repr 0x0000ffff000003b4);
(Int64.repr 0x0000000000220215);
(Int64.repr 0x0000ffff000004b4);
(Int64.repr 0x0000ffff000003b4);
(Int64.repr 0x00000000000026bf);
(Int64.repr 0x00000167000102a5);
(Int64.repr 0x00000167000006b7);
(Int64.repr 0x000000000000621f);
(Int64.repr 0x00000000000065bf);
(Int64.repr 0xffffffff00000507);
(Int64.repr 0x0000002000000567);
(Int64.repr 0x0000002000000577);
(Int64.repr 0x00000000000017bf);
(Int64.repr 0x0000000000007069);
(Int64.repr 0x000000000000030c);
(Int64.repr 0x0000000200000707);
(Int64.repr 0x00000000000030bc);
(Int64.repr 0x000000000000400c);
(Int64.repr 0xffffffff00000604);
(Int64.repr 0x00000000000004bc);
(Int64.repr 0x00000000fff80655);
(Int64.repr 0x00000000000004bc);
(Int64.repr 0x0000001000000474);
(Int64.repr 0x0000ffff00000054);
(Int64.repr 0x000000000000400c);
(Int64.repr 0x00000000000034bc);
(Int64.repr 0x0000001000000474);
(Int64.repr 0x0000ffff00000354);
(Int64.repr 0x000000000000430c);
(Int64.repr 0x0000000100000567);
(Int64.repr 0x000000000000510f);
(Int64.repr 0x0000000200000107);
(Int64.repr 0x00000000000004bc);
(Int64.repr 0x00000000ffe20255);
(Int64.repr 0x0001000100000024);
(Int64.repr 0xffff000000000054);
(Int64.repr 0x00000000000031bc);
(Int64.repr 0x0000001000000174);
(Int64.repr 0x0000ffff00000354);
(Int64.repr 0x000000000000130c);
(Int64.repr 0x000000000000304c);
(Int64.repr 0x0000000000000095)
].

(*
Open Scope string_scope.
Compute print_rBPF_prog (decode_prog test_fletcher32_int64).
"0x0: r0 =.32 -0x10000";
"0x1: r3 =.32 0xffff";
"0x2: if r2 == 0x0 goto 0x22";
"0x3: r4 =.32 0xffff";
"0x4: r3 =.32 0xffff";
"0x5: r6 = r2";
"0x6: if r2 < 0x167 goto 0x1";
"0x7: r6 = 0x167";
"0x8: r2 -= r6";
"0x9: r5 = r6";
"0xa: r5 += -0x1";
"0xb: r5 <<= 0x20";
"0xc: r5 >>= 0x20";
"0xd: r7 = r1";
"0xe: r0 = *(u16 *)(r7 + 0x0)";
"0xf: r3 +=.32 r0";
"0x10: r7 += 0x2";
"0x11: r0 =.32 r3";
"0x12: r0 +=.32 r4";
"0x13: r6 +=.32 -0x1";
"0x14: r4 =.32 r0";
"0x15: if r6 != 0x0 goto -0x8";
"0x16: r4 =.32 r0";
"0x17: r4 >>=.32 0x10";
"0x18: r0 &=.32 0xffff";
"0x19: r0 +=.32 r4";
"0x1a: r4 =.32 r3";
"0x1b: r4 >>=.32 0x10";
"0x1c: r3 &=.32 0xffff";
"0x1d: r3 +=.32 r4";
"0x1e: r5 <<= 0x1";
"0x1f: r1 += r5";
"0x20: r1 += 0x2";
"0x21: r4 =.32 r0";
"0x22: if r2 != 0x0 goto -0x1e";
"0x23: r0 *=.32 0x10001";
"0x24: r0 &=.32 -0x10000";
"0x25: r1 =.32 r3";
"0x26: r1 >>=.32 0x10";
"0x27: r3 &=.32 0xffff";
"0x28: r3 +=.32 r1";
"0x29: r0 |=.32 r3";
"0x2a: exit"

where,
- epl = [0; 37; 3; 15; 17; 22; 33; 35]
*)

Definition init_mem := Mem.alloc Mem.empty 0 361.

Definition input_blk: block := (snd init_mem).

(**r mem1 includes the jit_state *)
Definition mem1 :=
  match Mem.storebytes (fst init_mem) input_blk 0%Z wrap_around_data_byte with
  | Some m => m
  | None => Mem.empty
  end.

Definition fletcher32_init_regs := {|
  r0_val  := val64_zero;
  r1_val  := Vlong (Int64.repr 0);
  r2_val  := Vlong (Int64.repr 180);
  r3_val  := val64_zero;
  r4_val  := val64_zero;
  r5_val  := val64_zero;
  r6_val  := val64_zero;
  r7_val  := val64_zero;
  r8_val  := val64_zero;
  r9_val  := val64_zero;
  r10_val := val64_zero;
|}.

Definition fletcher32_init_memory_regions := [{|
  start_addr := Vint (Int.repr 0); (**r assume a memory region *)
  block_size := Vint (Int.repr 361);
  block_perm := Readable;
  block_ptr  := Vptr input_blk Ptrofs.zero;
|}].

Definition fletcher32_init_bpf_state: state := {|
  pc_loc    := Int.zero;
  flag      := BPF_OK;
  regs_st   := fletcher32_init_regs;
  mrs_num   := 1;
  bpf_mrs   := fletcher32_init_memory_regions;
  ins_len   := List.length test_fletcher32_int64;
  ins       := test_fletcher32_int64;
  bpf_m     := mem1;
|}.

Definition fletcher32_test_main := bpf_interpreter 1000%nat fletcher32_init_bpf_state.

Extract Constant rBPFMonadOp.exec_function => "function v -> returnM v".
Extract Constant rBPFMonadOp._bpf_get_call => "function v -> returnM v".
(*
Extraction "/home/shyuan/GitLab/workspace/jit/example/fletcher32InterpreterSimulator1.ml" fletcher32_test_main. *)
