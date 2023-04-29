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
*)

Definition init_mem := Mem.alloc Mem.empty 0 361.

Definition input_blk: block := (snd init_mem).

(**r mem1 includes the jit_state *)
Definition mem1 :=
  match Mem.storebytes (fst init_mem) input_blk 0%Z wrap_around_data_byte with
  | Some m => m
  | None => Mem.empty
  end.

Definition init_mem2 := Mem.alloc mem1 0 512.

Definition stk_blk: block := snd init_mem2.

Definition mem2 := fst init_mem2.


(**r CompCertBPF will use R10, so we need to declare one stack in the memory *)

Definition fletcher32_init_regs := {|
  r0_val  := val64_zero;
  r1_val  := Vlong (Int64.repr 0);  (**r R1 points to the start address of the input block *)
  r2_val  := Vlong (Int64.repr 180);
  r3_val  := val64_zero;
  r4_val  := val64_zero;
  r5_val  := val64_zero;
  r6_val  := val64_zero;
  r7_val  := val64_zero;
  r8_val  := val64_zero;
  r9_val  := val64_zero;
  r10_val := Vlong (Int64.repr 1512); (**r R10 points to the end of the bpf stack *)
|}.

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

Definition fletcher32_init_bpf_state: state := {|
  pc_loc    := Int.zero;
  flag      := BPF_OK;
  regs_st   := fletcher32_init_regs;
  mrs_num   := 2;
  bpf_mrs   := fletcher32_init_memory_regions;
  ins_len   := List.length test_fletcher32_int64;
  ins       := test_fletcher32_int64;
  bpf_m     := mem2;
|}.

Definition fletcher32_test_main := bpf_interpreter 5000%nat fletcher32_init_bpf_state.


Extract Constant rBPFMonadOp.exec_function => "function v -> returnM v".
Extract Constant rBPFMonadOp._bpf_get_call => "function v -> returnM v".
(*
Extraction "/home/shyuan/GitLab/workspace/jit/example/fletcher32InterpreterSimulatorCompCert1.ml" fletcher32_test_main. *)
