From compcert Require Import Integers Memdata Memory AST Values.
From compcert.arm Require Import AsmSyntax BinSyntax ABinSem.

From Coq Require Import Ascii String List ZArith.
Import ListNotations.

From bpf.comm Require Import ListAsArray.
From bpf.model Require Import Decode Encode PrintrBPF.
From bpf.jit.arm Require Import List32 JitState Arm32Decode ArmJITOpcode ArmJIT Arm32Decode PrintJIT.

From bpf.example Require Import DebugExtraction.

Open Scope string_scope.

Definition test_add_reg_int: list int :=
[
(Int.repr 0x0000f240);
(Int.repr 0x0000002a);
(Int.repr 0x00004740)
].

(**r arm32 thumb instruction 
       0:	f2 40 00 2a mov.w r0, #42; 0x2a
       2:	47 70       bx lr

*)

Definition init_mem := Mem.alloc Mem.empty 0 20.

Definition test_store :=
  match Mem.storev Mint16unsigned (fst init_mem) (Vptr (snd init_mem) Ptrofs.zero) (Vint (Int.repr 0x0000f240)) with
  | None => Mem.empty
  | Some m =>
    match Mem.storev Mint16unsigned m (Vptr (snd init_mem) (Ptrofs.repr 2)) (Vint (Int.repr 0x0000002a)) with
    | None => Mem.empty
    | Some m =>
      match Mem.storev Mint16unsigned m (Vptr (snd init_mem) (Ptrofs.repr 4)) (Vint (Int.repr 0x00004740)) with
      | None => Mem.empty
      | Some m => m
      end
    end
  end.

Definition test_compcert_mem :=
  let t0 :=
    match Mem.loadv Mint8unsigned test_store (Vptr (snd init_mem) Ptrofs.zero) with
    | None => Vundef
    | Some r => r
    end in
  let t1 :=
    match Mem.loadv Mint8unsigned test_store (Vptr (snd init_mem) (Ptrofs.repr 1)) with
    | None => Vundef
    | Some r => r
    end in
  let t01 :=
    match Mem.loadv Mint16unsigned test_store (Vptr (snd init_mem) Ptrofs.zero) with
    | None => Vundef
    | Some r => r
    end in
  let t2 :=
    match Mem.loadv Mint8unsigned test_store (Vptr (snd init_mem) (Ptrofs.repr 2)) with
    | None => Vundef
    | Some r => r
    end in
  let t3 :=
    match Mem.loadv Mint8unsigned test_store (Vptr (snd init_mem) (Ptrofs.repr 3)) with
    | None => Vundef
    | Some r => r
    end in
  let t23 :=
    match Mem.loadv Mint16unsigned test_store (Vptr (snd init_mem) (Ptrofs.repr 2)) with
    | None => Vundef
    | Some r => r
    end in
  let t03 := match Mem.loadv Mint32 test_store (Vptr (snd init_mem) Ptrofs.zero) with
    | None => Vundef
    | Some r => r
    end in
    [t0; t1; t01; t2 ;t3; t23; t03].

Extraction "/home/shyuan/GitLab/workspace/jit/example/test_compcert_mem.ml" test_compcert_mem.