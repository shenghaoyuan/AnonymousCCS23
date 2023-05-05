From compcert.lib Require Import Integers.
From compcert.arm Require Import Asm AsmSyntax BinSyntax BinSem BinDecode.
From compcert.common Require Import Values Globalenvs Smallstep Memory Memdata Events AST.

From bpf.comm Require Import Flag BinrBPF ListAsArray Regs.
From bpf.model Require Import Encode Syntax.
From bpf.monadicmodel2 Require Import ConcreteState.
From bpf.jit.thumb Require Import LoadStoreRegs JITState.
From bpf.jit.thumb Require Import Arm32Reg ThumbJITOpcode ThumbInsOp.

From bpf.jit.verification Require Import rBPFSemInd JITSimple.
From bpf.jit.simulation Require Import BitfieldLemma.

From Coq Require Import List ZArith Arith String Lia.
Import ListNotations.
Open Scope Z_scope.
Open Scope bool_scope.
Open Scope asm.


Definition sync_regs := list reg.
Definition sync_iregs := list ireg.
Definition regs_agree (l: sync_regs) (st: state) (rs: Asm.regset): Prop :=
  forall r, List.In r l ->
    exists vi, rs (ireg_of_reg r) = Vint vi /\
      (eval_reg r st) = Some (Val.longofintu (Vint vi)).

Definition sub_mem_blk (m0 m1: mem) (b: block) (low high: Z): Prop :=
  forall chunk ofs, low <= ofs /\ ofs + (size_chunk chunk) <= high -> Mem.load chunk m0 b ofs = Mem.load chunk m1 b ofs.


Definition sub_stack (m0 m1: mem) (ptr: val) (l: list ireg): Prop :=
  forall r, List.In r l ->
    exists vi,
    Mem.loadv Mint32 m0 (Val.offset_ptr ptr (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4)))) = Some (Vint vi) /\
    Mem.loadv Mint32 m1 (Val.offset_ptr ptr (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4)))) = Some (Vint vi).

Definition reg_to_state_addr (r: reg): Z := (id_of_reg r) * 8 + 8.

Definition jit_state_memory_layout (m: mem) (st_blk: block): Prop :=
  forall r,
  (**r st_blk: 4bytes (pc) + 4 bytes (flag) + 8 bytes * 11 ... *)
    exists vi,
      Mem.load Mint32 m st_blk (reg_to_state_addr r) = Some (Vint vi).

Definition arm_initial_stack (m: mem) (rs: Asm.regset) (old_sp: val): Prop :=
  Mem.loadv Mint32 m (rs IR13) = Some old_sp /\
  forall r, List.In r arm_callee_save_regs ->
    Mem.loadv Mint32 m (Val.offset_ptr (rs IR13) (Ptrofs.of_intu (int_of_ireg r))) = Some Vundef.

Definition arm_initial_state (m: mem) (rs: Asm.regset) (st_blk regs_blk: block) (old_sp cur_pc: val): Prop :=
  (**r IR1 = jit_state block *)
  rs IR1 = Vptr st_blk Ptrofs.zero /\
  (**r Stack[0,3] = old_sp *)
  arm_initial_stack m rs old_sp /\
  (**r PC = cur_pc *)
  rs PC = cur_pc.

Definition match_registers_jit (rbpf_st: state) (m: mem) (l: sync_regs) (st_blk: block): Prop :=
  forall r,
    exists vi, eval_reg r rbpf_st = Some (Val.longofintu (Vint vi)) /\ (*
    (forall vj, (eval_reg r rbpf_st) = Some (Val.longofintu (Vint vj)) -> vi = vj) /\ *)
    (List.In r l ->
      Mem.load Mint32 m st_blk (reg_to_state_addr r) = Some (Vint vi)).

Definition ptr_range_perm (m: mem) (chunk: memory_chunk) (ptr: val) (p: permission): Prop :=
  match ptr with
  | Vptr b ofs => Mem.valid_access m chunk b (Ptrofs.unsigned ofs) p
  | _ => False
  end.

Definition arm_synch_stack (lsr_stack: sync_iregs) (rs0 rs1: Asm.regset) (m: mem) (old_sp: val): Prop := (*
  Mem.loadv Mint32 m (rs IR13) = Some old_sp /\ *)
  forall r,
    (List.In r lsr_stack ->
      Mem.loadv Mint32 m (Val.offset_ptr (rs1 IR13) (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4)))) = Some (rs0 r)).

Definition not_stack_blk (old_sp: val) (b: block): Prop :=
  match old_sp with
  | Vptr b1 _ => b <> b1
  | _ => False
  end.

Definition block_neq (flag_blk regs_blk jit_blk jit_state_blk: block): Prop :=
  (flag_blk <> regs_blk /\ jit_blk <> flag_blk /\ jit_blk <> regs_blk) /\
  (flag_blk <> jit_state_blk /\ jit_blk <> jit_state_blk /\ jit_state_blk <> regs_blk).

Definition arm_assume_register_map (rs: Asm.regset) : Prop :=
  forall r,
    match rs r with
    | Vundef | Vint _ | Vptr _ _ => True
    | _ => False
    end.


(*
(**r define some invariants about the most stages of our jit process *)
Record arm_memory_inv0 (st: jit_state) (rs0 rs1: Asm.regset) (m0 m1: mem)
  (flag_blk regs_blk jit_blk jit_state_blk: block) (P: block -> Z -> Prop): Prop := {
  arm_inv_stk:  rs0 IR13 = rs1 IR13 /\
                arm_stack_pointer_spec rs0 m0 flag_blk regs_blk jit_blk jit_state_blk;
  arm_inv_reg:  arm_assume_register_map rs0 /\
                arm_assume_register_map rs1 /\
                Mem.range_perm m0 regs_blk 0 88 Cur Writable;
  arm_inv_st:   jit_state_memory_layout st jit_state_blk regs_blk m0;
  arm_mem:      Mem.unchanged_on P m0 m1; (**r extends without transitivity *) (**r (fun b _ => not_stack_blk (rs0 IR13) b) *)
}. *)


Definition ins_is_sync_regs (ins: bpf_instruction) (lsr: sync_regs): Prop :=
  match ins with
  | BPF_BINARY a bop dst src =>
    match a with
    | A32 => (List.In dst lsr) /\ (
      match src with
      | inl r => List.In r lsr
      | inr i => True
      end)
    | A64 => False
    end
  | _ => False
  end.

Definition Mem_range_permv (m : mem) (v : val) (lo hi : Z) (k : perm_kind) (p : permission) :=
  match v with
  | Vptr b ofs => Mem.range_perm m b lo hi k p
  | _ => False
  end.
