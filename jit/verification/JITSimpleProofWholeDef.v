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
Definition regs_agree (lsr: sync_regs) (rbpf_st: state) (arm_rs: Asm.regset): Prop :=
  forall r, List.In r lsr ->
    exists vi, arm_rs (ireg_of_reg r) = Vint vi /\
      (eval_reg r rbpf_st) = Some (Val.longofintu (Vint vi)).

Definition sub_mem_blk (m0 m1: mem) (b: block) (ofs: Z): Prop :=
  forall chunk ofs1, 0 <= ofs1 /\ ofs1 + (size_chunk chunk) <= ofs -> Mem.load chunk m0 b ofs1 = Mem.load chunk m1 b ofs1.


Definition sub_stack (m0 m1: mem) (ptr: val) (l: list ireg): Prop :=
  forall r, List.In r l ->
    exists vi,
    Mem.loadv Mint32 m0 (Val.offset_ptr ptr (Ptrofs.of_intu (int_of_ireg r))) = Some vi /\
    Mem.loadv Mint32 m1 (Val.offset_ptr ptr (Ptrofs.of_intu (int_of_ireg r))) = Some vi.

Definition regs_layout (st: jit_state) (m: mem) (regs_blk: block): Prop :=
  forall r,
    exists vi, (eval_jit_reg r st) = Some (Val.longofintu (Vint vi)) /\
      Mem.load Mint64 m regs_blk (8 * id_of_reg r) = Some (Val.longofintu (Vint vi)) /\
      Mem.load Mint32 m regs_blk (8 * id_of_reg r) = Some (Vint vi) /\
      forall vj, (eval_jit_reg r st) = Some (Val.longofintu (Vint vj)) -> vi = vj.

Definition jit_state_memory_layout (st: jit_state) (st_blk regs_blk: block) (m: mem): Prop :=
  (**r st_blk: 4bytes (pc) + 4 bytes (flag_ptr) + 4 bytes (regs_ptr)... *)
  Mem.loadv Mint32 m (Vptr st_blk (Ptrofs.repr 8)) = Some (Vptr regs_blk Ptrofs.zero) /\
  (**r 8*r is the offset of the regs_blk of each rBPF register r \in [0, 11] *)
  regs_layout st m regs_blk.
(*
Definition arm_initial_stack_regs: list ireg := [IR3; IR4; IR5; IR6; IR7; IR8; IR9; IR10; IR11]. *)

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

Definition match_registers (rbpf_st: state) (jit_st: jit_state) (regs_blk: block): Prop :=
  regs_st rbpf_st = Vptr regs_blk Ptrofs.zero /\
  jit_regs jit_st = Vptr regs_blk Ptrofs.zero /\
  forall r,
    exists vi, (eval_reg r rbpf_st) = Some (Val.longofintu (Vint vi)) /\
      (eval_jit_reg r jit_st) = Some (Val.longofintu (Vint vi)) /\
      Mem.load Mint32 (jit_mem jit_st) regs_blk (8 * id_of_reg r) = Some (Vint vi) /\
        forall vj, (eval_reg r rbpf_st) = Some (Val.longofintu (Vint vj)) -> vi = vj.

Definition match_registers_syn (rbpf_st: state) (m: mem) (lsr: sync_regs) (regs_blk: block): Prop :=
  regs_st rbpf_st = Vptr regs_blk Ptrofs.zero /\
  Mem.range_perm (bpf_m rbpf_st) regs_blk 0 88 Cur Writable /\
  forall r,
    exists vi, eval_reg r rbpf_st = Some (Val.longofintu (Vint vi)) /\
    (forall vj, (eval_reg r rbpf_st) = Some (Val.longofintu (Vint vj)) -> vi = vj) /\
    (List.In r lsr ->
      Mem.load Mint32 m regs_blk (8 * id_of_reg r) = Some (Vint vi)).

Definition match_flag (rbpf_st: state) (jit_st: jit_state) (flag_blk: block) : Prop :=
  flag rbpf_st = Vptr flag_blk Ptrofs.zero /\
  jit_flag jit_st = Vptr flag_blk Ptrofs.zero /\
  exists f,
    eval_flag rbpf_st = Some (Vint (int_of_flag f)) /\
    eval_jit_flag jit_st = Some (Vint (int_of_flag f)).

Definition ptr_range_perm (m: mem) (chunk: memory_chunk) (ptr: val) (p: permission): Prop :=
  match ptr with
  | Vptr b ofs => Mem.valid_access m chunk b (Ptrofs.unsigned ofs) p
  | _ => False
  end.

Definition arm_synch_stack (m: mem) (rs old_rs: Asm.regset) (old_sp: val) (lsr_stack: sync_iregs): Prop :=
  Mem.loadv Mint32 m (rs IR13) = Some old_sp /\
  forall r,
    (List.In r lsr_stack ->
      Mem.loadv Mint32 m (Val.offset_ptr (rs IR13) (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4)))) = Some (old_rs r))  (*/\
    (~ List.In r lsr_stack /\ List.In r arm_callee_save_regs ->
      Mem.loadv Mint32 m (Val.offset_ptr (rs IR13) (Ptrofs.of_intu (int_of_ireg r))) = Some Vundef) *).

Definition not_stack_blk (old_sp: val) (b: block): Prop :=
  match old_sp with
  | Vptr b1 _ => b <> b1
  | _ => False
  end.

Definition block_neq (flag_blk regs_blk jit_blk jit_state_blk: block): Prop :=
  (flag_blk <> regs_blk /\ jit_blk <> flag_blk /\ jit_blk <> regs_blk) /\
  (flag_blk <> jit_state_blk /\ jit_blk <> jit_state_blk /\ jit_state_blk <> regs_blk).

Definition arm_stack_pointer_spec (rs: Asm.regset) (m: mem) (flag_blk regs_blk jit_blk jit_state_blk: block): Prop :=
  exists sp_blk,
    (rs IR13) = Vptr sp_blk Ptrofs.zero /\
    (sp_blk <> flag_blk /\ sp_blk <> jit_blk /\ sp_blk <> regs_blk /\ sp_blk <> jit_state_blk) /\
    (block_neq flag_blk regs_blk jit_blk jit_state_blk) /\
    Mem.range_perm m sp_blk 0 48 Cur Writable.

Definition arm_assume_register_map (rs: Asm.regset) : Prop :=
  forall r,
    match rs r with
    | Vundef | Vint _ | Vptr _ _ => True
    | _ => False
    end.



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
}.


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
