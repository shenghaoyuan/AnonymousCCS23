From compcert Require Import Integers.
From compcert.arm Require Import AsmSyntax BinSyntax BinDecode.

From bpf.comm Require Import Flag BinrBPF ListAsArray Regs.
From bpf.model Require Import Encode Syntax.
From bpf.jit.thumb Require Import KeyValue2 LoadStoreRegs JITState.
From bpf.jit.thumb Require Import ThumbDecode Arm32Reg ThumbJITOpcode ThumbInsOp.


From bpf.jit.verification Require Import JITListSet.

From Coq Require Import List ZArith Arith String.
Import ListNotations.
Open Scope Z_scope.
Open Scope bool_scope.
Open Scope asm.

(** List reprensent of LoadStoreRegs *)
(*
Lemma int64_repr_int_unsigned_same:
  forall x y,
    
    Int64.repr (Int.unsigned (Int.repr x)) = Int64.repr (Int.unsigned (Int.repr y)) ->
      x = y.
Proof.
  intros.
  Search Int64.repr.
  Transparent Int64.repr.
  injection H as Heq.
  rewrite ! Int64.Z_mod_modulus_eq in Heq.
  rewrite ! Zmod_small in Heq.
  Compute Int64.modulus.
Qed. *)

Definition jit_alu32_load_aux_list (ins: bpf_instruction): option (list reg) :=
  match ins with
  | BPF_BINARY a bop dst src =>
    match a with
    | A32 =>
      match bop with
      | BPF_ADD | BPF_SUB | BPF_MUL | BPF_DIV | BPF_OR | BPF_AND | BPF_XOR | BPF_MOV
      | BPF_LSH | BPF_RSH | BPF_ARSH =>
        match src with
        | inl r => if reg_eqb dst r then Some [dst] else Some [dst; r]
        | inr i => Some [dst]
        end
      | BPF_MOD => None
      end
    | A64 => None
    end
  | _ => None
  end.

Definition jit_alu32_store_aux_list (ins: bpf_instruction): option reg :=
  match ins with
  | BPF_BINARY a bop _ src =>
    match a with
    | A32 =>
      match bop with
      | BPF_ADD | BPF_SUB | BPF_MUL | BPF_DIV | BPF_OR | BPF_AND | BPF_XOR | BPF_MOV
      | BPF_LSH | BPF_RSH | BPF_ARSH =>
        match src with
        | inl r => Some r
        | inr i => None
        end
      | BPF_MOD => None
      end
    | A64 => None
    end
  | _ => None
  end.

Fixpoint jit_alu32_load_list (l: list bpf_instruction): option (list reg) :=
  match l with
  | [] => Some []
  | hd :: tl =>
    match jit_alu32_load_list tl with
    | Some nl =>
      match jit_alu32_load_aux_list hd with
      | Some rl => Some (app_no_repeat reg_eqb rl nl)
      | None => None
      end
    | None => None
    end
  end.

Fixpoint jit_alu32_store_list (l: list bpf_instruction): option (list reg) :=
  match l with
  | [] => Some []
  | hd :: tl =>
    match jit_alu32_store_list tl with
    | Some nl =>
      match jit_alu32_store_aux_list hd with
      | Some r => if list_in_bool reg_eqb r nl then Some nl else Some (r :: nl)
      | None => None
      end
    | None => None
    end
  end.

Definition jit_alu32_use_IR11_aux (ins: bpf_instruction): bool :=
  match ins with
  | BPF_BINARY a bop _ src =>
    match a with
    | A32 =>
      match src with
      | inl r => false
      | inr i =>
        match bop with
        | BPF_ADD | BPF_SUB | BPF_OR | BPF_AND | BPF_XOR | BPF_MOV =>
          if (Int.cmp Cle Int.zero i) && (Int.cmp Cle i (Int.repr 255)) then
            false
          else
            true
        | BPF_MUL => true
        | BPF_DIV | BPF_LSH | BPF_RSH | BPF_MOD| BPF_ARSH => false
        end
      end
    | A64 => false
    end
  | _ => false
  end.

Fixpoint jit_alu32_use_IR11 (l: list bpf_instruction): bool :=
  match l with
  | [] => false
  | hd :: tl =>
    if jit_alu32_use_IR11_aux hd then
      true
    else
      jit_alu32_use_IR11 tl
  end.


Definition construct_thumb2_shift_rd_rm (rd rm: int): int :=
  let ins_rd := encode_arm32 rd rm 8 4 in
    encode_arm32 (Int.repr 0xf) ins_rd 12 4.

Definition jit_alu32_thumb_load_store_template_jit (op rt rn imm12: int) (st: jit_state): option jit_state :=
  let str_low   := encode_arm32 rn op 0 4 in
  let str_high  := encode_arm32 rt imm12 12 4 in
    match upd_jitted_list str_low st with
    | Some str_st => upd_jitted_list str_high str_st
    | None => None
    end.

(** * Pre Stage: mov r12 r1 *)
Definition jit_alu32_pre (st: jit_state): option jit_state :=
  (* the allocation will do `STR IR12 [SP, #+0]` *)
  (**r MOV IR12 R1 *) (**r 12 = 0b 1100 *)
  let ins_rdn := encode_arm32 (Int.repr 4) MOV_R_OP 0 3 in
  let ins_rm  := encode_arm32 Int.one  ins_rdn 3 4 in
  let ins     := encode_arm32 Int.one ins_rm 7 1 in
    upd_jitted_list ins st.

(**r computing all used callee-save registers *)
Definition arm_callee_save_regs: list ireg := [IR4; IR5; IR6; IR7; IR8; IR9; IR10; IR11].

Definition filter_csr (ldr_l str_l: list reg) (ir: ireg) :=
  match (reg_of_ireg ir) with
  | Some r =>
    if (list_in_bool reg_eqb r ldr_l) || (list_in_bool reg_eqb r str_l) then
      true
    else
      false
  | None => true
  end.

Definition jit_alu32_stack_list (ldr_l str_l: list reg) (st: jit_state): list ireg :=
  List.filter (fun x => filter_csr ldr_l str_l x) arm_callee_save_regs.

(** * Spilling Stage: str ri [sp, #(i*4)] *)
Definition jit_alu32_thumb_upd_save (r: ireg) (st: jit_state): option jit_state :=
  jit_alu32_thumb_load_store_template_jit STR_I_OP (int_of_ireg r)
    (int_of_ireg SP) (Int.mul (int_of_ireg r) (Int.repr 4)) st.

Fixpoint jit_alu32_thumb_save (l: list ireg) (st: jit_state): option jit_state :=
  match l with
  | [] => Some st
  | r :: tl =>
    match jit_alu32_thumb_upd_save r st with
    | Some st1 => jit_alu32_thumb_save tl st1
    | None => None
    end
  end.

(** * Load Stage: Load selected BPF registers into corresponding arm32 registers *)

Definition jit_alu32_thumb_upd_load (r: reg) (st: jit_state): option jit_state :=
  jit_alu32_thumb_load_store_template_jit LDR_I_OP (int_of_reg r) (Int.repr 12)
    (Int.add (Int.mul (int_of_reg r) (Int.repr 8)) (Int.repr 8)) st.

Fixpoint jit_alu32_thumb_load (ldr_l: list reg) (st: jit_state): option jit_state :=
  match ldr_l with
  | [] => Some st
  | hd :: tl =>
    match jit_alu32_thumb_upd_load hd st with
    | Some st1 => jit_alu32_thumb_load tl st1
    | None => None
    end
  end.

(** * Jitted Code: from BPF alu32 to thumb alu *)


(**r move imm32 low16-bit to ireg *)

Definition mov_int_to_movw (i: int) (r: ireg) (st: jit_state): option jit_state :=
  let lo_imm8   := decode_arm32 i 0  8 in
  let lo_imm3   := decode_arm32 i 8  3 in
  let lo_i      := decode_arm32 i 11 1 in
  let lo_imm4   := decode_arm32 i 12 4 in
  let movw_lo_0 := encode_arm32 lo_imm4 MOVW_OP   0  4 in
  let movw_lo   := encode_arm32 lo_i    movw_lo_0 10 1 in

  let movw_hi_0 := encode_arm32 (int_of_ireg r) lo_imm8 8 4 in
  let movw_hi   := encode_arm32 lo_imm3 movw_hi_0 12 3 in
    match upd_jitted_list movw_lo st with
    | Some movw_st0 => upd_jitted_list movw_hi movw_st0
    | None => None
    end.

(**r move imm32 high16-bit to ireg *)

Definition mov_int_to_movt (i: int) (r: ireg) (st: jit_state): option jit_state :=
  let hi_imm8   := decode_arm32 i 16 8 in
  let hi_imm3   := decode_arm32 i 24 3 in
  let hi_i      := decode_arm32 i 27 1 in
  let hi_imm4   := decode_arm32 i 28 4 in
  let movt_lo_0 := encode_arm32 hi_imm4 MOVT_OP   0  4 in
  let movt_lo   := encode_arm32 hi_i    movt_lo_0 10 1 in

  let movt_hi_0 := encode_arm32 (int_of_ireg r) hi_imm8 8 4 in
  let movt_hi   := encode_arm32 hi_imm3 movt_hi_0 12 3 in
    match upd_jitted_list movt_lo st with
    | Some movt_st0 => upd_jitted_list movt_hi movt_st0
    | None => None
    end.

(** for alu32 reg *)
(**r src could be IR11 *)
Definition bpf_alu32_to_thumb_reg (bop: binOp) (dst: reg) (src: ireg) (st: jit_state): option jit_state :=
  match bop with
  | BPF_ADD =>
    let d       := if Int.lt (int_of_reg dst) (Int.repr 8) then Int.zero else Int.one in
    let rdn     := if Int.lt (int_of_reg dst) (Int.repr 8) then
                      (int_of_reg dst)
                    else
                      Int.sub (int_of_reg dst) (Int.repr 8) in
    let ins_rdn := encode_arm32 rdn ADD_R_OP 0 3 in
    let ins_rm  := encode_arm32 (int_of_ireg src) ins_rdn 3 4 in
    let ins     := encode_arm32 d ins_rm 7 1 in
      upd_jitted_list ins st
  | BPF_SUB =>
    let ins_lo  := encode_arm32 (int_of_reg dst) SUB_R_OP 0 4 in
    let ins_hi  := encode_arm32 (int_of_reg dst) (int_of_ireg src) 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_MUL =>
    let ins_lo  := encode_arm32 (int_of_reg dst) MUL_OP 0 4 in
    let ins_hi0 := encode_arm32 (int_of_reg dst) (int_of_ireg src) 8 4 in
    let ins_hi  := encode_arm32 (Int.repr 0xf) ins_hi0 12 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_DIV =>
      match upd_jitted_list UDIV_OP st with
      | Some div_st0 => upd_jitted_list (Int.repr 0xf0f1) div_st0
      | None => None
      end
  | BPF_OR =>
    let ins_lo  := encode_arm32 (int_of_reg dst) ORR_R_OP 0 4 in
    let ins_hi  := encode_arm32 (int_of_reg dst) (int_of_ireg src) 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_AND =>
    let ins_lo  := encode_arm32 (int_of_reg dst) AND_R_OP 0 4 in
    let ins_hi  := encode_arm32 (int_of_reg dst) (int_of_ireg src) 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_LSH =>
    let lsl_lo  := encode_arm32 (int_of_reg dst) LSL_R_OP 0 4 in
    let lsl_hi  := construct_thumb2_shift_rd_rm (int_of_reg dst) (int_of_ireg src) in
      match upd_jitted_list lsl_lo st with
      | Some lsl_st0 => upd_jitted_list lsl_hi lsl_st0
      | None => None
      end
  | BPF_RSH =>
    let lsr_lo  := encode_arm32 (int_of_reg dst) LSR_R_OP 0 4 in
    let lsr_hi  := construct_thumb2_shift_rd_rm (int_of_reg dst) (int_of_ireg src) in
      match upd_jitted_list lsr_lo st with
      | Some lsr_st0 => upd_jitted_list lsr_hi lsr_st0
      | None => None
      end
  | BPF_MOD => None
  | BPF_XOR =>
    let ins_lo  := encode_arm32 (int_of_reg dst) EOR_R_OP 0 4 in
    let ins_hi  := encode_arm32 (int_of_reg dst) (int_of_ireg src) 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_MOV =>
    let d       := if Int.lt (int_of_reg dst) (Int.repr 8) then Int.zero else Int.one in
    let rdn     := if Int.lt (int_of_reg dst) (Int.repr 8) then
                      (int_of_reg dst)
                    else
                      Int.sub (int_of_reg dst) (Int.repr 8) in
    let ins_rdn := encode_arm32 rdn MOV_R_OP 0 3 in
    let ins_rm  := encode_arm32 (int_of_ireg src)  ins_rdn 3 4 in
    let ins     := encode_arm32 d ins_rm 7 1 in
      upd_jitted_list ins st
  | BPF_ARSH =>
    let asr_lo  := encode_arm32 (int_of_reg dst) ASR_R_OP 0 4 in
    let asr_hi  := construct_thumb2_shift_rd_rm (int_of_reg dst) (int_of_ireg src) in
      match upd_jitted_list asr_lo st with
      | Some asr_st0 => upd_jitted_list asr_hi asr_st0
      | None => None
      end
  end.

(** for alu32 imm *)
Definition bpf_alu32_to_thumb_imm0 (imm32: int) (bop: binOp) (dst: reg) (st: jit_state): option jit_state :=
  match bop with
  | BPF_ADD =>
    let ins_lo    := encode_arm32 (int_of_reg dst) ADD_I_OP 0 4 in
    let ins_hi    := encode_arm32 (int_of_reg dst) imm32 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_SUB =>
    let ins_lo    := encode_arm32 (int_of_reg dst) SUB_I_OP 0 4 in
    let ins_hi    := encode_arm32 (int_of_reg dst) imm32 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_MUL =>
    let st0 := mov_int_to_movw imm32 IR11 st in
      bpf_alu32_to_thumb_reg BPF_MUL dst IR11 st
  | BPF_OR  =>
    let ins_lo    := encode_arm32 (int_of_reg dst) ORR_I_OP 0 4 in
    let ins_hi    := encode_arm32 (int_of_reg dst) imm32 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_AND =>
    let ins_lo    := encode_arm32 (int_of_reg dst) AND_I_OP 0 4 in
    let ins_hi    := encode_arm32 (int_of_reg dst) imm32 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_XOR =>
    let ins_lo    := encode_arm32 (int_of_reg dst) EOR_I_OP 0 4 in
    let ins_hi    := encode_arm32 (int_of_reg dst) Int.zero 8 4 in
      match upd_jitted_list ins_lo st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | BPF_MOV =>
    let ins_hi    := encode_arm32 (int_of_reg dst) imm32 8 4 in
      match upd_jitted_list MOVW_OP st with
      | Some ins_st0 => upd_jitted_list ins_hi ins_st0
      | None => None
      end
  | _ => None
  end.

Definition bpf_alu32_to_thumb_imm (bop: binOp) (dst: reg) (imm32: int) (st: jit_state): option jit_state :=
  let lo_32 := decode_arm32 imm32 0 16 in
  let hi_32 := decode_arm32 imm32 16 16 in
    if (Int.cmp Cle Int.zero imm32) && (Int.cmp Cle imm32 (Int.repr 255)) then
      bpf_alu32_to_thumb_imm0 imm32 bop dst st
    else 
      match mov_int_to_movw imm32 IR11 st with
      | Some st0 => 
        if Int.eq hi_32 Int.zero then
          bpf_alu32_to_thumb_reg bop dst IR11 st0
        else
          match mov_int_to_movt imm32 IR11 st0 with
          | Some st1 => bpf_alu32_to_thumb_reg bop dst IR11 st1
          | None => None
          end
      | None => None
      end.

Definition bpf_alu32_to_thumb (ins: bpf_instruction) (st: jit_state): option jit_state :=
  match ins with
  | BPF_BINARY a bop dst src =>
    match a with
    | A32 =>
      match src with
      | inl r => bpf_alu32_to_thumb_reg bop dst (ireg_of_reg r) st
      | inr i => bpf_alu32_to_thumb_imm bop dst i st
      end
    | A64 => None
    end
  | _ => None
  end.

Fixpoint jit_core (l: list bpf_instruction) (st: jit_state): option jit_state :=
  match l with
  | [] => Some st
  | hd :: tl =>
    match bpf_alu32_to_thumb hd st with
    | Some st0 => jit_core tl st0
    | None => None
    end
  end.

(** * Store Stage: Store selected arm32 registers into corresponding BPF registers *)


Definition jit_alu32_thumb_upd_store (r: reg) (st: jit_state): option jit_state :=
  jit_alu32_thumb_load_store_template_jit STR_I_OP (int_of_reg r) (Int.repr 12)
    (Int.add (Int.mul (int_of_reg r) (Int.repr 8)) (Int.repr 8)) st.

Fixpoint jit_alu32_thumb_store (str_l: list reg) (st: jit_state): option jit_state :=
  match str_l with
  | [] => Some st
  | hd :: tl =>
    match jit_alu32_thumb_upd_store hd st with
    | Some st1 => jit_alu32_thumb_store tl st1
    | None => None
    end
  end.

(** * Reloading Stage: recover the initial value of selected arm32 registers *)

Definition jit_alu32_thumb_upd_reset (r: ireg) (st: jit_state): option jit_state :=
  jit_alu32_thumb_load_store_template_jit LDR_I_OP
    (int_of_ireg r) (int_of_ireg SP) (Int.mul (int_of_ireg r) (Int.repr 4)) st.

Fixpoint jit_alu32_thumb_reset (l: list ireg) (st: jit_state): option jit_state :=
  match l with
  | [] => Some st
  | r :: tl =>
    match jit_alu32_thumb_upd_reset r st with
    | Some st1 => jit_alu32_thumb_reset tl st1
    | None => None
    end
  end.

(** Post: LDR SP [SP, #+0]; BX LR *)
Definition jit_alu32_post (st: jit_state): option jit_state :=
  (**r LDR SP [SP, #+0] *)
  match jit_alu32_thumb_load_store_template_jit LDR_I_OP (int_of_ireg SP) (int_of_ireg SP) Int.zero st with
  | Some st0 =>
    (**r BX LR *)
    let ins_rm   := encode_arm32 (int_of_ireg RA) BX_OP 3 4 in
      upd_jitted_list ins_rm st0
  | None => None
  end.

(** * Jit Procedure *)
Definition jit_alu32_to_thumb (l: list bpf_instruction) (st: jit_state): option jit_state :=
  let st0 := reset_init_jittedthumb st in
  let b   := jit_alu32_use_IR11 l in
  let st1 := upd_IR11_jittedthumb b st0 in
    match jit_alu32_load_list l with
    | Some ldr_l =>
      match jit_alu32_store_list l with
      | Some str_l =>
        match jit_alu32_pre st1 with
        | Some thumb_pre =>
          let csr_l := jit_alu32_stack_list ldr_l str_l thumb_pre in
            match jit_alu32_thumb_save csr_l thumb_pre with
            | Some thumb_save =>
              match jit_alu32_thumb_load ldr_l thumb_save with
              | Some thumb_load =>
                match jit_core l thumb_load with
                | Some thumb_core =>
                  match jit_alu32_thumb_store str_l thumb_core with
                  | Some thumb_store =>
                    match jit_alu32_thumb_reset csr_l thumb_store with
                    | Some thumb_reset => jit_alu32_post thumb_reset
                    | None => None
                    end
                  | None => None
                  end
                | None => None
                end
              | None => None
              end
            | None => None
            end
        | None => None
        end
      | None => None
      end
    | None => None
    end.

Close Scope string_scope.
Close Scope asm.
Close Scope bool_scope.
Close Scope Z_scope.