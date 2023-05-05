From compcert.lib Require Import Integers.
From compcert.arm Require Import Asm AsmSyntax BinSyntax BinSem BinDecode.
From compcert.common Require Import Values Globalenvs Smallstep Memory Memdata Events AST.

From bpf.comm Require Import Flag BinrBPF ListAsArray Regs rBPFMemType.
From bpf.model Require Import Encode Syntax.
From bpf.monadicmodel2 Require Import ConcreteState.
From bpf.jit.thumb Require Import LoadStoreRegs JITState.
From bpf.jit.thumb Require Import Arm32Reg ThumbJITOpcode ThumbInsOp.

From bpf.jit.verification Require Import JITListSet.
From bpf.jit.verification Require Import rBPFSemInd JITSimple JITSimpleProofWholeDef.
From bpf.jit.simulation Require Import BitfieldLemma.

From Coq Require Import List ZArith Arith String Lia.
Import ListNotations.
Open Scope Z_scope.
Open Scope bool_scope.
Open Scope asm.

Lemma sub_mem_blk_trans:
  forall m0 m1 m2 b low high
    (Hmem0: sub_mem_blk m0 m1 b low high)
    (Hmem1: sub_mem_blk m1 m2 b low high),
      sub_mem_blk m0 m2 b low high.
Proof.
  unfold sub_mem_blk; intros.
  rewrite Hmem0; auto.
Qed.

Lemma sub_mem_blk_less_than:
  forall m0 m1 b low0 high0 low1 high1
    (Hless: low0 <= low1 /\ high1 <= high0)
    (Hmem: sub_mem_blk m0 m1 b low0 high0),
      sub_mem_blk m0 m1 b low1 high1.
Proof.
  unfold sub_mem_blk; intros.
  rewrite Hmem; auto.
  lia.
Qed.


Lemma upd_jitted_list_unchange_jit_pc:
  forall i st0 st1,
    upd_jitted_list i st0 = Some st1 ->
      jit_pc st0 = jit_pc st1.
Proof.
  unfold upd_jitted_list; intros.
  destruct upd_jitted_list'; inversion H; simpl.
  reflexivity.
Qed.

Lemma upd_jitted_list_unchange_jit_pc_2:
  forall i j st0 st1,
    match upd_jitted_list i st0 with
     | Some ins_st0 => upd_jitted_list j ins_st0
     | None => None
     end = Some st1 ->
      jit_pc st0 = jit_pc st1.
Proof.
  unfold upd_jitted_list; intros.
  destruct upd_jitted_list'; inversion H; simpl.
  destruct upd_jitted_list'; inversion H1; simpl; clear.
  reflexivity.
Qed.

Lemma upd_jitted_list_unchange_jittted_list:
  forall i st0 st1,
    upd_jitted_list i st0 = Some st1 ->
      jitted_list st0 = jitted_list st1.
Proof.
  unfold upd_jitted_list; intros.
  destruct upd_jitted_list'; inversion H; simpl.
  reflexivity.
Qed.

Lemma upd_jitted_list_unchange_jittted_list_2:
  forall i j st0 st1,
    match upd_jitted_list i st0 with
     | Some ins_st0 => upd_jitted_list j ins_st0
     | None => None
     end = Some st1 ->
      jitted_list st0 = jitted_list st1.
Proof.
  unfold upd_jitted_list; intros.
  destruct upd_jitted_list'; inversion H; simpl.
  destruct upd_jitted_list'; inversion H1; simpl; clear.
  reflexivity.
Qed.

Lemma bpf_alu32_to_thumb_unchange_jittted_list:
  forall ins st0 st1
    (Hone : bpf_alu32_to_thumb ins st0 = Some st1),
      jitted_list st0 = jitted_list st1.
Proof.
  intros.
  unfold bpf_alu32_to_thumb in Hone.
  destruct ins in Hone; inversion Hone.
  clear - H0.
  destruct a; [| inversion H0].
  destruct s.
  + unfold bpf_alu32_to_thumb_reg in H0.
    destruct b; inversion H0.
    10:{ eapply upd_jitted_list_unchange_jittted_list; eauto. }
    1:{ eapply upd_jitted_list_unchange_jittted_list; eauto. }
    all: eapply upd_jitted_list_unchange_jittted_list_2; eauto.
  + unfold bpf_alu32_to_thumb_imm in H0.
    destruct (Int.cmp Cle Int.zero i && Int.cmp Cle i (Int.repr 255)).
    * unfold bpf_alu32_to_thumb_imm0 in H0.
      destruct b; inversion H0.
      all: eapply upd_jitted_list_unchange_jittted_list_2; eauto.
    * unfold mov_int_to_movw in H0.
      destruct upd_jitted_list eqn: Hst1 in H0; [| inversion H0].
      eapply upd_jitted_list_unchange_jittted_list in Hst1; eauto; rewrite Hst1; clear Hst1.
      destruct upd_jitted_list eqn: Hst1 in H0; [| inversion H0].
      eapply upd_jitted_list_unchange_jittted_list in Hst1; eauto; rewrite Hst1; clear Hst1.
      destruct Int.eq.
      { unfold bpf_alu32_to_thumb_reg in H0.
        destruct b; inversion H0.
        10:{ eapply upd_jitted_list_unchange_jittted_list; eauto. }
        1:{ eapply upd_jitted_list_unchange_jittted_list; eauto. }
        all: eapply upd_jitted_list_unchange_jittted_list_2; eauto.
      }
      {
        unfold mov_int_to_movt in H0.
        destruct upd_jitted_list eqn: Hst1 in H0; [| inversion H0].
        eapply upd_jitted_list_unchange_jittted_list in Hst1; eauto; rewrite Hst1; clear Hst1.
        destruct upd_jitted_list eqn: Hst1 in H0; [| inversion H0].
        eapply upd_jitted_list_unchange_jittted_list in Hst1; eauto; rewrite Hst1; clear Hst1.
        unfold bpf_alu32_to_thumb_reg in H0.
        destruct b; inversion H0.
        10:{ eapply upd_jitted_list_unchange_jittted_list; eauto. }
        1:{ eapply upd_jitted_list_unchange_jittted_list; eauto. }
        all: eapply upd_jitted_list_unchange_jittted_list_2; eauto.
      }
Qed.

Lemma jit_core_unchange_jittted_list:
  forall l st0 st1
    (Hjit : jit_core l st0 = Some st1),
      jitted_list st0 = jitted_list st1.
Proof.
  induction l; simpl; intros.
  - inversion Hjit; reflexivity.
  - destruct bpf_alu32_to_thumb eqn: Hone; [| inversion Hjit].
    specialize (IHl j st1 Hjit).
    rewrite <- IHl.
    eapply bpf_alu32_to_thumb_unchange_jittted_list; eauto.
Qed.


Lemma upd_jitted_list_jittted_len:
  forall i st0 st1,
    upd_jitted_list i st0 = Some st1 ->
      (S (jitted_len st0) = jitted_len st1)%nat.
Proof.
  unfold upd_jitted_list; intros.
  destruct upd_jitted_list'; inversion H; simpl.
  reflexivity.
Qed.

Lemma upd_jitted_list_jittted_len_2:
  forall i j st0 st1,
    match upd_jitted_list i st0 with
     | Some ins_st0 => upd_jitted_list j ins_st0
     | None => None
     end = Some st1 ->
      (S (S (jitted_len st0)) = jitted_len st1)%nat.
Proof.
  unfold upd_jitted_list; intros.
  destruct upd_jitted_list'; inversion H; simpl.
  destruct upd_jitted_list'; inversion H1; simpl; clear.
  reflexivity.
Qed.

Lemma bpf_alu32_to_thumb_jittted_len:
  forall ins st0 st1
    (Hone : bpf_alu32_to_thumb ins st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  intros.
  unfold bpf_alu32_to_thumb in Hone.
  destruct ins in Hone; inversion Hone.
  clear - H0.
  destruct a; [| inversion H0].
  destruct s.
  + unfold bpf_alu32_to_thumb_reg in H0.
    destruct b; inversion H0.
    10:{ eapply upd_jitted_list_jittted_len in H1; eauto. lia. }
    1:{ eapply upd_jitted_list_jittted_len in H1; eauto. lia. }
    all: eapply upd_jitted_list_jittted_len_2 in H0; eauto; lia.
  + unfold bpf_alu32_to_thumb_imm in H0.
    destruct (Int.cmp Cle Int.zero i && Int.cmp Cle i (Int.repr 255)).
    * unfold bpf_alu32_to_thumb_imm0 in H0.
      destruct b; inversion H0.
      all: eapply upd_jitted_list_jittted_len_2 in H0; eauto; lia.
    * unfold mov_int_to_movw in H0.
      destruct upd_jitted_list eqn: Hst1 in H0; [| inversion H0].
      eapply upd_jitted_list_jittted_len in Hst1; eauto.
      destruct upd_jitted_list eqn: Hst2 in H0; [| inversion H0].
      eapply upd_jitted_list_jittted_len in Hst2; eauto.
      destruct Int.eq.
      { unfold bpf_alu32_to_thumb_reg in H0.
        destruct b; inversion H0.
        10:{ eapply upd_jitted_list_jittted_len in H1; eauto. lia. }
        1:{ eapply upd_jitted_list_jittted_len in H1; eauto. lia. }
        all: eapply upd_jitted_list_jittted_len_2 in H1; eauto; lia.
      }
      {
        unfold mov_int_to_movt in H0.
        destruct upd_jitted_list eqn: Hst3 in H0; [| inversion H0].
        eapply upd_jitted_list_jittted_len in Hst3; eauto.
        destruct upd_jitted_list eqn: Hst4 in H0; [| inversion H0].
        eapply upd_jitted_list_jittted_len in Hst4; eauto.
        unfold bpf_alu32_to_thumb_reg in H0.
        destruct b; inversion H0.
        10:{ eapply upd_jitted_list_jittted_len in H1; eauto; lia. }
        1:{ eapply upd_jitted_list_jittted_len in H1; eauto; lia. }
        all: eapply upd_jitted_list_jittted_len_2 in H1; eauto; lia.
      }
Qed.

Global Transparent Archi.ptr64.

Lemma upd_jitted_list_load:
  forall i st0 st1 ofs1 chunk arm_blk,
    jitted_list st0 = Vptr arm_blk Ptrofs.zero ->
    0 <= ofs1 /\ ofs1 + size_chunk chunk <= Z.of_nat (2 * jitted_len st0) /\
    Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH ->
    upd_jitted_list i st0 = Some st1 ->
      Mem.load chunk (jit_mem st0) arm_blk ofs1 = Mem.load chunk (jit_mem st1) arm_blk ofs1.
Proof.
  unfold upd_jitted_list, JITTED_LIST_MAX_LENGTH; intros.
  unfold upd_jitted_list' in H1.
  rewrite H in *.
  simpl in *.
  rewrite Ptrofs.add_zero_l in *.
  unfold Ptrofs.of_int in *.
  rewrite Int.unsigned_repr in H1; [ | change Int.max_unsigned with 4294967295; lia ].
  rewrite Ptrofs.unsigned_repr in H1; [ | change Ptrofs.max_unsigned with 4294967295; lia ].
  symmetry.
  destruct (jitted_len st0 + (jitted_len st0 + 0) + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond; inversion H1.
  destruct Mem.store eqn: Hstore; inversion H3.
  eapply Mem.load_store_other; eauto.
  clear H1 H3 H4.
  lia.
Qed.

Lemma upd_jitted_list_load_2:
  forall i j st0 st1 ofs1 chunk arm_blk,
    jitted_list st0 = Vptr arm_blk Ptrofs.zero ->
    0 <= ofs1 /\ ofs1 + size_chunk chunk <= Z.of_nat (2 * jitted_len st0) /\
    Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH ->
    match upd_jitted_list i st0 with
     | Some ins_st0 => upd_jitted_list j ins_st0
     | None => None
     end = Some st1 ->
      Mem.load chunk (jit_mem st0) arm_blk ofs1 = Mem.load chunk (jit_mem st1) arm_blk ofs1.
Proof.
  intros.
  destruct upd_jitted_list eqn: Hupd1; inversion H1.

  eapply upd_jitted_list_unchange_jittted_list in Hupd1 as Heq1.
  eapply upd_jitted_list_jittted_len in Hupd1 as Hle1.

  eapply upd_jitted_list_load in Hupd1; eauto.
  rewrite Hupd1.
  eapply upd_jitted_list_load in H1; eauto.
  - rewrite <- Heq1.
    auto.
  - unfold upd_jitted_list, upd_jitted_list' in H1.
    destruct (2 * jitted_len j0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond; inversion H1.
    clear - H0 Hcond Heq1 Hle1.
    apply leb_complete in Hcond.
    lia.
Qed.

Lemma upd_jitted_list_max:
  forall v st0 st1,
  upd_jitted_list v st0 = Some st1 ->
    Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH.
Proof.
  unfold upd_jitted_list, upd_jitted_list'.
  intros.
  destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond; inversion H.
  apply leb_complete in Hcond.
  lia.
Qed.

Lemma bpf_alu32_to_thumb_sub_mem:
  forall ins st0 st1 arm_blk
    (Hptr: jitted_list st0 = Vptr arm_blk Ptrofs.zero)
    (Hmax: Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH)
    (Hone : bpf_alu32_to_thumb ins st0 = Some st1),
      sub_mem_blk (jit_mem st0) (jit_mem st1) arm_blk 0 (Z.of_nat (2 * jitted_len st0)).
Proof.
  unfold bpf_alu32_to_thumb, sub_mem_blk; intros.
  destruct ins in Hone; inversion Hone.
  clear - Hptr Hmax H H1.
  destruct a; [| inversion H1].
  destruct s.
  + unfold bpf_alu32_to_thumb_reg in H1.
    destruct b; inversion H1.
    10:{ eapply upd_jitted_list_load; eauto. lia. }
    1:{ eapply upd_jitted_list_load; eauto. lia. }
    all: eapply upd_jitted_list_load_2; eauto; lia.
  + unfold bpf_alu32_to_thumb_imm in H1.
    destruct (Int.cmp Cle Int.zero i && Int.cmp Cle i (Int.repr 255)).
    * unfold bpf_alu32_to_thumb_imm0 in H1.
      destruct b; inversion H1.
      all: eapply upd_jitted_list_load_2; eauto; lia.
    * unfold mov_int_to_movw in H1.
      destruct upd_jitted_list eqn: Hst1 in H1; [| inversion H1].

      eapply upd_jitted_list_unchange_jittted_list in Hst1 as Heq1.
      eapply upd_jitted_list_jittted_len in Hst1 as Hle1.

      eapply upd_jitted_list_load with (ofs1 := ofs) (chunk := chunk) in Hst1 as Hsame1; eauto; try lia.
      rewrite Hsame1; clear Hsame1.


      destruct upd_jitted_list eqn: Hst2 in H1; [| inversion H1].

      eapply upd_jitted_list_unchange_jittted_list in Hst2 as Heq2.
      eapply upd_jitted_list_jittted_len in Hst2 as Hle2.

      eapply upd_jitted_list_load with (ofs1 := ofs) (chunk := chunk) in Hst2 as Hsame2; eauto; try lia.
      2:{ rewrite <- Heq1. apply Hptr. }
      2:{ unfold upd_jitted_list, upd_jitted_list' in Hst2.
          destruct (2 * jitted_len j + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond; inversion Hst2.
          apply leb_complete in Hcond. lia.
      }

      rewrite Hsame2.

      assert (Hrange: 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len j0)). {
        lia.
      }
      assert (Hptr0: jitted_list j0 = Vptr arm_blk Ptrofs.zero). {
        rewrite <- Heq2.
        rewrite <- Heq1.
        auto.
      }


      destruct Int.eq.
      { unfold bpf_alu32_to_thumb_reg in H1.
        destruct b; inversion H1.
        10:{ eapply upd_jitted_list_load; eauto. eapply upd_jitted_list_max in H2; eauto; lia. }
        1:{ eapply upd_jitted_list_load; eauto. eapply upd_jitted_list_max in H2; eauto; lia. }
        all: eapply upd_jitted_list_load_2; eauto.
        all: destruct upd_jitted_list eqn: Hupd in H2; inversion H2.
        all:  eapply upd_jitted_list_max in Hupd; eauto; lia.
      }
      {
        unfold mov_int_to_movt in H1.
        destruct upd_jitted_list eqn: Hst3 in H1; [| inversion H1].

        eapply upd_jitted_list_unchange_jittted_list in Hst3 as Heq3.
        eapply upd_jitted_list_jittted_len in Hst3 as Hle3.

        eapply upd_jitted_list_load with (ofs1 := ofs) (chunk := chunk) in Hst3 as Hsame3; eauto; try lia.
        rewrite Hsame3; clear Hsame3.


        destruct upd_jitted_list eqn: Hst4 in H1; [| inversion H1].

        eapply upd_jitted_list_unchange_jittted_list in Hst4 as Heq4.
        eapply upd_jitted_list_jittted_len in Hst4 as Hle4.

        eapply upd_jitted_list_load with (ofs1 := ofs) (chunk := chunk) in Hst4 as Hsame4; eauto; try lia.
        2:{ rewrite <- Heq3. apply Hptr0. }
        2:{ unfold upd_jitted_list, upd_jitted_list' in Hst4.
            destruct (2 * jitted_len j1 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond; inversion Hst4.
            apply leb_complete in Hcond. lia.
        }

        2:{ eapply upd_jitted_list_max in Hst3; eauto; lia. }

        rewrite Hsame4.

        assert (Hrange1: 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len j2)). {
          lia.
        }
        assert (Hptr1: jitted_list j2 = Vptr arm_blk Ptrofs.zero). {
          rewrite <- Heq4.
          rewrite <- Heq3.
          auto.
        }

        unfold bpf_alu32_to_thumb_reg in H1.
        destruct b; inversion H1.
        10:{ eapply upd_jitted_list_load; eauto. eapply upd_jitted_list_max in H2; eauto; lia. }
        1:{ eapply upd_jitted_list_load; eauto. eapply upd_jitted_list_max in H2; eauto; lia. }
        all: eapply upd_jitted_list_load_2; eauto.
        all: destruct upd_jitted_list eqn: Hupd in H2; inversion H2.
        all:  eapply upd_jitted_list_max in Hupd; eauto; lia.
      }
Qed.

Lemma bpf_alu32_to_thumb_max:
  forall ins st0 st1
    (Hone: bpf_alu32_to_thumb ins st0 = Some st1),
      Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH.
Proof.
  unfold bpf_alu32_to_thumb; intros.
  destruct ins in Hone; inversion Hone.
  clear - H0.
  destruct a; [| inversion H0].
  destruct s.
  + unfold bpf_alu32_to_thumb_reg in H0.
    destruct b; inversion H0.
    10:{ eapply upd_jitted_list_max; eauto. }
    1:{ eapply upd_jitted_list_max; eauto. }
    all: destruct upd_jitted_list eqn: Hupd in H1; inversion H1.
    all: eapply upd_jitted_list_max; eauto.
  + unfold bpf_alu32_to_thumb_imm in H0.
    destruct (Int.cmp Cle Int.zero i && Int.cmp Cle i (Int.repr 255)).
    * unfold bpf_alu32_to_thumb_imm0 in H0.
      destruct b; inversion H0.
      all: destruct upd_jitted_list eqn: Hupd in H1; inversion H1.
      all: eapply upd_jitted_list_max; eauto.
    * unfold mov_int_to_movw in H0.
      destruct upd_jitted_list eqn: Hst1 in H0; [| inversion H0].
      all: eapply upd_jitted_list_max; eauto.
Qed.

Lemma jit_core_sub_mem:
  forall l st0 st1 arm_blk
    (Hptr: jitted_list st0 = Vptr arm_blk Ptrofs.zero)
    (Hjit : jit_core l st0 = Some st1),
      sub_mem_blk (jit_mem st0) (jit_mem st1) arm_blk 0 (Z.of_nat (2 * jitted_len st0)).
Proof.
  induction l; unfold sub_mem_blk; simpl; intros.
  - inversion Hjit; reflexivity.
  - destruct bpf_alu32_to_thumb eqn: Hone; [| inversion Hjit].
    rename j into stk.
    apply bpf_alu32_to_thumb_unchange_jittted_list in Hone as Hptr1.
    symmetry in Hptr1.
    rewrite Hptr in Hptr1.
    specialize (IHl stk st1 arm_blk Hptr1 Hjit).
    apply bpf_alu32_to_thumb_max in Hone as Hmax.
    apply bpf_alu32_to_thumb_sub_mem with (arm_blk := arm_blk) in Hone as Htmp; auto.

    unfold sub_mem_blk in *.

    rewrite <- IHl.
    + apply Htmp.
      lia.
    + apply bpf_alu32_to_thumb_jittted_len in Hone.
      lia.
Qed.

Lemma regs_agree_app_no_repeat_intro_r:
  forall l0 l1 rs0 rs1
    (Hreg_agree : regs_agree (app_no_repeat reg_eqb l0 l1) rs0 rs1),
      regs_agree l1 rs0 rs1.
Proof.
  induction l0;
  unfold regs_agree; intros.
  - apply Hreg_agree.
    simpl.
    assumption.
  - apply Hreg_agree.
    simpl.
    destruct list_in_bool.
    + apply list_in_app_no_repeat_r; simpl; auto.
    + simpl.
      right.
      apply list_in_app_no_repeat_r; simpl; auto.
Qed.


Lemma jit_core_jitted_len:
  forall l st0 st1
    (Hjit : jit_core l st0 = Some st1),
      Z.of_nat (2 * jitted_len st0) <= Z.of_nat (2 * jitted_len st1).
Proof.
  induction l; simpl; intros.
  - inversion Hjit; lia.
  - destruct bpf_alu32_to_thumb eqn: Halu32; inversion Hjit.
    clear H0.
    rename j into stk.
    specialize (IHl _ _ Hjit).
    eapply bpf_alu32_to_thumb_jittted_len in Halu32.
    lia.
Qed.

(**r more lemmas *)

Lemma jit_alu32_load_aux_list_spec:
  forall ins l,
    jit_alu32_load_aux_list ins = Some l ->
    (exists r0, l = [r0]) \/ (exists r0 r1, l = [r0; r1] /\ r0 <> r1).
Proof.
  unfold jit_alu32_load_aux_list; intros.
  destruct ins; inversion H.
  destruct a; inversion H.
  clear - H.
  destruct s.
  - destruct reg_eqb eqn: Heq.
    + apply reg_eqb_true in Heq.
      subst r0.
      left.
      destruct b; inversion H; exists r; reflexivity.
    + right.
      apply reg_eqb_false in Heq.
      destruct b; inversion H; exists r; exists r0; intuition.
  - left.
    destruct b; inversion H; exists r; reflexivity.
Qed.

Lemma nodup_load:
  forall l l1
    (Hldr: jit_alu32_load_list l = Some l1),
      NoDup l1.
Proof.
  induction l; simpl; intros.
  - inversion Hldr.
    constructor.
  - destruct jit_alu32_load_list eqn: Hload in Hldr; [| inversion Hldr].
    specialize (IHl l0 Hload).
    destruct jit_alu32_load_aux_list eqn: Haux; inversion Hldr.
    apply jit_alu32_load_aux_list_spec in Haux.
    destruct Haux.
    + destruct H as (r0 & H).
      subst l2.
      simpl.
      destruct list_in_bool eqn: Hb.
      * assumption.
      * apply List_in_bool_false in Hb.
        constructor; auto.
    + destruct H as (r0 & r1 & H & Hneq).
      subst l2.
      simpl.
      destruct list_in_bool eqn: Hb0.
      * destruct (list_in_bool reg_eqb r1 l0) eqn: Hb1.
        assumption.
        apply List_in_bool_false in Hb1.
        constructor; auto.
      * destruct (list_in_bool reg_eqb r1 l0) eqn: Hb1.
        apply List_in_bool_false in Hb0.
        constructor; auto.

        apply List_in_bool_false in Hb0, Hb1.
        constructor; auto.
        intros HF.
        apply Hb0.
        simpl in HF.
        intuition.

        constructor; auto.
Qed.

Lemma nodup_store:
  forall l l1
    (Hstr: jit_alu32_store_list l = Some l1),
      NoDup l1.
Proof.
  induction l; simpl; intros.
  - inversion Hstr.
    constructor.
  - destruct jit_alu32_store_list eqn: Hstore in Hstr; [| inversion Hstr].
    specialize (IHl l0 Hstore).
    destruct jit_alu32_store_aux_list eqn: Haux; inversion Hstr.
    destruct list_in_bool eqn: Hin.
    + injection Hstr as Heq.
      subst l1.
      assumption.
    + apply List_in_bool_false in Hin.
      injection Hstr as Heq.
      subst l1.
      constructor; auto.
Qed.

Lemma regs_agree_trans:
  forall l r st rs
    (Hreg_agree : regs_agree l st rs)
    (Heq : regs_agree [r] st rs),
      regs_agree (r :: l) st rs.
Proof.
  unfold regs_agree; intros.
  destruct H.
  - subst r0.
    apply Heq.
    unfold In.
    left; reflexivity.
  - apply Hreg_agree.
    assumption.
Qed.


Fixpoint build_rs (l: list reg) (rbpf_st: state) (rs: Asm.regset): option Asm.regset :=
  match l with
  | [] => Some rs
  | hd :: tl =>
    match (eval_reg hd rbpf_st) with
    | Some v => match v with
                | Vlong vl => build_rs tl rbpf_st (
                    (rs # (ireg_of_reg hd) <- (Vint (Int.repr (Int64.unsigned vl)))) # PC <-
                (Val.offset_ptr (rs # (ireg_of_reg hd) <- (Vint (Int.repr (Int64.unsigned vl))) PC) wsize))
                | _ => None
                end
    | None => None
    end
  end.


Lemma build_rs_one:
  forall l st rs0 rs1 a
    (Hnodup_load : ~ In a l /\ NoDup l)
    (Hrs1_eq : build_rs l st rs0 = Some rs1)
    (Hreg_agree : regs_agree [a] st rs0),
      regs_agree [a] st rs1.
Proof.
  induction l; simpl; intros.
  - inversion Hrs1_eq.
    subst rs0.
    assumption.
  - destruct eval_reg eqn: Heval_reg; inversion Hrs1_eq.
    clear H0.
    destruct v; inversion Hrs1_eq.
    clear H0.
    destruct Hnodup_load as (HF & Hnodup).
    apply NoDup_cons_iff in Hnodup.

    assert (Hneq: ~ In a0 l). {
      intros Hf.
      apply HF.
      right.
      assumption.
    }

    assert (Hneq0: a <> a0). {
      intros Hf.
      apply HF.
      left.
      assumption.
    }

    eapply IHl.
    intuition.
    apply Hrs1_eq.

    clear - Hneq0 Hreg_agree.
    unfold regs_agree in *.
    intros.
    specialize (Hreg_agree _ H).
    destruct Hreg_agree as (vi & Hrs0_eq & Heval_reg1).

    unfold In in H.
    destruct H.
    2:{ inversion H. }
    subst a0.

    exists vi.

    split; [| assumption].
    rewrite Pregmap.gso.
    2:{ intros HF; inversion HF. }
    rewrite Pregmap.gso.
    2:{ intros HF; apply Hneq0.
        destruct r; destruct a; inversion HF; try reflexivity.
      }
    assumption.
Qed.

Lemma upd_jitted_list_max2:
  forall (v : int) (st0 st1 : jit_state),
    upd_jitted_list v st0 = Some st1 -> Z.of_nat (2 * jitted_len st1) <= Z.of_nat JITTED_LIST_MAX_LENGTH.
Proof.
  intros.
  unfold upd_jitted_list, upd_jitted_list' in H.
  destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond; [| inversion H].
  destruct Mem.storev; [| inversion H].
  inversion H.
  apply Nat.leb_le in Hcond.
  unfold JITTED_LIST_MAX_LENGTH in *.
  change (jitted_len
     {|
       jit_pc := jit_pc st0;
       jit_flag := jit_flag st0;
       jit_regs := jit_regs st0;
       jit_mrs_num := jit_mrs_num st0;
       jit_mrs := jit_mrs st0;
       jit_ins_len := jit_ins_len st0;
       jit_ins := jit_ins st0;
       kv2 := kv2 st0;
       use_IR11 := use_IR11 st0;
       load_store_regs := load_store_regs st0;
       offset := offset st0;
       thumb_len := thumb_len st0;
       thumb := thumb st0;
       jitted_len := S (jitted_len st0);
       jitted_list := jitted_list st0;
       jit_mem := m
     |}) with (S (jitted_len st0)).
  lia.
Qed.

Lemma jit_alu32_thumb_load_jitted_len:
  forall l st0 st1,
    jit_alu32_thumb_load l st0 = Some st1 ->
      (jitted_len st1 = (jitted_len st0) + ((List.length l) * 2))%nat.
Proof.
  induction l; simpl; intros.
  - inversion H.
    subst st0.
    lia.
  - destruct jit_alu32_thumb_upd_load eqn: Hload; inversion H.
    clear H1.
    rename j into stk.
    unfold jit_alu32_thumb_upd_load in Hload.
    unfold jit_alu32_thumb_load_store_template_jit in Hload.
    apply upd_jitted_list_jittted_len_2 in Hload.
    specialize (IHl _ _ H).
    rewrite IHl.
    rewrite <- Hload.
    lia.
Qed.

Lemma build_rs_pc:
  forall l st rs0 rs1 jit_blk ofs0
    (Hrange: ((Z.of_nat (List.length l)) * 4) <= ofs0 + ((Z.of_nat (List.length l)) * 4) <= 1000)
    (Hnodup: NoDup l)
    (Hrs1_eq : build_rs l st rs0 = Some rs1)
    (Heq : rs0 PC = Vptr jit_blk (Ptrofs.repr ofs0)),
      rs1 PC = Vptr jit_blk (Ptrofs.repr (ofs0 + ((Z.of_nat (List.length l)) * 4))).
Proof.
  induction l; simpl; intros.
  - inversion Hrs1_eq.
    subst rs0.
    rewrite Z.add_0_r.
    assumption.
  - apply NoDup_cons_iff in Hnodup.
    destruct eval_reg eqn: Heval_reg; inversion Hrs1_eq.
    clear H0.
    destruct v; inversion Hrs1_eq.
    clear H0.
    destruct Hnodup as (Hnot_in & Hnodup).
    assert (Heq1: Z.of_nat (Datatypes.length l) * 4 <= (ofs0 + 4) + Z.of_nat (Datatypes.length l) * 4 <= 1000) by lia.
    specialize (IHl st _ _ jit_blk (ofs0 + 4) Heq1 Hnodup Hrs1_eq).

    assert (Heq2: (ofs0 + 4 + Z.of_nat (Datatypes.length l) * 4) = 
      ofs0 + Z.pos (Pos.of_succ_nat (Datatypes.length l) * 4)) by lia.
    rewrite <- Heq2; clear Heq2.
    apply IHl.
    rewrite Pregmap.gss.
    rewrite Pregmap.gso.
    2:{ intros HF; inversion HF. }
    rewrite Heq.
    simpl.
    f_equal.
    unfold Ptrofs.add, wsize.
    change (Ptrofs.unsigned (Ptrofs.repr 4)) with 4.
    rewrite Ptrofs.unsigned_repr.
    reflexivity.
    change Ptrofs.max_unsigned with 4294967295; lia.
Qed.

Lemma jit_alu32_thumb_load_st_max:
  forall l st0 st1,
    Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH ->
    jit_alu32_thumb_load l st0 = Some st1 ->
    Z.of_nat (2 * jitted_len st1) <= Z.of_nat JITTED_LIST_MAX_LENGTH.
Proof.
  induction l; simpl; intros.
  - inversion H0.
    subst st0.
    assumption.
  - destruct jit_alu32_thumb_upd_load eqn: Hload; [| inversion H0].
    unfold jit_alu32_thumb_upd_load in Hload.
    unfold jit_alu32_thumb_load_store_template_jit in Hload.
    destruct upd_jitted_list eqn: Hupd; [| inversion Hload].
    apply upd_jitted_list_max2 in Hload.
    specialize (IHl _ _ Hload H0).
    unfold JITTED_LIST_MAX_LENGTH in *.
    lia.
Qed.

Lemma jit_alu32_thumb_upd_load_unchange_jittted_list:
  forall r st0 st1
    (Hone : jit_alu32_thumb_upd_load r st0 = Some st1),
      jitted_list st0 = jitted_list st1.
Proof.
  intros.
  unfold jit_alu32_thumb_upd_load in Hone.
  unfold jit_alu32_thumb_load_store_template_jit in Hone.
  eapply upd_jitted_list_unchange_jittted_list_2; eauto.
Qed.

Lemma jit_alu32_thumb_upd_load_max:
  forall r st0 st1
    (Hone: jit_alu32_thumb_upd_load r st0 = Some st1),
      Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH.
Proof.
  intros.
  unfold jit_alu32_thumb_upd_load in Hone.
  unfold jit_alu32_thumb_load_store_template_jit in Hone.
  destruct upd_jitted_list eqn: Hupd; [| inversion Hone].
  eapply upd_jitted_list_max; eauto.
Qed.

Lemma jit_alu32_thumb_upd_load_sub_mem:
  forall r st0 st1 arm_blk
    (Hptr: jitted_list st0 = Vptr arm_blk Ptrofs.zero)
    (Hmax: Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH)
    (Hone : jit_alu32_thumb_upd_load r st0 = Some st1),
      sub_mem_blk (jit_mem st0) (jit_mem st1) arm_blk 0 (Z.of_nat (2 * jitted_len st0)).
Proof.
  unfold sub_mem_blk; intros.
  unfold jit_alu32_thumb_upd_load in Hone.
  unfold jit_alu32_thumb_load_store_template_jit in Hone.
  eapply upd_jitted_list_load_2; eauto.
  lia.
Qed.

Lemma jit_alu32_thumb_upd_load_jittted_len:
  forall r st0 st1
    (Hone : jit_alu32_thumb_upd_load r st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  intros.
  unfold jit_alu32_thumb_upd_load in Hone.
  unfold jit_alu32_thumb_load_store_template_jit in Hone.
  eapply upd_jitted_list_jittted_len_2 in Hone; eauto; lia.
Qed.

Lemma jit_load_sub_mem:
  forall l st0 st1 arm_blk
    (Hptr: jitted_list st0 = Vptr arm_blk Ptrofs.zero)
    (Hjit : jit_alu32_thumb_load l st0 = Some st1),
      sub_mem_blk (jit_mem st0) (jit_mem st1) arm_blk 0 (Z.of_nat (2 * jitted_len st0)).
Proof.
  induction l; unfold sub_mem_blk; simpl; intros.
  - inversion Hjit; reflexivity.
  - destruct jit_alu32_thumb_upd_load eqn: Hone in Hjit; [| inversion Hjit].
    rename j into stk.
    apply jit_alu32_thumb_upd_load_unchange_jittted_list in Hone as Hptr1.
    symmetry in Hptr1.
    rewrite Hptr in Hptr1.
    specialize (IHl stk st1 arm_blk Hptr1 Hjit).
    apply jit_alu32_thumb_upd_load_max in Hone as Hmax.
    apply jit_alu32_thumb_upd_load_sub_mem with (arm_blk := arm_blk) in Hone as Htmp; auto.

    unfold sub_mem_blk in *.

    rewrite <- IHl.
    + apply Htmp.
      lia.
    + apply jit_alu32_thumb_upd_load_jittted_len in Hone.
      lia.
Qed.

Lemma upd_jitted_list_unchanged_on_store:
  forall ins st0 st1 st b0
  (Hupd: upd_jitted_list ins st0 = Some st1)
    (Harm_blk : jitted_list st0 = Vptr b0 Ptrofs.zero)
    (Hunchanged : Mem.unchanged_on (fun (b : block) (_ : Z) => b <> b0) (jit_mem st0) (jit_mem st)),
  Mem.unchanged_on (fun (b : block) (_ : Z) => b <> b0) (jit_mem st1) (jit_mem st).
Proof.
  unfold upd_jitted_list, upd_jitted_list'; intros.
  destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat; [| inversion Hupd].
  rewrite Harm_blk in Hupd.
  simpl in Hupd.
  destruct Mem.store eqn: Hstore; [| inversion Hupd].
  injection Hupd as Hst1_eq; subst st1.
  eapply store_unchanged_on_1; eauto.
Qed.

Lemma mem_unchanged_on_store:
  forall r st0 st1 st b1
    (Hupd_load : jit_alu32_thumb_upd_load r st0 = Some st1)
    (Harm_blk : jitted_list st0 = Vptr b1 Ptrofs.zero)
    (Hunchanged : Mem.unchanged_on (fun (b : block) (_ : Z) => b <> b1) (jit_mem st0)
               (jit_mem st)),
      Mem.unchanged_on (fun (b : block) (_ : Z) => b <> b1) (jit_mem st1) (jit_mem st).
Proof.
  intros.
  unfold jit_alu32_thumb_upd_load in Hupd_load.
  unfold jit_alu32_thumb_load_store_template_jit in Hupd_load.
  destruct upd_jitted_list eqn: Hupd0; [| inversion Hupd_load].
  rename j into stk.

  apply upd_jitted_list_unchange_jittted_list in Hupd0 as Harm_stk.

  eapply upd_jitted_list_unchanged_on_store in Hupd0; eauto.
  destruct upd_jitted_list eqn: Hupd1; [| inversion Hupd_load].
  rename j into stj.
  injection Hupd_load as Hst1_eq; subst stj.
  eapply upd_jitted_list_unchanged_on_store in Hupd1; eauto.
  rewrite <- Harm_stk.
  assumption.
Qed.

Lemma upd_jitted_list_valid_blk:
  forall v st0 st1 b b0
    (Hupd: upd_jitted_list v st0 = Some st1)
    (Harm_blk : jitted_list st0 = Vptr b Ptrofs.zero)
    (Hvalid_blk : Mem.valid_block (jit_mem st0) b0)
    (Hneq : b <> b0),
      Mem.valid_block (jit_mem st1) b0.
Proof.
  unfold upd_jitted_list, upd_jitted_list'; intros.
  destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat; [| inversion Hupd].
  rewrite Harm_blk in Hupd.
  simpl in Hupd.
  destruct Mem.store eqn: Hstore; [| inversion Hupd].
  injection Hupd as Hst1_eq; subst st1.
  eapply Mem.store_valid_block_1; eauto.
Qed.

Lemma jit_alu32_load_list_tl:
  forall l a l1
    (Hldr : jit_alu32_load_list l = Some (a :: l1)),
      exists lr : list bpf_instruction, jit_alu32_load_list lr = Some l1.
Proof.
  induction l; simpl; intros.
  - inversion Hldr.
  - destruct jit_alu32_load_list eqn: Hload in Hldr; [| inversion Hldr].
    destruct jit_alu32_load_aux_list eqn: Haux in Hldr; [| inversion Hldr].
    apply jit_alu32_load_aux_list_spec in Haux.
    destruct Haux.
    + destruct H as (r0 & H); subst l2.
      injection Hldr as Heq.
      destruct list_in_bool.
      * subst l0.
        specialize (IHl _ _ Hload).
        assumption.
      * inversion Heq.
        subst a0 l1.
        exists l; assumption.
    + destruct H as (r0 & r1 & H & Hneq).
      injection Hldr as Heq.
      subst l2.
      simpl in Heq.
      destruct list_in_bool eqn: Hb0 in Heq.
      * destruct list_in_bool eqn: Hb1 in Heq.
        { subst l0.
          eapply IHl; eauto.
        }
        { inversion Heq.
          subst a0 l1.
          exists l; assumption.
        }
      * destruct list_in_bool eqn: Hb1 in Heq.
        { inversion Heq.
          subst a0 l1.
          exists l; assumption.
        }
        { inversion Heq.
          subst a0 l1.
          exists ((BPF_BINARY A32 BPF_ADD r1 (inl r1)) :: l).
          simpl.
          rewrite Hload.
          rewrite reg_eqb_refl.
          simpl.
          rewrite Hb1.
          reflexivity.
        }
Qed.

Lemma jit_alu32_store_list_tl:
  forall l a l1
    (Hstr : jit_alu32_store_list l = Some (a :: l1)),
      exists lr : list bpf_instruction, jit_alu32_store_list lr = Some l1.
Proof.
  induction l; simpl; intros.
  - inversion Hstr.
  - destruct jit_alu32_store_list eqn: Hstore in Hstr; [| inversion Hstr].
    destruct jit_alu32_store_aux_list eqn: Haux in Hstr; [| inversion Hstr].
    destruct list_in_bool eqn: Hin.
    + injection Hstr as Heq.
      subst l0.
      eapply IHl; eauto.
    + injection Hstr as Hr_eq Hl0_eq.
      subst a0 l1.
      exists l.
      assumption.
Qed.

Lemma upd_jitted_list_load_other:
  forall i st0 st1 ofs chunk arm_blk regs_blk,
    jitted_list st0 = Vptr arm_blk Ptrofs.zero ->
    arm_blk <> regs_blk ->
    upd_jitted_list i st0 = Some st1 ->
      Mem.load chunk (jit_mem st0) regs_blk ofs =
      Mem.load chunk (jit_mem st1) regs_blk ofs.
Proof.
  unfold upd_jitted_list, upd_jitted_list'; intros.
  destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat; [| inversion H1].
  rewrite H in H1.
  simpl in H1.
  destruct Mem.store eqn: Hstore; [| inversion H1].
  symmetry.
  inversion H1.
  eapply Mem.load_store_other; eauto.
Qed.

Lemma Hreg_mul4_unsigned:
  forall r,
    Ptrofs.unsigned (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4))) = (Z_of_ireg r) * 4.
Proof.
  intros.
  unfold Ptrofs.of_intu, Ptrofs.of_int, Int.mul.
  change (Int.unsigned (Int.repr 4)) with 4.
  unfold int_of_ireg, Z_of_ireg.
  rewrite Ptrofs.unsigned_repr.
  - rewrite Int.unsigned_repr.
    + destruct r; simpl; auto.
    + change Int.max_unsigned with 4294967295.
      rewrite Int.unsigned_repr.
      * destruct r; lia.
      * change Int.max_unsigned with 4294967295.
        destruct r; lia.
  - rewrite Int.unsigned_repr.
    + rewrite Int.unsigned_repr.
      * change Ptrofs.max_unsigned with 4294967295.
        destruct r; lia.
      * change Int.max_unsigned with 4294967295.
        destruct r; lia.
    + rewrite Int.unsigned_repr.
      * change Int.max_unsigned with 4294967295.
        destruct r; lia.
      * change Int.max_unsigned with 4294967295.
        destruct r; lia.
Qed.

Lemma Hreg_mul8_unsigned:
  forall r,
    Ptrofs.unsigned (Ptrofs.of_intu (Int.mul (int_of_reg r) (Int.repr 8))) = 8 * id_of_reg r.
Proof.
  intros.
  unfold Ptrofs.of_intu, Ptrofs.of_int, Int.mul.
  change (Int.unsigned (Int.repr 8)) with 8.
  unfold int_of_reg, id_of_reg.
  rewrite Ptrofs.unsigned_repr.
  - rewrite Int.unsigned_repr.
    + destruct r; simpl; auto.
    + change Int.max_unsigned with 4294967295.
      rewrite Int.unsigned_repr.
      * destruct r; lia.
      * change Int.max_unsigned with 4294967295.
        destruct r; lia.
  - rewrite Int.unsigned_repr.
    + rewrite Int.unsigned_repr.
      * change Ptrofs.max_unsigned with 4294967295.
        destruct r; lia.
      * change Int.max_unsigned with 4294967295.
        destruct r; lia.
    + rewrite Int.unsigned_repr.
      * change Int.max_unsigned with 4294967295.
        destruct r; lia.
      * change Int.max_unsigned with 4294967295.
        destruct r; lia.
Qed.

Lemma Hreg_unsigned:
  forall r,
    Ptrofs.unsigned (Ptrofs.of_intu (int_of_ireg r)) = Z_of_ireg r.
Proof.
  intros.
  unfold Ptrofs.of_intu, Ptrofs.of_int.
  unfold int_of_ireg, Z_of_ireg.
  rewrite Ptrofs.unsigned_repr.
  - rewrite Int.unsigned_repr.
    + reflexivity.
    + change Int.max_unsigned with 4294967295.
      destruct r; lia.
  - rewrite Int.unsigned_repr.
    + change Ptrofs.max_unsigned with 4294967295.
      destruct r; lia.
    + change Int.max_unsigned with 4294967295.
      destruct r; lia.
Qed.

Lemma jit_alu32_thumb_upd_save_jitted_len_leb:
  forall r st0 st1
    (Hjit_spilling : jit_alu32_thumb_upd_save r st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit.
  intros.
  eapply upd_jitted_list_jittted_len_2 in Hjit_spilling; eauto.
  lia.
Qed.

Lemma jit_alu32_thumb_save_jitted_len_leb:
  forall l st0 st1
    (Hjit_spilling : jit_alu32_thumb_save l st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  induction l; simpl; intros.
  - injection Hjit_spilling as Heq; rewrite Heq; lia.
  - destruct jit_alu32_thumb_upd_save eqn: Hone; [| inversion Hjit_spilling].
    specialize (IHl _ _ Hjit_spilling).
    rename j into stk.
    eapply jit_alu32_thumb_upd_save_jitted_len_leb in Hone; eauto.
    lia.
Qed.

Lemma jit_alu32_thumb_save_load_same:
  forall l st0 st1 jit_blk chunk ofs
    (Hjit_spilling : jit_alu32_thumb_save l st0 = Some st1)
    (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
    (Hrange : 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len st0)),
    Mem.load chunk (jit_mem st0) jit_blk ofs = Mem.load chunk (jit_mem st1) jit_blk ofs.
Proof.
  induction l; simpl; intros.
  - injection Hjit_spilling as Heq; rewrite Heq; reflexivity.
  - destruct jit_alu32_thumb_upd_save eqn: Hone; [| inversion Hjit_spilling].
    specialize (IHl _ _ jit_blk chunk ofs Hjit_spilling).
    rename j into stk.

    assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
      unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone.
      eapply upd_jitted_list_unchange_jittted_list_2 in Hone; eauto.
      rewrite <- Hone; assumption.
    }
    specialize (IHl Heq); clear Heq.

    assert (Heq: 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len stk)). {
      eapply jit_alu32_thumb_upd_save_jitted_len_leb in Hone; eauto.
      lia.
    }
    specialize (IHl Heq); clear Heq.
    rewrite <- IHl.
    unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone.
    eapply upd_jitted_list_load_2 in Hone; eauto.
    destruct upd_jitted_list eqn: Hupd; [| inversion Hone].
    eapply upd_jitted_list_max in Hupd; eauto.
    lia.
Qed.

Lemma jit_alu32_thumb_upd_load_jitted_len_leb:
  forall r st0 st1
    (Hjit_load : jit_alu32_thumb_upd_load r st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  unfold jit_alu32_thumb_upd_load, jit_alu32_thumb_load_store_template_jit.
  intros.
  eapply upd_jitted_list_jittted_len_2 in Hjit_load; eauto.
  lia.
Qed.

Lemma jit_alu32_thumb_load_jitted_len_leb:
  forall l st0 st1
    (Hjit_load : jit_alu32_thumb_load l st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  induction l; simpl; intros.
  - injection Hjit_load as Heq; rewrite Heq; lia.
  - destruct jit_alu32_thumb_upd_load eqn: Hone; [| inversion Hjit_load].
    specialize (IHl _ _ Hjit_load).
    rename j into stk.
    eapply jit_alu32_thumb_upd_load_jitted_len_leb in Hone; eauto.
    lia.
Qed.

Lemma jit_alu32_thumb_load_load_same:
  forall l st0 st1 jit_blk chunk ofs
    (Hjit_load : jit_alu32_thumb_load l st0 = Some st1)
    (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
    (Hrange : 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len st0)),
    Mem.load chunk (jit_mem st0) jit_blk ofs = Mem.load chunk (jit_mem st1) jit_blk ofs.
Proof.
  induction l; simpl; intros.
  - injection Hjit_load as Heq; rewrite Heq; reflexivity.
  - destruct jit_alu32_thumb_upd_load eqn: Hone; [| inversion Hjit_load].
    specialize (IHl _ _ jit_blk chunk ofs Hjit_load).
    rename j into stk.

    assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
      unfold jit_alu32_thumb_upd_load, jit_alu32_thumb_load_store_template_jit in Hone.
      eapply upd_jitted_list_unchange_jittted_list_2 in Hone; eauto.
      rewrite <- Hone; assumption.
    }
    specialize (IHl Heq); clear Heq.

    assert (Heq: 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len stk)). {
      eapply jit_alu32_thumb_upd_load_jitted_len_leb in Hone; eauto.
      lia.
    }
    specialize (IHl Heq); clear Heq.
    rewrite <- IHl.
    unfold jit_alu32_thumb_upd_load, jit_alu32_thumb_load_store_template_jit in Hone.
    eapply upd_jitted_list_load_2 in Hone; eauto.
    destruct upd_jitted_list eqn: Hupd; [| inversion Hone].
    eapply upd_jitted_list_max in Hupd; eauto.
    lia.
Qed.

Lemma upd_jitted_list_unsigned_repr:
  forall v st0 st1
    (Hupd: upd_jitted_list v st0 = Some st1),
Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (2 * jitted_len st0))) = (Z.of_nat (2 * jitted_len st0)).
Proof.
  intros.
  eapply upd_jitted_list_max in Hupd; eauto.
  unfold JITTED_LIST_MAX_LENGTH in Hupd.
  rewrite Ptrofs.unsigned_repr.
  - f_equal.
  - change Ptrofs.max_unsigned with 4294967295.
    lia.
Qed.

Lemma upd_jitted_list_unsigned_repr_int:
  forall v st0 st1
    (Hupd: upd_jitted_list v st0 = Some st1),
Int.unsigned (Int.repr (Z.of_nat (2 * jitted_len st0))) = (Z.of_nat (2 * jitted_len st0)).
Proof.
  intros.
  eapply upd_jitted_list_max in Hupd; eauto.
  unfold JITTED_LIST_MAX_LENGTH in Hupd.
  rewrite Int.unsigned_repr.
  - f_equal.
  - change Int.max_unsigned with 4294967295.
    lia.
Qed.

Lemma upd_jitted_list_unsigned_repr_add_2:
  forall v st0 st1
    (Hupd: upd_jitted_list v st0 = Some st1),
Ptrofs.unsigned
  (Ptrofs.repr
     (Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (2 * jitted_len st0))) +
      Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2)))) = (Z.of_nat (2 * jitted_len st0)) + 2.
Proof.
  change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2))) with 2.
  intros.
  erewrite upd_jitted_list_unsigned_repr; eauto.
  eapply upd_jitted_list_max in Hupd; eauto.
  unfold JITTED_LIST_MAX_LENGTH in Hupd.
  rewrite Ptrofs.unsigned_repr.
  - f_equal.
  - change Ptrofs.max_unsigned with 4294967295.
    lia.
Qed.

Lemma reg_mul_8_eq:
  forall r,
  (Int.mul (Int.repr (Z.of_nat (reg2nat r))) (Int.repr 8)) = (Int.repr (8 * id_of_reg r)).
Proof.
  unfold Int.mul; intros.
  f_equal.
  change (Int.unsigned (Int.repr 8)) with 8.
  rewrite Int.unsigned_repr.
  - unfold reg2nat, id_of_reg; destruct r; lia.
  - unfold reg2nat.
    change Int.max_unsigned with 4294967295.
    destruct r; lia.
Qed.


Lemma upd_jitted_list_unchange_jit_regs:
  forall v st0 st1
    (Hupd: upd_jitted_list v st0 = Some st1),
       jit_regs st0  = jit_regs st1.
Proof.
  unfold upd_jitted_list; intros.
  destruct upd_jitted_list'; inversion Hupd; simpl.
  reflexivity.
Qed.

Lemma upd_jitted_list_unchange_jit_regs_2:
  forall i j st0 st1
    (Hupd: match upd_jitted_list i st0 with
     | Some ins_st0 => upd_jitted_list j ins_st0
     | None => None
     end = Some st1),
       jit_regs st0  = jit_regs st1.
Proof.
  intros.
  destruct upd_jitted_list eqn: Hupd0; [| inversion Hupd].
  rename j0 into stk.
  eapply upd_jitted_list_unchange_jit_regs in Hupd0.
  eapply upd_jitted_list_unchange_jit_regs in Hupd.
  rewrite Hupd0.
  rewrite <- Hupd.
  reflexivity.
Qed.

Lemma upd_jitted_list_unchange_eval_jit_reg:
  forall r v jit_blk regs_blk st0 st1
    (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
    (Hreg_blk : jit_regs st0 = Vptr regs_blk Ptrofs.zero)
    (Hblk_neq : regs_blk <> jit_blk)
    (Hupd: upd_jitted_list v st0 = Some st1),
       eval_jit_reg r st0  = eval_jit_reg r st1.
Proof.
  intros.
  assert (Heq: jit_regs st0 = jit_regs st1) by (eapply upd_jitted_list_unchange_jit_regs; eauto).
  unfold upd_jitted_list, upd_jitted_list' in Hupd.
  unfold eval_jit_reg.

  destruct (2 * jitted_len _ + 4 <=? JITTED_LIST_MAX_LENGTH)%nat; [| inversion Hupd].
  rewrite Harm_blk in Hupd.
  rewrite Hreg_blk in *.
  unfold Mem.storev, Val.add, Archi.ptr64 in Hupd.
  rewrite <- Heq.
  unfold Mem.loadv, Val.add, Archi.ptr64.

  destruct Mem.store eqn: Hstore; [| inversion Hupd].
  injection Hupd as Hst_eq.
  clear - Hblk_neq Hstore Hst_eq.
  subst st1.
  symmetry.
  eapply Mem.load_store_other; eauto.
Qed.

Lemma upd_jitted_list_unchange_eval_jit_reg_2:
  forall r i j jit_blk regs_blk st0 st1
    (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
    (Hreg_blk : jit_regs st0 = Vptr regs_blk Ptrofs.zero)
    (Hblk_neq : regs_blk <> jit_blk)
    (Hupd: match upd_jitted_list i st0 with
     | Some ins_st0 => upd_jitted_list j ins_st0
     | None => None
     end = Some st1),
      eval_jit_reg r st0  = eval_jit_reg r st1.
Proof.
  intros.
  destruct upd_jitted_list eqn: Hupd0;[| inversion Hupd].

  eapply upd_jitted_list_unchange_jittted_list in Hupd0 as Heq.
  eapply upd_jitted_list_unchange_jit_regs in Hupd0 as Heq1.

  eapply upd_jitted_list_unchange_eval_jit_reg with (r := r) in Hupd0; eauto.
  rename j0 into stk.
  rewrite Hupd0.

  eapply upd_jitted_list_unchange_eval_jit_reg with (r := r) in Hupd; eauto.
  - rewrite <- Heq; auto.
  - rewrite <- Heq1; auto.
Qed.

Lemma jit_alu32_thumb_save_unchange_eval_jit_reg:
  forall l r st0 st1 jit_blk regs_blk
    (Hjit_spilling : jit_alu32_thumb_save l st0 = Some st1)
    (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
    (Hreg_blk : jit_regs st0 = Vptr regs_blk Ptrofs.zero)
    (Hblk_neq : regs_blk <> jit_blk),
      eval_jit_reg r st0  = eval_jit_reg r st1.
Proof.
  induction l; simpl; intros.
  - injection Hjit_spilling as Heq; rewrite Heq; reflexivity.
  - destruct jit_alu32_thumb_upd_save eqn: Hone; [| inversion Hjit_spilling].
    rename j into stk.
    specialize (IHl r stk st1 jit_blk regs_blk Hjit_spilling).

    assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
      unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone.
      eapply upd_jitted_list_unchange_jittted_list_2 in Hone; eauto.
      rewrite <- Hone; assumption.
    }
    specialize (IHl Heq); clear Heq.

    assert (Heq: jit_regs stk = Vptr regs_blk Ptrofs.zero). {
      unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone.
      eapply upd_jitted_list_unchange_jit_regs_2 in Hone; eauto.
      rewrite <- Hone.
      assumption.
    }
    specialize (IHl Heq Hblk_neq); clear Heq.

    rewrite <- IHl.
    unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone.
    eapply upd_jitted_list_unchange_eval_jit_reg_2 in Hone; eauto.
Qed.

Lemma bpf_alu32_to_thumb_unchange_jit_regs:
  forall ins st0 st1
    (Hone: bpf_alu32_to_thumb ins st0 = Some st1),
      jit_regs st0 = jit_regs st1.
Proof.
  unfold bpf_alu32_to_thumb; intros.
  destruct ins in Hone; inversion Hone.
  clear - H0.
  destruct a; [| inversion H0].
  destruct s.
  - unfold bpf_alu32_to_thumb_reg in H0.
    destruct b; inversion H0.
    10:{ eapply upd_jitted_list_unchange_jit_regs; eauto. }
    1:{ eapply upd_jitted_list_unchange_jit_regs; eauto. }
    all: eapply upd_jitted_list_unchange_jit_regs_2; eauto.
  - unfold bpf_alu32_to_thumb_imm in H0.
    destruct (Int.cmp Cle Int.zero i && Int.cmp Cle i (Int.repr 255)).
    + unfold bpf_alu32_to_thumb_imm0 in H0.
      destruct b; inversion H0.
      all: eapply upd_jitted_list_unchange_jit_regs_2; eauto.
    + unfold mov_int_to_movw in H0.
      destruct (match
         upd_jitted_list _ st0
       with
       | Some movw_st0 =>
           upd_jitted_list _ movw_st0
       | None => None
       end) eqn: Hupd;[| inversion H0].
      rename j into stk.
      eapply upd_jitted_list_unchange_jit_regs_2 in Hupd; eauto.
      rewrite Hupd.
      destruct Int.eq.
      * unfold bpf_alu32_to_thumb_reg in H0.
        destruct b; inversion H0.
        10:{ eapply upd_jitted_list_unchange_jit_regs; eauto. }
        1:{ eapply upd_jitted_list_unchange_jit_regs; eauto. }
        all: eapply upd_jitted_list_unchange_jit_regs_2; eauto.
      * unfold mov_int_to_movt in H0.
        destruct (match
           upd_jitted_list _ stk
         with
         | Some movw_st0 =>
             upd_jitted_list _ movw_st0
         | None => None
         end) eqn: Hupd0;[| inversion H0].
        rename j into stj.
        eapply upd_jitted_list_unchange_jit_regs_2 in Hupd0; eauto.
        rewrite Hupd0.
        clear Hupd Hupd0.

        unfold bpf_alu32_to_thumb_reg in H0.
        destruct b; inversion H0.
        10:{ eapply upd_jitted_list_unchange_jit_regs; eauto. }
        1:{ eapply upd_jitted_list_unchange_jit_regs; eauto. }
        all: eapply upd_jitted_list_unchange_jit_regs_2; eauto.
Qed.

Lemma jit_alu32_thumb_upd_store_jitted_len_leb:
  forall r st0 st1
    (Hjit_store : jit_alu32_thumb_upd_store r st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit.
  intros.
  eapply upd_jitted_list_jittted_len_2 in Hjit_store; eauto.
  lia.
Qed.

Lemma jit_alu32_thumb_store_jitted_len_leb:
  forall l st0 st1
    (Hjit_store : jit_alu32_thumb_store l st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  induction l; simpl; intros.
  - injection Hjit_store as Heq; rewrite Heq; lia.
  - destruct jit_alu32_thumb_upd_store eqn: Hone; [| inversion Hjit_store].
    specialize (IHl _ _ Hjit_store).
    rename j into stk.
    eapply jit_alu32_thumb_upd_store_jitted_len_leb in Hone; eauto.
    lia.
Qed.

Lemma jit_alu32_thumb_store_load_same:
  forall l st0 st1 jit_blk chunk ofs
    (Hjit_store : jit_alu32_thumb_store l st0 = Some st1)
    (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
    (Hrange : 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len st0)),
    Mem.load chunk (jit_mem st0) jit_blk ofs = Mem.load chunk (jit_mem st1) jit_blk ofs.
Proof.
  induction l; simpl; intros.
  - injection Hjit_store as Heq; rewrite Heq; reflexivity.
  - destruct jit_alu32_thumb_upd_store eqn: Hone; [| inversion Hjit_store].
    specialize (IHl _ _ jit_blk chunk ofs Hjit_store).
    rename j into stk.

    assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
      unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit in Hone.
      eapply upd_jitted_list_unchange_jittted_list_2 in Hone; eauto.
      rewrite <- Hone; assumption.
    }
    specialize (IHl Heq); clear Heq.

    assert (Heq: 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len stk)). {
      eapply jit_alu32_thumb_upd_store_jitted_len_leb in Hone; eauto.
      lia.
    }
    specialize (IHl Heq); clear Heq.
    rewrite <- IHl.
    unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit in Hone.
    eapply upd_jitted_list_load_2 in Hone; eauto.
    destruct upd_jitted_list eqn: Hupd; [| inversion Hone].
    eapply upd_jitted_list_max in Hupd; eauto.
    lia.
Qed.

Lemma jit_alu32_thumb_upd_reset_jitted_len_leb:
  forall r st0 st1
    (Hjit_reset : jit_alu32_thumb_upd_reset r st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  unfold jit_alu32_thumb_upd_reset, jit_alu32_thumb_load_store_template_jit.
  intros.
  eapply upd_jitted_list_jittted_len_2 in Hjit_reset; eauto.
  lia.
Qed.

Lemma jit_alu32_thumb_reset_jitted_len_leb:
  forall l st0 st1
    (Hjit_reset : jit_alu32_thumb_reset l st0 = Some st1),
      (jitted_len st0 <= jitted_len st1)%nat.
Proof.
  induction l; simpl; intros.
  - injection Hjit_reset as Heq; rewrite Heq; lia.
  - destruct jit_alu32_thumb_upd_reset eqn: Hone; [| inversion Hjit_reset].
    specialize (IHl _ _ Hjit_reset).
    rename j into stk.
    eapply jit_alu32_thumb_upd_reset_jitted_len_leb in Hone; eauto.
    lia.
Qed.

Lemma jit_alu32_thumb_reset_load_same:
  forall l st0 st1 jit_blk chunk ofs
    (Hjit_reset : jit_alu32_thumb_reset l st0 = Some st1)
    (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
    (Hrange : 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len st0)),
    Mem.load chunk (jit_mem st0) jit_blk ofs = Mem.load chunk (jit_mem st1) jit_blk ofs.
Proof.
  induction l; simpl; intros.
  - injection Hjit_reset as Heq; rewrite Heq; reflexivity.
  - destruct jit_alu32_thumb_upd_reset eqn: Hone; [| inversion Hjit_reset].
    specialize (IHl _ _ jit_blk chunk ofs Hjit_reset).
    rename j into stk.

    assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
      unfold jit_alu32_thumb_upd_reset, jit_alu32_thumb_load_store_template_jit in Hone.
      eapply upd_jitted_list_unchange_jittted_list_2 in Hone; eauto.
      rewrite <- Hone; assumption.
    }
    specialize (IHl Heq); clear Heq.

    assert (Heq: 0 <= ofs /\ ofs + size_chunk chunk <= Z.of_nat (2 * jitted_len stk)). {
      eapply jit_alu32_thumb_upd_reset_jitted_len_leb in Hone; eauto.
      lia.
    }
    specialize (IHl Heq); clear Heq.
    rewrite <- IHl.
    unfold jit_alu32_thumb_upd_reset, jit_alu32_thumb_load_store_template_jit in Hone.
    eapply upd_jitted_list_load_2 in Hone; eauto.
    destruct upd_jitted_list eqn: Hupd; [| inversion Hone].
    eapply upd_jitted_list_max in Hupd; eauto.
    lia.
Qed.

(**r the JIT proof has some admitted lemmas that is time comsuming, TODO... *)
Lemma upd_jitted_list_2_load:
  forall vi vj st0 st1 jit_blk
    (Hupd : match upd_jitted_list vi st0 with
            | Some str_st =>
                upd_jitted_list vj
                  str_st
            | None => None
            end = Some st1)
    (Hjit_inv : jitted_list st0 = Vptr jit_blk Ptrofs.zero),
      Mem.load Mint16unsigned (jit_mem st1) jit_blk
        (Ptrofs.unsigned (Ptrofs.of_int (Int.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))))) =
         Some (Val.load_result Mint16unsigned (Vint vi)).
Proof.
  intros.
  destruct upd_jitted_list eqn: Hupd2; [| inversion Hupd].
  rename j into stk.
  assert (Hinv: jitted_list stk = Vptr jit_blk Ptrofs.zero).
  - rewrite <- Hjit_inv.
    symmetry.
    erewrite upd_jitted_list_unchange_jittted_list; eauto.
  - eapply upd_jitted_list_load in Hupd2 as Heq; eauto.
    admit.
Admitted.


Lemma upd_jitted_list_2_load_2:
  forall vi vj st0 st1 jit_blk
    (Hupd : match upd_jitted_list vi st0 with
            | Some str_st =>
                upd_jitted_list vj
                  str_st
            | None => None
            end = Some st1)
    (Hjit_inv : jitted_list st0 = Vptr jit_blk Ptrofs.zero),
      Mem.load Mint16unsigned (jit_mem st1) jit_blk
        (Ptrofs.unsigned (Ptrofs.add (Ptrofs.repr
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.of_int (Int.repr 2)))) =
         Some (Val.load_result Mint16unsigned (Vint vj)).
Proof.
Admitted.

Lemma jit_alu32_jit_blk_memory_layout:
  forall l l_load l_store st st_pre st_save st_load st_core st_store st_reset jit_st0 m0 sp_blk jit_blk
  (Hload_list : jit_alu32_load_list l = Some l_load)
  (Hstore_list : jit_alu32_store_list l = Some l_store)
  (Hjit_pre : jit_alu32_pre st = Some st_pre)
  (Hjit_save : jit_alu32_thumb_save (jit_alu32_stack_list l_load l_store st_pre) st_pre = Some st_save)
  (Hjit_load : jit_alu32_thumb_load l_load st_save = Some st_load)
  (Hjit_core : jit_core l st_load = Some st_core)
  (Hjit_store : jit_alu32_thumb_store l_store st_core = Some st_store)
  (Hjit_reset : jit_alu32_thumb_reset (jit_alu32_stack_list l_load l_store st_pre) st_store = Some st_reset)
  (Hjit_post : jit_alu32_post st_reset = Some jit_st0)
  (Hmem: (m0, sp_blk) = Mem.alloc (jit_mem jit_st0) 0 48),
    sub_mem_blk (jit_mem st_pre)    m0          jit_blk (Z.of_nat (2 * jitted_len st))       (Z.of_nat (2 * jitted_len st_pre)) /\
    sub_mem_blk (jit_mem st_save)   m0          jit_blk (Z.of_nat (2 * jitted_len st_pre))   (Z.of_nat (2 * jitted_len st_save)).
  Proof.
  Admitted.

Definition arm_registers_pre (rs0 rs1: Asm.regset) (st_blk: block): Prop :=
  rs1 = nextinstr_nf false rs0 # IR12 <- (Vptr st_blk Ptrofs.zero).

Definition arm_registers_spilling_one (r: ireg) (rs0: Asm.regset) (m0: mem): option mem :=
  Mem.storev Mint32 m0
      (Val.offset_ptr (rs0 IR13) (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4)))) (rs0 r).

Fixpoint arm_registers_spilling_aux (l: list ireg) (rs0: Asm.regset) (m0: mem): option (Asm.regset * mem) :=
  match l with
  | [] => Some (rs0, m0)
  | hd :: tl =>
    match arm_registers_spilling_one hd rs0 m0 with
    | Some m1 => arm_registers_spilling_aux tl (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) ) m1
    | None => None
    end
  end.

Definition arm_registers_spilling (l: list ireg) (rs0 rs1: Asm.regset) (m0 m1: mem): Prop :=
  match arm_registers_spilling_aux l rs0 m0 with
  | Some (rs, m) => rs = rs1 /\ m = m1
  | None => False
  end.

Definition arm_registers_store_one (r: reg) (rs0: Asm.regset) (m0: mem): option mem :=
  Mem.storev Mint32 m0
      (Val.offset_ptr (rs0 IR12) (Ptrofs.of_intu (Int.repr ((id_of_reg r + 1) * 8)))) (rs0 (ireg_of_reg r)).

Fixpoint arm_registers_store_aux (l: list reg) (rs0: Asm.regset) (m0: mem): option (Asm.regset * mem) :=
  match l with
  | [] => Some (rs0, m0)
  | hd :: tl =>
    match arm_registers_store_one hd rs0 m0 with
    | Some m1 => arm_registers_store_aux tl (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)) m1
    | None => None
    end
  end.

Definition arm_registers_store (l: list reg) (rs0 rs1: Asm.regset) (m0 m1: mem): Prop :=
  match arm_registers_store_aux l rs0 m0 with
  | Some (rs, m) => rs = rs1 /\ m = m1
  | None => False
  end.


Lemma jit_alu32_jit_blk_memory_layout_1:
  forall l l_load l_store st st_pre st_save st_load st_core st_store st_reset jit_st0 m0 sp_blk jit_blk rs0 st_blk
  (Hload_list : jit_alu32_load_list l = Some l_load)
  (Hstore_list : jit_alu32_store_list l = Some l_store)
  (Hjit_pre : jit_alu32_pre st = Some st_pre)
  (Hjit_save : jit_alu32_thumb_save (jit_alu32_stack_list l_load l_store st_pre) st_pre = Some st_save)
  (Hjit_load : jit_alu32_thumb_load l_load st_save = Some st_load)
  (Hjit_core : jit_core l st_load = Some st_core)
  (Hjit_store : jit_alu32_thumb_store l_store st_core = Some st_store)
  (Hjit_reset : jit_alu32_thumb_reset (jit_alu32_stack_list l_load l_store st_pre) st_store = Some st_reset)
  (Hjit_post : jit_alu32_post st_reset = Some jit_st0)
  (Hmem: (m0, sp_blk) = Mem.alloc (jit_mem jit_st0) 0 48)
  (m1_spilling : mem) rs1_pre rs1_spilling
  (Hrs_const_pre : arm_registers_pre rs0 rs1_pre st_blk)
  (Hmem_pre: arm_registers_spilling (jit_alu32_stack_list l_load l_store st_pre) rs1_pre
                     rs1_spilling m0 m1_spilling),
    sub_mem_blk (jit_mem st_load)   m1_spilling jit_blk (Z.of_nat (2 * jitted_len st_save))  (Z.of_nat (2 * jitted_len st_load)) /\
    sub_mem_blk (jit_mem st_core)   m1_spilling jit_blk (Z.of_nat (2 * jitted_len st_load))  (Z.of_nat (2 * jitted_len st_core)) /\
    sub_mem_blk (jit_mem st_store)  m1_spilling jit_blk (Z.of_nat (2 * jitted_len st_core))  (Z.of_nat (2 * jitted_len st_store)).
  Proof.
  Admitted.

Lemma jit_alu32_jit_blk_memory_layout_2:
  forall l l_load l_store st st_pre st_save st_load st_core st_store st_reset jit_st0 m0 sp_blk jit_blk rs0 st_blk
  (Hload_list : jit_alu32_load_list l = Some l_load)
  (Hstore_list : jit_alu32_store_list l = Some l_store)
  (Hjit_pre : jit_alu32_pre st = Some st_pre)
  (Hjit_save : jit_alu32_thumb_save (jit_alu32_stack_list l_load l_store st_pre) st_pre = Some st_save)
  (Hjit_load : jit_alu32_thumb_load l_load st_save = Some st_load)
  (Hjit_core : jit_core l st_load = Some st_core)
  (Hjit_store : jit_alu32_thumb_store l_store st_core = Some st_store)
  (Hjit_reset : jit_alu32_thumb_reset (jit_alu32_stack_list l_load l_store st_pre) st_store = Some st_reset)
  (Hjit_post : jit_alu32_post st_reset = Some jit_st0)
  (Hmem: (m0, sp_blk) = Mem.alloc (jit_mem jit_st0) 0 48)
  (m1_spilling : mem) rs1_pre rs1_spilling rs1_core rs1_store m_store
  (Hrs_const_pre : arm_registers_pre rs0 rs1_pre st_blk)
  (Hmem_pre: arm_registers_spilling (jit_alu32_stack_list l_load l_store st_pre) rs1_pre
                     rs1_spilling m0 m1_spilling)
  (Hmem_store : arm_registers_store l_store rs1_core rs1_store m1_spilling m_store),
    sub_mem_blk (jit_mem st_reset)  m_store     jit_blk (Z.of_nat (2 * jitted_len st_store)) (Z.of_nat (2 * jitted_len st_reset)) /\
    sub_mem_blk (jit_mem jit_st0)   m_store     jit_blk (Z.of_nat (2 * jitted_len st_reset)) (Z.of_nat (2 * jitted_len jit_st0)).
  Proof.
  Admitted.



Lemma jit_alu32_jit_blk_unchange_jitted_list:
  forall l l_load l_store st st_pre st_save st_load st_core st_store st_reset jit_st0 m0 sp_blk jit_blk
  (Hload_list : jit_alu32_load_list l = Some l_load)
  (Hstore_list : jit_alu32_store_list l = Some l_store)
  (Hjit_pre : jit_alu32_pre st = Some st_pre)
  (Hjit_save : jit_alu32_thumb_save (jit_alu32_stack_list l_load l_store st_pre) st_pre = Some st_save)
  (Hjit_load : jit_alu32_thumb_load l_load st_save = Some st_load)
  (Hjit_core : jit_core l st_load = Some st_core)
  (Hjit_store : jit_alu32_thumb_store l_store st_core = Some st_store)
  (Hjit_reset : jit_alu32_thumb_reset (jit_alu32_stack_list l_load l_store st_pre) st_store = Some st_reset)
  (Hjit_post : jit_alu32_post st_reset = Some jit_st0)
  (Hmem: (m0, sp_blk) = Mem.alloc (jit_mem jit_st0) 0 48)
  (Hjit_list: jitted_list st = Vptr jit_blk Ptrofs.zero),
    jitted_list st_pre = Vptr jit_blk Ptrofs.zero /\
    jitted_list st_save = Vptr jit_blk Ptrofs.zero /\
    jitted_list st_load = Vptr jit_blk Ptrofs.zero /\
    jitted_list st_core = Vptr jit_blk Ptrofs.zero /\
    jitted_list st_store = Vptr jit_blk Ptrofs.zero /\
    jitted_list st_reset = Vptr jit_blk Ptrofs.zero /\
    jitted_list jit_st0 = Vptr jit_blk Ptrofs.zero.
  Proof.
  Admitted.


Lemma jit_alu32_jit_spilling_load_registers:
  forall l l_load l_store st st_pre st_save st_load st_core st_store st_reset jit_st0 m0 sp_blk jit_blk
    r m1_spilling rs1_pre rs1_spilling st_blk
  (Hload_list : jit_alu32_load_list l = Some l_load)
  (Hstore_list : jit_alu32_store_list l = Some l_store)
  (Hjit_pre : jit_alu32_pre st = Some st_pre)
  (Hjit_save : jit_alu32_thumb_save (jit_alu32_stack_list l_load l_store st_pre) st_pre = Some st_save)
  (Hjit_load : jit_alu32_thumb_load l_load st_save = Some st_load)
  (Hjit_core : jit_core l st_load = Some st_core)
  (Hjit_store : jit_alu32_thumb_store l_store st_core = Some st_store)
  (Hjit_reset : jit_alu32_thumb_reset (jit_alu32_stack_list l_load l_store st_pre) st_store = Some st_reset)
  (Hjit_post : jit_alu32_post st_reset = Some jit_st0)
  (Hmem: (m0, sp_blk) = Mem.alloc (jit_mem jit_st0) 0 48)
  (Hjit_list: jitted_list st = Vptr jit_blk Ptrofs.zero)
  (Hrs_const_spilling : arm_registers_spilling (jit_alu32_stack_list l_load l_store st_pre) rs1_pre
                     rs1_spilling m0 m1_spilling),
    Mem.load Mint32 m1_spilling st_blk ((id_of_reg r + 1) * 8) =
    Mem.load Mint32 (jit_mem jit_st0) st_blk (8 * id_of_reg r + 8).
  Proof.
  Admitted.


Lemma jit_alu32_jit_spilling_callee_save:
  forall l l_load l_store st st_pre st_save st_load st_core st_store st_reset jit_st0 m0 sp_blk jit_blk
    m1_spilling rs1_pre rs1_spilling
  (Hload_list : jit_alu32_load_list l = Some l_load)
  (Hstore_list : jit_alu32_store_list l = Some l_store)
  (Hjit_pre : jit_alu32_pre st = Some st_pre)
  (Hjit_save : jit_alu32_thumb_save (jit_alu32_stack_list l_load l_store st_pre) st_pre = Some st_save)
  (Hjit_load : jit_alu32_thumb_load l_load st_save = Some st_load)
  (Hjit_core : jit_core l st_load = Some st_core)
  (Hjit_store : jit_alu32_thumb_store l_store st_core = Some st_store)
  (Hjit_reset : jit_alu32_thumb_reset (jit_alu32_stack_list l_load l_store st_pre) st_store = Some st_reset)
  (Hjit_post : jit_alu32_post st_reset = Some jit_st0)
  (Hmem: (m0, sp_blk) = Mem.alloc (jit_mem jit_st0) 0 48)
  (Hjit_list: jitted_list st = Vptr jit_blk Ptrofs.zero)
  (Hrs_const_spilling : arm_registers_spilling (jit_alu32_stack_list l_load l_store st_pre) rs1_pre
                     rs1_spilling m0 m1_spilling),
    (forall r0 : ireg,
    ~ In r0 (jit_alu32_stack_list l_load l_store st_pre) /\ In r0 arm_callee_save_regs ->
    forall r1 : reg, In r1 l_load -> ireg_of_reg r1 <> r0) /\
    (forall r0 : ireg,
    ~ In r0 (jit_alu32_stack_list l_load l_store st_pre) /\ In r0 arm_callee_save_regs ->
    forall r1 : reg, In r1 l_load -> ireg_of_reg r1 <> r0) /\
    (forall r0 : ireg,
    ~ In r0 (jit_alu32_stack_list l_load l_store st_pre) /\ In r0 arm_callee_save_regs ->
    forall r1 : reg, In r1 l_store -> ireg_of_reg r1 <> r0).
  Proof.
  Admitted.




Lemma jit_alu32_load_store_subst:
  forall l l0 l1
    (Hload_list : jit_alu32_load_list l = Some l0)
    (Hstore_list : jit_alu32_store_list l = Some l1),
      list_subset l1 l0.
Proof.
Admitted.

Lemma jit_alu32_store_load_subst:
  forall l l0 l1
    (Hload_list : jit_alu32_load_list l = Some l0)
    (Hstore_list : jit_alu32_store_list l = Some l1),
      list_subset l0 l1.
Proof.
Admitted.

Lemma list_subst_nodup:
  forall (l0:list reg) l1,
    NoDup l1 ->
    list_subset l0 l1 ->
      NoDup l0.
Proof.
Admitted.