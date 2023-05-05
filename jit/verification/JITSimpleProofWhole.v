From compcert.lib Require Import Integers.
From compcert.arm Require Import Asm AsmSyntax BinSyntax BinSem BinDecode.
From compcert.common Require Import Values Globalenvs Smallstep Memory Memdata Events AST.

From bpf.comm Require Import Flag BinrBPF ListAsArray Regs rBPFMemType.
From bpf.model Require Import Encode Syntax.
From bpf.monadicmodel2 Require Import ConcreteState.
From bpf.jit.thumb Require Import LoadStoreRegs JITState.
From bpf.jit.thumb Require Import Arm32Reg ThumbJITOpcode ThumbInsOp.

From bpf.jit.verification Require Import JITListSet.
From bpf.jit.verification Require Import rBPFSemInd JITSimple JITSimpleProofWholeDef JITSimpleProofWholeAux JITSimpleProof0.
From bpf.jit.simulation Require Import BitfieldLemma.

From Coq Require Import List ZArith Arith String Lia Logic.FunctionalExtensionality.
Import ListNotations.
Open Scope Z_scope.
Open Scope bool_scope.
Open Scope asm.

Lemma add_ofs_2:
  forall st0 st1 ofs0 ofs1
    (Hlen_eq : S (jitted_len st0) = jitted_len st1)
    (Hofs0 : ofs0 = Z.of_nat (2 * jitted_len st0))
    (Hofs1 : ofs1 = Z.of_nat (2 * jitted_len st1))
    (Hcond : (2 * jitted_len st0 <= 1000)%nat),
      Ptrofs.add (Ptrofs.repr ofs0) (Ptrofs.repr 2) = Ptrofs.repr ofs1.
Proof.
  intros.
  rewrite Hofs0, Hofs1.
  rewrite <- Hlen_eq.
  unfold Ptrofs.add.
  change (Ptrofs.unsigned (Ptrofs.repr 2)) with 2.
  rewrite Ptrofs.unsigned_repr.
  - f_equal.
    lia.
  - change Ptrofs.max_unsigned with 4294967295; lia.
Qed.

Lemma add_ofs_4:
  forall st0 st1 ofs0 ofs1
    (Hlen_eq : S (S (jitted_len st0)) = jitted_len st1)
    (Hofs0 : ofs0 = Z.of_nat (2 * jitted_len st0))
    (Hofs1 : ofs1 = Z.of_nat (2 * jitted_len st1))
    (Hcond : (2 * jitted_len st0 <= 1000)%nat),
      Ptrofs.add (Ptrofs.repr ofs0) (Ptrofs.repr 4) = Ptrofs.repr ofs1.
Proof.
  intros.
  rewrite Hofs0, Hofs1.
  rewrite <- Hlen_eq.
  unfold Ptrofs.add.
  change (Ptrofs.unsigned (Ptrofs.repr 4)) with 4.
  rewrite Ptrofs.unsigned_repr.
  - f_equal.
    lia.
  - change Ptrofs.max_unsigned with 4294967295; lia.
Qed.

Lemma Ptrofs_unsigned_of_int_reg_add_mul_8:
  forall r,
    (Ptrofs.unsigned (Ptrofs.of_int (Int.add
      (Int.mul (int_of_reg r) (Int.repr 8)) (Int.repr 8)))) = (id_of_reg r + 1) * 8.
Proof.
  intros.
  unfold Ptrofs.of_int, Int.add, Int.mul, int_of_reg.
  change (Int.unsigned (Int.repr 8)) with 8.
  rewrite Int.unsigned_repr with (z := id_of_reg r).
  - rewrite Int.unsigned_repr with (z := (id_of_reg r * 8)).
    + rewrite Int.unsigned_repr with (z := (id_of_reg r * 8 + 8)).
      * rewrite Ptrofs.unsigned_repr; [lia | ].
        unfold id_of_reg; change Ptrofs.max_unsigned with 4294967295; destruct r; lia.
      * unfold id_of_reg; change Int.max_unsigned with 4294967295; destruct r; lia.
    + unfold id_of_reg; change Int.max_unsigned with 4294967295; destruct r; lia.
  - unfold id_of_reg; change Int.max_unsigned with 4294967295; destruct r; lia.
Qed.

Lemma upd_jitted_list_load:
  forall vi st0 st1 jit_blk
    (Hupd : upd_jitted_list vi st0 = Some st1)
    (Hjit_inv : jitted_list st0 = Vptr jit_blk Ptrofs.zero),
      Mem.load Mint16unsigned (jit_mem st1) jit_blk
        (Ptrofs.unsigned (Ptrofs.of_int (Int.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))))) =
         Some (Val.load_result Mint16unsigned (Vint vi)).
Proof.
  intros.
  unfold upd_jitted_list, upd_jitted_list' in Hupd.
  destruct ((2 * jitted_len _ + 4 <=? JITTED_LIST_MAX_LENGTH)%nat); [| inversion Hupd].
  rewrite Hjit_inv in Hupd.
  simpl in Hupd.
  destruct Mem.store eqn: Hstore; [| inversion Hupd].
  injection Hupd as Heq.
  rename m into st1_m.
  rewrite Ptrofs.add_zero_l in Hstore.
  eapply Mem.load_store_same in Hstore; eauto.

  assert (Heq1: (jit_mem st1) = st1_m). {
    rewrite <- Heq.
    simpl; reflexivity.
  }
  rewrite Heq1 in *; clear Heq1.
  assumption.
Qed.


Section JITProofWhole.

  (*Variable old_sp: val. *)
  Variable ge : genv.

  Hypothesis arm_reg_value: forall (arm_rs : regset) (r : PregEq.t),
    (exists vi : int, arm_rs r = Vint vi) \/ (exists (b : block) (o : ptrofs), arm_rs r = Vptr b o).

  Hypothesis llvm_enable_alu32:
    forall r st v st_blk,
    eval_reg r st = Some (Val.longofintu (Vint v))  <->
    Mem.load Mint32 (bpf_m st) st_blk (8 * id_of_reg r + 8) = Some (Vint v).

  Section MatchStaterBPFJIT.
  (*
    Record match_state_jit (rbpf_st: state) (jit_st: jit_state): Prop :=
    {
      munchange:  Mem.unchanged_on (fun b _ => b <> jit_blk /\ b <> jit_state_blk) (bpf_m rbpf_st) (jit_mem jit_st);
      mpc      :  pc_loc rbpf_st = jit_pc jit_st;
      mflag    :  match_flag rbpf_st jit_st flag_blk;
      mregs    :  match_registers rbpf_st jit_st regs_blk;
      mmrs_num :  mrs_num rbpf_st = jit_mrs_num jit_st;
      mbpf_mrs :  bpf_mrs rbpf_st = jit_mrs jit_st;
      mins_len :  ins_len rbpf_st = jit_ins_len jit_st;
      mins     :  ins rbpf_st = jit_ins jit_st;
      mjit     :  Vptr jit_blk Ptrofs.zero = jitted_list jit_st; (*
      mjit_len :  (jitted_len jit_st <= 500)%nat; *)

      mperm    :  ptr_range_perm (bpf_m rbpf_st) Mint32 (flag rbpf_st) Freeable /\
                  (forall r, ptr_range_perm (bpf_m rbpf_st) Mint64
                      (Val.add (regs_st rbpf_st) (Vint (Int.repr (8 * (id_of_reg r))))) Freeable) /\
                  ptr_range_perm (jit_mem jit_st) Mint32 (jit_flag jit_st) Freeable /\
                  (forall r, ptr_range_perm (jit_mem jit_st) Mint64
                    (Val.add (jit_regs jit_st) (Vint (Int.repr (8 * (id_of_reg r))))) Freeable) /\
                  (forall pc, (0 <= pc < Nat.div JITTED_LIST_MAX_LENGTH 2)%nat ->
                    ptr_range_perm (jit_mem jit_st) Mint16unsigned
                      (Val.add (JITState.jitted_list jit_st) (Vint (Int.repr (Z.of_nat (2 * pc))))) Freeable);
      minvalid :  ~Mem.valid_block (bpf_m rbpf_st) jit_blk /\
                  Mem.valid_block (jit_mem jit_st) jit_blk /\
                    (block_neq flag_blk regs_blk jit_blk jit_state_blk) /\
                    (forall b, b <> jit_blk ->
                      Mem.valid_block (bpf_m rbpf_st) b -> Mem.valid_block (jit_mem jit_st) b);
      mvalid   : jit_state_memory_layout jit_st jit_state_blk regs_blk (jit_mem jit_st);
    }. *)
    Definition rbpf_state_inv (st: jit_state) (jit_blk: block) : Prop :=
      jitted_list st = Vptr jit_blk Ptrofs.zero (* /\
      jit_regs st = Vptr regs_blk Ptrofs.zero *).
  End MatchStaterBPFJIT.

  Definition  regs_unchanged (l: sync_regs) (m0 m1: mem) (st_blk: block): Prop := (**r for all BPF unused registers *)
  forall r, ~ List.In r l ->
    exists vi : int,
      Mem.load Mint32 m0 st_blk (8 * id_of_reg r + 8) = Some (Vint vi) /\
      Mem.load Mint32 m1 st_blk (8 * id_of_reg r + 8) = Some (Vint vi).

  Definition  stack_unchanged (lsr_stack: sync_iregs) (rs0 rs1: Asm.regset): Prop :=(**r for all callee-save unused registers *)
  forall r,
    ~List.In r lsr_stack /\ List.In r arm_callee_save_regs ->
      rs0 r = rs1 r.

  Section MatchStateJITARM.

    Record match_state_arm (st: state) (old_rs rs: Asm.regset) (m: mem)
      (lr: sync_regs) (ls: sync_iregs)
      (st_blk jit_blk sp_blk ra_blk: block) (ofs: Z) (old_sp: val): Prop := {
      arm_rs14:       exists ofs, rs IR14 = Vptr ra_blk ofs;
      arm_reg_syn:    regs_agree lr st rs /\ NoDup lr;
      arm_reg_same:   regs_unchanged lr (bpf_m st) m st_blk;
      arm_stack_syn:  arm_synch_stack ls old_rs rs m old_sp /\ NoDup ls;
      arm_old_sp:     rs IR13 = Vptr sp_blk Ptrofs.zero /\ Mem.loadv Mint32 m (rs IR13) = Some old_sp;
      arm_pc:         rs PC = Vptr jit_blk (Ptrofs.repr ofs);
      arm_valid_blk:  (st_blk <> jit_blk /\ st_blk <> sp_blk /\ jit_blk <> sp_blk) /\
                      Mem.valid_block m jit_blk /\ Mem.valid_block m st_blk /\ Mem.valid_block m sp_blk;
      arm_blk_perm:   Mem.range_perm m st_blk 0 96 Cur Writable /\
                      Mem.range_perm m sp_blk 0 48 Cur Writable;
      arm_reg_valid:  forall (arm_rs: Asm.regset) r, (exists vi, arm_rs r = Vint vi) \/ (exists b o, arm_rs r = Vptr b o)
    }.
  End MatchStateJITARM.


  Section JITPre.

    Lemma jit_pre_simulation: forall rbpf_st st0 st1 rs0 m0 ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
    (Hjit_pre: jit_alu32_pre st0 = Some st1)
    (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
    (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
    (Hmem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
    (Hjit_inv: rbpf_state_inv st0 jit_blk)
    (Hinitial_arm_st0: match_state_arm rbpf_st rs0 rs0 m0 [] [] st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
    (Hrs_1: rs0 IR1 = Vptr st_blk Ptrofs.zero),
      exists rs1,
        arm_registers_pre rs0 rs1 st_blk /\
        rs1 IR12 = Vptr st_blk Ptrofs.zero /\
        stack_unchanged [] rs0 rs1 /\
        star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
        match_state_arm rbpf_st rs0 rs1 m0 [] [] st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      intros.
      unfold arm_registers_pre.
      eexists.
      split; [(**r arm_registers_pre *) reflexivity | ].

      destruct Hinitial_arm_st0 as (Harm_ir14, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, Harm_old_sp,
        Harm_pc, Harm_valid_blk, Harm_blk_perm, arm_reg_valid).

      split.
      { (**r rs1 IR12 = Vptr st_blk Ptrofs.zero *)
        unfold nextinstr_nf, nextinstr, undef_flags.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Pregmap.gss.
        f_equal.
      }


      unfold jit_alu32_pre, jit_alu32_thumb_load_store_template_jit in Hjit_pre.
      apply upd_jitted_list_jittted_len in Hjit_pre as Hlen_eq.

      assert (Hcond: (2 * jitted_len st0 <= 1000)%nat). {
        simpl.
        apply upd_jitted_list_max2 in Hjit_pre.
        clear - Hjit_pre Hlen_eq.
        unfold JITTED_LIST_MAX_LENGTH in Hjit_pre.
        lia.
      }

      assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
        rewrite Ptrofs.unsigned_repr.
        reflexivity.
        change Ptrofs.max_unsigned with 4294967295; lia.
      }

      split.
      {
        (**r stack_unchanged *)
        unfold stack_unchanged.
        intros r Hin.
        unfold nextinstr_nf, nextinstr, undef_flags.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        - reflexivity.
        - destruct Hin as (_ & Hin).
          subst r.
          repeat (destruct Hin as [Hin | Hin]; [inversion Hin |]).
          inversion Hin.
      }

      split.
      { (**r star BinSem.step *)
        eapply star_one; eauto.
        eapply exec_step_bin with (i := Pmov IR12 (SOreg IR1)) (w := false); eauto.
        - (**r find_instr *)
          rewrite Harm_pc.

          unfold find_instr.
          eapply upd_jitted_list_load in Hjit_pre; eauto.

          rewrite Hofs0; simpl.
          unfold sub_mem_blk in Hmem.
          rewrite <- Hmem.
          2:{
            rewrite Hofs1, Hofs0.
            rewrite <- Hlen_eq.
            rewrite Hlen_eq0.
            change (size_chunk Mint16unsigned) with 2.
            lia.
          }

          unfold Ptrofs.of_int in Hjit_pre.
          rewrite Int.unsigned_repr in Hjit_pre.
          2:{ change Int.max_unsigned with 4294967295; lia. }
          rewrite Hjit_pre.
          eapply lemma_thumb_mov_1; eauto.
        - (**r exec_instr *)
          simpl.
          rewrite Hrs_1.
          f_equal.
      }

      constructor; try assumption.
      - (**r Harm_stack_syn *)
        destruct Harm_reg_syn as (Harm_reg_syn & Hnodup).
        split; [| assumption].
        unfold regs_agree.
        intros r Hin; inversion Hin.

      - (**r Harm_pc *)
        unfold nextinstr_nf, nextinstr, undef_flags.
        rewrite Pregmap.gss.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Harm_pc.
        unfold Val.offset_ptr, wsize.
        f_equal.
        eapply add_ofs_2; eauto.
    Qed.
  End JITPre.


  Lemma jit_alu32_stack_list_callee_save:
    forall l l0 l1 st
      (Hf: jit_alu32_stack_list l0 l1 st = l),
        forall r, List.In r l -> List.In r arm_callee_save_regs.
  Proof.
    unfold jit_alu32_stack_list.
    intros.
    eapply filter_In; eauto.
    rewrite Hf.
    assumption.
  Qed.


  Lemma jit_alu32_stack_list_NoDup:
    forall l l0 l1 st
      (Hf: jit_alu32_stack_list l0 l1 st = l),
        NoDup l.
  Proof.
    unfold jit_alu32_stack_list.
    intros.
    rewrite <- Hf.
    eapply NoDup_filter; eauto.
    unfold arm_callee_save_regs.
    do 8 (constructor; [simpl; intuition congruence | ]).
    constructor.
  Qed.


  Section JITSpilling.

    Lemma arm_registers_spilling_aux_load_same:
      forall l rs0 m0 rs1 m1 r sp_blk
        (Haux: arm_registers_spilling_aux l rs0 m0 = Some (rs1, m1))
        (Harm_callee_save: forall r : ireg, In r l -> In r arm_callee_save_regs)
        (Hnin : ~ In r l)
        (Hr_in: In r arm_callee_save_regs)
        (Hsp_blk: rs0 IR13 = Vptr sp_blk Ptrofs.zero)
        (Hstack2 : Mem.load Mint32 m0 sp_blk (Z_of_ireg r * 4) = Some (rs0 r)),
          Mem.load Mint32 m1 sp_blk (Z_of_ireg r * 4) = Some (rs0 r).
    Proof.
      induction l; intros.
      { simpl in Haux.
        injection Haux as Hrs_eq Hm_eq.
        subst rs1 m1.
        assumption.
      }
      simpl in Haux.
      destruct arm_registers_spilling_one eqn: Hone; [| inversion Haux].
      rename m into mk.

      unfold arm_registers_spilling_one in Hone.
      rewrite Hsp_blk in Hone.
      simpl in Hone.
      rewrite Ptrofs.add_zero_l in *.
      rewrite Hreg_mul4_unsigned in *.

      eapply IHl with (rs0 := rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)); eauto.
      - intros r0 Hin.
        apply Harm_callee_save.
        right; assumption.
      - intros HF.
        apply Hnin.
        right; assumption.
      - rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite <- Hstack2.

        eapply Mem.load_store_other; eauto.
        right.
        assert (Hneq: r <> a). {
          intro HF.
          apply Hnin. left.
          rewrite HF; reflexivity.
        }

        assert (Hin: In a arm_callee_save_regs). {
          apply Harm_callee_save.
          left; reflexivity.
        }

        change (size_chunk Mint32) with 4.
        clear - Hr_in Hneq Hin.
        unfold Z_of_ireg.

        destruct Hr_in as [Hr_in | Hr_in].
        { subst r.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          exfalso; apply Hneq; reflexivity.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          inversion Hin.
        }

        destruct Hr_in as [Hr_in | Hr_in].
        { subst r.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          exfalso; apply Hneq; reflexivity.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          inversion Hin.
        }

        destruct Hr_in as [Hr_in | Hr_in].
        { subst r.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          exfalso; apply Hneq; reflexivity.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          inversion Hin.
        }

        destruct Hr_in as [Hr_in | Hr_in].
        { subst r.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          exfalso; apply Hneq; reflexivity.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          inversion Hin.
        }

        destruct Hr_in as [Hr_in | Hr_in].
        { subst r.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          exfalso; apply Hneq; reflexivity.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          inversion Hin.
        }

        destruct Hr_in as [Hr_in | Hr_in].
        { subst r.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          exfalso; apply Hneq; reflexivity.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          inversion Hin.
        }

        destruct Hr_in as [Hr_in | Hr_in].
        { subst r.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          exfalso; apply Hneq; reflexivity.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          inversion Hin.
        }

        destruct Hr_in as [Hr_in | Hr_in].
        { subst r.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          exfalso; apply Hneq; reflexivity.
          repeat (destruct Hin as [Hin | Hin]; [subst a; try lia | ]).
          inversion Hin.
        }
        inversion Hr_in.
    Qed.

    Definition arm_registers_pre_weak (rs: Asm.regset) (st_blk: block): Prop :=
      (rs IR12) = (Vptr st_blk Ptrofs.zero).

    Lemma arm_registers_pre_implies:
      forall rs rs0 st_blk
        (Hpre: arm_registers_pre rs rs0 st_blk),
          arm_registers_pre_weak rs0 st_blk.
    Proof.
      unfold arm_registers_pre, arm_registers_pre_weak; intros.
      rewrite Hpre.
      unfold nextinstr_nf, nextinstr, undef_flags. Search undef_flags.
      rewrite Pregmap.gso; [ | intros HF; inversion HF].
      rewrite Pregmap.gss.
      reflexivity.
    Qed.

    Lemma jit_spilling_one_simulation: forall l r rbpf_st st0 st1 rs_init rs0 m0 ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hupd_spilling : jit_alu32_thumb_upd_save r st0 = Some st1)

      (Hlist_not_in: ~ List.In r l)
      (Hcallee_save: In r arm_callee_save_regs)
      (Harm_callee_save: forall r, List.In r l -> List.In r arm_callee_save_regs)
      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))

      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)

      (Hstack_unchanged:  stack_unchanged l rs_init rs0)
      (Harm: match_state_arm rbpf_st rs_init rs0 m0 [] l st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hr_eq: rs0 r = rs_init r)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1 m1,
          arm_registers_spilling_one r rs0 m0 = Some m1 /\
          rs1 = rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          stack_unchanged (r::l) rs_init rs1 /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
          match_state_arm rbpf_st rs_init rs1 m1 [] (r::l) st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      intros.
      destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
        Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

      exists (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)).

      (**r arm_registers_spilling_one *)
      unfold arm_registers_spilling_one.
      rewrite Hsp_blk.
      simpl.
      rewrite Ptrofs.add_zero_l.
      rewrite Hreg_mul4_unsigned.
      unfold Mem.range_perm in Hsp_blk_perm. (**r here we need the info: r is in arm_callee_save *)

      assert (Heq: Mem.valid_access m0 Mint32 sp_blk (Z_of_ireg r * 4) Writable). {
        unfold Mem.valid_access.
        split.
        - unfold Mem.range_perm.
          intros ofs Hrange.
          apply Hsp_blk_perm.
          change (size_chunk Mint32) with 4 in Hrange.
          clear - Hrange Hcallee_save.
          unfold Z_of_ireg in Hrange.
          repeat (destruct Hcallee_save as [Hcallee_save | Hcallee_save]; [ subst r; lia | ]).
          inversion Hcallee_save.
        - simpl.
          apply Z.divide_factor_r.
      }
      eapply Mem.valid_access_store with (v := rs0 r) in Heq; eauto.
      destruct Heq as (m1 & Hstore).

      assert (Hst0_eq: S (S (jitted_len st0)) = jitted_len st1). {
        clear - Hupd_spilling.
        unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_spilling.
        eapply upd_jitted_list_jittted_len_2; eauto.
      }

      assert (Hle: (2 * jitted_len st0 <= JITTED_LIST_MAX_LENGTH)%nat). {
        clear - Hupd_spilling.
        unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_spilling.
        destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_spilling].
        eapply upd_jitted_list_max in Hupd; eauto.
        lia.
      }
      unfold JITTED_LIST_MAX_LENGTH in Hle.

      assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
        rewrite Ptrofs.unsigned_repr.
        reflexivity.
        change Ptrofs.max_unsigned with 4294967295; lia.
      }

      assert (Hlen_eq1: Ptrofs.unsigned (Ptrofs.add 
          (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.of_int (Int.repr 2))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
        unfold Ptrofs.add.
        change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2))) with 2.
        rewrite Ptrofs.unsigned_repr.
        - rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
        - change Ptrofs.max_unsigned with 4294967295.
          rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
      }

      exists m1.
      split; [assumption | ].
      split; [(**r rs PC *) f_equal | ].
      split.
      { (**r rs IR12 *)
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        assumption.
      }

      split.
      {
        (**r stack_unchanged *)
        unfold stack_unchanged in *.
        intros r0 Hin.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        eapply Hstack_unchanged; eauto.
        destruct Hin as (Hin1 & Hin).
        split; [| assumption].
        intros HF; apply Hin1.
        right; assumption.
      }

      split.
      { (**r star BinSem.step *)
        eapply star_one.
        eapply exec_step_bin with (i := Pstr r IR13 (SOimm (Int.mul (int_of_ireg r) (Int.repr 4)))) (w := true).
        - (**r find_instr *)
          rewrite Harm_pc.

          assert (Heq: find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) m0 =
            find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st1)). {
            unfold find_instr.
            unfold sub_mem_blk in Hsub_mem.

            unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_spilling.
            unfold rbpf_state_inv in Hjit_inv.
            eapply upd_jitted_list_2_load with (jit_blk := jit_blk) in Hupd_spilling as Hload; eauto.
            eapply upd_jitted_list_2_load_2 with (jit_blk := jit_blk) in Hupd_spilling as Hload2; eauto.

            simpl.
            rewrite Hofs0; simpl.
            rewrite <- Hsub_mem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hst0_eq.
              rewrite Hlen_eq0.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }

            unfold Ptrofs.of_int in Hload.
            rewrite Int.unsigned_repr in Hload.
            2:{ change Int.max_unsigned with 4294967295; lia. }
            rewrite Hload.
            simpl.
            rewrite <- Hsub_mem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hst0_eq.
              rewrite Hlen_eq1.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }
            unfold Ptrofs.of_int in Hload2.
            rewrite Int.unsigned_repr in Hload2.
            2:{ change Int.max_unsigned with 4294967295; lia. }
            unfold Ptrofs.of_int.
            change (Int.unsigned (Int.repr 2)) with 2.
            rewrite Hload2.
            simpl.
            reflexivity.
          }
          rewrite Heq.
          eapply lemma_thumb_str; eauto.

          change (Int.unsigned (Int.repr 4)) with 4.
          unfold int_of_ireg.
          rewrite Int.unsigned_repr;
            [| change Int.max_unsigned with 4294967295];
            unfold Z_of_ireg; destruct r; simpl; lia.
        - (**r exec_instr *)
          simpl.
          rewrite Hsp_blk; simpl.
          rewrite Ptrofs.add_zero_l.
          unfold exec_store; simpl.
          rewrite Hreg_mul4_unsigned.
          rewrite Hstore.
          f_equal.
      }
      { (**r match_state_arm *)
        constructor.
        + (**r ra_blk *)
          assumption.
        + (**r regs_agree *)
          assumption.
        + (**r regs_unchanged *)
          unfold regs_unchanged in *.
          intros r0 HT.
          specialize (Harm_reg_same _ HT).
          destruct Harm_reg_same as (vi & Hload0 & Hload1).
          exists vi.
          split; [assumption |].
          rewrite <- Hload1.
          eapply Mem.load_store_other; eauto.
          left.
          intuition.
        + (**r arm_synch_stack *)
          unfold arm_synch_stack in *.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].

          destruct Harm_stack_syn as (Hload & HNoDup).
          rewrite Hsp_blk in *.
          simpl in Hload.
          simpl.
          split.
          {
            intros r0 Hin.
            simpl.
            rewrite Ptrofs.add_zero_l.
            rewrite Hreg_mul4_unsigned.
            destruct Hin as [Hreg_eq | Hin].
            - (**r r0 = r *)
              subst r0.
              erewrite Mem.load_store_same; eauto.
              f_equal.
              rewrite Hr_eq.
              unfold Val.load_result.
              specialize (Harm_reg_valid rs_init r).
              destruct Harm_reg_valid as [(vi & Hrs_eq) | (b & o & Hrs_eq)];
                rewrite Hrs_eq; reflexivity.
            - (**r In r0 l *)
              specialize (Hload r0 Hin).
              rewrite <- Hload.
              simpl.
              rewrite Ptrofs.add_zero_l.
              rewrite Hreg_mul4_unsigned.
              erewrite Mem.load_store_other; eauto.
              right.

              clear - Hlist_not_in Hcallee_save Harm_callee_save Hin.
              specialize (Harm_callee_save r0 Hin).
              assert (Hneq: r <> r0). {
                intro HF.
                subst r0.
                apply Hlist_not_in.
                assumption.
              }
              clear Hin Hlist_not_in.
              change (size_chunk Mint32) with 4.
              unfold arm_callee_save_regs in *.
              unfold Z_of_ireg.
              rename Harm_callee_save into Hr0.
              rename Hcallee_save into Hr.
              destruct Hr0 as [Hr0 | Hr0]; [subst r0 | ].
              { repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                destruct Hr as [Hr | Hr]; [subst r; exfalso; apply Hneq; reflexivity | ];
                repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                inversion Hr.
              }
              destruct Hr0 as [Hr0 | Hr0]; [subst r0 | ].
              { repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                destruct Hr as [Hr | Hr]; [subst r; exfalso; apply Hneq; reflexivity | ];
                repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                inversion Hr.
              }
              destruct Hr0 as [Hr0 | Hr0]; [subst r0 | ].
              { repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                destruct Hr as [Hr | Hr]; [subst r; exfalso; apply Hneq; reflexivity | ];
                repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                inversion Hr.
              }
              destruct Hr0 as [Hr0 | Hr0]; [subst r0 | ].
              { repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                destruct Hr as [Hr | Hr]; [subst r; exfalso; apply Hneq; reflexivity | ];
                repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                inversion Hr.
              }
              destruct Hr0 as [Hr0 | Hr0]; [subst r0 | ].
              { repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                destruct Hr as [Hr | Hr]; [subst r; exfalso; apply Hneq; reflexivity | ];
                repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                inversion Hr.
              }
              destruct Hr0 as [Hr0 | Hr0]; [subst r0 | ].
              { repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                destruct Hr as [Hr | Hr]; [subst r; exfalso; apply Hneq; reflexivity | ];
                repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                inversion Hr.
              }
              destruct Hr0 as [Hr0 | Hr0]; [subst r0 | ].
              { repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                destruct Hr as [Hr | Hr]; [subst r; exfalso; apply Hneq; reflexivity | ];
                repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                inversion Hr.
              }
              destruct Hr0 as [Hr0 | Hr0]; [subst r0 | ].
              { repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                destruct Hr as [Hr | Hr]; [subst r; exfalso; apply Hneq; reflexivity | ];
                repeat (destruct Hr as [Hr | Hr]; [subst r; lia | ]);
                inversion Hr.
              }
              inversion Hr0.
          }
          (**r NoDup (r :: l) *)
          rewrite NoDup_cons_iff.
          split; assumption.

        + (**r rs IR13 *)
          rewrite Harm_pc.
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          rewrite Hsp_blk in *.
          split; [reflexivity |].
          rewrite <- Harm_old_sp.
          simpl.
          erewrite Mem.load_store_other; eauto.
          right.
          change (Ptrofs.unsigned Ptrofs.zero) with 0.
          change (size_chunk Mint32) with 4.
          clear - Hcallee_save.
          unfold Z_of_ireg.
          repeat (destruct Hcallee_save as [Hcallee_save | Hcallee_save]; [subst r; lia | ]).
          inversion Hcallee_save.

        + (**r rs PC *)
          rewrite Pregmap.gss.
          rewrite Harm_pc.
          simpl.
          f_equal.
          unfold Ptrofs.add, wsize.
          change (Ptrofs.unsigned (Ptrofs.repr 4)) with 4.
          eapply add_ofs_4; eauto.

        + (**r Mem.valid_block *)
          clear Hsp_blk.
          destruct Harm_valid_blk as (Ht & Hjit_blk & Hst_blk & Hsp_blk).
          split; [assumption |].
          repeat split; try (eapply Mem.store_valid_block_1; eauto).
        + (**r Mem.range_perm *)
          unfold Mem.range_perm in *.
          clear - Harm_valid_blk Hst_blk_perm Hsp_blk_perm Hstore.
          split; intros ofs Hofs; eapply Mem.perm_store_1; eauto.

        + (**r arm_reg_valid *)
          assumption.
      }
    Qed.

    Lemma jit_spilling_simulation: forall l rbpf_st st0 st1 rs0 m0 ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hjit_spilling: jit_alu32_thumb_save l st0 = Some st1)
      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)

      (Hnodup: NoDup l)
      (Harm_callee_save: forall r, List.In r l -> List.In r arm_callee_save_regs)

      (Hstack_unchanged: stack_unchanged [] rs0 rs0)
      (Harm: match_state_arm rbpf_st rs0 rs0 m0 [] [] st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1 m1,
          arm_registers_spilling l rs0 rs1 m0 m1 /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          stack_unchanged l rs0 rs1 /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
          match_state_arm rbpf_st rs0 rs1 m1 [] l st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      induction l; intros.
      { (**r l = [] *)
        simpl in *.
        injection Hjit_spilling as Hst_eq.
        subst st0.
        exists rs0.
        exists m0.
        split; [split; reflexivity | ].
        split; [assumption | ].
        split; [assumption | ].
        split; [ apply star_refl | ].
        rewrite <- Hofs0 in Hofs1.
        subst ofs1.
        assumption.
      }

      (**r l = hd :: tl *)
      unfold arm_registers_spilling; simpl.

      simpl in Hjit_spilling.

      destruct jit_alu32_thumb_upd_save eqn: Hone_spilling; [| inversion Hjit_spilling].
      rename j into stk.

      assert (Hle: (2 * jitted_len st0 <= JITTED_LIST_MAX_LENGTH)%nat). {
        clear - Hone_spilling.
        unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
        destruct upd_jitted_list eqn: Hupd; [| inversion Hone_spilling].
        eapply upd_jitted_list_max in Hupd; eauto.
        lia.
      }
      unfold JITTED_LIST_MAX_LENGTH in Hle.

      eapply jit_spilling_one_simulation with
        (rbpf_st := rbpf_st) (rs0 := rs0) (l := []) (old_sp := old_sp) (sp_blk := sp_blk)
        (ofs0 := ofs0) (ofs1 := Z.of_nat (2 * jitted_len stk)) (m0 := m0)
        in Hone_spilling as Hone; eauto.

      - (**r MAIN *)
        destruct Hone as (rsk & mk & Harm_registers_spilling_one & Hrs_eq & Hpre_rs1 & Hstack_unchangedk & Hsemk & Hmatch_armk).
        rewrite Harm_registers_spilling_one.
        unfold arm_registers_spilling in IHl.

        specialize (IHl rbpf_st stk st1
          (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)) mk
          (Z.of_nat (2 * jitted_len stk)) ofs1 st_blk jit_blk sp_blk ra_blk old_sp).

        specialize (IHl Hjit_spilling).

        assert (Heq: Z.of_nat (2 * jitted_len stk) = Z.of_nat (2 * jitted_len stk)) by reflexivity.
        specialize (IHl Heq Hofs1); clear Heq.

        assert (Heq: sub_mem_blk (jit_mem st1) mk jit_blk (Z.of_nat (2 * jitted_len stk)) ofs1). {
          clear - Hone_spilling Hjit_spilling Hofs0 Hofs1 Hsub_mem Harm Harm_registers_spilling_one.
          unfold sub_mem_blk in *.
          intros chunk ofs Hofs.
          erewrite Hsub_mem.
          - unfold arm_registers_spilling_one in Harm_registers_spilling_one.
            destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).
            rewrite Hsp_blk in *.
            simpl in Harm_registers_spilling_one.
            symmetry.
            erewrite Mem.load_store_other; eauto.
            left.
            clear- Harm_valid_blk; destruct Harm_valid_blk as (Harm_valid_blk & _); intuition.
          - split; [| lia].
            unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
            eapply upd_jitted_list_jittted_len_2 in Hone_spilling; eauto.
            lia.
        }
        specialize (IHl Heq); clear Heq.

        unfold rbpf_state_inv in *.
        assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
          clear - Hjit_inv Hone_spilling.
          unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
          eapply upd_jitted_list_unchange_jittted_list_2 in Hone_spilling.
          rewrite <- Hone_spilling.
          assumption.
        }
        specialize (IHl Heq); clear Heq.

        assert (Heq: NoDup l). {
          rewrite NoDup_cons_iff in Hnodup.
          destruct Hnodup as (_ & Hnodup); assumption.
        }
        specialize (IHl Heq); clear Heq.

        assert (Heq: forall r : ireg, In r l -> In r arm_callee_save_regs). {
          intros r Hin.
          assert (Heq: In r (a :: l)). {
            simpl. right; assumption.
          }
          specialize (Harm_callee_save r Heq).
          assumption.
        }
        specialize (IHl Heq); clear Heq.

        assert (Heq: stack_unchanged [] rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) rs0 # 
          PC <- (Val.offset_ptr (rs0 PC) wsize)). {
          unfold stack_unchanged in *.
          intros r Hin.
          f_equal.
        }
        specialize (IHl Heq); clear Heq.

        assert (Heq: match_state_arm rbpf_st rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)
                      rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) mk [] [] st_blk jit_blk sp_blk ra_blk
                      (Z.of_nat (2 * jitted_len stk)) old_sp). {
          clear - Hone_spilling Harm Harm_callee_save Harm_registers_spilling_one Hnodup Hofs0 Hle.
          destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

          unfold arm_registers_spilling_one in Harm_registers_spilling_one.
          rewrite Hsp_blk in *.
          simpl in Harm_registers_spilling_one.
          rewrite Ptrofs.add_zero_l in Harm_registers_spilling_one.
          rewrite Hreg_mul4_unsigned in Harm_registers_spilling_one.

          constructor; try assumption.
          - (**r regs_unchanged *)
            unfold regs_unchanged in *.
            intros r HT.
            specialize (Harm_reg_same _ HT).
            destruct Harm_reg_same as (vi & Hload1 & Hload2).
            exists vi.
            split; [assumption |].
            rewrite <- Hload2.
            eapply Mem.load_store_other; eauto.
            left. intuition.
          - (**r arm_synch_stack *)
            unfold arm_synch_stack in *.
            split; [| apply NoDup_nil].
            intros r Hin; inversion Hin.
          - (**r rs0 IR13 *)
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            split; [assumption | ].
            rewrite <- Harm_old_sp.
            rewrite Hsp_blk in *.
            simpl.
            eapply Mem.load_store_other; eauto.
            right.
            change (Ptrofs.unsigned Ptrofs.zero) with 0.
            change (size_chunk Mint32) with 4.

            specialize (Harm_callee_save a).
            assert (Heq: In a (a :: l)).  {
              simpl.
              left; reflexivity.
            }

            specialize (Harm_callee_save Heq); clear Heq.
            clear - Harm_callee_save.
            unfold arm_callee_save_regs in Harm_callee_save.
            unfold Z_of_ireg; destruct a; try lia.
            rename Harm_callee_save into HF.
            do 8 (destruct HF as [HF | HF]; [inversion HF | ]).
            inversion HF.
          - (**r rs PC *)
            rewrite  Pregmap.gss.
            unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
            rewrite Harm_pc.
            unfold Val.offset_ptr, wsize.
            f_equal.
            eapply upd_jitted_list_jittted_len_2 in Hone_spilling as Heq; eauto.
            rewrite <- Heq.
            erewrite add_ofs_4; eauto.

          - (**r Mem.valid_block *)
            clear Hsp_blk.
            destruct Harm_valid_blk as (Ht & Hjit_blk & Hst_blk & Hsp_blk).
            split; [assumption |].
            repeat split; try (eapply Mem.store_valid_block_1; eauto).
          - (**r Mem.range_perm *)
            unfold Mem.range_perm in *.
            clear - Harm_valid_blk Hst_blk_perm Hsp_blk_perm Harm_registers_spilling_one.
            split; intros ofs Hofs; eapply Mem.perm_store_1; eauto.
        }
        specialize (IHl Heq); clear Heq.

        assert (Hpre_cond: rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) IR12 = Vptr st_blk Ptrofs.zero). {
          clear - Hpre_rs1 Hrs_eq.
          rewrite <- Hpre_rs1.
          rewrite Hrs_eq.
          reflexivity.
        }
        specialize (IHl Hpre_cond); clear Hpre_cond.

        destruct IHl as (rs1 & m1 & Haux & Hpre_rs & Hstack_unchanged1 & Hsem1 & Hmatch_arm1).
        exists rs1, m1.
        rewrite <- Hrs_eq in *.
        split; [assumption | ].
        split; [assumption | ].
        split.
        {
          (**r stack_unchanged *)
          unfold stack_unchanged in *.
          intros r Hin.
          destruct Hin as (Hin1 & Hin).
          erewrite <- Hstack_unchanged1; eauto.
          2:{
            split; [| assumption].
            intros HF; apply Hin1.
            right; assumption.
          }
          erewrite <- Hstack_unchangedk; eauto.
          split; [| assumption].
          intros HF; apply Hin1.
          left.
          destruct HF as [Heq| HF]; [| inversion HF].
          assumption.
        }

        split; [ (**r star BinSem.step *) eapply star_trans; eauto | ].

        (**r match_state_arm *)
        destruct arm_registers_spilling_aux eqn: Hspilling_n; [| inversion Haux].
        destruct p.
        destruct Haux as (Haux_rs & Haux_m).
        subst r m.

        destruct Hmatch_arm1 as (Hra_blk1, Harm_reg_syn1, Harm_reg_same1, Harm_stack_syn1, (Hsp_blk1 & Harm_old_sp1),
              Harm_pc1, Harm_valid_blk1, (Hst_blk_perm1 & Hsp_blk_perm1), Harm_reg_valid1).

        destruct Hmatch_armk as (Har_blkk, Harm_reg_synk, Harm_reg_samek, Harm_stack_synk, (Hsp_blkk & Harm_old_spk),
              Harm_pck, Harm_valid_blkk, (Hst_blk_permk & Hsp_blk_permk), Harm_reg_validk).

        constructor; try assumption.
        + (**r arm_synch_stack *)
          unfold arm_synch_stack in *.
          destruct Harm_stack_syn1 as (Harm_stack_syn1 & _).
          destruct Harm_stack_synk as (Harm_stack_synk & _).
          split; [ | assumption].
          intros r Hin.

          destruct Hin as [Hreg_eq | Hin].
          { subst a.

            assert (Hin: In r [r]) by (left; reflexivity).
            specialize (Harm_stack_synk r Hin); clear Hin.
            assert (Heq: rs0 r = rsk r). {
              rewrite Hrs_eq.
              rewrite Pregmap.gso; [ reflexivity | intros HF; inversion HF].
            }
            rewrite Heq in *; clear Heq.

            rewrite NoDup_cons_iff in Hnodup.
            destruct Hnodup as (Hnin & Hnodup).
            rewrite Hsp_blk1, Hsp_blkk in *.
            simpl.
            simpl in Harm_stack_synk.
            rewrite Ptrofs.add_zero_l in *.
            repeat rewrite Hreg_mul4_unsigned in *.

            eapply arm_registers_spilling_aux_load_same with (rs0 := rsk) (rs1:= rs1); eauto.
            + intros r0 Hr0_in.
              apply Harm_callee_save.
              right; assumption.
            + apply Harm_callee_save; left; reflexivity.
          }

          specialize (Harm_stack_syn1 r Hin).
          assert (Heq: rs0 r = rsk r). {
            rewrite Hrs_eq.
            rewrite Pregmap.gso; [ reflexivity | intros HF; inversion HF].
          }
          rewrite Heq in *; clear Heq.
          apply Harm_stack_syn1; auto.
        + (**r rs1 IR13 *)
          split; assumption.
        + (**r Mem.range_perm *)
          split; assumption.

      - (**r In a arm_callee_save_regs *)
        apply Harm_callee_save; left; reflexivity.
      - (**r arm_callee_save_regs *)
        intros r HF.
        inversion HF.
      - (**r sub_mem_blk *)
        unfold sub_mem_blk in Hsub_mem.
        unfold sub_mem_blk.
        intros chunk ofs2 Hrange.
        specialize (Hsub_mem chunk ofs2).
        assert (Heq: ofs0 <= ofs2 /\ ofs2 + size_chunk chunk <= ofs1). {
          assert (Heq: (jitted_len stk <= jitted_len st1)%nat). {
            eapply jit_alu32_thumb_save_jitted_len_leb; eauto.
          }
          lia.
        }
        specialize (Hsub_mem Heq); clear Heq.

        rewrite <- Hsub_mem.

        eapply jit_alu32_thumb_save_load_same; eauto.
        unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
        eapply upd_jitted_list_unchange_jittted_list_2 in Hone_spilling; eauto.
        rewrite <- Hone_spilling; assumption.
        lia.
    Qed.

  End JITSpilling.

  Section JITLoad.
(*
    Lemma upd_jitted_list_unchange_match_state_jit:
      forall rbpf_st st0 st1 v
      (Hst: match_state_jit rbpf_st st0)
      (Hupd: upd_jitted_list v st0 = Some st1)
      (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero),
        match_state_jit rbpf_st st1.
    Proof.
      unfold upd_jitted_list, upd_jitted_list'.
      intros.
      destruct (2 * jitted_len _ + 4 <=? JITTED_LIST_MAX_LENGTH)%nat; [| inversion Hupd].
      rewrite Harm_blk in Hupd.
      unfold Mem.storev, Val.add, Archi.ptr64 in Hupd.
      rewrite Ptrofs.add_zero_l in Hupd.
      destruct Mem.store eqn: Hstore; [| inversion Hupd].
      injection Hupd as Hst_eq.
      subst st1.

      destruct Hst.
      constructor; simpl; try assumption.

      - (**r Mem.unchanged_on *)
        eapply store_unchanged_on_3; eauto.
        intros.
        intro HF.
        destruct HF as (HF & _).
        apply HF.
        reflexivity.
      - (**r match_flag *)
        unfold match_flag in *; simpl.
        destruct mflag0 as (Hflag0 & Hflag1 & Hflag2).
        split; [assumption | ].
        split; [assumption | ].
        destruct Hflag2 as (f & Hflag3 & Hflag4).
        exists f.
        split; [assumption | ].
        unfold eval_jit_flag.
        unfold eval_jit_flag in Hflag4.
        simpl.
        rewrite <- Hflag4.
        rewrite Hflag1.
        simpl.
        eapply Mem.load_store_other; eauto.
        left.
        destruct minvalid0 as (_ & _ & Hblk_neq & _).
        unfold block_neq in Hblk_neq.
        destruct Hblk_neq as ( (_ & Hneq & _) & _).
        clear - Hneq.
        intuition.
      - (**r match_registers *)
        unfold match_registers in mregs0.
        destruct mregs0 as (Hreg0 & Hreg1 & Hreg2).
        unfold match_registers; simpl.
        split; [assumption | ].
        split; [assumption | ].
        intro r.
        specialize (Hreg2 r).
        destruct Hreg2 as (vi & Heval_reg0 & Heval_reg1 & Hreg_load & Haxiom).
        exists vi.
        split; [assumption | ].
        split.
        {
          unfold eval_jit_reg in Heval_reg1.
          unfold eval_jit_reg; simpl.
          simpl in Heval_reg1.
          rewrite <- Heval_reg1.
          rewrite Hreg1; simpl.
          eapply Mem.load_store_other; eauto.
          left.
          destruct minvalid0 as (_ & _ & Hblk_neq & _).
          unfold block_neq in Hblk_neq.
          destruct Hblk_neq as ( (_ & _ & Hneq) & _).
          clear - Hneq.
          intuition.
        }
        split.
        {
          rewrite <- Hreg_load.
          simpl.
          eapply Mem.load_store_other; eauto.
          left.
          destruct minvalid0 as (_ & _ & Hblk_neq & _).
          unfold block_neq in Hblk_neq.
          destruct Hblk_neq as ( (_ & _ & Hneq) & _).
          clear - Hneq.
          intuition.
        }
        assumption.
      - (**r jit *)
        reflexivity.
      - (**r perm *)
        destruct mperm0 as (Hperm0 & Hperm1 & Hperm2 & Hperm3 & Hperm4).
        split; [assumption | ].
        split; [assumption | ].
        unfold match_flag in mflag0.
        destruct mflag0 as (_ & Hflag0 & _).
        rewrite Hflag0 in *.
        split.
        {
          unfold ptr_range_perm in Hperm2.
          unfold ptr_range_perm.
          eapply Mem.store_valid_access_1; eauto.
        }
        split.
        {
          intros r.
          specialize (Hperm3 r).
          unfold ptr_range_perm in Hperm3.
          unfold ptr_range_perm.
          unfold match_registers in mregs0.
          destruct mregs0 as (_ & Hreg0 & _).
          rewrite Hreg0 in *.
          simpl.
          simpl in Hperm3.
          eapply Mem.store_valid_access_1; eauto.
        }
        intros pc Hpc_range.
        simpl in Hperm4.
        specialize (Hperm4 pc Hpc_range).
        unfold ptr_range_perm in Hperm4.
        rewrite <- mjit0 in Hperm4.
        simpl in Hperm4.
        eapply Mem.store_valid_access_1; eauto.
      - (**r invalid *)
        destruct minvalid0 as (Hvalid0 & Hvalid1 & Hblk_neq & Hvalid2).
        split; [assumption | ].
        split.
        {
          eapply Mem.store_valid_block_1; eauto.
        }
        split; [assumption | ].
        intros b Hneq Hvalid3.
        specialize (Hvalid2 b Hneq Hvalid3).
        eapply Mem.store_valid_block_1; eauto.
      - (**r jit_state_memory_layout *)
        unfold jit_state_memory_layout.
        unfold jit_state_memory_layout in mvalid0.
        destruct mvalid0 as (Hvalid0 & Hvalid1).
        split.
        + simpl.
          simpl in Hvalid0.
          rewrite <- Hvalid0.
          eapply Mem.load_store_other; eauto.
          left.
          destruct minvalid0 as (_ & _ & Hblk_neq & _).
          unfold block_neq in Hblk_neq.
          destruct Hblk_neq as ( _ & (_ & Hneq & _)).
          clear - Hneq.
          intuition.
        + (**r regs_layout *)
          unfold regs_layout in Hvalid1.
          unfold regs_layout.
          intros r.
          specialize (Hvalid1 r).
          destruct Hvalid1 as (vi & Heval_reg & Hload64 & Hload32 & Haxiom).
          exists vi.

          unfold match_registers in mregs0.
          destruct mregs0 as (_ & mregs0 & _).

          split.
          { rewrite <- Heval_reg.
            unfold eval_jit_reg; simpl.
            rewrite mregs0; simpl.
            eapply Mem.load_store_other; eauto.
            left.
            destruct minvalid0 as (_ & _ & Hblk_neq & _).
            unfold block_neq in Hblk_neq.
            destruct Hblk_neq as ((_ & _ & Hneq) & _).
            clear - Hneq.
            intuition.
          }

          split.
          {
            rewrite <- Hload64.
            eapply Mem.load_store_other; eauto.
            left.
            destruct minvalid0 as (_ & _ & Hblk_neq & _).
            unfold block_neq in Hblk_neq.
            destruct Hblk_neq as ((_ & _ & Hneq) & _).
            clear - Hneq.
            intuition.
          }
          split.
          {
            rewrite <- Hload32.
            eapply Mem.load_store_other; eauto.
            left.
            destruct minvalid0 as (_ & _ & Hblk_neq & _).
            unfold block_neq in Hblk_neq.
            destruct Hblk_neq as ((_ & _ & Hneq) & _).
            clear - Hneq.
            intuition.
          }

          intros vj Heval_reg0.
          apply Haxiom.
          rewrite <- Heval_reg0.
          unfold eval_jit_reg; simpl.
          rewrite mregs0; simpl.
          symmetry.
          eapply Mem.load_store_other; eauto.
          left.
          destruct minvalid0 as (_ & _ & Hblk_neq & _).
          unfold block_neq in Hblk_neq.
          destruct Hblk_neq as ((_ & _ & Hneq) & _).
          clear - Hneq.
          intuition.
    Qed.

    Lemma jit_alu32_thumb_upd_load_unchange_match_state_jit:
      forall rbpf_st st0 st1 r v
      (Hst: match_state_jit rbpf_st st0)
      (Hupd_load: jit_alu32_thumb_upd_load r st0 = Some st1)
      (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
      (Hreg_val : eval_reg r rbpf_st = Some (Val.longofintu (Vint v))),
        match_state_jit rbpf_st st1.
    Proof.
      unfold jit_alu32_thumb_upd_load, jit_alu32_thumb_load_store_template_jit.
      intros.
      destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_load].
      rename j into stk.
      eapply upd_jitted_list_unchange_match_state_jit in Hupd as Hmatch; eauto.
      eapply upd_jitted_list_unchange_match_state_jit; eauto.
      eapply upd_jitted_list_unchange_jittted_list in Hupd; eauto.
      rewrite <- Hupd.
      assumption.
    Qed. *)

    Definition arm_registers_load_one (r: reg) (rs0: Asm.regset) (v: int): Asm.regset :=
      (rs0 # (ireg_of_reg r) <- (Vint v)) # PC <-
                    (Val.offset_ptr (rs0 # (ireg_of_reg r) <- (Vint v) PC) wsize).

    Fixpoint arm_registers_load_aux (l: list reg) 
      (rbpf_st: state) (rs0: Asm.regset): option Asm.regset :=
      match l with
      | [] => Some rs0
      | hd :: tl =>
        match eval_reg hd rbpf_st with
        | Some v =>
          match v with
          | Vlong vl =>
            let rs1 := arm_registers_load_one hd rs0 (Int.repr (Int64.unsigned vl)) in
              arm_registers_load_aux tl rbpf_st rs1
          | _ => None
          end
        | None => None
        end
      end.

    Definition arm_registers_load (l: list reg) (rbpf_st: state) (rs0 rs1: Asm.regset): Prop :=
      match arm_registers_load_aux l rbpf_st rs0 with
      | Some rs => rs = rs1
      | None => False
      end.

    Lemma arm_registers_load_unchange_nodup_register:
      forall l r st rs0 rs1
      (Hnoin: ~ In r l)
      (Hload: arm_registers_load l st rs0 rs1),
        rs0 (ireg_of_reg r) = rs1 (ireg_of_reg r).
    Proof.
      unfold arm_registers_load.
      induction l; simpl; intros.
      - simpl in Hload.
        subst rs0.
        f_equal.
      - apply Decidable.not_or in Hnoin.
        destruct Hnoin as (Hneq & Hnoin).
        destruct eval_reg eqn: Heval; [| inversion Hload].
        destruct v; try inversion Hload.
        specialize (IHl r st _ rs1 Hnoin Hload).
        rewrite <- IHl.
        unfold arm_registers_load_one.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Pregmap.gso; [reflexivity | intros HF; inversion HF].
        apply Hneq.
        apply ireg_of_reg_eq; auto.
    Qed.

    Lemma jit_load_one_simulation: forall r l v ls rbpf_st st0 st1 rs_init rs0 m0
      ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hupd_load : jit_alu32_thumb_upd_load r st0 = Some st1)

      (Hlist_not_in: ~ List.In r l)
      (Hlist_not_in1: forall r0, ~List.In r0 ls /\ List.In r0 arm_callee_save_regs ->  (ireg_of_reg r) <> r0)

      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))

      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)
      (Hstack_unchanged: stack_unchanged ls rs_init rs0)
      (Harm: match_state_arm rbpf_st rs_init rs0 m0 l ls st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hreg_rv0: (eval_reg r rbpf_st) = Some (Val.longofintu (Vint v)))
      (Hjit_rv0: Mem.load Mint32 m0 st_blk ((id_of_reg r + 1) * 8) =
        Some (Vint v)) (**r guarantee by the match_state between HAVM and CertrBPF*)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1,
          arm_registers_load_one r rs0 v = rs1 /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          stack_unchanged ls rs_init rs1 /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
           match_state_arm rbpf_st rs_init rs1 m0 (r :: l) ls st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      intros.
      unfold jit_alu32_thumb_upd_load in Hupd_load.
      unfold jit_alu32_thumb_load_store_template_jit in Hupd_load.
      unfold rbpf_state_inv in Hjit_inv.

      destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

      unfold arm_registers_load_one.
      eexists.
      split; [reflexivity | ].

      split.
      { (**r rs IR12 *)
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        - assumption.
        - clear - HF.
          unfold ireg_of_reg; destruct r; inversion HF.
      }

      apply upd_jitted_list_jittted_len_2 in Hupd_load as Hlen_eq.

      assert (Hcond: (2 * jitted_len st0 <= 1000)%nat). {
        simpl.
        unfold upd_jitted_list, upd_jitted_list' in Hupd_load.
        destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond1; [| inversion Hupd_load].
        clear Hupd_load.
        unfold JITTED_LIST_MAX_LENGTH in Hcond1.
        rewrite Nat.leb_le in Hcond1.
        lia.
      }

      assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
        rewrite Ptrofs.unsigned_repr.
        reflexivity.
        change Ptrofs.max_unsigned with 4294967295; lia.
      }

      assert (Hlen_eq1: Ptrofs.unsigned (Ptrofs.add 
          (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.of_int (Int.repr 2))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
        unfold Ptrofs.add.
        change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2))) with 2.
        rewrite Ptrofs.unsigned_repr.
        - rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
        - change Ptrofs.max_unsigned with 4294967295.
          rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
      }

      split.
      { (**r unused callee_save *)
        unfold stack_unchanged in *.
        intros r0 Hin.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        - eapply Hstack_unchanged; eauto.
        - eapply Hlist_not_in1; eauto.
      }

      split.
      { (**r star BinSem.step *)
        eapply star_one.
        eapply exec_step_bin with
          (i := Pldr (ireg_of_reg r) IR12
            (SOimm (Int.add (Int.mul (int_of_reg r) (Int.repr 8)) (Int.repr 8)))) (w := true).
        - (**r find_instr *)
          rewrite Harm_pc.

          assert (Heq: find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) m0 =
            find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st1)). {
            unfold find_instr.
            unfold sub_mem_blk in Hsub_mem.

            eapply upd_jitted_list_2_load with (jit_blk := jit_blk) in Hupd_load as Hload; eauto.
            eapply upd_jitted_list_2_load_2 with (jit_blk := jit_blk) in Hupd_load as Hload2; eauto.

            simpl.
            rewrite Hofs0; simpl.
            rewrite <- Hsub_mem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hlen_eq.
              rewrite Hlen_eq0.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }

            unfold Ptrofs.of_int in Hload.
            rewrite Int.unsigned_repr in Hload.
            2:{ change Int.max_unsigned with 4294967295; lia. }
            rewrite Hload.
            simpl.
            rewrite <- Hsub_mem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hlen_eq.
              rewrite Hlen_eq1.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }
            unfold Ptrofs.of_int in Hload2.
            rewrite Int.unsigned_repr in Hload2.
            2:{ change Int.max_unsigned with 4294967295; lia. }
            unfold Ptrofs.of_int.
            change (Int.unsigned (Int.repr 2)) with 2.
            rewrite Hload2.
            simpl.
            reflexivity.
          }
          rewrite Heq; clear Heq.
          assert (Heq: Int.unsigned (Int.mul (int_of_reg r) (Int.repr 8)) + Int.unsigned (Int.repr 8) =
            (id_of_reg r) * 8 + 8). {
            unfold Int.mul, int_of_reg, id_of_reg.
            change (Int.unsigned (Int.repr 8)) with 8.
            rewrite Int.unsigned_repr.
            - rewrite Int.unsigned_repr; [destruct r; lia |].
              change Int.max_unsigned with 4294967295; destruct r; lia.
            - rewrite Int.unsigned_repr;
                change Int.max_unsigned with 4294967295; destruct r; lia.
          }
          eapply lemma_thumb_ldr; eauto.
          + rewrite Heq.
            unfold id_of_reg; destruct r; lia.
          + unfold Int.add in Hupd_load.
            unfold int_of_ireg.
            change (Z_of_ireg IR12) with 12.
            clear Heq.
            assert (Heq: (int_of_reg r) = (Int.repr (Z_of_ireg (ireg_of_reg r)))).
            { unfold int_of_reg, id_of_reg, Z_of_ireg, ireg_of_reg.
              f_equal.
              destruct r; reflexivity.
            }
            rewrite <- Heq; clear Heq.
            assumption.
        - (**r exec_instr *)
          simpl.
          rewrite Hrs0_12; simpl.
          rewrite Ptrofs.add_zero_l.
          unfold exec_load; simpl.
          rewrite Ptrofs_unsigned_of_int_reg_add_mul_8.
          rewrite Hjit_rv0.
          f_equal.
      }

      { (**r match_state_arm *)
        constructor; try assumption.
        - (**r ra_blk *)
          destruct Hra_blk as (ofs & Hra_blk).
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          exists ofs; assumption.
          clear - HF.
          unfold ireg_of_reg in HF.
          destruct r; inversion HF.
        - (**r regs_agree *)
          unfold regs_agree in *.
          destruct Harm_reg_syn as (Harm_reg_syn & Hnodup).
          split; [ | rewrite NoDup_cons_iff; split; assumption ].

          intros r0 Hin.
          destruct Hin as [Hrs_eq | Hin].
          + subst r0.
            exists v.
            split; [| assumption].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gss.
            f_equal.
          + specialize (Harm_reg_syn r0 Hin).
            assert (Hneq: r0 <> r). {
              intro HF; subst r0.
              apply Hlist_not_in.
              assumption.
            }
            destruct Harm_reg_syn as (vj & Hrs0_eq & Heval_reg0).
            exists vj.
            split; [| assumption].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [assumption | intros HF; inversion HF].
            apply Hneq.
            apply ireg_of_reg_eq.
            assumption.
        - (**r regs_unchanged *)
          unfold regs_unchanged in *.
          intros r0 HT.
          assert (Heq: ~In r0 l). { 
            intros HF; apply HT.
            right; assumption.
          }
          specialize (Harm_reg_same _ Heq).
          destruct Harm_reg_same as (vi & Hload1 & Hload2).
          exists vi.
          split; assumption.
        - (**r arm_synch_stack *)
          unfold arm_synch_stack in Harm_stack_syn.
          unfold arm_synch_stack.
          destruct Harm_stack_syn as (Harm_stack_syn & Hnodup).
          split; [| assumption].
          intros r0 Hin.
          specialize (Harm_stack_syn r0 Hin).
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          assumption.
          clear - HF.
          unfold ireg_of_reg in HF.
          destruct r; inversion HF.
        - (**r rs IR13 *)
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          2:{
            clear - HF.
            unfold ireg_of_reg in HF.
            destruct r; inversion HF.
          }

          split; assumption.

        - (**r rs PC *)
          rewrite Pregmap.gss.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Harm_pc.
          unfold Val.offset_ptr, wsize.
          f_equal.
          eapply add_ofs_4; eauto.

        - (**r Mem.range_perm *)
          split; assumption.
      }
    Qed.


    Lemma jit_load_simulation: forall l1 l ls rbpf_st st0 st1 rs_init rs0 m0
      ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hldr: jit_alu32_load_list l = Some l1)
      (Hjit_load: jit_alu32_thumb_load l1 st0 = Some st1)

      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)
      (Hlist_not_in1: forall r0, ~List.In r0 ls /\ List.In r0 arm_callee_save_regs ->
        (forall r1, List.In r1 l1 -> (ireg_of_reg r1) <> r0))

      (Hstack_unchanged: stack_unchanged ls rs_init rs0)
      (**r match_state between HAVM and CertrBPF: the register maps are corresponding *)
      (Hreg_jit: forall r, exists v, eval_reg r rbpf_st = Some (Val.longofintu (Vint v)) /\
        Mem.load Mint32 m0 st_blk ((id_of_reg r + 1) * 8) = Some (Vint v))

      (Harm: match_state_arm rbpf_st rs_init rs0 m0 [] ls st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1,
          arm_registers_load l1 rbpf_st rs0 rs1 /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          stack_unchanged ls rs_init rs1 /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
          match_state_arm rbpf_st rs_init rs1 m0 l1 ls st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      induction l1; simpl; intros.
      { (**r l = [] *)
        unfold arm_registers_load.
        simpl.
        exists rs0.
        split; [reflexivity | ].
        split.
        { injection Hjit_load as Hst_eq; subst st0; assumption. }
        split; [assumption |].
        split; [econstructor; eauto | ].

        injection Hjit_load as Hst_eq; subst st1.
        rewrite <- Hofs1 in Hofs0.
        subst ofs1.
        assumption.
      }
      (**r l <> [] *)

      apply nodup_load in Hldr as Hnodup_load.

      destruct jit_alu32_thumb_upd_load eqn: Hupd_load in Hjit_load; [| inversion Hjit_load].
      rename j into stk.

      assert (Hexists_l: exists lr, jit_alu32_load_list lr = Some l1). {
        eapply jit_alu32_load_list_tl; eauto.
      }
      destruct Hexists_l as (lr & Hexists_l).
      specialize (IHl1 lr ls rbpf_st stk st1 rs_init).

      destruct jit_alu32_load_list eqn: Hnl1 in Hldr; [| inversion Hldr].
      rename l0 into nl1.

      specialize (Hreg_jit a) as Hreg_jit_eq.
      destruct Hreg_jit_eq as (v & Hreg_val & Hjit_val).

      eapply jit_load_one_simulation with (r := a) (l := []) (v := v) (ls := ls)
        (rbpf_st := rbpf_st) (rs_init := rs_init) (rs0 := rs0) (m0 := m0)
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk)
        (old_sp := old_sp) in Hupd_load as Hone_step; eauto.
      - (**r MAIN *)
        destruct Hone_step as (rsk & Hloadk & Hrs_12k & Hstack_unchangedk & Hstark & Hmatch_statek).
        unfold arm_registers_load_one in Hloadk.

        specialize (IHl1 rsk m0).
        specialize (IHl1  (Z.of_nat (2 * jitted_len stk)) ).
        specialize (IHl1 (Z.of_nat (2 * jitted_len st1)) st_blk jit_blk sp_blk ra_blk old_sp).

        specialize (IHl1 Hexists_l Hjit_load).

        assert (Heq: Z.of_nat (2 * jitted_len stk) = 
                    Z.of_nat (jitted_len stk + (jitted_len stk + 0))) by lia.
        specialize (IHl1 Heq); clear Heq.

        assert (Heq: Z.of_nat (2 * jitted_len st1) =
                      Z.of_nat (jitted_len st1 + (jitted_len st1 + 0))) by lia.
        specialize (IHl1 Heq); clear Heq.

        assert (Heq: sub_mem_blk (jit_mem st1) m0 jit_blk (Z.of_nat (2 * jitted_len stk))
         (Z.of_nat (2 * jitted_len st1))). {
          clear - Hupd_load Hjit_load Hofs0 Hofs1 Hsub_mem Harm Hloadk.
          unfold sub_mem_blk in *.
          intros chunk ofs Hofs.
          erewrite Hsub_mem; eauto.
          unfold jit_alu32_thumb_upd_load, jit_alu32_thumb_load_store_template_jit in Hupd_load.
          eapply upd_jitted_list_jittted_len_2 in Hupd_load; eauto.
          lia.
        }
        specialize (IHl1 Heq); clear Heq.

        unfold rbpf_state_inv in *.
        assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
          clear - Hjit_inv Hupd_load.
          unfold jit_alu32_thumb_upd_load, jit_alu32_thumb_load_store_template_jit in Hupd_load.
          eapply upd_jitted_list_unchange_jittted_list_2 in Hupd_load.
          rewrite <- Hupd_load.
          assumption.
        }
        specialize (IHl1 Heq); clear Heq.

        assert (Heq: (forall r0 : ireg, ~ In r0 ls /\ In r0 arm_callee_save_regs -> 
          forall r1 : reg, In r1 l1 -> ireg_of_reg r1 <> r0)). {
          intros r0 Hin0.
          intros r1 Hin1.
          eapply Hlist_not_in1; eauto.
        }
        specialize (IHl1 Heq Hstack_unchangedk Hreg_jit); clear Heq.

        assert (Heq: match_state_arm rbpf_st rs_init rsk m0 [] ls st_blk jit_blk sp_blk ra_blk
          (Z.of_nat (2 * jitted_len stk)) old_sp). {
          destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

          destruct Hmatch_statek as (Hra_blkk, Harm_reg_synk, Harm_reg_samek, Harm_stack_synk, (Hsp_blkk & Harm_old_spk),
              Harm_pck, Harm_valid_blkk, (Hst_blk_permk & Hsp_blk_permk), Harm_reg_validk).

          constructor; try assumption.
          - (**r regs_agree *)
            split; [| apply NoDup_nil].
            unfold regs_agree.
            intros r HF; inversion HF.
          - (**r rsk IR13 *)
            split; assumption.
          - (**r Mem.range_perm *)
            split; assumption.
        }
        specialize (IHl1 Heq Hrs_12k); clear Heq.

        destruct IHl1 as (rs1 & Hload1 & Hrs_121 & Hstack_unchanged1 & Hstar1 & Hmatch_state_1).

        unfold jit_alu32_thumb_upd_load in Hupd_load.
        unfold jit_alu32_thumb_load_store_template_jit in Hupd_load.
        apply upd_jitted_list_jittted_len_2 in Hupd_load as Hlen_eq.

        assert (Hcond: (2 * jitted_len st0 <= 1000)%nat). {
          simpl.
          unfold upd_jitted_list, upd_jitted_list' in Hupd_load.
          destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond1; [| inversion Hupd_load].
          clear Hupd_load.
          unfold JITTED_LIST_MAX_LENGTH in Hcond1.
          rewrite Nat.leb_le in Hcond1.
          lia.
        }

        assert (Hcond2: ( (2 * (jitted_len st0)) + 4 <= 1000)%nat). {
          simpl.
          unfold upd_jitted_list, upd_jitted_list' in Hupd_load.
          destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond1; [| inversion Hupd_load].
          clear Hupd_load.
          unfold JITTED_LIST_MAX_LENGTH in Hcond1.
          rewrite Nat.leb_le in Hcond1.
          lia.
        }

        assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
            (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
          rewrite Ptrofs.unsigned_repr.
          reflexivity.
          change Ptrofs.max_unsigned with 4294967295; lia.
        }

        exists rs1.
        split.
        { (**r arm_registers_load *)
          unfold arm_registers_load in *.
          simpl.
          rewrite Hreg_val.
          simpl.
          destruct arm_registers_load_aux eqn: Hload_aux; [| inversion Hload1].
          subst r.
          rewrite Int64.int_unsigned_repr.
          rewrite Int.repr_unsigned.
          unfold arm_registers_load_one.
          rewrite Hloadk.
          rewrite Hload_aux.
          reflexivity.
        }

        split; [ (**r rs1 IR12 *) assumption | ].

        split; [ (**r stack_unchanged *) assumption | ].

        split.
        { (**r star BinSem.step *)
          eapply star_trans with (s2 := State rsk m0) (t1 := E0); eauto.
        }

        (**r match_state_arm *)
        simpl in Hmatch_state_1.
        rewrite <- Hofs1 in Hmatch_state_1.

        destruct Hmatch_state_1 as (Hra_blk1, Harm_reg_syn1, Harm_reg_same1, Harm_stack_syn1, (Hsp_blk1 & Harm_old_sp1),
            Harm_pc1, Harm_valid_blk1, (Hst_blk_perm1 & Hsp_blk_perm1), Harm_reg_valid1).

        destruct Hmatch_statek as (Hra_blkk, Harm_reg_synk, Harm_reg_samek, Harm_stack_synk, (Hsp_blkk & Harm_old_spk),
            Harm_pck, Harm_valid_blkk, (Hst_blk_permk & Hsp_blk_permk), Harm_reg_validk).

        constructor; try assumption.
        + (**r regs_agree *)
          unfold regs_agree in *.
          split; [| assumption].
          intros r Hin.
          destruct Hin as [Hr | Hin].
          * subst a.
            assert (Heq: In r [r]) by (simpl; left; reflexivity).
            destruct Harm_reg_synk as (Harm_reg_synk & _).
            specialize (Harm_reg_synk r Heq); clear Heq.
            eapply arm_registers_load_unchange_nodup_register with (r := r) in Hload1; eauto.
            2:{
              apply NoDup_cons_iff in Hnodup_load.
              clear - Hnodup_load.
              intuition.
            }
            rewrite <- Hload1.
            assumption.
          * destruct Harm_reg_syn1 as (Harm_reg_syn1 & _).
            apply Harm_reg_syn1; auto.
        + (**r regs_unchanged *)
          unfold regs_unchanged in *.
          intros r HT.
          eapply Harm_reg_samek; eauto.
          intro HF; apply HT.
          left.
          destruct HF.
          assumption.
          inversion H.
        + (**r rs IR13 *)
          split; assumption.
        + (**r Mem.range_perm *)
          split; assumption.

    - (**r sub_mem_blk *)
      unfold sub_mem_blk in *.
      intros chunk ofs Hrange.
      rewrite <- Hsub_mem.
      2:{
        rewrite Hofs1.

        assert (Heq: (jitted_len stk <= jitted_len st1)%nat). {
          eapply jit_alu32_thumb_load_jitted_len_leb; eauto.
        }
        lia.
      }

      unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_load.
      eapply jit_alu32_thumb_load_load_same; eauto.
      + eapply upd_jitted_list_unchange_jittted_list_2 in Hupd_load; eauto.
        rewrite <- Hupd_load; assumption.
      + eapply upd_jitted_list_jittted_len_2 in Hupd_load; eauto.
        lia.
    Qed.
  End JITLoad.

  Section JITCore.

    Lemma jit_core_one_simulation: forall ins l ls rbpf_st0 rbpf_st1 st0 st1 rs_init rs0 m0
      ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hjit : bpf_alu32_to_thumb ins st0 = Some st1)

      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))

      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)

      (Hlist_not_in1: forall r0, ~List.In r0 ls /\ List.In r0 arm_callee_save_regs ->
        (forall r1, List.In r1 l -> (ireg_of_reg r1) <> r0))

      (Hreg_blk1: regs_st rbpf_st0 = Vptr st_blk (Ptrofs.repr 8)) (**r guaranteed by match_state between HAVM and CertrBPF *)
      (Hstack_unchanged: stack_unchanged ls rs_init rs0)
      (Hst: match_state_arm rbpf_st0 rs_init rs0 m0 l ls st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hins_regs: ins_is_sync_regs ins l)
      (Hstep: rbpf_step rbpf_st0 ins rbpf_st1)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1,
          plus BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          stack_unchanged ls rs_init rs1 /\
          match_state_arm rbpf_st1 rs_init rs1 m0 l ls st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      unfold bpf_alu32_to_thumb.
      intros.
      induction Hstep as [rbpf_st0 a op rd ri ret rbpf_st1].
      unfold rbpf_state_inv in Hjit_inv.
      destruct Hst as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
        Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

      destruct a; [| inversion Hjit].
      destruct ri.
      - (**r alu_reg *)
        unfold regs_agree in Harm_reg_syn.
        unfold arm_synch_stack in Harm_stack_syn.
        destruct Harm_reg_syn as (Harm_reg_syn & Hnodupl).
        destruct Harm_stack_syn as (Harm_stack_syn & Hnodupls).
        unfold ins_is_sync_regs in Hins_regs.
        destruct Hins_regs as (Hins_rd & Hins_r).
        specialize (Harm_reg_syn rd Hins_rd) as Hins_rd_eq.
        specialize (Harm_reg_syn r Hins_r) as Hins_r_eq.
        destruct Hins_rd_eq as (v0 & Hv0_eq & Hreg0).
        destruct Hins_r_eq as (v1 & Hv1_eq & Hreg1).

        unfold bpf_alu32_to_thumb_reg in Hjit.
        destruct op.
        + (**r add_reg *)
          simpl in *.
          unfold eval_alu_binary, eval_src32 in Heval_alu.
          rewrite Hreg0, Hreg1 in Heval_alu.
          injection Heval_alu as Heq.
          subst ret.
          rewrite ! Int64.int_unsigned_repr in Hupd_reg.
          remember Hjit.
          rename e into Hupd.
          clear Heqe.
          unfold upd_jitted_list in Hjit.
          unfold upd_jitted_list' in Hjit.
          destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hmax_jit_size; inversion Hjit.
          clear H0.

          rewrite Hjit_inv in *.
          unfold Val.add, Archi.ptr64, Ptrofs.of_int, Mem.storev in Hjit.
          rewrite Ptrofs.add_zero_l in Hjit.
          erewrite upd_jitted_list_unsigned_repr_int in Hjit; eauto.
          erewrite upd_jitted_list_unsigned_repr in Hjit; eauto.

          destruct Mem.store eqn: Hstore; [| inversion Hjit].

          assert (Hmem_eq: jit_mem st1 = m). {
            clear - Hjit.
            inversion Hjit;
            simpl;
            reflexivity.
          }

          assert (Hjit_ptr_eq: jitted_list st1 = Vptr jit_blk Ptrofs.zero). {
            clear - Hjit.
            inversion Hjit;
            simpl;
            reflexivity.
          }

          assert (Hjit_len_eq: jitted_len st1 = S (jitted_len st0)). {
            clear - Hjit.
            inversion Hjit;
            simpl;
            reflexivity.
          }

          assert (Hcond: (2 * jitted_len st0 <= 1000)%nat). {
            simpl.
            unfold upd_jitted_list, upd_jitted_list' in Hupd.
            destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond1; [| inversion Hupd].
            unfold JITTED_LIST_MAX_LENGTH in Hcond1.
            rewrite Nat.leb_le in Hcond1.
            lia.
          }

          assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
              (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
            rewrite Ptrofs.unsigned_repr.
            reflexivity.
            change Ptrofs.max_unsigned with 4294967295; lia.
          }

          assert (Hlen_eq1: Ptrofs.unsigned (Ptrofs.add 
              (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.of_int (Int.repr 2))) = 
              (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
            unfold Ptrofs.add.
            change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2))) with 2.
            rewrite Ptrofs.unsigned_repr.
            - rewrite Ptrofs.unsigned_repr; [lia |].
              change Ptrofs.max_unsigned with 4294967295; lia.
            - change Ptrofs.max_unsigned with 4294967295.
              rewrite Ptrofs.unsigned_repr; [lia |].
              change Ptrofs.max_unsigned with 4294967295; lia.
          }

          eexists.
          split.
          { (**r we know jitted code is only one *)
            eapply plus_one.
            eapply exec_step_bin with
              (i := Padd (ireg_of_reg rd) (ireg_of_reg rd) (SOreg (ireg_of_reg r))) (w := false).
            - (**r find_instr *)

              rewrite Harm_pc.
              unfold BinDecode.find_instr.

              rewrite Hofs0; simpl.
              subst m.

              unfold sub_mem_blk in Hsub_mem.
              rewrite Hlen_eq0.
              rewrite <- Hsub_mem.
              2:{
                change (size_chunk Mint16unsigned) with 2.
                rewrite Hofs0, Hofs1.
                lia.
              }
              simpl in Hstore.
              erewrite Mem.load_store_same; eauto.
              (**r we know BinDecode.is_thumb2 _ = false in this case *)
              eapply lemma_thumb_add_reg.
            - (**r exec_instr *)
              simpl.
              unfold nextinstr_nf, nextinstr.
              simpl.
              unfold undef_flags.
              rewrite Pregmap.gso; [ | intros HF; inversion HF].
              f_equal.
          }

          split.
          { (**r rs 12 *)
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            - assumption.
            - clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
          }

          split.
          { (**r stack_unchanged *)
            unfold stack_unchanged in *.
            intros Hr0 Hin0.
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            - eapply Hstack_unchanged; eauto.
            - eapply Hlist_not_in1 with (r1 := rd); eauto.
              split.
              + intro HF1.
                destruct Hin0 as (Hin0 & Hin1).
                apply Hin0.
                subst Hr0.
                assumption.
              + subst Hr0. clear - Hin0. intuition.
          }

          (**r match_state_arm *)
          constructor; try assumption.
          { (**r ra_blk *)
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            assumption.
            clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
          }
          { (**r regs_agree *)
            unfold regs_agree.
            split; [ | assumption].
            intros r0 Hin.
            specialize (Harm_reg_syn r0 Hin).
            destruct (reg_eqb r0 rd) eqn: Hreg_eq0;
              [ apply reg_eqb_true in Hreg_eq0 |
                apply reg_eqb_false in Hreg_eq0].
            - subst.
              erewrite ConcreteState.eval_upd_reg_same; eauto.
              rewrite Pregmap.gso.
              2:{ intro HF. inversion HF. }
              rewrite Hv0_eq, Hv1_eq.
              destruct (reg_eqb r rd) eqn: Hreg_eq1;
                [ apply reg_eqb_true in Hreg_eq1 |
                  apply reg_eqb_false in Hreg_eq1].
              + subst.
                rewrite Pregmap.gss.
                simpl.
                eexists.
                split; [reflexivity | ].
                rewrite ! Int.repr_unsigned.
                reflexivity.
              + rewrite Pregmap.gss.
                eexists; split; [reflexivity | ].
                rewrite ! Int.repr_unsigned.
                reflexivity.
            - destruct Harm_reg_syn as (vi & Hrs_eq & Hreg_r0).
              exists vi.
              rewrite Pregmap.gso.
              2:{ intro HF. inversion HF. }
              rewrite Pregmap.gso.
              2:{
                clear - Hreg_eq0.
                intro HF.
                apply Hreg_eq0; unfold ireg_of_reg in HF.
                destruct r0, rd; inversion HF; reflexivity.
              }
              split; [assumption | ].
              unfold Val.longofintu; simpl.
              rewrite <- Hreg_r0.
              symmetry.
              eapply ConcreteState.eval_upd_reg_other; eauto.
          }
          { (**r regs_unchanged *)
            unfold regs_unchanged in *.
            intros r0 HT.
            specialize (Harm_reg_same _ HT).
            destruct Harm_reg_same as (vi & Hload1 & Hload2).
            exists vi.
            split; [| assumption].
            rewrite <- Hload1.
            unfold upd_reg, upd_reg' in Hupd_reg.
            rewrite Hreg_blk1 in Hupd_reg.
            unfold Mem.storev, Val.add, Archi.ptr64 in Hupd_reg.
            destruct Mem.store eqn: Hstore_reg in Hupd_reg; [| inversion Hupd_reg].
            assert (Heq: (bpf_m rbpf_st1) = m1). {
              injection Hupd_reg as Heq.
              rewrite <- Heq.
              auto.
            }
            rewrite Heq.
            eapply Mem.load_store_other; eauto.
            right.
            change (size_chunk Mint32) with 4.
            unfold Ptrofs.add, Ptrofs.of_int.
            change (Ptrofs.unsigned (Ptrofs.repr 8)) with 8.
            rewrite Int.unsigned_repr.
            2:{ change Int.max_unsigned with 4294967295; destruct rd; simpl; lia. }
            rewrite Ptrofs.unsigned_repr with (z := 8 * id_of_reg rd).
            2:{ change Ptrofs.max_unsigned with 4294967295; destruct rd; simpl; lia. }
            rewrite Ptrofs.unsigned_repr.
            2:{ change Ptrofs.max_unsigned with 4294967295; destruct rd; simpl; lia. }
            change (size_chunk Mint64) with 8.
            clear - HT Hins_rd.
            unfold id_of_reg; destruct r0; destruct rd; simpl; try lia.
            all: exfalso; apply HT; assumption.
          }
          { (**r arm_synch_stack *)
            unfold arm_synch_stack in *.
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            split; [| assumption].
            intros r0 Hin.
            specialize (Harm_stack_syn r0 Hin).
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            - eapply Harm_stack_syn; eauto.
            - clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
          }
          { (**r rs IR13 *)
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            - split; assumption.
            - clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
          }
          { (**r rs PC *)
            rewrite Pregmap.gss.
            rewrite Harm_pc.
            unfold Val.offset_ptr.
            f_equal.
            unfold isize.
            eapply add_ofs_2; eauto.
          }
          { (**r Mem.range_perm *)
            split; assumption.
          }
        all: admit. (**r the most of the rest part is trivial, but div and shifts need an additional invariant as precondition TODO *)
    Admitted.

    Lemma jit_core_simulation: forall l rbpf_st0 rbpf_st1
      (Hstep: rbpf_sem rbpf_st0 l rbpf_st1)
      l1 l2 ls st0 st1 rs_init rs0 m0
      ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp

      (Hldr: jit_alu32_load_list l = Some l1)
      (Hldr_sub: list_subset l1 l2)
      (Hjit: jit_core l st0 = Some st1)


      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Hmem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)
      (Hreg_blk1: regs_st rbpf_st0 = Vptr st_blk (Ptrofs.repr 8))


      (Hlist_not_in1: forall r0, ~List.In r0 ls /\ List.In r0 arm_callee_save_regs ->
        (forall r1, List.In r1 l2 -> (ireg_of_reg r1) <> r0))
      (Hstack_unchanged: stack_unchanged ls rs_init rs0)
      (Harm: match_state_arm rbpf_st0 rs_init rs0 m0 l2 ls st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
      exists rs1,
        star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
        rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          stack_unchanged ls rs_init rs1 /\
        match_state_arm rbpf_st1 rs_init rs1 m0 l2 ls st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      unfold rbpf_state_inv.
      induction 1 as [ | rbpf_st0 rbpf_stk rbpf_st1 hd tl Hone_step Hplus_step IH].
      { (**r l = [] *)
        intros.
        injection Hjit as Hjit_eq; subst st1.
        exists rs0.
        split; [ apply star_refl | ].

        injection Hldr as Hlsr_eq; subst l1.
        rewrite <- Hofs0 in Hofs1.
        subst ofs1.
        split; [assumption |].
        split; assumption.
      }

      (**r l = hd :: tl *)
      simpl.
      intros.
      destruct bpf_alu32_to_thumb eqn: Hone_ins; [| inversion Hjit].
      rename j into stk.

      assert (Hstk_ptr: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
        clear - Hone_ins Hjit_inv.
        rewrite <- Hjit_inv.
        symmetry.
        eapply bpf_alu32_to_thumb_unchange_jittted_list; eauto.
      }

      destruct jit_alu32_load_list eqn: Hldr_eq; [| inversion Hldr].

      induction Hone_step as [rbpf_st0 a op rd ri ret rbpf_stk].

      destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

      specialize (IH l l2 ls stk st1 rs_init).

      remember (Z.of_nat (jitted_len stk + (jitted_len stk + 0))) as ofsk.
      rename Heqofsk into Hofsk.

      eapply jit_core_one_simulation with
        (ins := BPF_BINARY a op rd ri) (ofs0 := ofs0) (ofs1 := ofsk)
        (st0 := st0) (st1 := stk) (l := l2)(ls := ls)
        (rbpf_st0 := rbpf_st0) (rbpf_st1 := rbpf_stk)
        (rs_init := rs_init) (rs0 := rs0) (m0 := m0) (sp_blk := sp_blk) (old_sp := old_sp)
      in Hone_ins as Hstk_eq; eauto.

      2:{ (**r sub_mem *)
        clear - Hofs0 Hofs1 Hofsk Hstk_ptr Hjit Hmem.
        assert (Hle: Z.of_nat (2 * jitted_len stk) <= Z.of_nat (2 * jitted_len st1)). {
          clear - Hjit.
          eapply jit_core_jitted_len; eauto.
        }

        eapply jit_core_sub_mem in Hjit; eauto.
        subst ofs1 ofsk.
        assert (Heq: sub_mem_blk (jit_mem stk) (jit_mem st1) jit_blk ofs0 (Z.of_nat (2 * jitted_len stk))). {
          clear - Hofs0 Hjit.
          unfold sub_mem_blk in *.
          intros chunk ofs Hofs.
          eapply Hjit; eauto.
          split; [lia |].
          destruct Hofs as (_ & Hofs).
          assumption.
        }
        eapply sub_mem_blk_less_than with (low1 := ofs0) (high1 := Z.of_nat (2 * jitted_len stk)) in Hmem; eauto.
        - simpl in *.
          eapply sub_mem_blk_trans; eauto.
        - simpl in *; split; lia.
      }
      2:{ (**r match_state_arm *)
        constructor; try assumption; try (split; assumption).
        eapply Hra_blk; eauto.
      }

      2:{ (**r ins_is_sync_regs *)
        clear - Hldr Hldr_sub.
        destruct jit_alu32_load_aux_list eqn: Haux; inversion Hldr.
        unfold ins_is_sync_regs.
        unfold jit_alu32_load_aux_list in Haux.
        destruct a; inversion Haux.
        clear H1.
        subst l1.

        destruct ri.
        - assert (Hin: List.In rd l0 /\ List.In r l0). {
            clear - Haux.
            destruct op; inversion Haux; simpl; auto.
            all: destruct reg_eqb eqn: Heq;
            [ apply reg_eqb_true in Heq;
              injection Haux as Hreg_eq; rewrite <- Hreg_eq, Heq; intuition |
              inversion Haux; intuition ].
          }
          clear Haux.
          destruct Hin.
          split; eapply list_subset_app_no_repeat_in_l; eauto.
        - assert (Hin: List.In rd l0). {
            clear - Haux.
            destruct op; inversion Haux; simpl; auto.
          }
          clear Haux.
          split; try eapply list_subset_app_no_repeat_in_l; eauto.
      }

      2:{ (**r rbpf_step *)
        econstructor; eauto.
      }

      destruct Hstk_eq as (rsk & Hplus & Hpre_weak & Hstack_unchangedk & Hmatch_state).

      specialize (IH rsk m0 (Z.of_nat (2 * jitted_len stk)) ofs1 st_blk jit_blk sp_blk ra_blk old_sp).

      assert (Heq: Some l = Some l) by reflexivity.
      specialize (IH Heq); clear Heq.

      assert (Hsub: list_subset l l2). {
        clear - Hldr Hldr_sub.
        destruct jit_alu32_load_aux_list eqn: Haux; inversion Hldr.
        subst l1.
        unfold list_subset in *.
        intros.
        apply Hldr_sub.
        eapply list_subset_app_no_repeat_in_r with (l0 := l0); eauto.
        apply list_subset_refl.
      }
      specialize (IH Hsub Hjit).

      assert (Heq: Z.of_nat (2 * jitted_len stk) = Z.of_nat (2 * jitted_len stk)) by reflexivity.
      specialize (IH Heq Hofs1); clear Heq.

      assert (Heq: sub_mem_blk (jit_mem st1) m0 jit_blk (Z.of_nat (2 * jitted_len stk)) ofs1). {
        unfold sub_mem_blk in *.
        intros chunk ofs Hofs.
        erewrite Hmem; eauto.
        apply bpf_alu32_to_thumb_jittted_len in Hone_ins.
        destruct Hofs as (Hofs & Hofs').
        split; lia.
      }
      specialize (IH Heq); clear Heq.

      assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
        eapply bpf_alu32_to_thumb_unchange_jittted_list in Hone_ins; eauto.
      }
      specialize (IH Heq); clear Heq.

      assert (Heq: regs_st rbpf_stk = Vptr st_blk (Ptrofs.repr 8)). {
        clear - Hupd_reg Hreg_blk1.
        unfold upd_reg in Hupd_reg.
        destruct upd_reg'; inversion Hupd_reg.
        simpl.
        assumption.
      }
      specialize (IH Heq); clear Heq.

      assert (Heq: (forall r0 : ireg, ~ In r0 ls /\ In r0 arm_callee_save_regs -> 
        forall r1 : reg, In r1 l2 -> ireg_of_reg r1 <> r0)). {
        intros r0 Hin0 r1 Hin1.
        eapply Hlist_not_in1; eauto.
      }
      specialize (IH Heq Hstack_unchangedk); clear Heq.

      rewrite Hofsk in Hmatch_state.
      change (Z.of_nat (jitted_len stk + (jitted_len stk + 0))) with (Z.of_nat (2 * jitted_len stk)) in Hmatch_state.
      specialize (IH Hmatch_state Hpre_weak).
(*
      assert (Hmatch1: match_state_arm rbpf_stk rsk m0 old_rs l2 l3 jit_blk (Z.of_nat (2 * jitted_len stk)) st_final). {
        clear - Hofs0 Hofsk Hldr Hone_ins Hjit Hmatch_state.

        induction Hmatch_state as [l2 l3 rbpf_stk rsk old_rs st_final jit_blk ofsk].
        eapply exec_step; eauto.
        rewrite Hofsk in HPC_eq.
        rewrite HPC_eq.
        f_equal.
      } *)

      destruct IH as (rs1 & Hstar & Hrs1_12 & Hstack_unchanged2 & Hmatch2).

      destruct Hmatch2 as (Hra_blk1, Harm_reg_syn1, Harm_reg_same1, Harm_stack_syn1, (Hsp_blk1 & Harm_old_sp1),
        Harm_pc1, Harm_valid_blk1, (Hst_blk_perm1 & Hsp_blk_perm1), Harm_reg_valid1).

      destruct Hmatch_state as (Hra_blkk, Harm_reg_synk, Harm_reg_samek, Harm_stack_synk, (Hsp_blkk & Harm_old_spk),
        Harm_pck, Harm_valid_blkk, (Hst_blk_permk & Hsp_blk_permk), Harm_reg_validk).

      exists rs1; split.
      + eapply star_trans with (s2 := (State rsk m0)) (t1 := E0); eauto.
        eapply plus_star; eauto.
      + split; [assumption | ].
        split; [assumption | ].
         constructor; try assumption; try (split; assumption).
  Qed.

  End JITCore.

  Section JITStore.

    Lemma jit_store_one_simulation: forall l ls r rbpf_st0 st0 st1 rs_init rs0 m0
      ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hupd_store : jit_alu32_thumb_upd_store r st0 = Some st1)

      (Hlist_not_in: ~ List.In r l)
      (Hnodup: NoDup l)
      (**r TODO: this should be guarantee by core stage *)

      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)

      (Hlist_not_in1: forall r0, ~List.In r0 ls /\ List.In r0 arm_callee_save_regs ->
        (forall r1, List.In r1 l -> (ireg_of_reg r1) <> r0))

      (Hstack_unchanged: stack_unchanged ls rs_init rs0)
      (Harm: match_state_arm rbpf_st0 rs_init rs0 m0 (r :: l) ls st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hvint_r: exists vi, Vint vi = rs0 (ireg_of_reg r))
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1 m1,
          arm_registers_store_one r rs0 m0 = Some m1 /\
          rs1 = rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          stack_unchanged ls rs_init rs1 /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
          match_state_arm rbpf_st0 rs_init rs1 m1 l ls st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit.
      unfold rbpf_state_inv.

      intros.
      exists (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)).

      destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

      unfold arm_registers_store_one.
      rewrite Hrs0_12.
      simpl.
      rewrite Ptrofs.add_zero_l.
      unfold Ptrofs.of_intu, Ptrofs.of_int.
      rewrite Int.unsigned_repr.
      2:{
        change Int.max_unsigned with 4294967295;
        unfold id_of_reg; destruct r; simpl; lia.
      }
      rewrite Ptrofs.unsigned_repr.
      2:{
        change Ptrofs.max_unsigned with 4294967295;
        unfold id_of_reg; destruct r; simpl; lia.
      }

      destruct Hvint_r as (vi & Hrs0_r_eq).
      rewrite <- Hrs0_r_eq.

      assert (Heq: exists m1,
        Mem.store Mint32 m0 st_blk ((id_of_reg r + 1) * 8) (Vint vi) = Some m1). {
        clear - Hst_blk_perm.
        assert (Heq: Mem.valid_access m0 Mint32 st_blk ((id_of_reg r + 1) * 8) Writable). {
          unfold Mem.valid_access.
          split.
          - unfold Mem.range_perm in *.
            intros ofs Hrange.
            apply Hst_blk_perm.
            clear - Hrange.
            change (size_chunk Mint32) with 4 in *.
            unfold id_of_reg in *; destruct r; lia.
          - change (align_chunk Mint32) with 4.
            replace ((id_of_reg r + 1) * 8) with ((id_of_reg r + 1) * 2 * 4).
            apply Z.divide_factor_r.
            unfold id_of_reg in *; destruct r; lia.
        }
        apply Mem.valid_access_store with (v := Vint vi) in Heq.
        destruct Heq as (m2 & Hstore).
        exists m2; assumption.
      }
      destruct Heq as (m1 & Hstore).
      exists m1.


      assert (Hst0_eq: S (S (jitted_len st0)) = jitted_len st1). {
        clear - Hupd_store.
        eapply upd_jitted_list_jittted_len_2; eauto.
      }

      assert (Hle: Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH). {
        clear - Hupd_store.
        destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_store].
        eapply upd_jitted_list_max; eauto.
      }

      (**r Mem.store *)
      split; [assumption | ].
      (**r rs0 # PC *)
      split; [reflexivity | ].

      split.
      { (**r rs0 # IR12 *)
        rewrite Pregmap.gso; [| intro HF; inversion HF].
        assumption.
      }

      unfold JITTED_LIST_MAX_LENGTH in Hle.
      assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
        rewrite Ptrofs.unsigned_repr.
        reflexivity.
        change Ptrofs.max_unsigned with 4294967295; lia.
      }

      assert (Hlen_eq1: Ptrofs.unsigned (Ptrofs.add 
          (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.of_int (Int.repr 2))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
        unfold Ptrofs.add.
        change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2))) with 2.
        rewrite Ptrofs.unsigned_repr.
        - rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
        - change Ptrofs.max_unsigned with 4294967295.
          rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
      }

      split.
      { (**r stack_unchanged *)
        unfold stack_unchanged in *.
        intros r0 Hin0.
        rewrite Pregmap.gso; [ | intro HF; inversion HF].
        eapply Hstack_unchanged; eauto.
      }

      split.
      { (**r star BinSem.step *)

        eapply star_one.
        eapply exec_step_bin with (w := true)
          (i := Pstr (ireg_of_reg r) IR12 (SOimm (Int.repr ((id_of_reg r + 1) * 8)))).
        - (**r find_instr *)
          rewrite Harm_pc.

          assert (Heq: find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) m0 =
            find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st1)). {
            unfold find_instr.
            unfold sub_mem_blk in Hsub_mem.

            eapply upd_jitted_list_2_load with (jit_blk := jit_blk) in Hupd_store as Hload; eauto.
            eapply upd_jitted_list_2_load_2 with (jit_blk := jit_blk) in Hupd_store as Hload2; eauto.

            simpl.
            rewrite Hofs0; simpl.
            rewrite <- Hsub_mem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hst0_eq.
              rewrite Hlen_eq0.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }

            unfold Ptrofs.of_int in Hload.
            rewrite Int.unsigned_repr in Hload.
            2:{ change Int.max_unsigned with 4294967295; lia. }
            rewrite Hload.
            simpl.
            rewrite <- Hsub_mem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hst0_eq.
              rewrite Hlen_eq1.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }
            unfold Ptrofs.of_int in Hload2.
            rewrite Int.unsigned_repr in Hload2.
            2:{ change Int.max_unsigned with 4294967295; lia. }
            unfold Ptrofs.of_int.
            change (Int.unsigned (Int.repr 2)) with 2.
            rewrite Hload2.
            simpl.
            reflexivity.
          }
          rewrite Heq.
          eapply lemma_thumb_str; eauto.

          + unfold id_of_reg.
            destruct r; simpl; lia.
          + clear - Hupd_store.
            unfold int_of_ireg.
            change (Z_of_ireg IR12) with 12.
            assert (Heq: (int_of_reg r) = (Int.repr (Z_of_ireg (ireg_of_reg r)))).
            { unfold int_of_reg, id_of_reg, Z_of_ireg, ireg_of_reg.
              f_equal.
              destruct r; reflexivity.
            }
            rewrite <- Heq; clear Heq.
            assert (Heq: Int.add (Int.mul (int_of_reg r) (Int.repr 8)) (Int.repr 8) = 
              Int.repr ((id_of_reg r + 1) * 8)). {
              unfold Int.add, Int.mul.
              f_equal.
              change (Int.unsigned (Int.repr 8)) with 8.
              unfold int_of_reg, id_of_reg; rewrite Int.unsigned_repr.
              - rewrite Int.unsigned_repr; [lia | ].
                change Int.max_unsigned with 4294967295; destruct r; simpl; lia.
              - rewrite Int.unsigned_repr;
                change Int.max_unsigned with 4294967295; destruct r; simpl; lia.
            }
            rewrite <- Heq.
            assumption.
        - (**r exec_instr *)
          simpl.
          rewrite Hrs0_12; simpl.
          rewrite Ptrofs.add_zero_l.
          unfold exec_store; simpl.
          unfold Ptrofs.of_int.
          rewrite Int.unsigned_repr.
          2:{ unfold id_of_reg.
            change Int.max_unsigned with 4294967295; destruct r; simpl; lia.
          }
          rewrite Ptrofs.unsigned_repr.
          2:{ unfold id_of_reg.
            change Ptrofs.max_unsigned with 4294967295; destruct r; simpl; lia.
          }
          rewrite <- Hrs0_r_eq.
          rewrite Hstore.
          f_equal.
      }

      { (**r match_state_arm *)

        constructor; try assumption.
        + (**r regs_agree *)
          unfold regs_agree in Harm_reg_syn.
          unfold regs_agree.
          destruct Harm_reg_syn as (Harm_reg_syn & _).
          split; [| assumption].
          intros r0 Hin.
          assert (Heq: In r0 (r :: l)) by (right; assumption).
          specialize (Harm_reg_syn r0 Heq); clear Heq.
          rewrite Pregmap.gso; [ | intro HF; inversion HF].
          assumption.
        + (**r regs_unchanged *)
          unfold regs_unchanged in *.
          intros r0 HT.
          destruct (reg_eqb r r0) eqn: Heq; [ rewrite <- reg_eqb_true in Heq | rewrite <- reg_eqb_false in Heq].
          {
            subst r0.
            exists vi.
            unfold regs_agree in Harm_reg_syn.
            destruct Harm_reg_syn as (Harm_reg_syn & _).
            assert (Heq: In r (r :: l)) by (left; reflexivity).
            specialize (Harm_reg_syn _ Heq).
            destruct Harm_reg_syn as (vj & Heq1 & Heq2).
            rewrite Heq1 in Hrs0_r_eq.
            injection Hrs0_r_eq as Hr_eq; subst vj.
            eapply llvm_enable_alu32 in Heq2; eauto.
            split. apply Heq2.
            replace (8 * id_of_reg r + 8) with ((id_of_reg r + 1) * 8).
            erewrite Mem.load_store_same; eauto.
            f_equal.
            destruct r; simpl; lia.
          }
          assert (Hneq: ~ In r0 (r :: l)). {
            intros HF.
            apply HT.
            destruct HF as [Hneq | Hin]; [| assumption].
            exfalso; apply Heq. assumption.
          }
          specialize (Harm_reg_same _ Hneq).
          destruct Harm_reg_same as (vj & Hload1 & Hload2).
          exists vj.
          split; [assumption | ].
          rewrite <- Hload2.
          erewrite Mem.load_store_other; eauto.
          right.
          change (size_chunk Mint32) with 4.
          clear - Heq.
          destruct r0; destruct r; simpl; try lia.
          all: exfalso; apply Heq; reflexivity.
        + (**r arm_synch_stack *)
          unfold arm_synch_stack in *.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          destruct Harm_stack_syn as (Harm_stack_syn & Hnodup_ls).
          split; [| assumption].

          rewrite Hsp_blk in *.

          intros r0 Hin.
          simpl.
          specialize (Harm_stack_syn r0 Hin).
          rewrite <- Harm_stack_syn.
          rewrite Ptrofs.add_zero_l.
          rewrite Hreg_mul4_unsigned.
          simpl.
          rewrite Ptrofs.add_zero_l.
          rewrite Hreg_mul4_unsigned.
          erewrite Mem.load_store_other; eauto.
          left.
          clear - Harm_valid_blk.
          intuition.

        + (**r rs IR13 *)
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          split; [assumption | ].
          rewrite Hsp_blk in *.
          rewrite <- Harm_old_sp.
          simpl.
          erewrite Mem.load_store_other; eauto.
          left.
          clear - Harm_valid_blk.
          intuition.

        + (**r rs PC *)
          rewrite Harm_pc.
          rewrite Pregmap.gss.
          unfold Val.offset_ptr.
          f_equal.
          unfold wsize.
          eapply add_ofs_4; eauto.
          lia.

        + (**r Harm_valid_blk *)
          destruct Harm_valid_blk as (Hneq & Hvalid0 & Hvalid1 & Hvalid2).
          split; [assumption | ].
          repeat split; (eapply Mem.store_valid_block_1; eauto).
        + (**r Mem.range_perm *)
          unfold Mem.range_perm in *.
          clear - Hst_blk_perm Hsp_blk_perm Hstore.
          split; intros ofs Hofs; eapply Mem.perm_store_1; eauto.
      }
    Qed.

    Lemma jit_store_simulation: forall l0 l ls rbpf_st st0 st1 rs_init rs0 m0 ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hstr: jit_alu32_store_list l = Some l0) (*
      (Hcomple: complementary_reg_list l0 L = l2)
      (HnoemptyL: L <> [])
      (HnodupL: NoDup L) *)
      (Hjit_store : jit_alu32_thumb_store l0 st0 = Some st1)

      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)


      (Hlist_not_in1: forall r0, ~List.In r0 ls /\ List.In r0 arm_callee_save_regs ->
        (forall r1, List.In r1 l0 -> (ireg_of_reg r1) <> r0))
      (Hstack_unchanged: stack_unchanged ls rs_init rs0)
      (Harm: match_state_arm rbpf_st rs_init rs0 m0 l0 ls st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1 m1,
          arm_registers_store l0 rs0 rs1 m0 m1 /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          stack_unchanged ls rs_init rs1 /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
          match_state_arm rbpf_st rs_init rs1 m1 [] ls st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      induction l0; simpl; intros.
      { (**r l = [] *)
        unfold arm_registers_store.
        simpl.
        exists rs0. exists m0.
        split; [split; reflexivity | ].
        split; [assumption | ].
        split; [assumption | ].
        injection Hjit_store as Heq.
        subst st1.
        split; [econstructor; eauto | ].

        rewrite <- Hofs1 in Hofs0.
        subst ofs1.
        assumption.
      }
      (**r l <> [] *)

      apply nodup_store in Hstr as Hnodup_store.

      destruct jit_alu32_thumb_upd_store eqn: Hupd_store in Hjit_store; [| inversion Hjit_store].
      rename j into stk.

      assert (Hexists_l: exists lr, jit_alu32_store_list lr = Some l0). {
        eapply jit_alu32_store_list_tl; eauto.
      }
      destruct Hexists_l as (lr & Hexists_l).
      specialize (IHl0 lr ls rbpf_st stk st1 rs_init).

      destruct jit_alu32_store_list eqn: Hnl1 in Hstr; [| inversion Hstr].
      rename l0 into nl1.


      destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

      assert (Hst0_eq: S (S (jitted_len st0)) = jitted_len stk). {
        clear - Hupd_store.
        unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit in Hupd_store.
        eapply upd_jitted_list_jittted_len_2; eauto.
      }

      assert (Hle: Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH). {
        clear - Hupd_store.
        unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit in Hupd_store.
        destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_store].
        eapply upd_jitted_list_max; eauto.
      }
      unfold JITTED_LIST_MAX_LENGTH in Hle.

      assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
        rewrite Ptrofs.unsigned_repr.
        reflexivity.
        change Ptrofs.max_unsigned with 4294967295; lia.
      }

      assert (Hlen_eq1: Ptrofs.unsigned (Ptrofs.add 
          (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.of_int (Int.repr 2))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
        unfold Ptrofs.add.
        change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2))) with 2.
        rewrite Ptrofs.unsigned_repr.
        - rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
        - change Ptrofs.max_unsigned with 4294967295.
          rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
      }
      unfold rbpf_state_inv in *.

      eapply jit_store_one_simulation with
        (l := nl1) (ls := ls)
        (rbpf_st0 := rbpf_st) (rs_init := rs_init)
        (rs0 := rs0) (m0 := m0) (ofs0 := ofs0) (ofs1 := Z.of_nat (2 * jitted_len stk))
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk) (old_sp := old_sp) in Hupd_store as Hone_step; eauto.

      6:{ (**r rs0 (ireg_of_reg a) *)
        clear - Harm_reg_syn.
        unfold regs_agree in Harm_reg_syn.
        assert (Hin: In a (a :: nl1)) by (left; reflexivity).
        destruct Harm_reg_syn as (Harm_reg_syn & _).
        specialize (Harm_reg_syn _ Hin).
        destruct Harm_reg_syn as (vi & Heq & _).
        exists vi; rewrite Heq; reflexivity.
      }

      - (**r MAIN *)
        destruct Hone_step as (rsk & mk & Hstorek & Hrsk_eq & Hrsk_12 & Hstack_unchangedk & Hstark & Hmatch_statek).

        unfold arm_registers_store_one in Hstorek.

        rewrite Hrs0_12 in *.
        simpl in Hstorek.
        rewrite Ptrofs.add_zero_l in Hstorek.
        unfold Ptrofs.of_intu, Ptrofs.of_int in Hstorek.
        rewrite Int.unsigned_repr in Hstorek.
        2:{ unfold id_of_reg.
          change Int.max_unsigned with 4294967295; destruct a; simpl; lia.
        }
        rewrite Ptrofs.unsigned_repr in Hstorek.
        2:{ unfold id_of_reg.
          change Ptrofs.max_unsigned with 4294967295; destruct a; simpl; lia.
        }

        specialize (IHl0 rsk mk).
        specialize (IHl0  (Z.of_nat (2 * jitted_len stk)) ).
        specialize (IHl0 (Z.of_nat (2 * jitted_len st1)) st_blk jit_blk sp_blk ra_blk old_sp).
        specialize (IHl0 Hexists_l Hjit_store).

        assert (Heq: Z.of_nat (2 * jitted_len stk) = Z.of_nat (2 * jitted_len stk)) by auto.
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: Z.of_nat (2 * jitted_len st1) = Z.of_nat (2 * jitted_len st1)) by auto.
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: sub_mem_blk (jit_mem st1) mk jit_blk (Z.of_nat (2 * jitted_len stk)) (Z.of_nat (2 * jitted_len st1))). {
          unfold sub_mem_blk in *.
          intros chunk ofs Hofs.
          destruct Hofs as (Hofs & Hofs').
          erewrite Hsub_mem; eauto.
          - symmetry.
            erewrite Mem.load_store_other; eauto.
            left.
            clear - Harm_valid_blk; intuition.
          - rewrite Hofs1.
            split; lia.
        }
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
          unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit in Hupd_store.
          eapply upd_jitted_list_unchange_jittted_list_2 in Hupd_store; eauto.
          rewrite <- Hupd_store; assumption.
        }
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: (forall r0 : ireg, ~ In r0 ls /\ In r0 arm_callee_save_regs ->
          forall r1 : reg, In r1 nl1 -> ireg_of_reg r1 <> r0)). {
          intros r0 Hin0 r Hin1.
          eapply Hlist_not_in1; eauto.
        }
        specialize (IHl0 Heq Hstack_unchangedk); clear Heq.

        destruct Hmatch_statek as (Hra_blkk, Harm_reg_synk, Harm_reg_samek, Harm_stack_synk, (Hsp_blkk & Harm_old_spk),
                Harm_pck, Harm_valid_blkk, (Hst_blk_permk & Hsp_blk_permk), Harm_reg_validk).

        assert (Heq: match_state_arm rbpf_st rs_init rsk mk nl1 ls st_blk jit_blk sp_blk ra_blk
         (Z.of_nat (2 * jitted_len stk)) old_sp). {
          constructor; try assumption; try (split; assumption).
          eapply Hra_blkk; eauto.
        }
        specialize (IHl0 Heq Hrsk_12); clear Heq.

        destruct IHl0 as (rs1 & m1 & Hstore1 & Hrs1_12 & Hstack_unchanged1 & Hstar1 & Harm1).
        destruct Harm1 as (Hra_blk1, Harm_reg_syn1, Harm_reg_same1, Harm_stack_syn1, (Hsp_blk1 & Harm_old_sp1),
                Harm_pc1, Harm_valid_blk1, (Hst_blk_perm1 & Hsp_blk_perm1), Harm_reg_valid1).

        exists rs1, m1.
        split.
        { (**r arm_registers_store *)
          unfold arm_registers_store in *.
          simpl.
          unfold arm_registers_store_one.
          rewrite Hrs0_12 in *.
          simpl.
          rewrite Ptrofs.add_zero_l.
          unfold Ptrofs.of_intu, Ptrofs.of_int.
          rewrite Int.unsigned_repr.
          2:{ unfold id_of_reg.
            change Int.max_unsigned with 4294967295; destruct a; simpl; lia.
          }
          rewrite Ptrofs.unsigned_repr.
          2:{ unfold id_of_reg.
            change Ptrofs.max_unsigned with 4294967295; destruct a; simpl; lia.
          }
          rewrite Hstorek.
          rewrite <- Hrsk_eq.
          assumption.
        }

        split; [(**r rs1 IR12 *) assumption | ].
        split; [(**r stack_unchanged *) assumption | ].
        split.
        { (**r star BinSem.step *)
          eapply star_trans with (s2 := State rsk mk) (t1 := E0); eauto.
        }

        (**r match_state_arm *)
        constructor; try assumption; try (split; assumption).
        rewrite Harm_pc1.
        f_equal.
        rewrite Hofs1.
        simpl; f_equal.

      - (**r ~ In a nl1 *)
        clear - Hnodup_store.
        rewrite NoDup_cons_iff in *.
        intuition.
      - (**r NoDup nl1 *)
        clear - Hnodup_store.
        rewrite NoDup_cons_iff in *.
        intuition.
      - (**r sub_mem_blk *)
        unfold sub_mem_blk in *.
        intros chunk ofs Hrange.
        rewrite <- Hsub_mem.
        2:{
          rewrite Hofs1.

          assert (Heq: (jitted_len stk <= jitted_len st1)%nat). {
            eapply jit_alu32_thumb_store_jitted_len_leb; eauto.
          }
          lia.
        }

        unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit in Hupd_store.
        eapply jit_alu32_thumb_store_load_same; eauto.
        + eapply upd_jitted_list_unchange_jittted_list_2 in Hupd_store; eauto.
          rewrite <- Hupd_store; assumption.
        + eapply upd_jitted_list_jittted_len_2 in Hupd_store; eauto.
          lia.
      - (**r match_state_arm *)
        constructor; try assumption; try (split; assumption).
    Qed.
  End JITStore.

  Section JITReloading.

    Definition arm_registers_reloading_one (r: ireg) (rs0: Asm.regset) (m: mem): option Asm.regset :=
      match Mem.loadv Mint32 m (Val.offset_ptr (rs0 IR13)
        (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4)))) with
      | Some v => Some ((rs0 # r <- v) # PC <- (Val.offset_ptr (rs0 # r <- v PC) wsize))
      | None => None
      end.

    Fixpoint arm_registers_reloading_aux (l: list ireg) (rs0: Asm.regset) (m: mem): option Asm.regset :=
      match l with
      | [] => Some rs0
      | hd :: tl =>
        match arm_registers_reloading_one hd rs0 m with
        | Some rs => arm_registers_reloading_aux tl rs m
        | None => None
        end
      end.

    Definition arm_registers_reloading (l: list ireg) (rs0 rs1: Asm.regset) (m: mem): Prop :=
      match arm_registers_reloading_aux l rs0 m with
      | Some rs => rs = rs1
      | None => False
      end.

    Definition callee_save_used_sync (lsr_stack: sync_iregs) (rs0 rs1: Asm.regset): Prop :=
      forall r, List.In r lsr_stack -> rs0 r =  rs1 r.


    Lemma jit_reloading_one_simulation: forall r lu ls rbpf_st st0 st1 rs_init rs0 m0
      ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hupd_load : jit_alu32_thumb_upd_reset r st0 = Some st1)

      (Hlist_not_in: ~ List.In r ls /\ ~ List.In r lu /\ List.In r arm_callee_save_regs)
      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))

      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)

      (Hcallee_used: callee_save_used_sync lu rs_init rs0)

      (Harm: match_state_arm rbpf_st rs_init rs0 m0 [] (r :: ls) st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1,
          arm_registers_reloading_one r rs0 m0 = Some rs1 /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          callee_save_used_sync (r :: lu) rs_init rs1 /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
           match_state_arm rbpf_st rs_init rs1 m0 [] ls st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      unfold jit_alu32_thumb_upd_reset, jit_alu32_thumb_load_store_template_jit.
      unfold arm_registers_reloading_one, rbpf_state_inv.
      intros.
      destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

      destruct Harm_stack_syn as (Harm_stack_syn & Hnodup_ls).
      unfold arm_synch_stack in Harm_stack_syn.

      rewrite Hsp_blk in *.
      simpl.
      assert (Heq: In r (r :: ls)) by (left; reflexivity).
      specialize (Harm_stack_syn _ Heq) as Hload_sp; clear Heq.
      simpl in Hload_sp.
      rewrite Ptrofs.add_zero_l in *.
      rewrite Hreg_mul4_unsigned in *.
      rewrite Hload_sp.
      eexists.
      split; [ (**r rs1 *) reflexivity |].

      split.
      { (**r rs IR12 = Vptr st_blk Ptrofs.zero *)
        rewrite Pregmap.gso; [| intros HF; inversion HF].
        (**r here we need know r is a callee-save register *)
        clear - Hlist_not_in Hrs0_12.
        rewrite Pregmap.gso; [assumption |].
        intros HF; inversion HF.
        subst r.
        destruct Hlist_not_in as (_ & _ & Hin).
        repeat (destruct Hin as [Hin | Hin]; [inversion Hin |]).
        inversion Hin.
      }

      split.
      { (**r callee_save_used_sync *)
        unfold callee_save_used_sync in *.
        intros r0 Hin.
        destruct Hin as [Hr_eq | Hin].
        - subst r0.
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          rewrite Pregmap.gss.
          f_equal.
        - rewrite Hcallee_used; auto.
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          rewrite Pregmap.gso; [f_equal | intros HF; inversion HF].
          subst r0.
          destruct Hlist_not_in as (_ & Hnin & _).
          apply Hnin; assumption.
      }

      assert (Hst0_eq: S (S (jitted_len st0)) = jitted_len st1). {
        eapply upd_jitted_list_jittted_len_2; eauto.
      }

      assert (Hle: (2 * jitted_len st0 <= JITTED_LIST_MAX_LENGTH)%nat). {
        destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_load].
        eapply upd_jitted_list_max in Hupd; eauto.
        lia.
      }
      unfold JITTED_LIST_MAX_LENGTH in Hle.

      assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
        rewrite Ptrofs.unsigned_repr.
        reflexivity.
        change Ptrofs.max_unsigned with 4294967295; lia.
      }

      assert (Hlen_eq1: Ptrofs.unsigned (Ptrofs.add 
          (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.of_int (Int.repr 2))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
        unfold Ptrofs.add.
        change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2))) with 2.
        rewrite Ptrofs.unsigned_repr.
        - rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
        - change Ptrofs.max_unsigned with 4294967295.
          rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
      }

      split.
      { (**r star BinSem.step *)
        eapply star_one.
        eapply exec_step_bin with (i := Pldr r IR13 (SOimm (Int.mul (int_of_ireg r) (Int.repr 4)))) (w := true).
        - (**r find_instr *)
          rewrite Harm_pc.

          assert (Heq: find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) m0 =
            find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st1)). {
            unfold find_instr.
            unfold sub_mem_blk in Hsub_mem.

            eapply upd_jitted_list_2_load with (jit_blk := jit_blk) in Hupd_load as Hload; eauto.
            eapply upd_jitted_list_2_load_2 with (jit_blk := jit_blk) in Hupd_load as Hload2; eauto.

            simpl.
            rewrite Hofs0; simpl.
            rewrite <- Hsub_mem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hst0_eq.
              rewrite Hlen_eq0.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }

            unfold Ptrofs.of_int in Hload.
            rewrite Int.unsigned_repr in Hload.
            2:{ change Int.max_unsigned with 4294967295; lia. }
            rewrite Hload.
            simpl.
            rewrite <- Hsub_mem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hst0_eq.
              rewrite Hlen_eq1.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }
            unfold Ptrofs.of_int in Hload2.
            rewrite Int.unsigned_repr in Hload2.
            2:{ change Int.max_unsigned with 4294967295; lia. }
            unfold Ptrofs.of_int.
            change (Int.unsigned (Int.repr 2)) with 2.
            rewrite Hload2.
            simpl.
            reflexivity.
          }
          rewrite Heq.
          eapply lemma_thumb_ldr; eauto.

          change (Int.unsigned (Int.repr 4)) with 4.
          unfold int_of_ireg.
          rewrite Int.unsigned_repr;
            [| change Int.max_unsigned with 4294967295];
            unfold Z_of_ireg; destruct r; simpl; lia.
        - (**r exec_instr *)
          simpl.
          rewrite Hsp_blk; simpl.
          rewrite Ptrofs.add_zero_l.
          unfold exec_load; simpl.
          rewrite Hreg_mul4_unsigned.
          rewrite Hload_sp.
          f_equal.
      }

      (**r match_state_arm *)
      constructor; try assumption; try (split; assumption).
      - (**r ra_blk *)
        rewrite Pregmap.gso; [| intros HF; inversion HF].
        rewrite Pregmap.gso; [| intros HF; inversion HF].
        assumption.

        subst r.
        clear - Hlist_not_in.
        destruct Hlist_not_in as (_ & _ & Hin).
        repeat (destruct Hin as [Hin | Hin]; [inversion Hin |]).
        inversion Hin.
      - (**r regs_agree *)
        unfold regs_agree in *.
        destruct Harm_reg_syn as (Harm_reg_syn & Hnodupl).
        split; [| assumption].
        intros ro Hin.
        inversion Hin.
      - (**r arm_synch_stack *)
        unfold arm_synch_stack.
        split.
        + intros r0 Hin.
          assert (Heq: In r0 (r :: ls)) by (right; assumption).
          specialize (Harm_stack_syn _ Heq); clear Heq.
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          rewrite Hsp_blk; simpl.
          simpl in Harm_stack_syn.
          rewrite Ptrofs.add_zero_l in *.
          rewrite Hreg_mul4_unsigned in *.
          assumption.

          subst r.
          clear - Hlist_not_in.
          destruct Hlist_not_in as (_ & _ & Hin).
          repeat (destruct Hin as [Hin | Hin]; [inversion Hin |]).
          inversion Hin.

        + (**r NoDup ls *)
          rewrite NoDup_cons_iff in Hnodup_ls.
          destruct Hnodup_ls as (_ & Hnodup_ls).
          assumption.

      - (**r rs IR13 *)
        rewrite Pregmap.gso; [| intros HF; inversion HF].
        rewrite Pregmap.gso; [| intros HF; inversion HF].
        split; [assumption |].
        rewrite Hsp_blk; assumption.

        subst r.
        clear - Hlist_not_in.
        destruct Hlist_not_in as (_ & _ & Hin).
        repeat (destruct Hin as [Hin | Hin]; [inversion Hin |]).
        inversion Hin.

      - (**r rs PC *)
        rewrite Pregmap.gss.
        rewrite Pregmap.gso; [| intros HF; inversion HF].
        rewrite Harm_pc.
        unfold Val.offset_ptr, wsize; simpl.
        f_equal.
        eapply add_ofs_4; eauto.
    Qed.

    Lemma arm_registers_reloading_nodup:
      forall l rs0 rs1 m r
        (Hreloading : arm_registers_reloading l rs0 rs1 m)
        (Hnodup : NoDup (r :: l)),
          rs0 r = rs1 r.
    Proof.
      unfold arm_registers_reloading; induction l; simpl; intros.
      - subst rs0; f_equal.
      - unfold arm_registers_reloading_one in Hreloading.
        destruct Mem.loadv eqn: Hone; [| inversion Hreloading].
        specialize (IHl _ _ _ r Hreloading).
        rewrite <- IHl.
        + rewrite Pregmap.gso; [| intros HF; inversion HF].
          rewrite Pregmap.gso; [f_equal | intros HF; inversion HF].
          rewrite NoDup_cons_iff in *.
          destruct Hnodup as (Hnin & Hnodup).
          apply Hnin.
          left.
          auto.
        + rewrite NoDup_cons_iff in *.
          destruct Hnodup as (Hnin & Hnodup).
          rewrite NoDup_cons_iff in *.
          destruct Hnodup as (Hnin1 & Hnodup).
          split.
          * intro HF; apply Hnin.
            right; assumption.
          * assumption.
    Qed.

    Lemma jit_reloading_simulation: forall ls rbpf_st st0 st1 rs_init rs0 m0
      ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
      (Hupd_load : jit_alu32_thumb_reset ls st0 = Some st1)

      (Hnodup: NoDup ls)
      (Harm_callee_save: forall r, List.In r ls -> List.In r arm_callee_save_regs)
      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))

      (Hsub_mem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
      (Hjit_inv: rbpf_state_inv st0 jit_blk)
(*
      (Hsubset: list_subset lt ls) *)
      (Hcallee_used: callee_save_used_sync [] rs_init rs0)

      (Harm: match_state_arm rbpf_st rs_init rs0 m0 [] ls st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
      (Hrs0_12: (rs0 IR12) = (Vptr st_blk Ptrofs.zero)),
        exists rs1,
          arm_registers_reloading ls rs0 rs1 m0 /\
          rs1 IR12 = Vptr st_blk Ptrofs.zero /\
          callee_save_used_sync ls rs_init rs1 /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
           match_state_arm rbpf_st rs_init rs1 m0 [] [] st_blk jit_blk sp_blk ra_blk ofs1 old_sp.
    Proof.
      induction ls; intros.
      { (**r l = [] *)
        simpl in *.
        injection Hupd_load as Hst_eq.
        subst st0.
        exists rs0.
        split.
        { unfold arm_registers_reloading.
          simpl.
          reflexivity.
        }
        split; [assumption | ].
        split; [assumption | ].
        split; [ apply star_refl | ].
        rewrite <- Hofs0 in Hofs1.
        subst ofs1.
        assumption.
      }

      simpl in Hupd_load.
      destruct jit_alu32_thumb_upd_reset eqn: Hone_reload; [| inversion Hupd_load].
      rename j into stk.

      assert (Hle: (2 * jitted_len st0 <= JITTED_LIST_MAX_LENGTH)%nat). {
        clear - Hone_reload.
        unfold jit_alu32_thumb_upd_reset, jit_alu32_thumb_load_store_template_jit in Hone_reload.
        destruct upd_jitted_list eqn: Hupd; [| inversion Hone_reload].
        eapply upd_jitted_list_max in Hupd; eauto.
        lia.
      }
      unfold JITTED_LIST_MAX_LENGTH in Hle.

      eapply jit_reloading_one_simulation with (r := a) (lu := []) (ls := ls)
        (rbpf_st := rbpf_st) (rs_init := rs_init) (rs0 := rs0) (m0 := m0)
        (old_sp := old_sp) (sp_blk := sp_blk)
        (ofs0 := ofs0) (ofs1 := Z.of_nat (2 * jitted_len stk))
        in Hone_reload as Hone; eauto.

      - (**r Main *)
        destruct Hone as (rsk & Honek & Hrsk_12 & Hcalleek & Hstark & Harmk).
        specialize (IHls rbpf_st stk st1 rs_init rsk m0 (Z.of_nat (2 * jitted_len stk)) ofs1 st_blk jit_blk sp_blk ra_blk old_sp).
        specialize (IHls Hupd_load).

        assert (Heq: NoDup ls). {
          rewrite NoDup_cons_iff in Hnodup.
          destruct Hnodup as (_ & Hnodup).
          assumption.
        }
        specialize (IHls Heq); clear Heq.

        assert (Heq: forall r : ireg, In r ls -> In r arm_callee_save_regs). {
          intros r Hin.
          eapply Harm_callee_save; eauto.
          right; assumption.
        }
        specialize (IHls Heq); clear Heq.

        assert (Heq: Z.of_nat (2 * jitted_len stk) = Z.of_nat (2 * jitted_len stk)) by f_equal.
        specialize (IHls Heq Hofs1); clear Heq.

        assert (Heq: sub_mem_blk (jit_mem st1) m0 jit_blk (Z.of_nat (2 * jitted_len stk)) ofs1). {
          unfold sub_mem_blk in *.
          intros chunk ofs Hofs.
          erewrite Hsub_mem; eauto.
          split; [| lia].
          unfold jit_alu32_thumb_upd_reset, jit_alu32_thumb_load_store_template_jit in Hone_reload.
          eapply upd_jitted_list_jittted_len_2 in Hone_reload; eauto.
          lia.
        }
        specialize (IHls Heq); clear Heq.

        assert (Heq: rbpf_state_inv stk jit_blk). {
          unfold rbpf_state_inv in *.
          unfold jit_alu32_thumb_upd_reset, jit_alu32_thumb_load_store_template_jit in Hone_reload.
          eapply upd_jitted_list_unchange_jittted_list_2 in Hone_reload; eauto.
          rewrite <- Hone_reload.
          assumption.
        }
        specialize (IHls Heq); clear Heq.

        assert (Heq: callee_save_used_sync [] rs_init rsk). {
          unfold callee_save_used_sync.
          intros r HF; inversion HF.
        }
        specialize (IHls Heq); clear Heq.

        destruct Harmk as (Hra_blkk, Harm_reg_synk, Harm_reg_samek, Harm_stack_synk, (Hsp_blkk & Harm_old_spk),
              Harm_pck, Harm_valid_blkk, (Hst_blk_permk & Hsp_blk_permk), Harm_reg_validk).

        assert (Heq: match_state_arm rbpf_st rs_init rsk m0 [] ls st_blk jit_blk sp_blk ra_blk
         (Z.of_nat (2 * jitted_len stk)) old_sp). {
          constructor; try assumption; try (split; assumption).
        }
        specialize (IHls Heq Hrsk_12); clear Heq.

        destruct IHls as (rs1 & Hreloading1 & Hrs1_12 & Hcallee1 & Hstar1 & Harm1).

        destruct Harm1 as (Hra_blk1, Harm_reg_syn1, Harm_reg_same1, Harm_stack_syn1, (Hsp_blk1 & Harm_old_sp1),
              Harm_pc1, Harm_valid_blk1, (Hst_blk_perm1 & Hsp_blk_perm1), Harm_reg_valid1).

        exists rs1.
        split.
        { (**r arm_registers_reloading *)
          unfold arm_registers_reloading in *.
          simpl.
          rewrite Honek.
          assumption.
        }

        split; [(**r rs1 IR12*) assumption | ].
        split.
        { (**r callee_save_used_sync *)
          clear - Hreloading1 Hcallee1 Hcalleek Hnodup.
          unfold callee_save_used_sync in *; simpl.
          intros r Hin.
          destruct Hin as [Hr_eq | Hin].
          - subst a.
            rewrite Hcalleek; auto; [| left; reflexivity].
            clear - Hreloading1 Hnodup.
            eapply arm_registers_reloading_nodup; eauto.
          - eapply Hcallee1; eauto.
        }

        split; [ (**r star BinSem.step *) eapply star_trans; eauto | ].

        (**r match_state_arm *)
        constructor; try assumption; try (split; assumption).

      - (**r arm_callee_save_regs *)
        split.
        + rewrite NoDup_cons_iff in Hnodup.
          destruct Hnodup as (Hnodup & _).
          assumption.
        + split.
          * intro HF; inversion HF.
          * eapply Harm_callee_save; eauto.
            left; reflexivity.
      - (**r sub_mem_blk *)
        unfold sub_mem_blk in Hsub_mem.
        unfold sub_mem_blk.
        intros chunk ofs2 Hrange.
        specialize (Hsub_mem chunk ofs2).
        assert (Heq: ofs0 <= ofs2 /\ ofs2 + size_chunk chunk <= ofs1). {
          assert (Heq: (jitted_len stk <= jitted_len st1)%nat). {
            eapply jit_alu32_thumb_reset_jitted_len_leb; eauto.
          }
          lia.
        }
        specialize (Hsub_mem Heq); clear Heq.

        rewrite <- Hsub_mem.

        eapply jit_alu32_thumb_reset_load_same; eauto.
        unfold jit_alu32_thumb_upd_reset, jit_alu32_thumb_load_store_template_jit in Hone_reload.
        eapply upd_jitted_list_unchange_jittted_list_2 in Hone_reload; eauto.
        rewrite <- Hone_reload; assumption.
        lia.
    Qed.
  End JITReloading.

  Section JITPost.


    Section MatchStateJITARMWeak. (**r removing all old_sp info *)

    Record match_state_arm_weak (st: state) (old_rs rs: Asm.regset) (m: mem)
      (lr: sync_regs) (ls: sync_iregs)
      (st_blk jit_blk sp_blk: block) (ofs: Z): Prop := {
      arm_reg_syn_weak:    regs_agree lr st rs /\ NoDup lr; (*
      arm_stack_syn_weak:  arm_synch_stack ls old_rs rs m old_sp /\ NoDup ls;
      arm_old_sp:     rs IR13 = Vptr sp_blk Ptrofs.zero /\ Mem.loadv Mint32 m (rs IR13) = Some old_sp;
      arm_pc_weak:         rs PC = Vptr jit_blk (Ptrofs.repr ofs); *)
      arm_valid_blk_weak:  (st_blk <> jit_blk (*/\ st_blk <> sp_blk /\ jit_blk <> sp_blk *)) /\
                      Mem.valid_block m jit_blk /\ Mem.valid_block m st_blk (*/\ Mem.valid_block m sp_blk *);
      arm_blk_perm_weak:   Mem.range_perm m st_blk 0 96 Cur Writable; (* /\
                      Mem.range_perm m sp_blk 0 48 Cur Writable; *)
      arm_reg_valid_weak:  forall (arm_rs: Asm.regset) r, (exists vi, arm_rs r = Vint vi) \/ (exists b o, arm_rs r = Vptr b o)
    }.
  End MatchStateJITARMWeak.

    Definition arm_registers_post (rs0 rs1: Asm.regset) (m: mem): Prop :=
      match Mem.loadv Mint32 m (rs0 IR13) with
      | Some v =>  rs1 = (nextinstr true rs0 # IR13 <- v) # PC <- ((nextinstr_nf true rs0 # IR13 <- v) IR14)
      | _ => False
      end.

    Lemma jit_post_simulation: forall rbpf_st st0 st1 rs0 m0 ofs0 ofs1 st_blk jit_blk sp_blk ra_blk old_sp
    (Hjit_post: jit_alu32_post st0 = Some st1)
    (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
    (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
    (Hmem: sub_mem_blk (jit_mem st1) m0 jit_blk ofs0 ofs1)
    (Hjit_inv: rbpf_state_inv st0 jit_blk)
    (Harm: match_state_arm rbpf_st rs0 rs0 m0 [] [] st_blk jit_blk sp_blk ra_blk ofs0 old_sp)
    (Hrs_1: rs0 IR12 = Vptr st_blk Ptrofs.zero),
      exists rs1,
        arm_registers_post rs0 rs1 m0 /\
        rs1 IR12 = Vptr st_blk Ptrofs.zero /\
        rs1 PC = rs1 IR14 (**r RA *) /\
        rs1 IR13 = old_sp /\
        star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
        match_state_arm_weak rbpf_st rs0 rs1 m0 [] [] st_blk jit_blk sp_blk ofs1. (**r is_final_state should do `Pfreeframe` *)
    Proof.
      intros.
      unfold arm_registers_post.

      destruct Harm as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, Harm_old_sp,
        Harm_pc, Harm_valid_blk, Harm_blk_perm, arm_reg_valid).
      destruct Harm_old_sp as (Hsp_blk & Harm_old_sp).
      rewrite Harm_old_sp.
      eexists.
      split; [(**r arm_registers_pre *) reflexivity | ].

      split.
      { (**r rs1 IR12 = Vptr st_blk Ptrofs.zero *)
        unfold nextinstr_nf, nextinstr, undef_flags.
        repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]).
        assumption.
      }

      split.
      { (**r rs1 PC = rs1 IR14 *)
        unfold nextinstr_nf, nextinstr, undef_flags.
        rewrite Pregmap.gss.
        repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]).
        f_equal.
      }

      split.
      { (**r rs1 IR13 = old_sp *)
        unfold nextinstr_nf, nextinstr, undef_flags.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Pregmap.gss.
        reflexivity.
      }

      unfold jit_alu32_post, jit_alu32_thumb_load_store_template_jit in Hjit_post.

      destruct upd_jitted_list eqn: Hldr1; [| inversion Hjit_post].
      rename j into stk.

      destruct upd_jitted_list eqn: Hldr2 in Hjit_post; [| inversion Hjit_post].
      rename j into stn.
      apply upd_jitted_list_jittted_len in Hldr1 as Hlen_eq1.
      apply upd_jitted_list_jittted_len in Hldr2 as Hlen_eq2.
      apply upd_jitted_list_jittted_len in Hjit_post as Hlen_eq3.


      assert (Hcond: (2 * jitted_len st0 <= 1000)%nat). {
        simpl.
        apply upd_jitted_list_max in Hldr1.
        unfold JITTED_LIST_MAX_LENGTH in Hldr1.
        lia.
      }

      assert (Hlen_eq0: Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))). {
        rewrite Ptrofs.unsigned_repr.
        reflexivity.
        change Ptrofs.max_unsigned with 4294967295; lia.
      }

      assert (Hlen_eq4: Ptrofs.unsigned (Ptrofs.add 
          (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.of_int (Int.repr 2))) = 
          (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
        unfold Ptrofs.add.
        change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 2))) with 2.
        rewrite Ptrofs.unsigned_repr.
        - rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
        - change Ptrofs.max_unsigned with 4294967295.
          rewrite Ptrofs.unsigned_repr; [lia |].
          change Ptrofs.max_unsigned with 4294967295; lia.
      }

      split.
      { (**r star BinSem.step *)
        eapply star_two; eauto.
        - eapply exec_step_bin with (i := Pldr IR13 IR13 (SOimm Int.zero)) (w := true); eauto.
          + (**r find_instr *)
            rewrite Harm_pc.

            unfold Int.zero in *.

            erewrite <- lemma_thumb_ldr with (st1 := stn); eauto.

            2:{ lia. }
            2:{ rewrite Hldr1.
              rewrite Hldr2.
              f_equal.
            }

            unfold find_instr.
            rewrite Hofs0; simpl.
            unfold sub_mem_blk in Hmem.
            rewrite <- Hmem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hlen_eq3.
              rewrite <- Hlen_eq2.
              rewrite Hlen_eq0.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }

            rewrite <- Hmem.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hlen_eq3.
              rewrite <- Hlen_eq2.
              rewrite Hlen_eq4.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            }

            unfold rbpf_state_inv in *.
            assert (Heq1: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
              eapply upd_jitted_list_unchange_jittted_list in Hldr1; eauto.
              rewrite <- Hldr1; assumption.
            }

            assert (Heq2: jitted_list stn = Vptr jit_blk Ptrofs.zero). {
              eapply upd_jitted_list_unchange_jittted_list in Hldr2; eauto.
              rewrite <- Hldr2; assumption.
            }

            assert (Heq3: jitted_list st1 = Vptr jit_blk Ptrofs.zero). {
              eapply upd_jitted_list_unchange_jittted_list in Hjit_post; eauto.
              rewrite <- Hjit_post; assumption.
            }

            assert (Heq: Mem.load Mint16unsigned (jit_mem stn) jit_blk
                (Ptrofs.unsigned
                   (Ptrofs.repr
                      (Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0))))))) = 
            Mem.load Mint16unsigned (jit_mem st1) jit_blk
              (Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))))). {
              unfold upd_jitted_list, upd_jitted_list' in Hjit_post.
              destruct (2 * jitted_len _ + 4 <=? JITTED_LIST_MAX_LENGTH)%nat; [| inversion Hjit_post].
              rewrite Heq2 in Hjit_post.
              simpl in Hjit_post.
              rewrite Ptrofs.add_zero_l in Hjit_post.
              destruct Mem.store eqn: Hstore; [| inversion Hjit_post].
              assert (Heq: (jit_mem st1) = m). {
                injection Hjit_post as Heq.
                rewrite <- Heq.
                auto.
              }
              rewrite Heq.
              symmetry.
              erewrite Mem.load_store_other; eauto.
              rewrite Ptrofs.repr_unsigned.
              f_equal.
              right.
              rewrite <- Hlen_eq2.
              rewrite <- Hlen_eq1.
              change (size_chunk Mint16unsigned) with 2.
              simpl in Hcond.
              rewrite Ptrofs.unsigned_repr.
              2:{ change Ptrofs.max_unsigned with 4294967295; lia. }
              unfold Ptrofs.of_int.
              rewrite Ptrofs.unsigned_repr.
              2:{ rewrite Int.unsigned_repr.
                change Ptrofs.max_unsigned with 4294967295; lia.
                change Int.max_unsigned with 4294967295; lia.
              }
              rewrite Int.unsigned_repr.
              2:{ change Int.max_unsigned with 4294967295; lia. }
              lia.
            }
            rewrite Heq; clear Heq.

            assert (Heq: Mem.load Mint16unsigned (jit_mem stn) jit_blk
             (Ptrofs.unsigned
                (Ptrofs.add
                   (Ptrofs.repr
                      (Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0))))))
                   (Ptrofs.of_int (Int.repr 2)))) =
            Mem.load Mint16unsigned (jit_mem st1) jit_blk
             (Ptrofs.unsigned
                (Ptrofs.add (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0))))
                   (Ptrofs.of_int (Int.repr 2))))). {
              unfold upd_jitted_list, upd_jitted_list' in Hjit_post.
              destruct (2 * jitted_len _ + 4 <=? JITTED_LIST_MAX_LENGTH)%nat; [| inversion Hjit_post].
              rewrite Heq2 in Hjit_post.
              simpl in Hjit_post.
              rewrite Ptrofs.add_zero_l in Hjit_post.
              destruct Mem.store eqn: Hstore; [| inversion Hjit_post].
              assert (Heq: (jit_mem st1) = m). {
                injection Hjit_post as Heq.
                rewrite <- Heq.
                auto.
              }
              rewrite Heq.
              symmetry.
              erewrite Mem.load_store_other; eauto.
              rewrite Ptrofs.repr_unsigned.
              f_equal.
              right.
              rewrite <- Hlen_eq2.
              rewrite <- Hlen_eq1.
              change (size_chunk Mint16unsigned) with 2.
              simpl in Hcond.
              unfold Ptrofs.add, Ptrofs.of_int.
              change (Ptrofs.unsigned (Ptrofs.repr (Int.unsigned (Int.repr 2)))) with 2.
              rewrite Ptrofs.unsigned_repr.
              2:{ change Ptrofs.max_unsigned with 4294967295.
                rewrite Ptrofs.unsigned_repr. lia.
                change Ptrofs.max_unsigned with 4294967295; lia.
              }
              rewrite Ptrofs.unsigned_repr.
              2:{ change Ptrofs.max_unsigned with 4294967295; lia. }
              rewrite Int.unsigned_repr.
              2:{ change Int.max_unsigned with 4294967295; lia. }
              rewrite Ptrofs.unsigned_repr.
              2:{ change Ptrofs.max_unsigned with 4294967295; lia. }
              lia.
            }
            rewrite Heq; clear Heq.
            reflexivity.

          + (**r exec_instr *)
            simpl.
            rewrite Hsp_blk in *.
            unfold exec_load; simpl.
            change (Ptrofs.unsigned (Ptrofs.add Ptrofs.zero (Ptrofs.of_int Int.zero))) with 0.
            simpl in Harm_old_sp.
            change (Ptrofs.unsigned Ptrofs.zero) with 0 in Harm_old_sp.
            rewrite Harm_old_sp.
            f_equal.

        - eapply exec_step_bin with (i := Pbreg IR14) (w := false); eauto.
          + (**r find_instr *)
            unfold nextinstr.
            rewrite Pregmap.gss.
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Harm_pc.
            unfold Val.offset_ptr, wsize.

            unfold find_instr; simpl.
            unfold sub_mem_blk in Hmem.
            rewrite <- Hmem.
            rewrite Hofs0; simpl.
            2:{
              rewrite Hofs1, Hofs0.
              rewrite <- Hlen_eq3.
              rewrite <- Hlen_eq2.
              change (size_chunk Mint16unsigned) with 2.
              unfold Ptrofs.add.
              change (Ptrofs.unsigned (Ptrofs.repr 4)) with 4.
              rewrite Ptrofs.unsigned_repr.
              2:{
                change Ptrofs.max_unsigned with 4294967295.
                rewrite Ptrofs.unsigned_repr. lia.
                change Ptrofs.max_unsigned with 4294967295; lia.
              }
              rewrite Ptrofs.unsigned_repr.
              2:{
                change Ptrofs.max_unsigned with 4294967295; lia.
              }
              lia.
            }

            assert (Heq: (Ptrofs.unsigned
              (Ptrofs.add (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))) (Ptrofs.repr 4))) = 
              Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (2 *(jitted_len stn))))). {
              simpl.
              rewrite <- Hlen_eq2.
              rewrite <- Hlen_eq1.

              unfold Ptrofs.add.
              change (Ptrofs.unsigned (Ptrofs.repr 4)) with 4.
              rewrite Ptrofs.unsigned_repr.
              2:{
                change Ptrofs.max_unsigned with 4294967295.
                rewrite Ptrofs.unsigned_repr.
                2:{ change Ptrofs.max_unsigned with 4294967295; lia. }
                lia.
              }
              rewrite Ptrofs.unsigned_repr.
              2:{ change Ptrofs.max_unsigned with 4294967295; lia. }
              rewrite Ptrofs.unsigned_repr.
              2:{ change Ptrofs.max_unsigned with 4294967295; lia. }
              lia.
            }
            rewrite Heq.
            remember (Ptrofs.add (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0))))
                  (Ptrofs.repr 4)) as k.
            unfold Ptrofs.add.
            rewrite Heq; subst k.
            clear Heq.

            eapply lemma_thumb_breg; eauto.

            unfold rbpf_state_inv in *.
            assert (Heq1: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
              eapply upd_jitted_list_unchange_jittted_list in Hldr1; eauto.
              rewrite <- Hldr1; assumption.
            }

            assert (Heq2: jitted_list stn = Vptr jit_blk Ptrofs.zero). {
              eapply upd_jitted_list_unchange_jittted_list in Hldr2; eauto.
              rewrite <- Hldr2; assumption.
            }

            assumption.
        - rewrite E0_right; reflexivity.
      }

      constructor; try assumption.
      - (**r regs_agree *)
        destruct Harm_reg_syn as (Harm_reg_syn & Hnodup).
        split; [| assumption].
        unfold regs_agree.
        intros r Hin; inversion Hin.

      - (**r Mem.valid_block *)
        destruct Harm_valid_blk as (Hneq & Hvalid0 & Hvalid1 & Hvalid2).
        split; intuition.
      - (**r Mem.range_perm *)
        destruct Harm_blk_perm as (Harm_blk_perm & _); assumption.
    Qed.
  End JITPost.

  Section JITCorrectness. (*
    Definition relation_interpreter (rbpf_st: state) (jit_st: jit_state): Prop :=
      forall r, exists vi,
        eval_reg r rbpf_st = Some (Val.longofintu (Vint vi)) /\
        eval_jit_reg r jit_st = Some (Val.longofintu (Vint vi)). *)

    Definition relation_interpreter (m0 m1: mem) (st_blk: block): Prop := (**r m0 = bpf_m rbpf_st /\ m1 = jit_mem jit_st *)
      forall r : reg, exists vi : int,
        Mem.load Mint32 m0 st_blk (8 * id_of_reg r + 8) = Some (Vint vi) /\
        Mem.load Mint32 m1 st_blk (8 * id_of_reg r + 8) = Some (Vint vi).

    Theorem jit_correctness:
      forall l rbpf_st0 rbpf_st1 jit_st jit_st0 (rs0: Asm.regset) m0 ofs jit_blk st_blk ra_blk ra_ofs old_sp sp_blk
        (Hjit: jit_alu32_to_thumb l jit_st = Some jit_st0)
        (Hst: relation_interpreter (bpf_m rbpf_st0) (jit_mem jit_st0) st_blk)
        (Hinit_state: rs0 IR0 = Vptr jit_blk ofs /\ rs0 PC = Vptr jit_blk ofs /\
          rs0 IR1 = Vptr st_blk Ptrofs.zero /\ rs0 IR14 = Vptr ra_blk ra_ofs /\
          rs0 IR13 = Vptr sp_blk Ptrofs.zero /\ Mem.loadv Mint32 m0 (rs0 IR13) = Some old_sp /\
          (m0, sp_blk) = Mem.alloc (jit_mem jit_st0) 0 48)
        (Hofs_eq: (Ptrofs.repr (Z.of_nat (2 * jitted_len jit_st))) = ofs) (**r compute by JIT_ALU32 *)
        (Hinv: (st_blk <> jit_blk /\ st_blk <> sp_blk /\ jit_blk <> sp_blk) /\
            Mem.valid_block m0 jit_blk /\ Mem.valid_block m0 st_blk /\
            Mem.range_perm m0 st_blk 0 96 Cur Writable) (**r Mem_INV *)
        (Hreg_inv: regs_st rbpf_st0 = Vptr st_blk (Ptrofs.repr 8))
        (Hjit_inv: jitted_list jit_st = Vptr jit_blk Ptrofs.zero) (*
        (Hverifier_inv: bpf_verifier_for_compcert l = true) (**r an invariant for the unverified rBPF verifier, we will use it when we do div and shift instructions *) *)
        (Hrbpf_sem: rbpf_sem rbpf_st0 l rbpf_st1),
          exists rs1 m1,
            star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
            relation_interpreter (bpf_m rbpf_st1) m1 st_blk. (**r JIT state shares the memory with ARM *)
    Proof.
      unfold relation_interpreter, jit_alu32_to_thumb.
      intros.

      destruct jit_alu32_load_list eqn: Hload_list; [| inversion Hjit].
      rename l0 into l_load.
      destruct jit_alu32_store_list eqn: Hstore_list; [| inversion Hjit].
      rename l0 into l_store.
      destruct jit_alu32_pre eqn: Hjit_pre; [| inversion Hjit].
      rename j into st_pre.
      destruct jit_alu32_thumb_save eqn: Hjit_save; [| inversion Hjit].
      rename j into st_save.
      destruct jit_alu32_thumb_load eqn: Hjit_load; [| inversion Hjit].
      rename j into st_load.
      destruct jit_core eqn: Hjit_core; [| inversion Hjit].
      rename j into st_core.
      destruct jit_alu32_thumb_store eqn: Hjit_store; [| inversion Hjit].
      rename j into st_store.
      destruct jit_alu32_thumb_reset eqn: Hjit_reset; [| inversion Hjit].
      rename j into st_reset.

      eapply jit_pre_simulation with (rs0 := rs0)(m0 := m0) (rbpf_st := rbpf_st0)
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk) (ra_blk := ra_blk) (old_sp := old_sp)
        in Hjit_pre as Hone; eauto.

      4:{ destruct Hinit_state as (_ & _ & Hrs_1 & _).
        assumption.
      }

      3:{
        constructor; try assumption; try (split; assumption).
        - exists ra_ofs.
          destruct Hinit_state as (_ & _ & _ & Hrs_14 & _).
          assumption.
        - split; [| apply NoDup_nil].
          unfold regs_agree.
          intros r HF; inversion HF.
        - unfold regs_unchanged in *.
          intros r HF.
          specialize (Hst r).
          destruct Hst as (vi & Hload1 & Hload2).
          exists vi.
          split; [assumption | ].
          rewrite <- Hload2.
          destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
          eapply Mem.load_alloc_unchanged; eauto.
          destruct Hinv as (Hneq & _ & Hvalid & _).
          symmetry in Hmem.
          eapply Mem.valid_block_alloc_inv with (b' := st_blk) in Hmem; eauto.
          destruct Hmem as [Heq | Hmem]; [| assumption].
          destruct Hneq as (_ & Hneq & _).
          exfalso; apply Hneq; assumption.
        - split; [| apply NoDup_nil].
          unfold arm_synch_stack.
          intros r HF; inversion HF.
        - intuition.
        - destruct Hinit_state as (_ & Hrs_pc & _).
          rewrite Hrs_pc.
          f_equal.
          subst ofs.
          f_equal.
        - intuition.
          rename H12 into Hmem.
          eapply Mem.valid_new_block; eauto.
        - split; [intuition | ].
          destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
          unfold Mem.range_perm.
          intros ofs0 Hofs0.
          symmetry in Hmem.
          eapply Mem.perm_alloc_2 with (k := Cur) in Hmem; eauto.
          eapply Mem.perm_implies; eauto.
          constructor.
      }
      2:{
          eapply jit_alu32_jit_blk_memory_layout in Hjit_pre; eauto.
          destruct Hjit_pre as (Hjit_pre & _).
          eapply Hjit_pre.

          destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
          eapply Hmem.
      }

      destruct Hone as (rs1_pre & Hrs_const_pre & Hrs_12_pre & Hstack_unchanged_pre & Hstar_pre & Harm_pre).

      eapply jit_spilling_simulation with (rs0 := rs1_pre)(m0 := m0) (rbpf_st := rbpf_st0)
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk) (ra_blk := ra_blk) (old_sp := old_sp)
        in Hjit_save as Hone; eauto.

      7:{
        destruct Harm_pre as (Hra_blk_pre, Harm_reg_syn_pre, Harm_reg_same_pre, Harm_stack_syn_pre, Harm_old_sp_pre,
        Harm_pc_pre, Harm_valid_blk_pre, Harm_blk_perm_pre, arm_reg_valid_pre).
        constructor; try assumption; try (split; assumption).
        split; [| apply NoDup_nil].
        unfold arm_synch_stack.
        intros r HF; inversion HF.
      }

      6:{
        destruct Harm_pre as (Hra_blk_pre, Harm_reg_syn_pre, Harm_reg_same_pre, Harm_stack_syn_pre, Harm_old_sp_pre,
        Harm_pc_pre, Harm_valid_blk_pre, Harm_blk_perm_pre, arm_reg_valid_pre).
        constructor; try assumption; try (split; assumption).
      }

      5:{
        eapply jit_alu32_stack_list_callee_save; eauto.
      }

      4:{ eapply jit_alu32_stack_list_NoDup; eauto.
      }

      3:{
        unfold rbpf_state_inv.
        eapply jit_alu32_jit_blk_unchange_jitted_list in Hjit_save; eauto.
        destruct Hjit_save as (Hjit_save & _).
        assumption.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      2:{
        eapply jit_alu32_jit_blk_memory_layout in Hjit_save; eauto.
        destruct Hjit_save as (_ & Hjit_save).
        eapply Hjit_save.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }


      destruct Hone as (rs1_spilling & m1_spilling & Hrs_const_spilling & Hrs_12_spilling & Hstack_unchanged_spilling & Hstar_spilling & Harm_spilling).

      eapply jit_load_simulation with (rs0 := rs1_spilling)(m0 := m1_spilling) (rbpf_st := rbpf_st0)
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk) (ra_blk := ra_blk) (old_sp := old_sp)
        in Hjit_load as Hone; eauto.
      5:{
        intro r.
        specialize (Hst r).
        destruct Hst as (vi & Hload0 & Hload1).
        exists vi.
        split.
        - eapply llvm_enable_alu32; eauto.
        - rewrite <- Hload1.
          eapply jit_alu32_jit_spilling_load_registers; eauto.

          destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
          eapply Hmem.
      }

      4:{
        eapply jit_alu32_jit_spilling_callee_save; eauto.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      3:{
        unfold rbpf_state_inv.
        eapply jit_alu32_jit_blk_unchange_jitted_list in Hjit_load; eauto.
        destruct Hjit_load as (_ & Hjit_load & _).
        assumption.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      2:{
        eapply jit_alu32_jit_blk_memory_layout_1 in Hjit_load; eauto.
        destruct Hjit_load as (Hjit_load & _).
        eapply Hjit_load.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      destruct Hone as (rs1_load & Hrs_const_load & Hrs_12_load & Hstack_unchanged_load & Hstar_load & Harm_load).

      eapply jit_core_simulation with (rs0 := rs1_load)(m0 := m1_spilling) (rbpf_st0 := rbpf_st0)
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk) (ra_blk := ra_blk) (old_sp := old_sp)
        in Hjit_core as Hone; eauto.

      5:{
        eapply jit_alu32_jit_spilling_callee_save; eauto.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      4:{
        unfold rbpf_state_inv.
        eapply jit_alu32_jit_blk_unchange_jitted_list in Hjit_core; eauto.
        destruct Hjit_core as (_ & _ & Hjit_core & _).
        assumption.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      3:{
        eapply jit_alu32_jit_blk_memory_layout_1 in Hjit_core; eauto.
        destruct Hjit_core as (_ & Hjit_core & _).
        eapply Hjit_core.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      2:{
        apply list_subset_refl.
      }

      destruct Hone as (rs1_core & Hstar_core & Hstack_unchanged_core & Hrs_12_core & Harm_core).

      eapply jit_store_simulation with (rs0 := rs1_core)(m0 := m1_spilling) (rbpf_st := rbpf_st1) (rs_init := rs1_pre)
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk) (ra_blk := ra_blk) (old_sp := old_sp)
        (ls := (jit_alu32_stack_list l_load l_store st_pre))
        in Hjit_store as Hone; eauto.

      5:{
        destruct Harm_core as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

        constructor; try assumption; try (split; assumption).
        destruct Harm_reg_syn as (Harm_reg_syn & Hnodup).
        split.
        - unfold regs_agree in *.
          intros r Hin.
          eapply Harm_reg_syn; eauto.
          eapply jit_alu32_load_store_subst; eauto.
        - eapply list_subst_nodup; eauto.
          eapply jit_alu32_load_store_subst; eauto.
        - unfold regs_unchanged in *.
          intros r HT.
          eapply jit_alu32_store_load_subst in Hload_list as Heq1; eauto.
      }

      4:{
        eapply jit_alu32_jit_spilling_callee_save; eauto.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      3:{
        unfold rbpf_state_inv.
        eapply jit_alu32_jit_blk_unchange_jitted_list in Hjit_store; eauto.
        destruct Hjit_store as (_ & _ & _ & Hjit_store & _).
        assumption.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      2:{
        eapply jit_alu32_jit_blk_memory_layout_1 in Hjit_core; eauto.
        destruct Hjit_core as (_ & _ & Hjit_core).
        eapply Hjit_core.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }


      destruct Hone as (rs1_store & m_store & Hrs_const_store & Hrs_12_store & Hstack_unchanged_store & Hstar_store & Harm_store).

      eapply jit_reloading_simulation with (rs0 := rs1_store) (m0 := m_store) (rbpf_st := rbpf_st1) (rs_init := rs1_pre)
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk) (ra_blk := ra_blk) (old_sp := old_sp)
        in Hjit_reset as Hone; eauto.

      6:{
        unfold callee_save_used_sync.
        intros r HF; inversion HF.
      }

      5:{
        unfold rbpf_state_inv.
        eapply jit_alu32_jit_blk_unchange_jitted_list in Hjit_reset; eauto.
        destruct Hjit_reset as (_ & _ & _ & _ & Hjit_reset & _).
        assumption.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      4:{
        eapply jit_alu32_jit_blk_memory_layout_2 in Hjit_reset; eauto.
        destruct Hjit_reset as (Hjit_reset & _).
        eapply Hjit_reset.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      3:{
        eapply jit_alu32_stack_list_callee_save; eauto.
      }

      2:{
        eapply jit_alu32_stack_list_NoDup; eauto.
      }


      destruct Hone as (rs1_reloading & Hrs_const_reloading & Hrs_12_reloading & Hcallee & Hstar_reloading & Harm_reloading).

      eapply jit_post_simulation with (rs0 := rs1_reloading) (m0 := m_store) (rbpf_st := rbpf_st1)
        (st_blk := st_blk) (jit_blk := jit_blk) (sp_blk := sp_blk) (ra_blk := ra_blk) (old_sp := old_sp)
        in Hjit as Hone; eauto.

      4:{
        destruct Harm_reloading as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
              Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).

        constructor; try assumption; try (split; assumption).
        destruct Harm_stack_syn as (Harm_stack_syn & Hnodup).
        split.
        - unfold arm_synch_stack.
          intros r HF; inversion HF.
        - apply NoDup_nil.
      }

      3:{
        unfold rbpf_state_inv.
        eapply jit_alu32_jit_blk_unchange_jitted_list in Hjit; eauto.
        destruct Hjit as (_ & _ & _ & _ & _ & Hjit & _).
        assumption.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      2:{
        eapply jit_alu32_jit_blk_memory_layout_2 in Hjit; eauto.
        destruct Hjit as (_ & Hjit).
        eapply Hjit.

        destruct Hinit_state as (_ & _ & _ & _ & _ & _ & Hmem).
        eapply Hmem.
      }

      destruct Hone as (rs1_post & Hrs_const_post & Hrs_12_post & Hrs_pc_post & Hrs_13_post & Hstar_post & Harm_post).

      exists rs1_post, m_store.
      split.
      - clear - Hstar_pre Hstar_spilling Hstar_load Hstar_core Hstar_store Hstar_reloading Hstar_post.

        eapply star_trans with (s3 := (State rs1_spilling m1_spilling)) in Hstar_pre; eauto.

        eapply star_trans with (s3 := (State rs1_load m1_spilling)) in Hstar_pre; eauto.

        eapply star_trans with (s3 := (State rs1_core m1_spilling)) in Hstar_pre; eauto.

        eapply star_trans with (s3 := (State rs1_store m_store)) in Hstar_pre; eauto.

        eapply star_trans with (s3 := (State rs1_reloading m_store)) in Hstar_pre; eauto.

        eapply star_trans with (s3 := (State rs1_post m_store)) in Hstar_pre; eauto.
      - destruct Harm_store as (Hra_blk, Harm_reg_syn, Harm_reg_same, Harm_stack_syn, (Hsp_blk & Harm_old_sp),
          Harm_pc, Harm_valid_blk, (Hst_blk_perm & Hsp_blk_perm), Harm_reg_valid).
        clear - Harm_reg_same.
        unfold regs_unchanged in *.
        intro r.
        eapply Harm_reg_same; eauto.
    Qed.
  End JITCorrectness.

End JITProofWhole.