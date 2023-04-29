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

From Coq Require Import List ZArith Arith String Lia.
Import ListNotations.
Open Scope Z_scope.
Open Scope bool_scope.
Open Scope asm.

Section JITProofWhole.

  Variable old_sp: val.
  Variable ge : genv.

  (** Definition *)
  Variable flag_blk: block.
  Variable regs_blk: block.
  Variable jit_blk: block.
  Variable jit_state_blk: block.


  Section MatchStaterBPFJIT.

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
    }.

  End MatchStaterBPFJIT.

(*
  Section MatchStaterBPFJITSYNC.

    Record match_state_jit_sync (rbpf_st: state) (jit_st: jit_state) (l: sync_regs): Prop :=
    {
      munchange_sync:  Mem.unchanged_on (fun b _ => b <> jit_blk /\ b <> jit_state_blk) (bpf_m rbpf_st) (jit_mem jit_st);
      mpc_sync      :  Int.cmp Cle (jit_pc jit_st) (pc_loc rbpf_st) = true;
      mflag_sync    :  match_flag rbpf_st jit_st flag_blk;
      mregs_sync    :  match_registers_syn rbpf_st jit_st l regs_blk;
      mmrs_num_sync :  mrs_num rbpf_st = jit_mrs_num jit_st;
      mbpf_mrs_sync :  bpf_mrs rbpf_st = jit_mrs jit_st;
      mins_len_sync :  ins_len rbpf_st = jit_ins_len jit_st;
      mins_sync     :  ins rbpf_st = jit_ins jit_st;
      mjit_sync     :  Vptr jit_blk Ptrofs.zero = jitted_list jit_st;

      mperm_sync    :  ptr_range_perm (bpf_m rbpf_st) Mint32 (flag rbpf_st) Freeable /\
                  (forall r, ptr_range_perm (bpf_m rbpf_st) Mint64
                      (Val.add (regs_st rbpf_st) (Vint (Int.repr (8 * (id_of_reg r))))) Freeable) /\
                  ptr_range_perm (jit_mem jit_st) Mint32 (jit_flag jit_st) Freeable /\
                  (forall r, ptr_range_perm (jit_mem jit_st) Mint64
                    (Val.add (jit_regs jit_st) (Vint (Int.repr (8 * (id_of_reg r))))) Freeable) /\
                  (forall pc, (0 <= pc < Nat.div JITTED_LIST_MAX_LENGTH 2)%nat ->
                    ptr_range_perm (jit_mem jit_st) Mint16unsigned
                      (Val.add (JITState.jitted_list jit_st) (Vint (Int.repr (Z.of_nat (2 * pc))))) Freeable);
      minvalid_sync :  ~Mem.valid_block (bpf_m rbpf_st) jit_blk /\
                  Mem.valid_block (jit_mem jit_st) jit_blk /\
                    (block_neq flag_blk regs_blk jit_blk jit_state_blk) /\
                    (forall b, b <> jit_blk ->
                      Mem.valid_block (bpf_m rbpf_st) b -> Mem.valid_block (jit_mem jit_st) b);
      mvalid_sync   : jit_state_memory_layout jit_st jit_state_blk regs_blk (jit_mem jit_st);
    }.

  End MatchStaterBPFJITSYNC.
*)

  Section MatchStateJITARM.

    Record match_state_initial_arm (st0 st_final: jit_state) (rs: Asm.regset) (m: mem): Prop :=
    {
      munchange_init0: Mem.unchanged_on (fun b _ => (* b <> jit_blk /\ *) b <> regs_blk /\
                          (*b <> jit_state_blk /\ *) not_stack_blk (rs IR13) b) m (jit_mem st_final);
      munchange_init1: Mem.unchanged_on (fun b _ => (* b <> jit_blk /\ *) b <> regs_blk /\
                          (*b <> jit_state_blk /\ *) not_stack_blk (rs IR13) b) (jit_mem st_final) m;
      munchange_init2: Mem.unchanged_on (fun b _ => not_stack_blk (rs IR13) b) (jit_mem st0) m;
      mregs_init:      arm_initial_state m rs jit_state_blk regs_blk old_sp
                        (Vptr jit_blk (Ptrofs.repr (Z.of_nat (2 * (jitted_len st0)))));(*
      mvalid_init:    Mem.valid_block m jit_blk; *)
    }.

    Inductive match_state_arm:
      state -> (**r rbpf_state *)
      Asm.regset -> mem -> (**r ARM state *)
      Asm.regset -> (**r initial ARM register map *)
      sync_regs -> (**r synchronous rBPF regsiters *)
      sync_iregs -> (**r callee-save ARM registers *)
      block -> Z -> (**r jitted block and PC offset *) (*
      jit_state -> (**r the current JIT state *) *)
      jit_state -> (**r the after-JIT JIT state *)
      Prop :=
    | exec_step:
        forall lsr lsr_stack rbpf_st rs old_rs jit_st_final arm_blk ofs arm_mem
          (**r all rBPF registers in lsr are synchronous with the ARM registers *)
          (Hreg_agree: regs_agree lsr rbpf_st rs)
          (**r all call-save registers in lsr_stack are stored in ARM stack, and the rest in ARM stack are Vundef *)
          (Hstack: arm_synch_stack arm_mem rs old_rs old_sp lsr_stack)
          (**r the current PC location *)
          (HPC_eq: rs PC = Vptr arm_blk (Ptrofs.repr ofs))
          (**r the arm memory is a subset of the final jit state *) (* This is an assumption 
          (Hsub_mem: sub_mem_blk (jit_mem jit_st) (jit_mem jit_st_final) arm_blk ofs) *)
          (Hvalid_arm_blk: Mem.valid_block (jit_mem jit_st_final) arm_blk)
          (Harm_mem1: Mem.unchanged_on (fun b _ => b <> regs_blk /\ not_stack_blk (rs IR13) b) (jit_mem jit_st_final) arm_mem)
          (Harm_mem2: Mem.unchanged_on (fun b _ => b <> regs_blk /\ not_stack_blk (rs IR13) b) arm_mem (jit_mem jit_st_final))
          ,
          match_state_arm rbpf_st rs arm_mem old_rs lsr lsr_stack arm_blk ofs jit_st_final.
  End MatchStateJITARM.


  Section JITPre.

    Definition arm_registers_pre (rs0 rs1: Asm.regset): Prop :=
      rs1 = (rs0 # IR12 <- (Vptr regs_blk Ptrofs.zero)) # PC <- (Val.offset_ptr (rs0 PC) wsize).

    Lemma jit_pre_simulation: forall rbpf_st st0 st1 st_final rs0 ofs0 ofs1 init_arm_mem
    (Hjit_pre: jit_alu32_pre st0 = Some st1)
    (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
    (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1)))) (*
    (Harm_blk: jitted_list st0 = Vptr jit_blk Ptrofs.zero) *)
    (Hmem: sub_mem_blk (jit_mem st1) (jit_mem st_final) jit_blk ofs1)
    (Hinitial_arm_st0: match_state_initial_arm st0 st_final rs0 init_arm_mem)
    (Harm_inv: arm_memory_inv0 st0 rs0 rs0 init_arm_mem init_arm_mem
        flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True) )
(*
    (Hunchanged: Mem.unchanged_on (fun (b : block) (_ : Z) =>
        b <> jit_blk /\ not_stack_blk (rs0 IR13) b) (jit_mem st0) (jit_mem st_final)) *)

    (Hst: match_state_jit rbpf_st st0),
      exists rs1,
        arm_registers_pre rs0 rs1 /\
        arm_memory_inv0 st1 rs0 rs1 init_arm_mem init_arm_mem 
          flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True) /\
        star BinSem.step ge (State rs0 init_arm_mem) E0 (State rs1 init_arm_mem) /\
        match_state_arm rbpf_st rs1 init_arm_mem rs0 [] [] jit_blk ofs1 st_final.
    Proof.
      intros.
      assert(Hmatch_state_0: match_state_arm rbpf_st rs0 init_arm_mem rs0 [] [] jit_blk ofs0 st_final). {
        clear - Hofs0 (*Harm_blk*) Hst Hinitial_arm_st0 Hst Harm_inv.
        destruct Hinitial_arm_st0 as (Hregs_init0, Hregs_init1, Hregs_init2, Hinit_stack).
        unfold arm_initial_state in Hinit_stack.
        split.
        - (**r regs_agree *)
          unfold regs_agree.
          intros r HF.
          inversion HF.
        - (**r arm_synch_stack *)
          unfold arm_synch_stack.
          destruct Hinit_stack as (_ & arm_stack & _).
          unfold arm_initial_stack in arm_stack.
          destruct arm_stack as (Hold_sp & Hstack).
          split; [assumption | ].
          intros r HF; inversion HF.
        - (**r PC *)
          destruct Hinit_stack as (_ & _ & Hpc).
          rewrite Hofs0.
          assumption.
        - (**r Mem.valid_block *)
          destruct Hst.
          destruct minvalid0 as (_ & Hvalid & _).
          eapply Mem.valid_block_unchanged_on in Hregs_init2; eauto.
          eapply Mem.valid_block_unchanged_on in Hregs_init0; eauto.
        - (**r Mem.unchanged_on *)
          assumption.
        - (**r Mem.unchanged_on *)
          assumption.
      }

      clear Hmatch_state_0.

      unfold arm_registers_pre.
      exists ((rs0 # IR12 <- (Vptr regs_blk Ptrofs.zero)) # PC <- (Val.offset_ptr (rs0 PC) wsize)).
      split; [(**r arm_registers_pre *) reflexivity | ].

      destruct Hinitial_arm_st0 as (Hmem_init0, Hmem_init1, Hmem_init2, Hinit_stack).
      destruct Hst.
      unfold arm_initial_state in Hinit_stack.
      destruct Hinit_stack as (Hr1 & Hstack & Hpc).

      unfold jit_alu32_pre, jit_alu32_thumb_load_store_template_jit in Hjit_pre.
      apply upd_jitted_list_jittted_len_2 in Hjit_pre as Hlen_eq.

      assert (Hcond: (2 * jitted_len st0 <= 1000)%nat). {
        simpl.
        destruct upd_jitted_list in Hjit_pre; [| inversion Hjit_pre].
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
      { (**r arm_memory_inv0 *)
        clear - Hjit_pre Hlen_eq Hofs0 Hofs1 Harm_inv mjit0 mflag0 mregs0.
        destruct Harm_inv.
        destruct arm_inv_reg as (Harm_inv_reg0 & arm_inv_reg1 & arm_inv_reg2).
        split; try assumption.
        2:{ (**r jit_state_memory_layout *)
          unfold jit_state_memory_layout in *.
          destruct arm_inv_st as (Hload & Hregs_layout).
          split; [assumption | ].
          unfold regs_layout in *.
          intros r.
          specialize (Hregs_layout r).
          destruct Hregs_layout as (vi & Heval_reg & Hload64 & Hload32 & Haxiom).
          exists vi.
          split.
          - rewrite <- Heval_reg.
            symmetry.
            eapply upd_jitted_list_unchange_eval_jit_reg_2
              with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
            + (**r jit_regs st0 = Vptr regs_blk Ptrofs.zero *)
              unfold match_registers in mregs0.
              destruct mregs0 as (_ & Heq & _).
              assumption.
            + destruct arm_inv_stk as (_ & Hsp_spec).
              unfold arm_stack_pointer_spec in Hsp_spec.
              destruct Hsp_spec as (sp_blk & _ & _ & Hneq & _).
              clear - Hneq; unfold block_neq in Hneq.
              intuition.
          - split; [assumption | ].
            split; [assumption | ].
            intros vj Heval_reg0.
            apply Haxiom.
            rewrite <- Heval_reg0.
            eapply upd_jitted_list_unchange_eval_jit_reg_2
              with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
            + (**r jit_regs st0 = Vptr regs_blk Ptrofs.zero *)
              unfold match_registers in mregs0.
              destruct mregs0 as (_ & Heq & _).
              assumption.
            + destruct arm_inv_stk as (_ & Hsp_spec).
              unfold arm_stack_pointer_spec in Hsp_spec.
              destruct Hsp_spec as (sp_blk & _ & _ & Hneq & _).
              clear - Hneq; unfold block_neq in Hneq.
              intuition.
        }
        split; [ assumption | ].
        split; [  | assumption ].
        clear - Harm_inv_reg0.
        unfold arm_assume_register_map in *.
        intros r.
        specialize (Harm_inv_reg0 r).
        destruct r.
        - rewrite Pregmap.gso; [ | intros HF; inversion HF].
          destruct i; try (rewrite Pregmap.gss; auto);
            try (rewrite Pregmap.gso; [ | intros HF; inversion HF]); try assumption.
        - repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
        - repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
        - rewrite Pregmap.gss; auto.
          unfold Val.offset_ptr; destruct (rs0 PC); auto.
      }

      split.
      - (**r star BinSem.step *)
        eapply star_one; eauto.
        eapply exec_step_bin; eauto.
        + (**r find_instr *)
          rewrite Hpc.
          assert (Hjit_eq: find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st_final) =
                          find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) init_arm_mem). {
            unfold find_instr.
            rewrite Hofs0; simpl.
            erewrite Mem.load_unchanged_on_1 with (m := init_arm_mem); eauto.
            - (**r Mem.load *)
              destruct Mem.load; [| reflexivity].
              destruct v; try reflexivity.
              erewrite Mem.load_unchanged_on_1 with (m := init_arm_mem); eauto.
              + (**r Mem.valid_block *)
                destruct minvalid0 as (_ & Hvalid_blk & _).
                eapply Mem.valid_block_unchanged_on in Hmem_init2; eauto.
              + intros.
                simpl.
                clear - minvalid0 Harm_inv.
                unfold block_neq in minvalid0.
                destruct minvalid0 as (_ & _ & (Hblk_neq0 & Hblk_neq1) & _).
                destruct Hblk_neq0 as (Hblk_neq00 & Hblk_neq01 & Hblk_neq02).
                destruct Hblk_neq1 as (Hblk_neq10 & Hblk_neq11 & Hblk_neq12).
                split; [assumption |].
                unfold not_stack_blk.
                destruct Harm_inv.
                destruct arm_inv_stk as (_ & arm_inv_stk0).
                destruct arm_inv_stk0 as (old_sp_blk & Hold_sp_eq & Harm_sp).
                rewrite Hold_sp_eq.
                destruct Harm_sp as ((_ & Hneq & _) & _).
                clear - Hneq.
                auto.
            - (**r Mem.valid_block *)
              destruct minvalid0 as (_ & Hvalid_blk & _).
              eapply Mem.valid_block_unchanged_on in Hmem_init2; eauto.
            - (**r block neq *)
              intros.
              simpl.
              clear - minvalid0 Harm_inv.
              unfold block_neq in minvalid0.
              destruct minvalid0 as (_ & _ & (Hblk_neq0 & Hblk_neq1) & _).
              destruct Hblk_neq0 as (Hblk_neq00 & Hblk_neq01 & Hblk_neq02).
              destruct Hblk_neq1 as (Hblk_neq10 & Hblk_neq11 & Hblk_neq12).
              split; [assumption |].
              unfold not_stack_blk.
              destruct Harm_inv.
              destruct arm_inv_stk as (_ & arm_inv_stk0).
              destruct arm_inv_stk0 as (old_sp_blk & Hold_sp_eq & Harm_sp).
              rewrite Hold_sp_eq.
              destruct Harm_sp as ((_ & Hneq & _) & _).
              clear - Hneq.
              auto.
          }
          rewrite <- Hofs0.
          rewrite <- Hjit_eq; clear.

          assert (Heq: find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st_final) =
                        find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st1)). {
            unfold find_instr.
            rewrite Hofs0; simpl.
            clear - flag_blk regs_blk jit_blk jit_state_blk Hofs1 Hjit_pre Hmem Hcond Hlen_eq Hlen_eq0.
            unfold sub_mem_blk in Hmem.
            repeat rewrite <- Hmem.

            3:{
              rewrite Hofs1.
              rewrite Hlen_eq0.
              simpl.
              lia.
            }

            2:{
              simpl.
              unfold Ptrofs.add, Ptrofs.of_int.
              rewrite Hlen_eq0.
              change (Ptrofs.unsigned (Ptrofs.repr (Int.unsigned (Int.repr 2)))) with 2.

              assert (Hlen_eq1: Ptrofs.unsigned (Ptrofs.repr 
                    (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2))  = 
                  (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
                rewrite Ptrofs.unsigned_repr.
                reflexivity.
                change Ptrofs.max_unsigned with 4294967295; lia.
              }
              rewrite Hlen_eq1.
              lia.
            }
            reflexivity.
          }

          rewrite Heq; clear Heq.

          instantiate (2 := Pldr IR12 IR1 (SOimm (Int.repr 8))).
          instantiate (1 := true).
          eapply lemma_thumb_ldr; eauto.
          lia.
        + (**r exec_instr *)
          simpl.
          rewrite Hr1; simpl.
          rewrite Ptrofs.add_zero_l.
          unfold exec_load; simpl.

          clear - mvalid0 Hmem_init2 Harm_inv.
          unfold jit_state_memory_layout, Mem.loadv in mvalid0.
          destruct mvalid0 as (Hload & _).
          eapply Mem.load_unchanged_on in Hload; eauto.
          * (**r Mem.load *)
            change (Ptrofs.unsigned (Ptrofs.repr 8)) with 8 in Hload.
            change (Ptrofs.unsigned (Ptrofs.of_int (Int.repr 8))) with 8.
            rewrite Hload.
            unfold nextinstr; simpl.
            rewrite Pregmap.gso; [ reflexivity | intros HF; inversion HF].
          * (**r P neq *)
            intros; simpl.
            unfold not_stack_blk.
            destruct Harm_inv.
            destruct arm_inv_stk as (_ & arm_inv_stk0).
            destruct arm_inv_stk0 as (old_sp_blk & Hold_sp_eq & Harm_sp).
            rewrite Hold_sp_eq.
            destruct Harm_sp as ((_ & _ & _ & Hneq) & _).
            clear - Hneq.
            auto.

      - (**r match_state_arm *)
        split.
        + (**r regs_agree *)
          unfold regs_agree.
          intros r HF.
          inversion HF.
        + (**r arm_synch_stack *)
          unfold arm_synch_stack.
          unfold arm_initial_stack in Hstack.
          split.
          * (**r Mem.loadv old_sp *)
            unfold Mem.loadv in *; simpl.
            clear - Hstack.
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            destruct Hstack as (Hload & Hlayout).
            rewrite Hload.
            reflexivity.
          * (**r stack layout *)
            intros r HF; inversion HF.
        + (**r PC eq *)
          rewrite Pregmap.gss.
          rewrite Hpc.
          rewrite <- Hlen_eq in Hofs1.
          rewrite Hofs1.

          unfold Val.offset_ptr, wsize, Ptrofs.add.
          f_equal. f_equal.
          change (Ptrofs.unsigned (Ptrofs.repr 4)) with 4.
          rewrite Ptrofs.unsigned_repr.
          2:{
            change Ptrofs.max_unsigned with 4294967295; lia.
          }

          lia.
        + (**r Mem.valid_block *)
          destruct minvalid0 as (_ & Hvalid & _).
          eapply Mem.valid_block_unchanged_on in Hmem_init2; eauto.
          eapply Mem.valid_block_unchanged_on in Hmem_init0; eauto.
        + (**r Mem.unchanged_on *)
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          assumption.
        + (**r Mem.unchanged_on *)
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          assumption.
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

    Lemma arm_registers_spilling_aux_load_same:
      forall l rs0 m0 rs1 m1 r
        (Haux: arm_registers_spilling_aux l rs0 m0 = Some (rs1, m1))
        (Harm_callee_save: forall r : ireg, In r l -> In r arm_callee_save_regs)
        (Hnin : ~ In r l)
        (Hr_in: In r arm_callee_save_regs)
        (Hsp_spec: arm_stack_pointer_spec rs0 m0 flag_blk regs_blk jit_blk jit_state_blk)
        (Hstack2 : Mem.loadv Mint32 m0 (Val.offset_ptr (rs0 IR13) (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4)))) = Some (rs0 r)),
          Mem.loadv Mint32 m1 (Val.offset_ptr (rs0 IR13) (Ptrofs.of_intu (Int.mul (int_of_ireg r) (Int.repr 4)))) = Some (rs0 r).
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

      unfold arm_stack_pointer_spec in Hsp_spec.
      unfold arm_stack_pointer_spec.
      unfold block_neq in Hsp_spec.
      destruct Hsp_spec as (sp_blk & Hrs_eq & (Hneq0 & Hneq1 & Hneq2 & Hnew3) & Hneq4 & Hperm).
      unfold arm_registers_spilling_one in Hone.

      eapply IHl with (rs0 := rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)); eauto.
      - intros r0 Hin.
        apply Harm_callee_save.
        right; assumption.
      - intros HF.
        apply Hnin.
        right; assumption.
      - exists sp_blk.
        split.
        { rewrite Pregmap.gso; [ | intros HF; inversion HF]. assumption. }
        split.
        { repeat (split; [assumption | ]). assumption. }
        split.
        { unfold block_neq. assumption.  }

        unfold Mem.range_perm in *.
        intros ofs Hofs.
        specialize (Hperm ofs Hofs).
        unfold arm_registers_spilling_one in *.
        rewrite Hrs_eq in Hone.
        simpl in Hone.
        rewrite Ptrofs.add_zero_l in Hone.
        eapply Mem.perm_store_1; eauto.
      - rewrite Pregmap.gso; [ | intros HF; inversion HF].
        rewrite Hrs_eq in *.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        simpl. simpl in Hstack2.
        rewrite <- Hstack2.
        simpl in Hone.
        rewrite Ptrofs.add_zero_l in *.
        eapply Mem.load_store_other; eauto.
        right.
        repeat rewrite Hreg_mul4_unsigned.
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
        unfold ireg2nat.

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

    Definition arm_registers_pre_weak (rs: Asm.regset): Prop :=
      (rs IR12) = (Vptr regs_blk Ptrofs.zero).

    Lemma arm_registers_pre_implies:
      forall rs rs0
        (Hpre: arm_registers_pre rs rs0),
          arm_registers_pre_weak rs0.
    Proof.
      unfold arm_registers_pre, arm_registers_pre_weak; intros.
      rewrite Hpre.
      rewrite Pregmap.gso; [ | intros HF; inversion HF].
      rewrite Pregmap.gss.
      reflexivity.
    Qed.


    Lemma jit_spilling_one_simulation: forall l r rbpf_st st0 st1 st_final rs0 ofs0 ofs1 m0
      (Hpre: arm_registers_pre_weak rs0)
      (Hupd_spilling : jit_alu32_thumb_upd_save r st0 = Some st1)

      (Hlist_not_in: ~ List.In r l)
      (Hcallee_save: In r arm_callee_save_regs)
      (Harm_callee_save: forall r, List.In r l -> List.In r arm_callee_save_regs)
      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Harm_blk: jitted_list st0 = Vptr jit_blk Ptrofs.zero)
      (Hreg_blk: jit_regs st0 = Vptr regs_blk Ptrofs.zero)

      (Harm_inv: arm_memory_inv0 st0 rs0 rs0 m0 m0 
        flag_blk regs_blk jit_blk jit_state_blk (fun b _ => not_stack_blk (rs0 IR13) b))
      (Hsub_mem: sub_mem_blk (jit_mem st1) (jit_mem st_final) jit_blk ofs1)

      (Harm: match_state_arm rbpf_st rs0 m0 rs0 [] l jit_blk ofs0 st_final)
(*
      (Hunchanged: Mem.unchanged_on (fun (b : block) (_ : Z) =>
          b <> jit_blk /\ not_stack_blk old_sp b) (jit_mem st0) (jit_mem st_final)) *),
        exists rs1 m1,
          arm_registers_spilling_one r rs0 m0 = Some m1 /\
          rs1 = rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) /\
          arm_registers_pre_weak rs1 /\
          arm_memory_inv0 st1 rs0 rs1 m0 m1
            flag_blk regs_blk jit_blk jit_state_blk (fun b _ => not_stack_blk (rs0 IR13) b) /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
          match_state_arm rbpf_st rs1 m1 rs0 [] (r::l) jit_blk ofs1 st_final.
    Proof.
      intros.
      set (el := []).
      assert (Heq1: el = []) by auto.
      rewrite <- Heq1 in Harm.

      destruct Harm_inv.
      destruct arm_inv_stk as (_ & arm_inv_stk0).
      unfold arm_stack_pointer_spec in arm_inv_stk0.
      destruct arm_inv_stk0 as (sp_blk & Hsp_blk & (Hneq0 & Hneq1 & Hneq2 & Hneq3) & Hneq4 & Hperm).

      exists (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)).

      (**r arm_registers_spilling_one *)
      unfold arm_registers_spilling_one.
      rewrite Hsp_blk.
      simpl.
      rewrite Ptrofs.add_zero_l.
      rewrite Hreg_mul4_unsigned.
      unfold Mem.range_perm in Hperm. (**r here we need the info: r is in arm_callee_save *)

      assert (Heq: Mem.valid_access m0 Mint32 sp_blk (Z.of_nat (ireg2nat r) * 4) Writable). {
        unfold Mem.valid_access.
        split.
        - unfold Mem.range_perm.
          intros ofs Hrange.
          apply Hperm.
          change (size_chunk Mint32) with 4 in Hrange.
          clear - Hrange Hcallee_save.
          unfold ireg2nat in Hrange.
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

      assert (Hle: Z.of_nat (2 * jitted_len st0) <= Z.of_nat JITTED_LIST_MAX_LENGTH). {
        clear - Hupd_spilling.
        unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_spilling.
        destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_spilling].
        eapply upd_jitted_list_max; eauto.
      }

      exists m1.
      split; [assumption | ].
      split; [(**r rs PC *) f_equal | ].
      split.
      { (**r arm_registers_pre_weak *)
        unfold arm_registers_pre_weak.
        rewrite Pregmap.gso; [ | intros HF; inversion HF].
        unfold arm_registers_pre_weak in Hpre.
        assumption.
      }
      split.
      { (**r arm_memory_inv0 *)
        constructor; try assumption.
        - (**r rs IR13 *)
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          split; [f_equal | ].
          unfold arm_stack_pointer_spec.
          exists sp_blk.
          split; [assumption | ].
          split.
          { repeat (split; [assumption | ]). assumption. }
          split; [assumption | ].
          unfold Mem.range_perm; assumption.
        - (**r arm_assume_register_map *)
          destruct arm_inv_reg as (Harm_inv_reg0 & Harm_inv_reg1 & arm_inv_reg2).
          split; [assumption | ].
          split; [ | assumption].
          clear - Harm_inv_reg0.
          unfold arm_assume_register_map in *.
          intros r.
          specialize (Harm_inv_reg0 r).
          destruct r.
          + rewrite Pregmap.gso; [ | intros HF; inversion HF].
            destruct i; try (rewrite Pregmap.gss; auto);
              try (rewrite Pregmap.gso; [ | intros HF; inversion HF]); try assumption.
          + repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
          + repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
          + rewrite Pregmap.gss; auto.
            unfold Val.offset_ptr; destruct (rs0 PC); auto.
        - (**r jit_state_memory_layout *)
          unfold jit_state_memory_layout in *.
          destruct arm_inv_st as (Hload & Hregs_layout).
          split; [assumption | ].
          unfold regs_layout in *.
          intros r0.
          specialize (Hregs_layout r0).
          destruct Hregs_layout as (vi & Heval_reg & Hload64 & Hload32 & Haxiom).
          exists vi.
          split.
          + rewrite <- Heval_reg.
            symmetry.
            eapply upd_jitted_list_unchange_eval_jit_reg_2
              with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
            clear - Hneq4; unfold block_neq in Hneq4.
            intuition.
          + split; [assumption | ].
            split; [assumption | ].
            intros vj Heval_reg0.
            apply Haxiom.
            rewrite <- Heval_reg0.
            eapply upd_jitted_list_unchange_eval_jit_reg_2
              with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
            clear - Hneq4; unfold block_neq in Hneq4.
            intuition.
        - (**r Mem.unchanged_on *)
          eapply Mem.store_unchanged_on; eauto. (*
          intros ofs Hrange.
          rewrite Hsp_blk.
          unfold not_stack_blk.
          auto. *)
      }
      split.
      { (**r star BinSem.step *)
        set (old_rs := rs0).
        assert (Heq: match_state_arm rbpf_st rs0 m0 old_rs el l jit_blk ofs0 st_final) by auto.
        clear Harm; rename Heq into Harm.
        assert (Hrs_eq: old_rs = rs0) by auto.

        destruct Harm.
        subst lsr old_rs.
        rename rs into rs0.
        rename arm_mem0 into m0.
        rename ofs into ofs0.

        eapply star_one.
        eapply exec_step_bin.
        - (**r find_instr *)
          instantiate (2 := Pstr r IR13 (SOimm (Int.mul (int_of_ireg r) (Int.repr 4)))).
          instantiate (1 := true).

          rewrite HPC_eq.

          assert (Heq: find_instr (Vptr arm_blk (Ptrofs.repr ofs0)) (jit_mem jit_st_final) =
                        find_instr (Vptr arm_blk (Ptrofs.repr ofs0)) m0). {
            unfold find_instr.
            simpl.
            erewrite <- Mem.load_unchanged_on_1; eauto.

            - (**r Mem.load *)
              destruct Mem.load eqn: Hload; [| reflexivity].
              erewrite <- Mem.load_unchanged_on_1; eauto.
              + (**r block neq *)
                intros ofs Hofs_range.
                simpl.
                rewrite Hsp_blk; unfold not_stack_blk.
                clear - Hneq1 Hneq4.
                unfold block_neq in Hneq4.
                intuition.
            - (**r block neq *)
              intros ofs Hofs_range.
              simpl.
              rewrite Hsp_blk; unfold not_stack_blk.
              clear - Hneq1 Hneq4.
              unfold block_neq in Hneq4.
              intuition.
          }
          rewrite <- Heq; clear Heq.

          assert (Heq: find_instr (Vptr arm_blk (Ptrofs.repr ofs0)) (jit_mem jit_st_final) =
                        find_instr (Vptr arm_blk (Ptrofs.repr ofs0)) (jit_mem st1)). {

            unfold find_instr.
            simpl.
            unfold sub_mem_blk in Hsub_mem.
            erewrite Hsub_mem; eauto.
            destruct Mem.load eqn: Hload; [| reflexivity].
            - erewrite Hsub_mem; eauto.
              rewrite <- Hst0_eq in Hofs1.
              rewrite Hofs1, Hofs0.

              unfold Ptrofs.add.
              unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_spilling.
              destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_spilling].
              erewrite upd_jitted_list_unsigned_repr_add_2; eauto.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            - rewrite <- Hst0_eq in Hofs1.
              rewrite Hofs1, Hofs0.

              unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_spilling.
              destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_spilling].
              erewrite upd_jitted_list_unsigned_repr; eauto.
              change (size_chunk Mint16unsigned) with 2.
              lia.
          }
          rewrite Heq; clear Heq.
          eapply lemma_thumb_str; eauto.
          change (Int.unsigned (Int.repr 4)) with 4.
          unfold int_of_ireg.
          rewrite Int.unsigned_repr;
            [| change Int.max_unsigned with 4294967295];
            unfold ireg2nat; destruct r; simpl; lia.
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

        set (old_rs := rs0).
        assert (Heq: match_state_arm rbpf_st rs0 m0 old_rs el l jit_blk ofs0 st_final) by auto.
        clear Harm; rename Heq into Harm.
        assert (Heq3: old_rs = rs0) by auto.

        destruct Harm.
        subst old_rs lsr.

        rename ofs into ofs0.
        rename rs into rs0.
        rename arm_mem0 into m0.
        rename lsr_stack into l.

        constructor.
        + (**r regs_agree *)
          unfold regs_agree.
          intros r0 HF.
          inversion HF.
        + (**r arm_synch_stack *)

          unfold arm_synch_stack.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].

          unfold arm_synch_stack in Hstack.

          destruct Hstack as (Hload & Hstack).
          rewrite Hsp_blk in *.
          simpl in Hload.
          split.
          {
            simpl.
            rewrite <- Hload.
            clear - Hcallee_save Hstore.
            eapply Mem.load_store_other; eauto.
            right; left.
            change (Ptrofs.unsigned Ptrofs.zero + size_chunk Mint32) with 4.
            unfold ireg2nat.
            repeat (destruct Hcallee_save as [Hcallee_save | Hcallee_save]; [ subst r; lia | ]).
            inversion Hcallee_save.
          }

          intros r0 Hin.
          simpl.
          rewrite Ptrofs.add_zero_l.
          rewrite Hreg_mul4_unsigned.
          destruct Hin as [Hreg_eq | Hin].
          { (**r r0 = r *)
            subst r0.
            erewrite Mem.load_store_same; eauto.
            f_equal.
            unfold Val.load_result.
            destruct arm_inv_reg as (Harm_inv_reg0 & _).
            clear - Harm_inv_reg0.
            unfold arm_assume_register_map in *.
            specialize (Harm_inv_reg0 r).
            destruct (rs0 r); inversion Harm_inv_reg0; try reflexivity.
          }
          (**r In r0 l *)
          specialize (Hstack r0 Hin).
          rewrite <- Hstack.
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
          unfold ireg2nat.
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

        + (**r rs PC *)
          rewrite HPC_eq.
          rewrite Pregmap.gss.
          unfold Val.offset_ptr.
          f_equal.
          rewrite <- Hst0_eq in Hofs1.
          rewrite Hofs1, Hofs0.
          unfold Ptrofs.add.

          unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_spilling.
          destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_spilling].
          erewrite upd_jitted_list_unsigned_repr; eauto.

          f_equal.
          change (Ptrofs.unsigned wsize) with 4.
          lia.

        + (**r Mem.valid_block *)
          assumption.
        + (**r Mem.unchanged_on *)
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          eapply store_unchanged_on_3; eauto.
          intros ofs2 HF.
          destruct HF as (_ & HF).
          rewrite Hsp_blk in HF.
          unfold not_stack_blk in HF.
          apply HF.
          reflexivity.

        + (**r Mem.unchanged_on *)
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          eapply store_unchanged_on_4; eauto.
          intros ofs2 HF.
          destruct HF as (_ & HF).
          rewrite Hsp_blk in HF.
          unfold not_stack_blk in HF.
          apply HF.
          reflexivity.
      }
    Qed.

    Lemma jit_spilling_simulation: forall l rbpf_st st0 st1 st_final rs0 ofs0 ofs1 m0
      (Hpre: arm_registers_pre_weak rs0)
      (Hjit_spilling: jit_alu32_thumb_save l st0 = Some st1)
      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Harm_blk: jitted_list st0 = Vptr jit_blk Ptrofs.zero)
      (Hreg_blk: jit_regs st0 = Vptr regs_blk Ptrofs.zero)
      (Hmem: sub_mem_blk (jit_mem st1) (jit_mem st_final) jit_blk ofs1)
      (Harm_inv: arm_memory_inv0 st0 rs0 rs0 m0 m0
        flag_blk regs_blk jit_blk jit_state_blk (fun b _ => not_stack_blk (rs0 IR13) b))

      (Hnodup: NoDup l)
      (Harm_callee_save: forall r, List.In r l -> List.In r arm_callee_save_regs)

      (Harm: match_state_arm rbpf_st rs0 m0 rs0 [] l jit_blk ofs0 st_final),
        exists rs1 m1,
          arm_registers_spilling l rs0 rs1 m0 m1 /\
          arm_registers_pre_weak rs1 /\
          arm_memory_inv0 st1 rs0 rs1 m0 m1
            flag_blk regs_blk jit_blk jit_state_blk (fun b _ => not_stack_blk (rs0 IR13) b) /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
          match_state_arm rbpf_st rs1 m1 rs0 [] l jit_blk ofs1 st_final.
    Proof.
      induction l; intros.
      { (**r l = [] *)
        unfold arm_registers_spilling.
        simpl.
        exists rs0.
        exists m0.
        split; [split; reflexivity | ].
        split; [assumption | ].
        split.
        { simpl in Hjit_spilling.
          injection Hjit_spilling as Hst_eq.
          subst st0.
          assumption.
        }
        split;[constructor | ].
        simpl in Hjit_spilling.
        injection Hjit_spilling as Heq.
        subst st1.
        rewrite <- Hofs0 in Hofs1.
        subst ofs1.
        assumption.
      }

      (**r l = hd :: tl *)
      unfold arm_registers_spilling; simpl.

      simpl in Hjit_spilling.

      destruct jit_alu32_thumb_upd_save eqn: Hone_spilling; [| inversion Hjit_spilling].
      rename j into stk.

      eapply jit_spilling_one_simulation with (st_final := st_final)
        (rbpf_st := rbpf_st) (rs0 := rs0) (l := l)
        (ofs0 := ofs0) (ofs1 := Z.of_nat (2 * jitted_len stk)) (m0 := m0)
        in Hone_spilling as Hone; eauto.

      - (**r MAIN *)
        destruct Hone as (rsk & mk & Harm_registers_spilling_one & Hrs_eq & Hpre_rs1 & Harm_memory_inv0 & Hsemk & Hmatch_armk).
        rewrite Harm_registers_spilling_one.
        unfold arm_registers_spilling in IHl.

        specialize (IHl rbpf_st stk st1 st_final
          (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)) (Z.of_nat (2 * jitted_len stk)) ofs1 mk).

        assert (Hpre_cond: arm_registers_pre_weak rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)). {
          clear - Hpre.
          unfold arm_registers_pre_weak in *.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          assumption.
        }
        specialize (IHl Hpre_cond Hjit_spilling); clear Hpre_cond.

        assert (Heq: Z.of_nat (2 * jitted_len stk) = Z.of_nat (2 * jitted_len stk)) by reflexivity.
        specialize (IHl Heq Hofs1); clear Heq.

        assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
          clear - old_sp Hone_spilling Harm_blk.
          unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
          eapply upd_jitted_list_unchange_jittted_list_2 in Hone_spilling.
          rewrite <- Hone_spilling.
          assumption.
        }
        specialize (IHl Heq); clear Heq.

        assert (Heq: jit_regs stk = Vptr regs_blk Ptrofs.zero). {
          clear - old_sp Hone_spilling Hreg_blk.
          unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
          eapply upd_jitted_list_unchange_jit_regs_2 in Hone_spilling.
          rewrite <- Hone_spilling.
          assumption.
        }
        specialize (IHl Heq Hmem); clear Heq.

        assert (Heq: arm_memory_inv0 stk rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)
                      rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) mk mk
                      flag_blk regs_blk jit_blk jit_state_blk
                        (fun b _ => not_stack_blk (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) IR13) b)). {
          clear - Harm_registers_spilling_one Hone_spilling Harm_blk Hreg_blk Harm_inv.
          destruct Harm_inv.

          destruct arm_inv_stk as (_ & arm_inv_stk0).
          unfold arm_stack_pointer_spec in *.
          destruct arm_inv_stk0 as (sp_blk & Heq & (Hneq0 & Hneq1 & Hneq2 & Hneq3) & Hneq4 & Hperm).

          unfold arm_registers_spilling_one in *.
          rewrite Heq in Harm_registers_spilling_one.
          simpl in Harm_registers_spilling_one.
          rewrite Ptrofs.add_zero_l in Harm_registers_spilling_one.

          split.
          - (**r arm_inv_stk *)
            split; [reflexivity | ].
            exists sp_blk.
            split.
            + rewrite Pregmap.gso; [ | intros HF; inversion HF].
              assumption.
            + split.
              { repeat (split; [assumption | ]).
                assumption.
              }
              split; [assumption | ].
              clear - Harm_registers_spilling_one Heq Harm_blk Hperm.

              unfold Mem.range_perm in *.
              intros ofs Hrange.
              specialize (Hperm ofs Hrange).
              eapply Mem.perm_store_1; eauto.
          - (**r arm_inv_reg *)
            destruct arm_inv_reg as (Harm_inv_reg0 & _ & Hrange_perm).
            split.
            { unfold arm_assume_register_map.
              unfold arm_assume_register_map in Harm_inv_reg0.
              intros r.
              specialize (Harm_inv_reg0 r).
              destruct r.
              + rewrite Pregmap.gso; [ | intros HF; inversion HF].
                destruct i; try (rewrite Pregmap.gss; auto);
                  try (rewrite Pregmap.gso; [ | intros HF; inversion HF]); try assumption.
              + repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              + repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              + rewrite Pregmap.gss; auto.
                unfold Val.offset_ptr; destruct (rs0 PC); auto.
            }
            split.
            { unfold arm_assume_register_map.
              unfold arm_assume_register_map in Harm_inv_reg0.
              intros r.
              specialize (Harm_inv_reg0 r).
              destruct r.
              + rewrite Pregmap.gso; [ | intros HF; inversion HF].
                destruct i; try (rewrite Pregmap.gss; auto);
                  try (rewrite Pregmap.gso; [ | intros HF; inversion HF]); try assumption.
              + repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              + repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              + rewrite Pregmap.gss; auto.
                unfold Val.offset_ptr; destruct (rs0 PC); auto.
            }
            unfold Mem.range_perm in *.
            intros ofs Hrange.
            specialize (Hrange_perm ofs Hrange).
            eapply Mem.perm_store_1; eauto.
          - (**r jit_state_memory_layout *)
            unfold jit_state_memory_layout in *.
            destruct arm_inv_st as (arm_inv_st0 & arm_inv_st1).
            split.
            + rewrite <- arm_inv_st0.
              eapply Mem.load_store_other; eauto.
            + unfold regs_layout in *.
              intros r.
              specialize (arm_inv_st1 r).
              destruct arm_inv_st1 as (vi & Hreg & Hload0 & Hload1 & Hreg_eq).
              exists vi.
              split.
              * rewrite <- Hreg.
                symmetry.
                unfold jit_alu32_thumb_upd_save in Hone_spilling.
                unfold jit_alu32_thumb_load_store_template_jit in Hone_spilling.
                eapply upd_jitted_list_unchange_eval_jit_reg_2
                  with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
                (**r jit_regs st0 = Vptr regs_blk Ptrofs.zero *)
                clear - Hneq4; unfold block_neq in Hneq4.
                intuition.
              * rewrite <- Hload0.
                split; [eapply Mem.load_store_other; eauto | ].
                rewrite <- Hload1.
                split; [eapply Mem.load_store_other; eauto | ].

                intros vj Heval_reg0.
                apply Hreg_eq.
                rewrite <- Heval_reg0.
                eapply upd_jitted_list_unchange_eval_jit_reg_2
                  with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
                clear - Hneq4; unfold block_neq in Hneq4.
                intuition.
          - (**r Mem.extends *)
            apply Mem.unchanged_on_refl.
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

        assert (Heq: match_state_arm rbpf_st rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) mk
                      rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) [] l jit_blk
                      (Z.of_nat (2 * jitted_len stk)) st_final). {
          clear - Hone_spilling Harm Harm_callee_save Harm_registers_spilling_one Harm_memory_inv0 Hnodup Hofs0.

          eapply exec_step.
          - (**r regs_agree *)
            unfold regs_agree in *.
            intros r Hin.
            inversion Hin.
          - (**r arm_synch_stack *)
            destruct Harm_memory_inv0.
            destruct arm_inv_stk as (Hrs0_eq & Hsp_spec).
            unfold arm_stack_pointer_spec in Hsp_spec.
            destruct Hsp_spec as (sp_blk & Hrs13_eq & Hneq).

            set (rs := rs0).
            assert (Heq: match_state_arm rbpf_st rs0 m0 rs [] (a :: l) jit_blk ofs0 st_final). {
              subst rs.
              assumption.
            }
            assert (Heq1: rs = rs0) by auto.
            set (tl := a :: l).
            assert (Heq2: tl = a :: l) by auto.
            rewrite <- Heq2 in *.
            destruct Heq.
            subst rs lsr_stack.

            rename old_rs into rs0.

            unfold arm_synch_stack in *.
            destruct Hstack as (Hstack0 & Hstack1).
            rewrite Pregmap.gso; [ | intros HF; inversion HF].

            split.
            +
              unfold arm_registers_spilling_one in Harm_registers_spilling_one.
              rewrite Hrs13_eq in *.
              rewrite <- Hstack0.
              simpl.
              clear - Harm_callee_save Harm_registers_spilling_one Hneq.
              simpl in Harm_registers_spilling_one.
              eapply Mem.load_store_other; eauto.
              right. left. (**r here we need the ireg is a callee-save register *)

              specialize (Harm_callee_save a).
              assert (Heq: In a (a :: l)).  {
                simpl.
                left; reflexivity.
              }

              specialize (Harm_callee_save Heq); clear Heq.
              clear - Harm_callee_save.
              unfold arm_callee_save_regs in Harm_callee_save.
              rewrite Ptrofs.add_zero_l.
              change (Ptrofs.unsigned Ptrofs.zero) with 0.
              change (0 + size_chunk Mint32) with 4.
              rewrite Hreg_mul4_unsigned.
              unfold ireg2nat; destruct a; try lia.
              rename Harm_callee_save into HF.
              do 8 (destruct HF as [HF | HF]; [inversion HF | ]).
              inversion HF.

            + intros r.
              specialize (Hstack1 r). (*
              destruct Hstack1 as (Hstack1 & Hstack2). *)

              rewrite Pregmap.gso; [ | intros HF; inversion HF].

              intro Hin.

              assert (Heq: In r (a :: l)) by (simpl; right; assumption).
              specialize (Hstack1 Heq); clear Heq.

              unfold arm_registers_spilling_one in Harm_registers_spilling_one.
              rewrite Hrs13_eq in *.
              rewrite <- Hstack1.
              simpl.
              simpl in Harm_registers_spilling_one.
              rewrite Ptrofs.add_zero_l in *.
              eapply Mem.load_store_other; eauto.
              right.
              rewrite Hreg_mul4_unsigned.

              (**r here we need to know r <> a because of NoDup (a :: l) *)
              assert (Heq: In r (a :: l)) by (simpl; right; assumption).
              specialize (Harm_callee_save r Heq) as Hr_range.
              assert (Hneq_reg: r <> a). {
                rewrite NoDup_cons_iff in Hnodup.
                destruct Hnodup as (Hnin & _).
                intro HF.
                apply Hnin.
                subst r; assumption.
              }
              clear Heq.
              assert (Heq: In a (a :: l)) by (simpl; left; reflexivity).
              specialize (Harm_callee_save a Heq) as Ha_range; clear Heq.
              rewrite Hreg_mul4_unsigned.
              change (size_chunk Mint32) with 4.
              clear - Hneq_reg Hr_range Ha_range.
              unfold ireg2nat.
              unfold arm_callee_save_regs, In in *.
              destruct Hr_range as [Hr_range | Hr_range].
              { subst r.
                destruct Ha_range as [Ha_range | Ha_range].
                { subst a. exfalso. apply Hneq_reg; reflexivity. }
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                inversion Ha_range.
              }

              destruct Hr_range as [Hr_range | Hr_range].
              { subst r.
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                destruct Ha_range as [Ha_range | Ha_range].
                { subst a. exfalso. apply Hneq_reg; reflexivity. }
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                inversion Ha_range.
              }

              destruct Hr_range as [Hr_range | Hr_range].
              { subst r.
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                destruct Ha_range as [Ha_range | Ha_range].
                { subst a. exfalso. apply Hneq_reg; reflexivity. }
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                inversion Ha_range.
              }

              destruct Hr_range as [Hr_range | Hr_range].
              { subst r.
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                destruct Ha_range as [Ha_range | Ha_range].
                { subst a. exfalso. apply Hneq_reg; reflexivity. }
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                inversion Ha_range.
              }

              destruct Hr_range as [Hr_range | Hr_range].
              { subst r.
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                destruct Ha_range as [Ha_range | Ha_range].
                { subst a. exfalso. apply Hneq_reg; reflexivity. }
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                inversion Ha_range.
              }

              destruct Hr_range as [Hr_range | Hr_range].
              { subst r.
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                destruct Ha_range as [Ha_range | Ha_range].
                { subst a. exfalso. apply Hneq_reg; reflexivity. }
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                inversion Ha_range.
              }

              destruct Hr_range as [Hr_range | Hr_range].
              { subst r.
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                destruct Ha_range as [Ha_range | Ha_range].
                { subst a. exfalso. apply Hneq_reg; reflexivity. }
                destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | inversion Ha_range].
              }

              destruct Hr_range as [Hr_range | Hr_range].
              { subst r.
                repeat (destruct Ha_range as [Ha_range | Ha_range]; [ subst a; lia | ]).
                destruct Ha_range as [Ha_range | Ha_range].
                { subst a. exfalso. apply Hneq_reg; reflexivity. }
                inversion Ha_range.
              }

              inversion Hr_range.
          - (**r rs PC *)
            rewrite  Pregmap.gss.

            (**r TODO: currently, we have to use this way to destruct an inductive semantics in Coq *)
            destruct Harm_memory_inv0.
            destruct arm_inv_stk as (Hrs0_eq & Hsp_spec).
            unfold arm_stack_pointer_spec in Hsp_spec.
            destruct Hsp_spec as (sp_blk & Hrs13_eq & Hneq).

            set (rs := rs0).
            assert (Heq: match_state_arm rbpf_st rs0 m0 rs [] (a :: l) jit_blk ofs0 st_final) by auto.
            assert (Heq1: rs = rs0) by auto.
            set (tl := a :: l).
            assert (Heq2: tl = a :: l) by auto.
            rewrite <- Heq2 in *.
            destruct Heq.
            subst rs lsr_stack.

            rename old_rs into rs0.

            rewrite HPC_eq, Hofs0.

            unfold Val.offset_ptr.
            f_equal.
            unfold Ptrofs.add, wsize.
            change (Ptrofs.unsigned (Ptrofs.repr 4)) with 4.
            f_equal.

            clear - Hone_spilling.
            unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in *.
            eapply upd_jitted_list_jittted_len_2 in Hone_spilling as Heq.
            rewrite <- Heq.

            destruct upd_jitted_list eqn: Hupd; [| inversion Hone_spilling].
            apply upd_jitted_list_max in Hupd.
            unfold JITTED_LIST_MAX_LENGTH in Hupd.
            rewrite Ptrofs.unsigned_repr; [lia | ].
            change Ptrofs.max_unsigned with 4294967295; lia.
          - (**r Mem.valid_block *)
            destruct Harm_memory_inv0.
            destruct Harm.
            assumption.
          - (**r Mem.unchanged_on *)
            destruct Harm_memory_inv0.

            set (rs := rs0).
            assert (Heq: match_state_arm rbpf_st rs0 m0 rs [] (a :: l) jit_blk ofs0 st_final) by auto.
            assert (Heq1: rs = rs0) by auto.
            set (tl := a :: l).
            assert (Heq2: tl = a :: l) by auto.
            rewrite <- Heq2 in *.
            destruct Heq.
            subst old_rs lsr_stack.

            rename rs into rs0.
            rewrite HPC_eq.
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            apply Mem.unchanged_on_trans with (m2 := arm_mem0); auto.
            clear - arm_inv_stk arm_mem.
            eapply Mem.unchanged_on_implies; eauto.
            intros.
            simpl.
            destruct arm_inv_stk as (_ & Hsp_spec).
            unfold arm_stack_pointer_spec in Hsp_spec.
            destruct Hsp_spec as (sp_blk & Hrs_eq & Hneq).
            intuition.
          - (**r Mem.unchanged_on *)
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            destruct Harm_memory_inv0.

            set (rs := rs0).
            assert (Heq: match_state_arm rbpf_st rs0 m0 rs [] (a :: l) jit_blk ofs0 st_final) by auto.
            assert (Heq1: rs = rs0) by auto.
            set (tl := a :: l).
            assert (Heq2: tl = a :: l) by auto.
            rewrite <- Heq2 in *.
            destruct Heq.
            subst old_rs lsr_stack.

            rename rs into rs0.
            rename arm_mem0 into m0.
            clear - arm_inv_stk Harm_registers_spilling_one Harm_mem2.
            unfold arm_registers_spilling_one in Harm_registers_spilling_one.
            destruct arm_inv_stk as (_ & Hsp_spec).
            unfold arm_stack_pointer_spec in Hsp_spec.
            destruct Hsp_spec as (sp_blk & Hrs_eq & Hneq).
            rewrite Hrs_eq in *.
            simpl in Harm_registers_spilling_one.
            rewrite Ptrofs.add_zero_l in Harm_registers_spilling_one.
            rewrite Hreg_mul4_unsigned in Harm_registers_spilling_one.
            eapply store_unchanged_on_4; eauto.
            intros.
            intro HF.
            simpl in HF.
            destruct HF as (_ & HF).
            apply HF.
            reflexivity.
        }
        specialize (IHl Heq); clear Heq.

        destruct IHl as (rs1 & m1 & Haux & Hpre_rs & Harm_memory_inv1 & Hsem1 & Hmatch_arm1).
        exists rs1, m1.
        rewrite <- Hrs_eq in *.
        split; [assumption | ].
        split; [assumption | ].

        assert (Heq0: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
          clear - old_sp Hone_spilling Harm_blk.
          unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
          eapply upd_jitted_list_unchange_jittted_list_2 in Hone_spilling.
          rewrite <- Hone_spilling.
          assumption.
        }

        assert (Heq1: jit_regs stk = Vptr regs_blk Ptrofs.zero). {
          clear - old_sp Hone_spilling Hreg_blk.
          unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
          eapply upd_jitted_list_unchange_jit_regs_2 in Hone_spilling.
          rewrite <- Hone_spilling.
          assumption.
        }

        split.
        + (**r arm_memory_inv0 *)
          clear - Harm_memory_inv1 Harm_memory_inv0 Hrs_eq Harm_registers_spilling_one
                  Hone_spilling Hjit_spilling Harm_blk Hreg_blk Heq0 Heq1.
          destruct Harm_memory_inv1.
          destruct Harm_memory_inv0.
          destruct arm_inv_stk as (Hrs_eqk & arm_inv_stk).
          destruct arm_inv_stk0 as (Hes_eq1 & arm_inv_stk0).
          destruct arm_inv_reg as (Hassumek & Hassume1 & Hrange_permk).
          destruct arm_inv_reg0 as (Hassume0 & _ & Hrange_perm0).
          constructor; try assumption.
          * (**r arm_stack_pointer_spec*)
            split; [rewrite Hes_eq1; assumption | assumption].
          * (**r arm_assume_register_map *)
            split;[assumption | ].
            split; assumption.
          * (**r jit_state_memory_layout *)
            unfold jit_state_memory_layout in *.
            destruct arm_inv_st0 as (arm_inv_st0 & arm_inv_st1).
            split; [assumption | ].
            unfold regs_layout in *.
            intros r.
            specialize (arm_inv_st1 r).
            destruct arm_inv_st1 as (vi & Hreg & Hload0 & Hload1 & Hreg_eq).
            exists vi.
            split.
            { rewrite <- Hreg.
              symmetry.
              eapply jit_alu32_thumb_save_unchange_eval_jit_reg
                with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.

              unfold arm_stack_pointer_spec in arm_inv_stk0.
              destruct arm_inv_stk0 as (sp_blk & _ & _ & Hneq & _ ).
              clear - Hneq; unfold block_neq in Hneq.
              intuition.
            }
            split;[assumption | ].
            split;[assumption | ].
            intros vj Heval_reg0.
            apply Hreg_eq.
            rewrite <- Heval_reg0.
            eapply jit_alu32_thumb_save_unchange_eval_jit_reg
              with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.

            unfold arm_stack_pointer_spec in arm_inv_stk0.
            destruct arm_inv_stk0 as (sp_blk & _ & _ & Hneq & _ ).
            clear - Hneq; unfold block_neq in Hneq.
            intuition.
          * (**r Mem.unchanged_on *)
            rewrite <- Hes_eq1 in arm_mem.
            eapply Mem.unchanged_on_trans; eauto.
        + split.
          * (**r star BinSem.step *)
            eapply star_trans; eauto.
          * (**r match_state_arm *)
            destruct Harm_memory_inv1.
            clear - flag_blk regs_blk jit_blk jit_state_blk arm_inv_stk Hnodup Haux Hmatch_arm1 Hmatch_armk Hrs_eq
              Hone_spilling Hjit_spilling Hofs0 Hofs1 Harm_registers_spilling_one Harm_callee_save.

            destruct arm_registers_spilling_aux eqn: Hspilling_n; [| inversion Haux].
            destruct p.
            destruct Haux as (Haux_rs & Haux_m).
            subst r m.

            set (el := []).
            assert (Heq1: el = []) by auto.
            rewrite <- Heq1 in *.

            set (jit_blk' := jit_blk).
            assert (Heq4: jit_blk' = jit_blk) by auto.
            rewrite <- Heq4 in *.

            destruct Hmatch_arm1.
            rename lsr_stack into l.

            (**r TODO: currently, we have to use this way to destruct an inductive semantics in Coq *)
            set (tl := a :: l).
            assert (Heq2: tl = a :: l) by auto.
            rewrite <- Heq2 in *.
            set (ofsk := Z.of_nat (2 * jitted_len stk)).
            assert (Heq3: ofsk = Z.of_nat (2 * jitted_len stk)) by auto.
            rewrite <- Heq3 in *.

            destruct Hmatch_armk.
            subst lsr lsr_stack ofs1.
            rename ofs into ofs1.

            rename arm_mem0 into mk.
            rename arm_mem into m1.
            rename rs into rs1.
            rename rs0 into rsk.
            rename old_rs into rs0.

            constructor; try assumption.

            (**r arm_synch_stack *)
            unfold arm_synch_stack in *.
            destruct Hstack as (Hstack & Hstack1).
            destruct Hstack0 as (Hstack0 & Hstack2).
            split; [assumption | ].
            intros r Hin.
            specialize (Hstack2 r Hin).
            assert (Heq: rs0 r = rsk r). {
              rewrite Hrs_eq.
              rewrite Pregmap.gso; [ reflexivity | intros HF; inversion HF].
            }
            rewrite Heq in *; clear Heq.

            destruct Hin as [Hreg_eq | Hin].
            { subst a.
              rewrite NoDup_cons_iff in Hnodup.
              destruct Hnodup as (Hnin & Hnodup).
              destruct arm_inv_stk as (Hrsk_eq & Hsp_spec).
              unfold arm_stack_pointer_spec in Hsp_spec.
              destruct Hsp_spec as (sp_blk & Hrsk_ptr & (Hneq0 & Hneq1 & Hneq2 & Hneq3) & Hneq4 & Hperm).
              rewrite <- Hrsk_eq.

              eapply arm_registers_spilling_aux_load_same with (rs0 := rsk) (rs1:= rs1); eauto.
              + intros r0 Hr0_in.
                apply Harm_callee_save.
                right; assumption.
              + apply Harm_callee_save; left; reflexivity.
              + unfold arm_stack_pointer_spec.
                exists sp_blk.
                subst arm_blk.
                split; [assumption | ].
                split.
                { repeat (split; [assumption | ]).
                  assumption.
                }
                split; assumption.
            }
            apply Hstack1; auto.

      - (**r ~ In *)
        rewrite NoDup_cons_iff in Hnodup.
        destruct Hnodup as (Hnin & _).
        assumption.
      - (**r arm_callee_save_regs *)
        apply Harm_callee_save; left; reflexivity.
      - (**r arm_callee_save_regs *)
        intros.
        apply Harm_callee_save.
        right; assumption.
      - (**r sub_mem_blk *)
        unfold sub_mem_blk in Hmem.
        unfold sub_mem_blk.
        intros chunk ofs2 Hrange.
        specialize (Hmem chunk ofs2).
        assert (Heq: 0 <= ofs2 /\ ofs2 + size_chunk chunk <= ofs1). {
          assert (Heq: (jitted_len stk <= jitted_len st1)%nat). {
            eapply jit_alu32_thumb_save_jitted_len_leb; eauto.
          }
          lia.
        }
        specialize (Hmem Heq); clear Heq.

        rewrite <- Hmem.

        eapply jit_alu32_thumb_save_load_same; eauto.
        unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hone_spilling.
        eapply upd_jitted_list_unchange_jittted_list_2 in Hone_spilling; eauto.
        rewrite <- Hone_spilling; assumption.
      - (**r match_state_arm *)

        set (rs := rs0).
        assert (Heq: match_state_arm rbpf_st rs0 m0 rs [] (a :: l) jit_blk ofs0 st_final). {
          subst rs.
          assumption.
        }
        assert (Heq0: rs = rs0) by auto.
        set (el := []).
        assert (Heq1: el = []) by auto.
        rewrite <- Heq1 in *.

        set (jit_blk' := jit_blk).
        assert (Heq4: jit_blk' = jit_blk) by auto.
        rewrite <- Heq4 in *.

        set (tl := a :: l).
        assert (Heq2: tl = a :: l) by auto.
        rewrite <- Heq2 in *.

        destruct Heq.
        subst lsr lsr_stack arm_blk old_rs.
        rename ofs into ofs0.
        rename arm_mem into m0.
        rename rs into rs0.

        constructor; try assumption.
        (**r arm_synch_stack *)
        unfold arm_synch_stack in *.
        destruct Hstack as (Hstack & Hstack1).
        split; [assumption | ].
        intros r Hin.
        apply Hstack1.
        right; assumption.
    Qed.

  End JITSpilling.

  Section JITLoad.

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
    Qed.

    Definition arm_registers_load_one (r: reg) (rs0: Asm.regset) (v: int): Asm.regset :=
      (rs0 # (ireg_of_reg r) <- (Vint v)) # PC <-
                    (Val.offset_ptr (rs0 # (ireg_of_reg r) <- (Vint v) PC) wsize).

    Fixpoint arm_registers_load_aux (l: list reg) (rbpf_st: state) (rs0: Asm.regset): option Asm.regset :=
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


    Lemma jit_load_one_simulation: forall r l l1 v rbpf_st st0 st1 st_final old_rs rs0 ofs0 ofs1 m0
      (Hpre: arm_registers_pre_weak rs0)
      (Hupd_load : jit_alu32_thumb_upd_load r st0 = Some st1)

      (Hlist_not_in: ~ List.In r l)
      (Harm_inv: arm_memory_inv0 st0 rs0 rs0 m0 m0
        flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True))

      (Hsub_mem: sub_mem_blk (jit_mem st1) (jit_mem st_final) jit_blk ofs1)
      (Hofs0 : ofs0 = Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))
      (Hofs1 : ofs1 = Z.of_nat (jitted_len st1 + (jitted_len st1 + 0)))
      (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
      (Hreg_blk: jit_regs st0 = Vptr regs_blk Ptrofs.zero)
      (Harm: match_state_arm rbpf_st rs0 m0 old_rs l l1 jit_blk ofs0 st_final)
      (Hreg_rv0: (eval_reg r rbpf_st) = Some (Val.longofintu (Vint v)))
      (Hreg_rv1: (eval_jit_reg r st0) = Some (Val.longofintu (Vint v))),
        exists rs1,
          arm_registers_load_one r rs0 v = rs1 /\
          arm_memory_inv0 st1 rs0 rs1 m0 m0
            flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True) /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
          match_state_arm rbpf_st rs1 m0 old_rs (r::l) l1 jit_blk ofs1 st_final.
    Proof.
      intros.
      unfold jit_alu32_thumb_upd_load in Hupd_load.
      unfold jit_alu32_thumb_load_store_template_jit in Hupd_load.
      destruct Harm_inv.
      destruct arm_inv_stk as (_ & Hsp_spec).
      unfold arm_stack_pointer_spec in Hsp_spec.
      destruct Hsp_spec as (sp_blk & Hrs13_eq & (Hneq0 & Hneq1 & Hneq2 & Hneq3) & Hneq4 & Hperm).


      unfold arm_registers_load_one.
      exists ((rs0 # (ireg_of_reg r) <- (Vint v)) # PC <-
  (Val.offset_ptr (rs0 # (ireg_of_reg r) <- (Vint v) PC) wsize)).
      split; [reflexivity | ].

      split.
      { (**r arm_memory_inv0 *)
        constructor; try assumption.
        - (**r rs IR13 + arm_stack_pointer_spec *)
          rewrite Hrs13_eq.
          split.
          + rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Hrs13_eq.
            reflexivity.
            clear - HF.
            unfold ireg_of_reg in HF; destruct r; simpl in HF; inversion HF.
          + unfold arm_stack_pointer_spec.
            exists sp_blk.
            split; [assumption | ].
            split.
            { repeat (split; [assumption | ]); assumption. }
            split; assumption.
        - (**r arm_assume_register_map *)
          destruct arm_inv_reg as (Harm_inv_reg0 & _ & Hrange_perm).
          split; [assumption | ].
          split; [ | assumption].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          unfold arm_assume_register_map in Harm_inv_reg0.
          unfold arm_assume_register_map.
          intros r0.
          specialize (Harm_inv_reg0 r0).
          destruct r0.
          + rewrite Pregmap.gso; [ | intros HF; inversion HF].

            destruct (ireg_eqb (ireg_of_reg r) i) eqn: Hreg_eqb;
              [ apply ireg_eqb_true in Hreg_eqb |
                apply ireg_eqb_false in Hreg_eqb
              ].
            * subst i.
              rewrite Pregmap.gss.
              auto.
            * rewrite Pregmap.gso; [assumption | intros HF; inversion HF].
              apply Hreg_eqb.
              auto.
          + repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
          + repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
          + rewrite Pregmap.gss; auto.
            unfold Val.offset_ptr; destruct (rs0 PC); auto.
        - (**r jit_state_memory_layout *)
          unfold jit_state_memory_layout in *.
          destruct arm_inv_st as (arm_inv_st0 & arm_inv_st1).
          split; [assumption | ].
          unfold regs_layout in *.
          intros r0.
          specialize (arm_inv_st1 r0).
          destruct arm_inv_st1 as (vi & Hreg & Hload0 & Hload1 & Hreg_eq).
          exists vi.
          split.
          + rewrite <- Hreg.
            symmetry.
            eapply upd_jitted_list_unchange_eval_jit_reg_2
              with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
            (**r jit_regs st0 = Vptr regs_blk Ptrofs.zero *)
            clear - Hneq4; unfold block_neq in Hneq4.
            intuition.
          + split; [assumption | ].
            split; [assumption | ].

            intros vj Heval_reg0.
            apply Hreg_eq.
            rewrite <- Heval_reg0.
            eapply upd_jitted_list_unchange_eval_jit_reg_2
              with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
            clear - Hneq4; unfold block_neq in Hneq4.
            intuition.
      }

      destruct Harm.

      rename ofs into ofs0.
      rename rs into rs0.
      rename arm_mem0 into m0.
      rename lsr_stack into l.

      rename arm_blk into jit_blk.
      rename jit_st_final into st_final.

      unfold arm_registers_pre_weak in Hpre.
      unfold jit_state_memory_layout in arm_inv_st.

      simpl in arm_inv_st.
      destruct arm_inv_st as (Hst_load & Hlayout).
      unfold regs_layout in Hlayout.

      specialize (Hlayout r).
      destruct Hlayout as (vi & Hlayout).

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

      split.
      { (**r star BinSem.step *)
        eapply star_one.
        eapply exec_step_bin.
        - (**r find_instr *)
          rewrite HPC_eq.

          assert (Heq: find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st_final) =
                        find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) m0). {
            unfold find_instr.
            simpl.
            erewrite <- Mem.load_unchanged_on_1; eauto.

            - (**r Mem.load *)
              clear Hst_load Hlayout.
              destruct Mem.load eqn: Hload; [| reflexivity].
              erewrite <- Mem.load_unchanged_on_1; eauto.
              + (**r block neq *)
                intros ofs Hofs_range.
                simpl.
                rewrite Hrs13_eq; unfold not_stack_blk.
                clear - Hneq1 Hneq4.
                unfold block_neq in Hneq4.
                intuition.
            - (**r block neq *)
              intros ofs Hofs_range.
              simpl.
              rewrite Hrs13_eq; unfold not_stack_blk.
              clear - Hneq1 Hneq4.
              unfold block_neq in Hneq4.
              intuition.
          }
          rewrite <- Heq; clear Heq.

          assert (Heq: find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st_final) =
                        find_instr (Vptr jit_blk (Ptrofs.repr ofs0)) (jit_mem st1)). {
            unfold find_instr.
            rewrite Hofs0; simpl.
            clear - flag_blk regs_blk jit_blk jit_state_blk Hofs1 Hupd_load Hsub_mem Hcond Hlen_eq Hlen_eq0.
            unfold sub_mem_blk in Hsub_mem.
            repeat rewrite <- Hsub_mem.

            3:{
              rewrite Hofs1.
              rewrite Hlen_eq0.
              simpl.
              lia.
            }

            2:{
              simpl.
              unfold Ptrofs.add, Ptrofs.of_int.
              rewrite Hlen_eq0.
              change (Ptrofs.unsigned (Ptrofs.repr (Int.unsigned (Int.repr 2)))) with 2.

              assert (Hlen_eq1: Ptrofs.unsigned (Ptrofs.repr 
                    (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2))  = 
                  (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)) + 2)). {
                rewrite Ptrofs.unsigned_repr.
                reflexivity.
                change Ptrofs.max_unsigned with 4294967295; lia.
              }
              rewrite Hlen_eq1.
              lia.
            }
            reflexivity.
          }
          rewrite Heq; clear Heq.

          instantiate (2 := Pldr (ireg_of_reg r) IR12 (SOimm (Int.repr (8 * id_of_reg r)))).
          instantiate (1 := true).
          eapply lemma_thumb_ldr; eauto.
          + destruct r; simpl; lia.
          + unfold int_of_ireg.
            change (Z.of_nat (ireg2nat IR12)) with 12.
            rewrite ireg2nat_ireg_of_reg_reg2nat.
            change (Int.unsigned (Int.repr 8)) with 8.
            rewrite reg_mul_8_eq in Hupd_load.
            assumption.
        - (**r exec_instr *)
          remember ((Int.repr (8 * id_of_reg r))) as Hnum.
          simpl.
          rewrite Hpre; simpl.
          rewrite Ptrofs.add_zero_l.
          unfold exec_load; simpl.
          subst Hnum.
          rewrite ptrofs_unsigned_repr_reg_mul_8.
          destruct Hlayout as (Heval_reg & Hload64 & Hload32 & Haxiom).
          rewrite Hload32.
          f_equal.
          unfold nextinstr.
          specialize (Haxiom v Hreg_rv1).
          subst vi.
          reflexivity.
      }

      { (**r match_state_arm *)
        constructor; try assumption.
        - (**r regs_agree *)
          unfold regs_agree in *.
          intros r0 Hin.
          destruct Hin as [Hrs_eq | Hin].
          + subst r0.
            exists v.
            split; [| assumption].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gss.
            f_equal.
          + specialize (Hreg_agree r0 Hin).
            assert (Hneq: r0 <> r). {
              intro HF; subst r0.
              apply Hlist_not_in.
              assumption.
            }
            destruct Hreg_agree as (vj & Hrs0_eq & Heval_reg0).
            exists vj.
            split; [| assumption].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [assumption | intros HF; inversion HF].
            apply Hneq.
            apply ireg_of_reg_eq.
            assumption.
        - (**r arm_synch_stack *)
          unfold arm_synch_stack in Hstack.
          unfold arm_synch_stack.
          destruct Hstack as (Hload & Hstack).
          split.
          + rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            assumption.
            clear - HF.
            unfold ireg_of_reg in HF.
            destruct r; inversion HF.
          + intros r0 Hin.
            specialize (Hstack r0 Hin).
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            assumption.
            clear - HF.
            unfold ireg_of_reg in HF.
            destruct r; inversion HF.
        - (**r rs PC *)
          rewrite Pregmap.gss.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite HPC_eq.
          unfold Val.offset_ptr, wsize.
          f_equal.
          rewrite <- Hlen_eq in Hofs1.
          rewrite Hofs0, Hofs1.
          unfold Ptrofs.add.
          f_equal.
          rewrite Hlen_eq0.
          change (Ptrofs.unsigned (Ptrofs.repr 4)) with 4.
          lia.
        - (**r Mem.unchanged_on *)
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          assumption.
          clear - HF.
          unfold ireg_of_reg in HF.
          destruct r; inversion HF.
        - (**r Mem.unchanged_on *)
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          assumption.
          clear - HF.
          unfold ireg_of_reg in HF.
          destruct r; inversion HF.
      }
    Qed.


    Lemma jit_load_simulation: forall l1 l l2 rbpf_st st0 st1 st_final old_rs rs0 ofs0 ofs1 m0
      (Hpre: arm_registers_pre_weak rs0)
      (Hldr: jit_alu32_load_list l = Some l1)
      (Hjit_load: jit_alu32_thumb_load l1 st0 = Some st1)

      (Harm_inv: arm_memory_inv0 st0 rs0 rs0 m0 m0
        flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True))
      (Hst: match_state_jit rbpf_st st0)

      (Hsub_mem: sub_mem_blk (jit_mem st1) (jit_mem st_final) jit_blk ofs1)
      (Hofs0 : ofs0 = Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))
      (Hofs1 : ofs1 = Z.of_nat (jitted_len st1 + (jitted_len st1 + 0)))
      (Harm_blk : jitted_list st0 = Vptr jit_blk Ptrofs.zero)
      (Hreg_blk: jit_regs st0 = Vptr regs_blk Ptrofs.zero)
      (Harm: match_state_arm rbpf_st rs0 m0 old_rs [] l2 jit_blk ofs0 st_final),
        exists rs1,
          arm_registers_load l1 rbpf_st rs0 rs1 /\
          arm_memory_inv0 st1 rs0 rs1 m0 m0
            flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True) /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
          match_state_arm rbpf_st rs1 m0 old_rs l1 l2 jit_blk ofs1 st_final.
    Proof.
      induction l1; simpl; intros.
      { (**r l = [] *)
        unfold arm_registers_load.
        simpl.
        exists rs0.
        split; [reflexivity | ].
        split.
        { injection Hjit_load as Hst_eq; subst st0; assumption. }
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
      specialize (IHl1 lr l2 rbpf_st stk st1 st_final).

      destruct jit_alu32_load_list eqn: Hnl1 in Hldr; [| inversion Hldr].
      rename l0 into nl1.

      assert (Hreg_val: exists v, eval_reg a rbpf_st = Some (Val.longofintu (Vint v))). {
        destruct Hst.
        unfold match_registers in mregs0.
        destruct mregs0 as (Hreg_blk_eq & Hjit_blk & Hmregs0).
        specialize (Hmregs0 a).
        destruct Hmregs0 as (vi & Heval_reg & _).
        exists vi. assumption.
      }

      destruct Hreg_val as (v & Hreg_val).

      eapply jit_load_one_simulation with
        (rbpf_st := rbpf_st) (l := []) (v := v) (st_final := st_final) in Hupd_load as Hone_step; eauto.
      - (**r MAIN *)
        destruct Hone_step as (rsk & Hloadk & Hinvk & Hstark & Hmatch_statek).
        unfold arm_registers_load_one in Hloadk. (*
        remember ((rs0 # (ireg_of_reg a) <- (Vint v)) # PC <-
               (Val.offset_ptr (rs0 # (ireg_of_reg a) <- (Vint v) PC) wsize)) as rsk.
        rewrite Hreg_val in Hrs1_eq.
        unfold Val.longofintu in Hrs1_eq. *)
        specialize (IHl1 old_rs rsk).
        specialize (IHl1  (Z.of_nat (2 * jitted_len stk)) ).
        specialize (IHl1 (Z.of_nat (2 * jitted_len st1)) ).
        specialize (IHl1 m0).

        assert (Heq: arm_registers_pre_weak rsk). {
          subst rsk.
          clear - Hpre.
          unfold arm_registers_pre_weak in *.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          assumption.
          clear - HF.
          unfold ireg_of_reg in HF.
          destruct a; inversion HF.
        }
        specialize (IHl1 Heq); clear Heq.
        specialize (IHl1 Hexists_l Hjit_load).

        assert (Heq: arm_memory_inv0 stk rsk rsk m0 m0
                  flag_blk regs_blk jit_blk jit_state_blk  (fun _ _ => True)). {
          subst rsk.
          clear - Hupd_load Harm_blk Hreg_blk Harm_inv.

          set (old_rs := rs0).
          assert (Heq: arm_memory_inv0 st0 rs0 old_rs m0 m0
            flag_blk regs_blk jit_blk jit_state_blk  (fun _ _ => True)) by auto.
          clear Harm_inv; rename Heq into Harm_inv.
          assert (Heq0: old_rs = rs0) by auto.

          set (m := m0).
          assert (Heq: arm_memory_inv0 st0 rs0 old_rs m0 m
            flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True)) by auto.
          clear Harm_inv; rename Heq into Harm_inv.
          assert (Heq1: m = m0) by auto.

          destruct Harm_inv.
          subst m old_rs.
          destruct arm_inv_stk as (_ & Hsp_spec).
          unfold arm_stack_pointer_spec in Hsp_spec.
          destruct Hsp_spec as (sp_blk & Hrs_eq & Hneq0 & Hneq1 & Hperm).

          constructor; try assumption.
          - (**r PC + arm_stack_pointer_spec *)
            split; [f_equal | ].
            unfold arm_stack_pointer_spec in *.
            exists sp_blk.
            split.
            + rewrite Pregmap.gso; [ | intros HF; inversion HF].
              rewrite Pregmap.gso; [ | intros HF; inversion HF].
              assumption.
              clear - HF.
              unfold ireg_of_reg in HF.
              destruct a; inversion HF.
            + split; [assumption | ].
              split; assumption.
          - (**r arm_assume_register_map *)
            destruct arm_inv_reg as (Harm_inv_reg0 & _ & Hrange_perm).
            split.
            + rewrite Pregmap.gso; [ | intros HF; inversion HF].
              unfold arm_assume_register_map in *.
              intros r.
              specialize (Harm_inv_reg0 r).
              destruct r.
              * rewrite Pregmap.gso; [ | intros HF; inversion HF].

                destruct (ireg_eqb (ireg_of_reg a) i) eqn: Hreg_eqb;
                  [ apply ireg_eqb_true in Hreg_eqb |
                    apply ireg_eqb_false in Hreg_eqb
                  ].
                { subst i.
                  rewrite Pregmap.gss.
                  auto.
                }
                { rewrite Pregmap.gso; [assumption | intros HF; inversion HF].
                  apply Hreg_eqb.
                  auto.
                }
              * repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              * repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              * rewrite Pregmap.gss; auto.
                unfold Val.offset_ptr; destruct (rs0 PC); auto.
            + split; [| assumption].
              rewrite Pregmap.gso; [ | intros HF; inversion HF].
              unfold arm_assume_register_map in *.
              intros r.
              specialize (Harm_inv_reg0 r).
              destruct r.
              * rewrite Pregmap.gso; [ | intros HF; inversion HF].

                destruct (ireg_eqb (ireg_of_reg a) i) eqn: Hreg_eqb;
                  [ apply ireg_eqb_true in Hreg_eqb |
                    apply ireg_eqb_false in Hreg_eqb
                  ].
                { subst i.
                  rewrite Pregmap.gss.
                  auto.
                }
                { rewrite Pregmap.gso; [assumption | intros HF; inversion HF].
                  apply Hreg_eqb.
                  auto.
                }
              * repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              * repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              * rewrite Pregmap.gss; auto.
                unfold Val.offset_ptr; destruct (rs0 PC); auto.
          - (**r jit_state_memory_layout *)
            unfold jit_state_memory_layout in *.
            destruct arm_inv_st as (arm_inv_st0 & arm_inv_st1).
            split; [assumption | ].
            unfold regs_layout in *.
            intros r0.
            specialize (arm_inv_st1 r0).
            destruct arm_inv_st1 as (vi & Hreg & Hload0 & Hload1 & Hreg_eq).
            exists vi.
            split.
            + rewrite <- Hreg.
              symmetry.
              eapply upd_jitted_list_unchange_eval_jit_reg_2
                with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
              (**r jit_regs st0 = Vptr regs_blk Ptrofs.zero *)
              clear - Hneq1; unfold block_neq in Hneq1.
              intuition.
            + split; [assumption | ].
              split; [assumption | ].

              intros vj Heval_reg0.
              apply Hreg_eq.
              rewrite <- Heval_reg0.
              eapply upd_jitted_list_unchange_eval_jit_reg_2
                with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
              clear - Hneq1; unfold block_neq in Hneq1.
              intuition. (*
          - (**r Mem.unchanged_on *)
            apply Mem.unchanged_on_refl. *)
        }
        specialize (IHl1 Heq); clear Heq.

        assert (Heq: match_state_jit rbpf_st stk). {
          eapply jit_alu32_thumb_upd_load_unchange_match_state_jit; eauto.
        }
        specialize (IHl1 Heq); clear Heq.

        rewrite Hofs1 in Hsub_mem.
        specialize (IHl1 Hsub_mem).

        assert (Heq: Z.of_nat (2 * jitted_len stk) = 
                    Z.of_nat (jitted_len stk + (jitted_len stk + 0))) by lia.
        specialize (IHl1 Heq); clear Heq.

        assert (Heq: Z.of_nat (2 * jitted_len st1) =
                      Z.of_nat (jitted_len st1 + (jitted_len st1 + 0))) by lia.
        specialize (IHl1 Heq); clear Heq.

        assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
          unfold jit_alu32_thumb_upd_load, jit_alu32_thumb_load_store_template_jit in Hupd_load.
          eapply upd_jitted_list_unchange_jittted_list_2 in Hupd_load; eauto.
          rewrite <- Hupd_load.
          assumption.
        }
        specialize (IHl1 Heq); clear Heq.

        assert (Heq: jit_regs stk = Vptr regs_blk Ptrofs.zero). {
          unfold jit_alu32_thumb_upd_load, jit_alu32_thumb_load_store_template_jit in Hupd_load.
          eapply upd_jitted_list_unchange_jit_regs_2 in Hupd_load; eauto.
          rewrite <- Hupd_load.
          assumption.
        }
        specialize (IHl1 Heq); clear Heq.

        remember (Z.of_nat (jitted_len stk + (jitted_len stk + 0))) as ofsk.
        rename Heqofsk into Hofsk.

        set (onel := [a]).
        assert (Heq: match_state_arm rbpf_st rsk m0 old_rs onel l2 jit_blk ofsk st_final) by auto.
        clear Hmatch_statek; rename Heq into Hmatch_statek.
        assert (Heq0: onel = [a]) by auto.

        assert (Heq: match_state_arm rbpf_st rsk m0 old_rs [] l2 jit_blk (Z.of_nat (2 * jitted_len stk))
         st_final). {
          simpl.
          rewrite <- Hofsk.

          clear - Hnodup_load Hofs0 Hofs1 Harm Hreg_val Hloadk Hofsk Hmatch_statek.

          set (el := []).
          assert (Heq: match_state_arm rbpf_st rs0 m0 old_rs el l2 jit_blk ofs0 st_final) by auto.
          clear Harm; rename Heq into Harm.
          assert (Heq1: el = []) by auto.

          destruct Harm.
          subst lsr.
          rename arm_mem into m0.


          destruct Hmatch_statek.

          rename arm_blk into jit_blk.
          rename lsr_stack into l2.
          rename rs0 into rsk.
          rename rs into rs0.
          rename arm_mem into m0.
          rename ofs0 into ofsk.
          rename ofs into ofs0.

          constructor; try assumption.
          - unfold regs_agree; simpl.
            intros r HF.
            inversion HF.
        }
        specialize (IHl1 Heq); clear Heq.

        destruct IHl1 as (rs1 & Hload1 & Hinv1 & Hstar1 & Hmatch_state_1).


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

        split.
        { (**r arm_memory_inv0 *)
          destruct Hinv1.
          destruct Hinvk.
          destruct arm_inv_stk as (Hrsk_r13 & arm_inv_stk).
          destruct arm_inv_stk0 as (Hrs0_r13 & arm_inv_stk0).
          destruct arm_inv_reg as (arm_inv_reg_rsk & arm_inv_reg_rs1 & Hrange_perm).
          destruct arm_inv_reg0 as (arm_inv_reg_rs0 & _).
          constructor; try assumption.
          - (**r rs IR13 *)
            split.
            + rewrite <- Hrsk_r13.
              assumption.
            + assumption.
          - (**r arm_assume_register_map *)
            split; [assumption | ].
            split; assumption.
        }

        split.
        { (**r star BinSem.step *)
          eapply star_trans with (s2 := State rsk m0) (t1 := E0); eauto.
        }

        (**r match_state_arm *)
        simpl in Hmatch_state_1.
        rewrite <- Hofs1 in Hmatch_state_1.

        destruct Hinv1.
        destruct Hinvk.
        destruct Harm_inv.
        clear Hst.
        destruct Hmatch_state_1.
        destruct Hmatch_statek.
        rename rs1 into rsk.
        rename rs into rs1.
        rename lsr into l1.
        rename lsr_stack into l2.
        rename arm_blk into jit_blk.
        rename ofs1 into ofsk.
        rename ofs into ofs1.
        rename jit_st_final into st_final.

        subst lsr0.

        constructor; try assumption.

        (**r regs_agree *)
        unfold regs_agree in *.
        intros r Hin.
        destruct Hin as [Hr | Hin].
        * subst a.
          assert (Heq: In r [r]) by (simpl; left; reflexivity).
          specialize (Hreg_agree0 r Heq); clear Heq.
          eapply arm_registers_load_unchange_nodup_register with (r := r) in Hload1; eauto.
          2:{
            apply NoDup_cons_iff in Hnodup_load.
            clear - Hnodup_load.
            intuition.
          }
          rewrite <- Hload1.
          assumption.
        * apply Hreg_agree; auto.
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

      eapply jit_alu32_thumb_load_load_same; eauto.
      unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_load.
      eapply upd_jitted_list_unchange_jittted_list_2 in Hupd_load; eauto.
      rewrite <- Hupd_load; assumption.
    - rewrite <- Hreg_val.
      symmetry.
      clear - Hst.
      destruct Hst.
      clear - mregs0.
      unfold match_registers in mregs0.
      destruct mregs0 as (_ & _ & Hreg).
      specialize (Hreg a).
      destruct Hreg as (vi & Hreg0 & Hreg1 & _).
      rewrite Hreg0, Hreg1.
      f_equal.
    Qed.
  End JITLoad.

  Section JITCore.

    Lemma jit_core_one_simulation: forall ins l l1 rbpf_st0 rbpf_st1 st0 st1 st_final old_rs rs0 ofs0 ofs1 m0 arm_blk (**r Here I can not use jit_blk directly *)
      (Hpre: arm_registers_pre_weak rs0)
      (Hjit : bpf_alu32_to_thumb ins st0 = Some st1)

      (Harm_inv: arm_memory_inv0 st0 rs0 rs0 m0 m0
        flag_blk regs_blk arm_blk jit_state_blk (fun _ _ => True))

      (Hsub_mem: sub_mem_blk (jit_mem st1) (jit_mem st_final) arm_blk ofs1)
      (Hofs0 : ofs0 = Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))
      (Hofs1 : ofs1 = Z.of_nat (jitted_len st1 + (jitted_len st1 + 0)))
      (Harm_blk : jitted_list st0 = Vptr arm_blk Ptrofs.zero)
      (Hreg_blk: jit_regs st0 = Vptr regs_blk Ptrofs.zero)
      (Hreg_blk1: regs_st rbpf_st0 = Vptr regs_blk Ptrofs.zero)
      (Hst: match_state_arm rbpf_st0 rs0 m0 old_rs l l1 arm_blk ofs0 st_final)
      (Hins_regs: ins_is_sync_regs ins l)
      (Hstep: rbpf_step rbpf_st0 ins rbpf_st1),
        exists rs1,
          plus BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
          arm_registers_pre_weak rs1 /\
          arm_memory_inv0 st1 rs0 rs1 m0 m0
            flag_blk regs_blk arm_blk jit_state_blk (fun _ _ => True) /\
          match_state_arm rbpf_st1 rs1 m0 old_rs l l1 arm_blk ofs1 st_final.
    Proof.
      unfold bpf_alu32_to_thumb.
      intros.
      induction Hstep as [rbpf_st0 a op rd ri ret rbpf_st1].
      destruct Harm_inv.

      induction Hst as [l l1 rbpf_st0 rs0 old_rs st_final arm_blk ofs0 m0].
      destruct a; [| inversion Hjit].
      destruct ri.
      - (**r alu_reg *)
        unfold regs_agree in Hreg_agree.
        unfold ins_is_sync_regs in Hins_regs.
        destruct Hins_regs as (Hins_rd & Hins_r).
        specialize (Hreg_agree rd Hins_rd) as Hins_rd_eq.
        specialize (Hreg_agree r Hins_r) as Hins_r_eq.
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

          rewrite Harm_blk in *.
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

          assert (Hjit_ptr_eq: jitted_list st1 = Vptr arm_blk Ptrofs.zero). {
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

          eexists.
          split.
          { (**r we know jitted code is only one *)
            eapply plus_one.
            eapply exec_step_bin with
              (i := Padd (ireg_of_reg rd) (ireg_of_reg rd) (SOreg (ireg_of_reg r))) (w := false).
            {

              rewrite HPC_eq.
              unfold BinDecode.find_instr.

              rewrite Hofs0; simpl.
              subst m.

              assert (Heq1:  Mem.load Mint16unsigned (jit_mem st_final) arm_blk
                              (Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0))))) = 
                            Mem.load Mint16unsigned m0 arm_blk
                              (Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))))). {
                clear - Hstore Hofs1 Hjit_len_eq arm_inv_stk Hvalid_arm_blk Hupd Harm_mem1 Harm_mem2.
                eapply Mem.load_unchanged_on_1; eauto.
                - (**r Mem.valid_block *)
                  eapply Mem.valid_block_unchanged_on; eauto.
                - intros; simpl.
                  clear - arm_inv_stk.
                  destruct arm_inv_stk as (_ & Hsp_spec).
                  unfold arm_stack_pointer_spec in Hsp_spec.
                  destruct Hsp_spec as (sp_blk & Hrs_eq & Hneq0 & Hneq1 & Hperm).
                  split.
                  + clear - Hneq1; unfold block_neq in Hneq1.
                    intuition.
                  + rewrite Hrs_eq.
                    unfold not_stack_blk.
                    clear - Hneq0; intuition.
              }
              rewrite <- Heq1; clear Heq1.

              assert (Heq1:  Mem.load Mint16unsigned (jit_mem st_final) arm_blk
                              (Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0))))) = 
                            Mem.load Mint16unsigned (jit_mem st1) arm_blk
                              (Ptrofs.unsigned (Ptrofs.repr (Z.of_nat (jitted_len st0 + (jitted_len st0 + 0)))))). {
                clear - Hsub_mem Hstore Hofs1 Hjit_len_eq Hupd.
                rewrite Hjit_len_eq in Hofs1.
                unfold sub_mem_blk in Hsub_mem.
                symmetry.
                apply Hsub_mem.
                rewrite Hofs1.
                set (Hlemma:= upd_jitted_list_unsigned_repr); simpl in Hlemma.
                erewrite Hlemma; eauto.
                change (size_chunk Mint16unsigned) with 2.
                lia.
              }

              rewrite Heq1; clear Heq1.

              erewrite Mem.load_store_same with (m1 := jit_mem st0).
              2:{
                set (Hlemma:= upd_jitted_list_unsigned_repr).
                simpl in Hlemma.
                erewrite Hlemma; eauto.
              }

              simpl.
              (**r we know BinDecode.is_thumb2 _ = false in this case *)
              apply lemma_thumb_add_reg.
            }
            {
              simpl.
              unfold nextinstr_nf, nextinstr.
              simpl.
              unfold undef_flags.
              rewrite Pregmap.gso; [ | intros HF; inversion HF].
              reflexivity.
            }
          }

          split.
          { (**r arm_registers_pre_weak *)
            unfold arm_registers_pre_weak in *.
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            rewrite Pregmap.gso; [ | intros HF; inversion HF].
            assumption.
            clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
          }

          destruct arm_inv_stk as (_ & arm_inv_stk).
          destruct arm_inv_reg as (arm_inv_reg_rs0 & _ & Hrange_perm).
          split.
          { (**r arm_memory_inv0 *)
            constructor; try assumption.
            - (**r rs0 IR13 *)
              split.
              + unfold Val.offset_ptr.
                rewrite Pregmap.gso; [ | intros HF; inversion HF].
                rewrite Pregmap.gso; [ reflexivity | intros HF; inversion HF].
                clear - HF.
                unfold ireg_of_reg in HF.
                destruct rd; inversion HF.
              + assumption.
            - (**r arm_assume_register_map *)
              split; [assumption | ].
              split; [| assumption ].
              unfold Val.offset_ptr, arm_assume_register_map.
              intros r0.
              unfold arm_assume_register_map in arm_inv_reg_rs0.
              specialize (arm_inv_reg_rs0 r0).

              clear - arm_inv_reg_rs0 Hupd_reg Hv0_eq Hreg0 Hv1_eq Hreg1 Hupd.
              destruct r0; auto.
              + (**r IR *)
                rewrite Pregmap.gso; [ | intros HF; inversion HF].

                destruct (ireg_eqb (ireg_of_reg rd) i) eqn: Hreg_eqb;
                  [ apply ireg_eqb_true in Hreg_eqb |
                    apply ireg_eqb_false in Hreg_eqb
                  ].
                * subst i.
                  rewrite Pregmap.gss.
                  rewrite Hv0_eq, Hv1_eq.
                  simpl.
                  auto.
                * rewrite Pregmap.gso; [assumption | intros HF; inversion HF].
                  apply Hreg_eqb.
                  auto.
              + (**r CR *)
                rewrite Pregmap.gso; [ | intros HF; inversion HF].
                auto.
              + (**r PC *)
                rewrite Pregmap.gss.
                destruct (rs0 PC); auto.
            - (**r jit_state_memory_layout *)
              clear - arm_inv_stk Hstore Hjit Harm_blk Hreg_blk arm_inv_st.
              injection Hjit as Hst_eq; subst st1.

              unfold arm_stack_pointer_spec in arm_inv_stk.
              destruct arm_inv_stk as (sp_blk & Hrs_13 & Hneq1 & Hneq2 & _).

              unfold jit_state_memory_layout in *.
              destruct arm_inv_st as (Hload_32 & Hregs_layout).
              split; [assumption | ].
              unfold regs_layout in *.
              intros r0.
              specialize (Hregs_layout r0).
              destruct Hregs_layout as (vi & Heval_reg & Hload64 & Hload32 & Haxiom).

              exists vi.
              split.
              + rewrite <- Heval_reg.
                unfold eval_jit_reg.
                rewrite Hreg_blk; simpl.
                eapply Mem.load_store_other; eauto.
                left.
                clear - Hneq2; unfold block_neq in Hneq2.
                intuition.
              + split; [assumption | ].
                split; [assumption | ].

                intros vj Heval_reg0.
                apply Haxiom.
                rewrite <- Heval_reg0.
                unfold eval_jit_reg.
                rewrite Hreg_blk; simpl.
                symmetry.
                eapply Mem.load_store_other; eauto.
                left.
                clear - Hneq2; unfold block_neq in Hneq2.
                intuition.
          }

          eapply exec_step; eauto.
          { (**r regs_agree *)
            unfold regs_agree.
            intros r0 H.
            specialize (Hreg_agree r0 H).
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
            - destruct Hreg_agree as (vi & Hrs_eq & Hreg_r0).
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
          { (**r arm_synch_stack *)
            unfold arm_synch_stack in *.
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            destruct Hstack as (Hload_sp & Hload_stack).
            split.
            - rewrite Pregmap.gso; [| intro HF; inversion HF].
              + assumption.
              + clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
            - intros r0 Hin.
              rewrite Pregmap.gso; [| intro HF; inversion HF].
              + eapply Hload_stack; eauto.
              + clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
          }
          { (**r rs PC *)
            rewrite Pregmap.gss.
            rewrite HPC_eq, Hofs0, Hofs1, Hjit_len_eq.
            unfold Val.offset_ptr.
            f_equal.
            unfold Ptrofs.add, isize.
            f_equal.
            change (Ptrofs.unsigned (Ptrofs.repr 2)) with 2.
            clear - Hmax_jit_size.
            rewrite Nat.leb_le in Hmax_jit_size.
            unfold JITTED_LIST_MAX_LENGTH in Hmax_jit_size.
            rewrite Ptrofs.unsigned_repr; [ | change Ptrofs.max_unsigned with 4294967295; lia].
            f_equal.
            lia.
          }
          { (**r Mem.unchanged_on *)
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            - assumption.
            - clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
          }
          { (**r Mem.unchanged_on *)
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            rewrite Pregmap.gso; [| intro HF; inversion HF].
            - assumption.
            - clear - HF; unfold ireg_of_reg in HF; destruct rd; inversion HF.
          }
        + admit.
        + admit.
        + admit. (**r the rest part is trivial *)
        + admit.
        + admit.
        + admit.
        + admit.
        + admit.
        + admit.
        + admit.
        + admit.
      - (**r alu_imm *)
        admit.
    Admitted.


    Lemma jit_core_simulation: forall l rbpf_st0 rbpf_st1
      (Hstep: rbpf_sem rbpf_st0 l rbpf_st1) l1 l2 l3 st0 st1 st_final rs0 m0 old_rs ofs0 ofs1
      (Hpre: arm_registers_pre_weak rs0)
      (Hldr: jit_alu32_load_list l = Some l1)
      (Hldr_sub: list_subset l1 l2)
      (Hjit: jit_core l st0 = Some st1)

      (Harm_inv: arm_memory_inv0 st0 rs0 rs0 m0 m0
        flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True))

      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Harm_blk: jitted_list st0 = Vptr jit_blk Ptrofs.zero)
      (Hreg_blk: jit_regs st0 = Vptr regs_blk Ptrofs.zero)
      (Hreg_blk1: regs_st rbpf_st0 = Vptr regs_blk Ptrofs.zero)
      (Hmem: sub_mem_blk (jit_mem st1) (jit_mem st_final) jit_blk ofs1)
      (Hst: match_state_arm rbpf_st0 rs0 m0 old_rs l2 l3 jit_blk ofs0 st_final),
      exists rs1,
        star BinSem.step ge (State rs0 m0) E0 (State rs1 m0) /\
        arm_memory_inv0 st1 rs0 rs1 m0 m0
          flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True) /\
        match_state_arm rbpf_st1 rs1 m0 old_rs l2 l3 jit_blk ofs1 st_final.
    Proof.
      induction 1 as [ | rbpf_st0 rbpf_stk rbpf_st1 hd tl Hone_step Hplus_step IH].
      { (**r l = [] *)
        intros; simpl in *.
        injection Hjit as Hjit_eq; subst st1.
        exists rs0.
        split; [ apply star_refl | ].

        injection Hldr as Hlsr_eq; subst l1.
        rewrite <- Hofs0 in Hofs1.
        subst ofs1.
        split; assumption.
      }

      (**r l = hd :: tl *)
      simpl.
      intros.
      destruct bpf_alu32_to_thumb eqn: Hone_ins; [| inversion Hjit].
      rename j into stk.

      assert (Hstk_ptr: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
        clear - Hone_ins Harm_blk.
        rewrite <- Harm_blk.
        symmetry.
        eapply bpf_alu32_to_thumb_unchange_jittted_list; eauto.
      }

      destruct jit_alu32_load_list eqn: Hldr_eq; [| inversion Hldr].

      induction Hone_step as [rbpf_st0 a op rd ri ret rbpf_stk].

      set (jit_blk' := jit_blk).
      assert (Heq_blk : jit_blk' = jit_blk) by auto.

      induction Hst as [l2 l3 rbpf_st0 rs0 old_rs st_final jit_blk ofs0 m0].
      subst jit_blk'.

      specialize (IH l l2 l3 stk st1 st_final).

      remember (Z.of_nat (jitted_len stk + (jitted_len stk + 0))) as ofsk.
      rename Heqofsk into Hofsk.

      apply jit_core_one_simulation with
        (ins := BPF_BINARY a op rd ri) (ofs0 := ofs0) (ofs1 := ofsk)
        (st0 := st0) (st1 := stk) (st_final := st_final)(l := l2)(l1 := l3)
        (rbpf_st0 := rbpf_st0) (rbpf_st1 := rbpf_stk)
        (old_rs := old_rs) (rs0 := rs0) (m0 := m0) (arm_blk := jit_blk)
      in Hone_ins as Hstk_eq; auto.

      2:{ (**r sub_mem *)
        clear - Hofs1 Hofsk Hstk_ptr Hjit Hmem.
        assert (Hle: Z.of_nat (2 * jitted_len stk) <= Z.of_nat (2 * jitted_len st1)). {
          clear - Hjit.
          eapply jit_core_jitted_len; eauto.
        }

        eapply jit_core_sub_mem in Hjit; eauto.
        subst ofs1 ofsk.
        eapply sub_mem_blk_less_than with (ofs1 := Z.of_nat (2 * jitted_len stk)) in Hmem; eauto.
        eapply sub_mem_blk_trans; eauto.
      }

      2:{ (**r match_state_arm *)
        eapply exec_step; eauto.
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

      destruct Hstk_eq as (rsk & Hplus & Hpre_weak & arm_memory_inv & Hmatch_state).

      specialize (IH rsk m0 old_rs (Z.of_nat (2 * jitted_len stk)) ofs1 Hpre_weak).

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

      assert (Heq: arm_memory_inv0 stk rsk rsk m0 m0
                flag_blk regs_blk jit_blk jit_state_blk (fun _ _ => True)). {
        clear - Hone_ins Harm_blk Hreg_blk Hplus arm_memory_inv Hmatch_state.

        destruct arm_memory_inv.
        destruct arm_inv_stk as (Hrs13_eq & Hsp_spec).
        destruct arm_inv_reg as (Harm_inv_reg_rs0 & Harm_inv_reg_rsk & Hperm).
        unfold arm_stack_pointer_spec in Hsp_spec.
        destruct Hsp_spec as (sp_blk & Hrs0_13_eq & Hneq1 & Hneq2 & Hrange_perm).

        constructor; try assumption.
        - (**r arm_stack_pointer_spec *)
          split; [reflexivity | ].
          unfold arm_stack_pointer_spec.
          exists sp_blk.
          rewrite Hrs13_eq in Hrs0_13_eq.
          do 2 (split; [assumption | ]).
          split; assumption.
        - (**r arm_assume_register_map *)
          split; [assumption | ].
          split; assumption. (*
        - (**r Mem.unchanged_on *)
          rewrite <- Hrs13_eq.
          assumption. *)
      }
      specialize (IH Heq); clear Heq.

      assert (Heq: Z.of_nat (2 * jitted_len stk) = Z.of_nat (2 * jitted_len stk)) by reflexivity.
      specialize (IH Heq Hofs1 Hstk_ptr); clear Heq.

      assert (Heq: jit_regs stk = Vptr regs_blk Ptrofs.zero). {
        eapply bpf_alu32_to_thumb_unchange_jit_regs in Hone_ins; eauto.
        rewrite <- Hone_ins.
        assumption.
      }
      specialize (IH Heq); clear Heq.

      assert (Heq: regs_st rbpf_stk = Vptr regs_blk Ptrofs.zero). {
        clear - Hupd_reg Hreg_blk1.
        unfold upd_reg in Hupd_reg.
        destruct upd_reg'; inversion Hupd_reg.
        simpl.
        assumption.
      }
      specialize (IH Heq Hmem); clear Heq.

      assert (Hmatch1: match_state_arm rbpf_stk rsk m0 old_rs l2 l3 jit_blk (Z.of_nat (2 * jitted_len stk)) st_final). {
        clear - Hofs0 Hofsk Hldr Hone_ins Hjit Hmatch_state.

        induction Hmatch_state as [l2 l3 rbpf_stk rsk old_rs st_final jit_blk ofsk].
        eapply exec_step; eauto.
        rewrite Hofsk in HPC_eq.
        rewrite HPC_eq.
        f_equal.
      }

      specialize (IH Hmatch1).
      destruct IH as (rs1 & Hstar & Hinv & Hmatch2).
      exists rs1; split.
      + eapply star_trans with (s2 := (State rsk m0)) (t1 := E0); eauto.
        eapply plus_star; eauto.
      + split.
        * clear - Hinv arm_memory_inv Harm_inv.
          destruct Hinv.
          destruct Harm_inv.
          destruct arm_memory_inv.
          constructor; try assumption.
          { (**r arm_stack_pointer_spec *)
            split.
            - destruct arm_inv_stk as (arm_inv_stk & _).
              destruct arm_inv_stk1 as (arm_inv_stk1 & _).
              rewrite arm_inv_stk1.
              assumption.
            - destruct arm_inv_stk0 as (_ & arm_inv_stk0).
              assumption.
          }
          { (**r arm_assume_register_map *)
            destruct arm_inv_reg as (Hrsk & Hrs1 & Hperm).
            destruct arm_inv_reg0 as (Hrs0 & _).
            split; [assumption | ].
            split; assumption.
          }
        * clear - Hmatch2 Hldr.
          induction Hmatch2 as [l2 l3 rbpf_st1 rs1 old_rs st_final jit_blk ofs1].
          eapply exec_step; eauto.
  Qed.

  End JITCore.

  Section JITStore.
(*
    Lemma jit_core_guarantees_jit_store_exists_vint:
      forall r rbpf_st rs m0 old_rs l1 l2 jit_blk ofs1 st_final
      (Hst: match_state_arm rbpf_st rs m0 old_rs l1 l2 jit_blk ofs1 st_final) (**r updated rs *)
      (Hin: List.In r l1),
          exists vi : int, Vint vi = rs (ireg_of_reg r).
    Proof.
      intros.
      destruct Hst.
      unfold regs_agree in Hreg_agree.
      specialize (Hreg_agree _ Hin).
      clear - Hreg_agree.
      

      destruct l; simpl; intros.
      { (** l = [] *)
        injection Hldr as Heq; subst l1.
        inversion Hin.
      }
      (**r l <> [] *)
      remember (b :: l) as bl.
      destruct Hstep.
      { inversion Heqbl. }
      injection Heqbl as Hb_eq Hl_eq.
      subst hd tl.
      destruct Hone_step.
      
      simpl 
    Qed. *)

    Definition arm_registers_store_one (r: reg) (rs0: Asm.regset) (m0: mem): option mem :=
      Mem.storev Mint32 m0
          (Val.offset_ptr (rs0 IR12) (Ptrofs.of_intu (Int.mul (int_of_reg r) (Int.repr 8)))) (rs0 (ireg_of_reg r)).

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


    Lemma jit_store_one_simulation: forall l0 l1 L r rbpf_st0 st0 st1 st_final rs0 ofs0 ofs1 m0 arm_blk
      (Hpre: arm_registers_pre_weak rs0)
      (Hupd_store : jit_alu32_thumb_upd_store r st0 = Some st1)

      (Hlist_not_in: ~ List.In r l0 /\ ~ List.In r L)
      (Hvint_r: exists vi, Vint vi = rs0 (ireg_of_reg r))
      (**r TODO: this should be guarantee by core stage *)

      (HnodupL: NoDup L)
      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Harm_blk: jitted_list st0 = Vptr arm_blk Ptrofs.zero)
      (Hreg_blk: jit_regs st0 = Vptr regs_blk Ptrofs.zero)

      (Harm_inv: arm_memory_inv0 st0 rs0 rs0 m0 m0
        flag_blk regs_blk arm_blk jit_state_blk (fun b _ => b <> regs_blk))
      (Hsub_mem: sub_mem_blk (jit_mem st1) (jit_mem st_final) arm_blk ofs1)

      (Hsyn: match_registers_syn rbpf_st0 m0 L regs_blk)

      (Harm: match_state_arm rbpf_st0 rs0 m0 rs0 (r :: l0) l1 arm_blk ofs0 st_final),
        exists rs1 m1,
          arm_registers_store_one r rs0 m0 = Some m1 /\ (*
          upd_reg r (Val.longofintu (rs0 (ireg_of_reg r))) rbpf_st0 = Some rbpf_st1 /\ *)
          rs1 = rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize) /\
          match_registers_syn rbpf_st0 m1 (r :: L) regs_blk /\
          arm_registers_pre_weak rs1 /\
          arm_memory_inv0 st1 rs0 rs1 m0 m1
            flag_blk regs_blk arm_blk jit_state_blk (fun b _ => b <> regs_blk) /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
          match_state_arm rbpf_st0 rs1 m1 rs0 l0 l1 arm_blk ofs1 st_final.
    Proof.
      unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit.
      unfold arm_registers_pre_weak.

      intros.
      exists (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)).

      destruct Harm_inv.
      destruct arm_inv_stk as (_ & Hsp_spec).
      unfold arm_stack_pointer_spec in Hsp_spec.
      destruct Hsp_spec as (sp_blk & Hrs13_eq & Hneq0 & Hneq1 & Hsp_range_perm).
      destruct arm_inv_reg as (Harm_rs0 & _ & Hregs_range_perm).

      unfold arm_registers_store_one.
      rewrite Hpre.
      simpl.
      rewrite Ptrofs.add_zero_l.
      rewrite Hreg_mul8_unsigned.

      destruct Hvint_r as (vi & Hrs0_r_eq).
      rewrite <- Hrs0_r_eq.

      assert (Heq: exists m1,
        Mem.store Mint32 m0 regs_blk (8 * id_of_reg r) (Vint vi) = Some m1). {
        clear - Hregs_range_perm.
        assert (Heq: Mem.valid_access m0 Mint32 regs_blk (8 * id_of_reg r) Writable). {
          unfold Mem.valid_access.
          split.
          - unfold Mem.range_perm in *.
            intros ofs Hrange.
            apply Hregs_range_perm.
            clear - Hrange.
            change (size_chunk Mint32) with 4 in *.
            unfold id_of_reg in *; destruct r; lia.
          - change (align_chunk Mint32) with 4.
            replace (8 * id_of_reg r) with (4 * (2 * id_of_reg r)).
            apply Z.divide_factor_l.
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


      unfold upd_reg, upd_reg'.
      unfold match_registers_syn in Hsyn.
      destruct Hsyn as (Hreg_blk_eq & Hbpf_regs_range_perm & Hsyn). (*
      rewrite Hreg_blk_eq.
      unfold Mem.storev, Val.add, Archi.ptr64.
      rewrite Ptrofs.add_zero_l.
      rewrite ptrofs_unsigned_repr_reg_mul_8.
      unfold Val.longofintu.
      assert (Heq: exists m2, Mem.store Mint64
        (bpf_m rbpf_st0) regs_blk (8 * id_of_reg r) (Vlong (Int64.repr (Int.unsigned vi))) = Some m2). {
        clear - Hbpf_regs_range_perm.
        assert (Heq: Mem.valid_access (bpf_m rbpf_st0) Mint64 regs_blk (8 * id_of_reg r) Writable). {
          unfold Mem.valid_access.
          split.
          - unfold Mem.range_perm in *.
            intros ofs Hrange.
            apply Hbpf_regs_range_perm.
            clear - Hrange.
            change (size_chunk Mint64) with 8 in *.
            unfold id_of_reg in *; destruct r; lia.
          - change (align_chunk Mint64) with 8.
            apply Z.divide_factor_l.
        }
        apply Mem.valid_access_store with (v := Vlong (Int64.repr (Int.unsigned vi))) in Heq.
        destruct Heq as (m2 & Hstore).
        exists m2; assumption.
      }
      destruct Heq as (m2 & Hstore_bpf).
      rewrite Hstore_bpf.
      exists (upd_mem m2 rbpf_st0). *)

      split; [assumption | ].
(*
      split.
      {
        unfold upd_mem.
        rewrite Hreg_blk_eq.
        reflexivity.
      } *)

      split; [reflexivity | ].

      remember (r :: l0) as el.
      set (old_rs := rs0).
      assert (Heq: match_state_arm rbpf_st0 rs0 m0 old_rs el l1 arm_blk ofs0 st_final) by auto.
      clear Harm; rename Heq into Harm.
      assert (Hrs_eq: old_rs = rs0) by auto.

      destruct Harm as (el, l1, rbpf_st0, rs0, old_rs, st_final, arm_blk, ofs0, m0, Hagree, Harm_syn, HPC_eq, Hvalid, Hunchanged0, Hunchanged1).
      subst old_rs el.

      split.
      { (**r match_registers_syn *)
        unfold match_registers_syn.
        split; [assumption | ].
        split; [assumption | ].
        intros r0.
        specialize (Hsyn r0).
        destruct Hsyn as (vj & Heval_reg & Haxiom & HinL).

        destruct (reg_eqb r r0) eqn: Hreg_eq;
          [ rewrite <- reg_eqb_true in Hreg_eq |
            rewrite <- reg_eqb_false in Hreg_eq ].
        - subst r0.
          exists vi.
          split.
          + rewrite Heval_reg.
            unfold regs_agree in Hagree.
            assert (Heq: In r (r :: l0)) by (left; reflexivity).
            specialize (Hagree r Heq); clear Heq.
            unfold eval_reg.

            destruct Hagree as (vk & Hrs_eq & Heval_regk).
            rewrite <- Hrs0_r_eq in Hrs_eq.
            injection Hrs_eq as Heq.
            subst vk.
            specialize (Haxiom vi Heval_regk).
            subst vj.
            reflexivity.
          + split.
            * intros vt Heval_regt.

              unfold regs_agree in Hagree.
              assert (Heq: In r (r :: l0)) by (left; reflexivity).
              specialize (Hagree r Heq); clear Heq.
              destruct Hagree as (vk & Hrs_eq & Heval_regk).
              rewrite <- Hrs0_r_eq in Hrs_eq.
              injection Hrs_eq as Heq.
              subst vk.
              specialize (Haxiom vi Heval_regk) as Heq.
              specialize (Haxiom vt Heval_regt).
              subst vj vt.
              reflexivity.
            * intros Hin.
              destruct Hin as [Hr_eq | Hin].
              { erewrite Mem.load_store_same; eauto.
                f_equal.
              }
              exfalso.
              destruct Hlist_not_in as (_ & Hnot_in).
              apply Hnot_in; assumption.
        - exists vj.
          split; [assumption | ].
          split; [assumption | ].

          intro Hin.
          destruct Hin as [Hr_eq | Hin].
          + exfalso.
            apply Hreg_eq.
            assumption.
          + specialize (HinL Hin).
            rewrite <- HinL.
            eapply Mem.load_store_other; eauto.
            right.
            change (size_chunk Mint32) with 4.
            clear - Hreg_eq.
            unfold id_of_reg.
            destruct r; destruct r0; try lia.
            all: exfalso; apply Hreg_eq; reflexivity.
      }

      split.
      { (**r rs0 # PC *)
        rewrite Pregmap.gso; [| intro HF; inversion HF].
        assumption.
      }

      split.
      { (**r arm_memory_inv0 *)
        constructor.
        - (**r arm_stack_pointer_spec *)
          split.
          + rewrite Pregmap.gso; [| intro HF; inversion HF].
            reflexivity.
          + unfold arm_stack_pointer_spec.
            exists sp_blk.
            repeat (split; [assumption | ]).
            assumption.
        - (**r arm_assume_register_map *)
          split; [assumption | ].
          split; [ | assumption].
          unfold arm_assume_register_map.
          intros.
          unfold arm_assume_register_map in Harm_rs0.
          specialize (Harm_rs0 r0).
          destruct r0.
          * rewrite Pregmap.gso; [assumption | intro HF; inversion HF].
          * rewrite Pregmap.gso; [assumption | intro HF; inversion HF].
          * rewrite Pregmap.gso; [assumption | intro HF; inversion HF].
          * rewrite Pregmap.gss.
            rewrite HPC_eq; simpl.
            auto.
        - (**r jit_state_memory_layout *)
          unfold jit_state_memory_layout in *.
          destruct arm_inv_st as (Hload & Hlayout).
          split; [assumption | ].
          unfold regs_layout in *.
          intros r0.
          specialize (Hlayout r0).
          destruct Hlayout as (v & Heval0 & Hload64 & Hload32 & Haxiom).
          exists v.
          split.
          + rewrite <- Heval0.
            symmetry.
            eapply upd_jitted_list_unchange_eval_jit_reg_2; eauto.
            clear - Hneq1; unfold block_neq in Hneq1; intuition.
          + split; [assumption | ].
            split; [assumption | ].
            intros vj Heval.
            apply Haxiom.
            rewrite <- Heval.
            eapply upd_jitted_list_unchange_eval_jit_reg_2; eauto.
            clear - Hneq1; unfold block_neq in Hneq1; intuition.
        - (**r Mem.unchanged_on *)
          eapply Mem.store_unchanged_on; eauto.
      }

      split.
      { (**r star BinSem.step *)

        eapply star_one.
        eapply exec_step_bin.
        - (**r find_instr *)
          instantiate (2 := Pstr (ireg_of_reg r) IR12 (SOimm (Int.mul (int_of_reg r) (Int.repr 8)))).
          instantiate (1 := true).

          rewrite HPC_eq.

          assert (Heq: find_instr (Vptr arm_blk (Ptrofs.repr ofs0)) (jit_mem st_final) =
                        find_instr (Vptr arm_blk (Ptrofs.repr ofs0)) m0). {
            unfold find_instr.
            simpl.
            erewrite <- Mem.load_unchanged_on_1; eauto.

            - (**r Mem.load *)
              destruct Mem.load eqn: Hload; [| reflexivity].
              erewrite <- Mem.load_unchanged_on_1; eauto.
              (**r block neq *)
              intros ofs Hofs_range.
              simpl.
              split.
              + clear - Hneq1.
                unfold block_neq in Hneq1; intuition.
              + rewrite Hrs13_eq.
                unfold not_stack_blk; simpl.
                clear - Hneq0; intuition.
            - (**r block neq *)
              intros ofs Hofs_range.
              simpl.
              split.
              + clear - Hneq1.
                unfold block_neq in Hneq1; intuition.
              + rewrite Hrs13_eq.
                unfold not_stack_blk; simpl.
                clear - Hneq0; intuition.
          }
          rewrite <- Heq; clear Heq.

          assert (Heq: find_instr (Vptr arm_blk (Ptrofs.repr ofs0)) (jit_mem st_final) =
                        find_instr (Vptr arm_blk (Ptrofs.repr ofs0)) (jit_mem st1)). {

            unfold find_instr.
            simpl.
            unfold sub_mem_blk in Hsub_mem.
            erewrite Hsub_mem; eauto.
            destruct Mem.load eqn: Hload; [| reflexivity].
            - erewrite Hsub_mem; eauto.
              rewrite <- Hst0_eq in Hofs1.
              rewrite Hofs1, Hofs0.

              unfold Ptrofs.add.
              destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_store].
              erewrite upd_jitted_list_unsigned_repr_add_2; eauto.
              change (size_chunk Mint16unsigned) with 2.
              lia.
            - rewrite <- Hst0_eq in Hofs1.
              rewrite Hofs1, Hofs0.

              destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_store].
              erewrite upd_jitted_list_unsigned_repr; eauto.
              change (size_chunk Mint16unsigned) with 2.
              lia.
          }
          rewrite Heq; clear Heq.
          eapply lemma_thumb_str; eauto.
          change (Int.unsigned (Int.repr 8)) with 8.
          unfold int_of_reg.
          rewrite Int.unsigned_repr;
            [| change Int.max_unsigned with 4294967295];
            unfold id_of_reg; destruct r; simpl; lia.
          unfold int_of_ireg.
          change (Z.of_nat (ireg2nat IR12)) with 12.
          unfold Int.mul in Hupd_store.
          replace (ireg2nat (ireg_of_reg r)) with (reg2nat r).
          unfold int_of_reg.
          replace (id_of_reg r) with (Z.of_nat (reg2nat r)).
          assumption.
          unfold reg2nat, id_of_reg; destruct r; lia.
          unfold reg2nat, ireg2nat, ireg_of_reg; destruct r; lia.
        - (**r exec_instr *)
          simpl.
          rewrite Hpre; simpl.
          rewrite Ptrofs.add_zero_l.
          unfold exec_store; simpl.
          rewrite Hreg_mul8_unsigned.
          rewrite <- Hrs0_r_eq.
          rewrite Hstore.
          f_equal.
      }

      { (**r match_state_arm *)

        constructor; try assumption.
        + (**r regs_agree *)
          unfold regs_agree in Hagree.
          unfold regs_agree.
          intros r0 Hin.
          assert (Heq: In r0 (r :: l0)) by (right; assumption).
          specialize (Hagree r0 Heq); clear Heq.
          rewrite Pregmap.gso; [ | intro HF; inversion HF].
          assumption.
        + (**r arm_synch_stack *)

          unfold arm_synch_stack in *.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].

          rewrite Hrs13_eq in *.

          destruct Harm_syn as (Hload & Hstack).
          simpl in Hload.
          split.
          {
            simpl.
            rewrite <- Hload.
            clear - Hneq0 Hstore.
            eapply Mem.load_store_other; eauto.
            left.
            intuition.
          }

          intros r0 Hin.
          simpl.
          specialize (Hstack r0 Hin).
          rewrite <- Hstack.
          rewrite Ptrofs.add_zero_l.
          rewrite Hreg_mul4_unsigned.
          simpl.
          rewrite Ptrofs.add_zero_l.
          rewrite Hreg_mul4_unsigned.
          erewrite Mem.load_store_other; eauto.
          left.
          clear - Hneq0; intuition.

        + (**r rs PC *)
          rewrite HPC_eq.
          rewrite Pregmap.gss.
          unfold Val.offset_ptr.
          f_equal.
          rewrite <- Hst0_eq in Hofs1.
          rewrite Hofs1, Hofs0.
          unfold Ptrofs.add.

          destruct upd_jitted_list eqn: Hupd; [| inversion Hupd_store].
          erewrite upd_jitted_list_unsigned_repr; eauto.

          f_equal.
          change (Ptrofs.unsigned wsize) with 4.
          lia.

        + (**r Mem.unchanged_on *)
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          eapply store_unchanged_on_3; eauto.
          intros ofs2 HF.
          destruct HF as (HF & _).
          apply HF.
          reflexivity.

        + (**r Mem.unchanged_on *)
          rewrite Pregmap.gso; [| intros HF; inversion HF].
          eapply store_unchanged_on_4; eauto.
          intros ofs2 HF.
          destruct HF as (HF & _).
          apply HF.
          reflexivity.
      }
    Qed.
(*
    Lemma jit_store_simulation: forall l0 l l1 L l2 rbpf_st0 st0 st1 st_final rs0 ofs0 ofs1 m0
      (Hpre: arm_registers_pre_weak rs0)
      (Hstr: jit_alu32_store_list l = Some l0)
      (Hcomple: complementary_reg_list l0 L = l2)
      (HnoemptyL: L <> [])
      (HnodupL: NoDup L)
      (Hjit_store : jit_alu32_thumb_store l0 st0 = Some st1)

      (Hofs0: ofs0 = (Z.of_nat (2 * (jitted_len st0))))
      (Hofs1: ofs1 = (Z.of_nat (2 * (jitted_len st1))))
      (Harm_blk: jitted_list st0 = Vptr jit_blk Ptrofs.zero)
      (Hreg_blk: jit_regs st0 = Vptr regs_blk Ptrofs.zero)

      (Harm_inv: arm_memory_inv0 st0 rs0 rs0 m0 m0
        flag_blk regs_blk jit_blk jit_state_blk (fun b _ => b <> regs_blk))
      (Hsub_mem: sub_mem_blk (jit_mem st1) (jit_mem st_final) jit_blk ofs1)

      (Hsyn: match_registers_syn rbpf_st0 m0 l2 regs_blk)

      (Harm: match_state_arm rbpf_st0 rs0 m0 rs0 l0 l1 jit_blk ofs0 st_final),
        exists rs1 m1,
          arm_registers_store l0 rs0 rs1 m0 m1 /\
          match_registers_syn rbpf_st0 m1 L regs_blk /\
          arm_registers_pre_weak rs1 /\
          arm_memory_inv0 st1 rs0 rs1 m0 m1
            flag_blk regs_blk jit_blk jit_state_blk (fun b _ => b <> regs_blk) /\
          star BinSem.step ge (State rs0 m0) E0 (State rs1 m1) /\
          match_state_arm rbpf_st0 rs1 m1 rs0 [] l1 jit_blk ofs1 st_final.
    Proof.
      induction l0; simpl; intros.
      { (**r l = [] *)
        unfold arm_registers_store.
        simpl.
        exists rs0. exists m0.
        split; [split; reflexivity | ].
        split.
        { unfold complementary_reg_list in Hstr.
          simpl in Hstr.
          apply filter_true_same in Hcomple.
          subst L.
          assumption.
        }
        split; [assumption | ].
        injection Hjit_store as Heq.
        subst st1.
        split; [assumption | ].
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
      specialize (IHl0 lr l1 L (complementary_reg_list l0 L) rbpf_st0 stk st1 st_final).

      destruct jit_alu32_store_list eqn: Hnl1 in Hstr; [| inversion Hstr].
      rename l0 into nl1.
(*
      remember (rs0 # PC <- (Val.offset_ptr (rs0 PC) wsize)) as rsk. *)

      assert (Hreg_val: exists vi : int, Vint vi = rs0 (ireg_of_reg a)). { (*
        subst rsk. *)
        clear - Harm.
        remember (a :: nl1) as al.
        set (old_rs := rs0).
        assert (Heq_rs: old_rs = rs0) by auto.
        assert (Heq: match_state_arm rbpf_st0 rs0 m0 old_rs al l1 jit_blk ofs0 st_final) by auto.
        clear Harm; rename Heq into Harm.
        destruct Harm.
        subst lsr old_rs; clear - Hreg_agree.
        unfold regs_agree in Hreg_agree.
        assert (Hin: In a (a :: nl1)) by (left; reflexivity).
        specialize (Hreg_agree _ Hin).
        destruct Hreg_agree as (vi & Heq & _). (*
        rewrite Pregmap.gso; [ | intros HF; inversion HF]. *)
        exists vi; rewrite Heq; reflexivity.
      }

      destruct Hreg_val as (v & Hreg_val).

(*
      unfold arm_registers_store; simpl.

      assert (Heq: exists mk, arm_registers_store_one a rs0 m0 = Some mk). {
        unfold arm_registers_store_one.
        unfold arm_registers_pre_weak in Hpre.
        rewrite Hpre.
        simpl.
        rewrite Ptrofs.add_zero_l.
        rewrite Hreg_mul8_unsigned.
        assert (Heq: rs0 (ireg_of_reg a) = Vint v). {
          subst rsk.
          rewrite Pregmap.gso in Hreg_val; [ | intros HF; inversion HF].
          rewrite Hreg_val; reflexivity.
        }
        rewrite Heq; clear Heq.
        clear - Harm_inv.
        destruct Harm_inv.
        clear - arm_inv_reg.
        destruct arm_inv_reg as (_ & _ & Hrange_perm).
        unfold Mem.range_perm in Hrange_perm.
        assert (Heq: 0 <=  (8 * id_of_reg a) < 88). {
          unfold id_of_reg; destruct a; lia.
        }
        assert (Haccess: Mem.valid_access m0 Mint32 regs_blk (8 * id_of_reg a) Writable). {
          unfold Mem.valid_access.
          split.
          - unfold Mem.range_perm in *.
            intros ofs Hrange.
            apply Hrange_perm.
            clear - Hrange.
            change (size_chunk Mint32) with 4 in *.
            unfold id_of_reg in *; destruct a; lia.
          - change (align_chunk Mint32) with 4.
            replace (8 * id_of_reg a) with (4 * (2 * id_of_reg a)).
            apply Z.divide_factor_l.
            unfold id_of_reg in *; destruct a; lia.
        }
        apply Mem.valid_access_store with (v := Vint v) in Haccess.
        destruct Haccess as (mk & Hstore).
        exists mk; assumption.
      }
      destruct Heq as (mk & Hstore_one).
      rewrite Hstore_one. *)

      eapply jit_store_one_simulation with
        (l0 := nl1) (l1 := l1) (L := l2)
        (rbpf_st0 := rbpf_st0) (st_final := st_final)
        (rs0 := rs0) (ofs0 := ofs0) (ofs1 := Z.of_nat (2 * jitted_len stk))
        (m0 := m0) (arm_blk := jit_blk) in Hupd_store as Hone_step; eauto.
      - (**r MAIN *)
        destruct Hone_step as (rsk & mk & Hstorek & Hrsk_eq & Hreg_synk & Hprek 
          & Hinvk & Hstark & Hmatch_statek).

        unfold arm_registers_store_one in Hstorek.

        unfold arm_registers_pre_weak in Hpre.
        rewrite Hpre in Hstorek.
        simpl in Hstorek.
        rewrite Ptrofs.add_zero_l in Hstorek.
        rewrite <- Hreg_val in Hstorek.
        rewrite Hreg_mul8_unsigned in Hstorek.
        specialize (IHl0 rsk).
        specialize (IHl0  (Z.of_nat (2 * jitted_len stk)) ).
        specialize (IHl0 (Z.of_nat (2 * jitted_len st1)) ).
        specialize (IHl0 mk).
(*
        assert (Heq: arm_registers_pre_weak rsk). {
          subst rsk.
          clear - Hpre.
          unfold arm_registers_pre_weak in *.
          rewrite Pregmap.gso; [ | intros HF; inversion HF].
          assumption.
        } *)
        specialize (IHl0 Hprek).
        specialize (IHl0 Hexists_l).

        assert (Heq: complementary_reg_list nl1 L = complementary_reg_list nl1 L) by auto.
        specialize (IHl0 Heq HnoemptyL HnodupL Hjit_store); clear Heq.

        assert (Heq: Z.of_nat (2 * jitted_len stk) = Z.of_nat (2 * jitted_len stk)) by auto.
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: Z.of_nat (2 * jitted_len st1) = Z.of_nat (2 * jitted_len st1)) by auto.
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: jitted_list stk = Vptr jit_blk Ptrofs.zero). {
          unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit in Hupd_store.
          eapply upd_jitted_list_unchange_jittted_list_2 in Hupd_store; eauto.
          rewrite <- Hupd_store.
          assumption.
        }
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: jit_regs stk = Vptr regs_blk Ptrofs.zero). {
          unfold jit_alu32_thumb_upd_store, jit_alu32_thumb_load_store_template_jit in Hupd_store.
          eapply upd_jitted_list_unchange_jit_regs_2 in Hupd_store; eauto.
          rewrite <- Hupd_store.
          assumption.
        }
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: arm_memory_inv0 stk rsk rsk mk mk
                  flag_blk regs_blk jit_blk jit_state_blk  (fun (b : block) (_ : Z) => b <> regs_blk)). {
          subst rsk.
          clear - Hupd_store Harm_blk Hreg_blk Harm_inv Hstorek.

          set (old_rs := rs0).
          assert (Heq: arm_memory_inv0 st0 rs0 old_rs m0 m0
            flag_blk regs_blk jit_blk jit_state_blk (fun (b : block) (_ : Z) => b <> regs_blk)) by auto.
          clear Harm_inv; rename Heq into Harm_inv.
          assert (Heq0: old_rs = rs0) by auto.

          set (m := m0).
          assert (Heq: arm_memory_inv0 st0 rs0 old_rs m0 m
            flag_blk regs_blk jit_blk jit_state_blk (fun (b : block) (_ : Z) => b <> regs_blk)) by auto.
          clear Harm_inv; rename Heq into Harm_inv.
          assert (Heq1: m = m0) by auto.

          destruct Harm_inv.
          subst m old_rs.
          destruct arm_inv_stk as (_ & Hsp_spec).
          unfold arm_stack_pointer_spec in Hsp_spec.
          destruct Hsp_spec as (sp_blk & Hrs_eq & Hneq0 & Hneq1 & Hperm).

          constructor; try assumption.
          - (**r PC + arm_stack_pointer_spec *)
            split; [f_equal | ].
            unfold arm_stack_pointer_spec in *.
            exists sp_blk.
            split.
            + rewrite Pregmap.gso; [ | intros HF; inversion HF].
              assumption.
            + split; [assumption | ].
              split; [assumption | ].
              clear - Hstorek Hperm.
              unfold Mem.range_perm in *.
              intros ofs Hofs.
              specialize (Hperm _ Hofs).
              eapply Mem.perm_store_1; eauto.
          - (**r arm_assume_register_map *)
            destruct arm_inv_reg as (Harm_inv_reg0 & _ & Hrange_perm).
            split.
            + unfold arm_assume_register_map in *.
              intros r.
              specialize (Harm_inv_reg0 r).
              destruct r.
              * rewrite Pregmap.gso; [ | intros HF; inversion HF].
                assumption.
              * repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              * repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
              * rewrite Pregmap.gss; auto.
                unfold Val.offset_ptr; destruct (rs0 PC); auto.
            + split.
              {
                unfold arm_assume_register_map in *.
                intros r.
                specialize (Harm_inv_reg0 r).
                destruct r.
                * rewrite Pregmap.gso; [ | intros HF; inversion HF].
                  assumption.
                * repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
                * repeat (rewrite Pregmap.gso; [ | intros HF; inversion HF]); assumption.
                * rewrite Pregmap.gss; auto.
                  unfold Val.offset_ptr; destruct (rs0 PC); auto.
              }
              clear - Hstorek Hrange_perm.
              unfold Mem.range_perm in *.
              intros ofs Hofs.
              specialize (Hrange_perm _ Hofs).
              eapply Mem.perm_store_1; eauto.
          - (**r jit_state_memory_layout *)
            unfold jit_state_memory_layout in *.
            destruct arm_inv_st as (arm_inv_st0 & arm_inv_st1).
            split.
            {
              rewrite <- arm_inv_st0.
              simpl.
              erewrite Mem.load_store_other; eauto.
              left.
              clear - Hneq1; unfold block_neq in Hneq1; intuition.
            }

            unfold regs_layout in *.
            intros r0.
            specialize (arm_inv_st1 r0).
            destruct arm_inv_st1 as (vi & Hreg & Hstore0 & Hstore1 & Hreg_eq).

            TODO ...

            exists vi.
            split.
            + rewrite <- Hreg.
              symmetry.
              eapply upd_jitted_list_unchange_eval_jit_reg_2
                with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
              (**r jit_regs st0 = Vptr regs_blk Ptrofs.zero *)
              clear - Hneq1; unfold block_neq in Hneq1.
              intuition.
            + split.
              {
                rewrite <- Hstore0.
                erewrite Mem.load_store_other; eauto.
                right. ... TODO ...
                clear - Hneq1; unfold block_neq in Hneq1; intuition.
              }

              split; [assumption | ].
              split; [assumption | ].

              intros vj Heval_reg0.
              apply Hreg_eq.
              rewrite <- Heval_reg0.
              eapply upd_jitted_list_unchange_eval_jit_reg_2
                with (jit_blk := jit_blk) (regs_blk := regs_blk); eauto.
              clear - Hneq1; unfold block_neq in Hneq1.
              intuition. (*
          - (**r Mem.unchanged_on *)
            apply Mem.unchanged_on_refl. *)
        }
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: sub_mem_blk (jit_mem st1) (jit_mem st_final) jit_blk
         (Z.of_nat (2 * jitted_len st1))). {
          simpl.
          rewrite <- Hofs1.
          assumption.
        }
        specialize (IHl0 Heq); clear Heq.

        assert (Heq: match_registers_syn rbpf_st0 m0 (complementary_reg_list nl1 L) regs_blk). { ../..
          simpl.
          rewrite <- Hofs1.
          assumption.
        }
        specialize (IHl0 Heq); clear Heq.

        

        assert (Heq: match_state_jit rbpf_st stk). {
          eapply jit_alu32_thumb_upd_store_unchange_match_state_jit; eauto.
        }
        specialize (IHl0 Heq); clear Heq.

        rewrite Hofs1 in Hsub_mem.
        specialize (IHl0 Hsub_mem).

        remember (Z.of_nat (jitted_len stk + (jitted_len stk + 0))) as ofsk.
        rename Heqofsk into Hofsk.

        set (onel := [a]).
        assert (Heq: match_state_arm rbpf_st rsk m0 old_rs onel l2 jit_blk ofsk st_final) by auto.
        clear Hmatch_statek; rename Heq into Hmatch_statek.
        assert (Heq0: onel = [a]) by auto.

        assert (Heq: match_state_arm rbpf_st rsk m0 old_rs [] l2 jit_blk (Z.of_nat (2 * jitted_len stk))
         st_final). {
          simpl.
          rewrite <- Hofsk.

          clear - Hnodup_store Hofs0 Hofs1 Harm Hreg_val Hstorek Hofsk Hmatch_statek.

          set (el := []).
          assert (Heq: match_state_arm rbpf_st rs0 m0 old_rs el l2 jit_blk ofs0 st_final) by auto.
          clear Harm; rename Heq into Harm.
          assert (Heq1: el = []) by auto.

          destruct Harm.
          subst lsr.
          rename arm_mem into m0.


          destruct Hmatch_statek.

          rename arm_blk into jit_blk.
          rename lsr_stack into l2.
          rename rs0 into rsk.
          rename rs into rs0.
          rename arm_mem into m0.
          rename ofs0 into ofsk.
          rename ofs into ofs0.

          constructor; try assumption.
          - unfold regs_agree; simpl.
            intros r HF.
            inversion HF.
        }
        specialize (IHl0 Heq); clear Heq.

        destruct IHl0 as (rs1 & Hstore1 & Hinv1 & Hstar1 & Hmatch_state_1).


        unfold jit_alu32_thumb_upd_store in Hupd_store.
        unfold jit_alu32_thumb_load_store_template_jit in Hupd_store.
        apply upd_jitted_list_jittted_len_2 in Hupd_store as Hlen_eq.

        assert (Hcond: (2 * jitted_len st0 <= 1000)%nat). {
          simpl.
          unfold upd_jitted_list, upd_jitted_list' in Hupd_store.
          destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond1; [| inversion Hupd_store].
          clear Hupd_store.
          unfold JITTED_LIST_MAX_LENGTH in Hcond1.
          rewrite Nat.leb_le in Hcond1.
          lia.
        }

        assert (Hcond2: ( (2 * (jitted_len st0)) + 4 <= 1000)%nat). {
          simpl.
          unfold upd_jitted_list, upd_jitted_list' in Hupd_store.
          destruct (2 * jitted_len st0 + 4 <=? JITTED_LIST_MAX_LENGTH)%nat eqn: Hcond1; [| inversion Hupd_store].
          clear Hupd_store.
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
        { (**r arm_registers_store *)
          unfold arm_registers_store in *.
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

        split.
        { (**r arm_memory_inv0 *)
          destruct Hinv1.
          destruct Hinvk.
          destruct arm_inv_stk as (Hrsk_r13 & arm_inv_stk).
          destruct arm_inv_stk0 as (Hrs0_r13 & arm_inv_stk0).
          destruct arm_inv_reg as (arm_inv_reg_rsk & arm_inv_reg_rs1 & Hrange_perm).
          destruct arm_inv_reg0 as (arm_inv_reg_rs0 & _).
          constructor; try assumption.
          - (**r rs IR13 *)
            split.
            + rewrite <- Hrsk_r13.
              assumption.
            + assumption.
          - (**r arm_assume_register_map *)
            split; [assumption | ].
            split; assumption.
        }

        split.
        { (**r star BinSem.step *)
          eapply star_trans with (s2 := State rsk m0) (t1 := E0); eauto.
        }

        (**r match_state_arm *)
        simpl in Hmatch_state_1.
        rewrite <- Hofs1 in Hmatch_state_1.

        destruct Hinv1.
        destruct Hinvk.
        destruct Harm_inv.
        clear Hst.
        destruct Hmatch_state_1.
        destruct Hmatch_statek.
        rename rs1 into rsk.
        rename rs into rs1.
        rename lsr into l1.
        rename lsr_stack into l2.
        rename arm_blk into jit_blk.
        rename ofs1 into ofsk.
        rename ofs into ofs1.
        rename jit_st_final into st_final.

        subst lsr0.

        constructor; try assumption.

        (**r regs_agree *)
        unfold regs_agree in *.
        intros r Hin.
        destruct Hin as [Hr | Hin].
        * subst a.
          assert (Heq: In r [r]) by (simpl; left; reflexivity).
          specialize (Hreg_agree0 r Heq); clear Heq.
          eapply arm_registers_load_unchange_nodup_register with (r := r) in Hload1; eauto.
          2:{
            apply NoDup_cons_iff in Hnodup_load.
            clear - Hnodup_load.
            intuition.
          }
          rewrite <- Hload1.
          assumption.
        * apply Hreg_agree; auto.
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

      eapply jit_alu32_thumb_load_load_same; eauto.
      unfold jit_alu32_thumb_upd_save, jit_alu32_thumb_load_store_template_jit in Hupd_load.
      eapply upd_jitted_list_unchange_jittted_list_2 in Hupd_load; eauto.
      rewrite <- Hupd_load; assumption.
    - rewrite <- Hreg_val.
      symmetry.
      clear - Hst.
      destruct Hst.
      clear - mregs0.
      unfold match_registers in mregs0.
      destruct mregs0 as (_ & _ & Hreg).
      specialize (Hreg a).
      destruct Hreg as (vi & Hreg0 & Hreg1 & _).
      rewrite Hreg0, Hreg1.
      f_equal.
    Admitted.
*)
  End JITStore.

End JITProofWhole.