From bpf.comm Require Import Regs.

From Coq Require Import List ZArith.
Import ListNotations.
Open Scope bool_scope.

(**r ListSet is defined as a NoDup list *)

Fixpoint list_in_bool {A:Type} (eq: A -> A -> bool) (a:A) (l:list A) : bool :=
  match l with
    | [] => false
    | b :: m =>
      if eq a b then
        true
      else
        list_in_bool eq a m
  end.

Fixpoint app_no_repeat {A} (eq: A -> A -> bool) (l0 l1: list A): list A :=
  match l0 with
  | [] => l1
  | hd :: tl =>
    if list_in_bool eq hd l1 then
      app_no_repeat eq tl l1
    else
      hd :: (app_no_repeat eq tl l1)
  end.


(**r related lemmas *)


Lemma List_in_bool_true:
  forall r l,
    list_in_bool reg_eqb r l = true ->
      List.In r l.
Proof.
  induction l; simpl; intros.
  - inversion H.
  - destruct reg_eqb eqn: Heq.
    + left.
      symmetry.
      rewrite reg_eqb_true; assumption.
    + right.
      auto.
Qed.


Lemma true_List_in_bool:
  forall r l,
    List.In r l ->
      list_in_bool reg_eqb r l = true.
Proof.
  induction l; simpl; intros.
  - inversion H.
  - destruct reg_eqb eqn: Heq.
    + reflexivity.
    + rewrite <- reg_eqb_false in Heq.
      apply IHl.
      destruct H.
      * exfalso.
        apply Heq.
        auto.
      * assumption.
Qed.

Lemma list_in_bool_true_iff:
  forall r l,
    list_in_bool reg_eqb r l = true <-> List.In r l.
Proof.
  intros.
  split.
  apply List_in_bool_true; auto.
  apply true_List_in_bool; auto.
Qed.

Lemma List_in_bool_false:
  forall r l,
    list_in_bool reg_eqb r l = false ->
      ~List.In r l.
Proof.
  induction l; simpl; intros.
  - intro HF; auto.
  - intros HF.
    destruct HF.
    + subst.
      rewrite reg_eqb_refl in H.
      inversion H.
    + destruct reg_eqb eqn: Heq.
      * inversion H.
      * apply IHl; auto.
Qed.

Lemma false_List_in_bool:
  forall r l,
    ~List.In r l ->
      list_in_bool reg_eqb r l = false.
Proof.
  induction l; simpl; intros.
  - reflexivity.
  - destruct reg_eqb eqn: Heq.
    + exfalso.
      apply H.
      rewrite <- reg_eqb_true in Heq.
      left; auto.
    + apply IHl.
      intro HF; apply H.
      right; auto.
Qed.

Lemma list_in_bool_false_iff:
  forall r l,
    list_in_bool reg_eqb r l = false <-> ~ List.In r l.
Proof.
  intros.
  split.
  apply List_in_bool_false; auto.
  apply false_List_in_bool; auto.
Qed.

Lemma list_in_app_no_repeat_r:
  forall r l0 l1
    (H : In r l1),
      In r (app_no_repeat reg_eqb l0 l1).
Proof.
  induction l0; simpl; intros.
  - assumption.
  - destruct list_in_bool eqn: Hb.
    + apply IHl0.
      assumption.
    + simpl.
      right.
      apply IHl0.
      auto.
Qed.

Lemma list_in_app_no_repeat_l:
  forall r l0 l1
    (H : In r l0),
      In r (app_no_repeat reg_eqb l0 l1).
Proof.
  induction l0; simpl; intros.
  - inversion H.
  - destruct H.
    + subst.
      destruct list_in_bool eqn: Hin.
      * apply list_in_app_no_repeat_r; auto.
        apply List_in_bool_true; auto.
      * simpl.
        auto.
    + destruct list_in_bool eqn: Hin.
      * apply IHl0; auto.
      * simpl.
        right.
        apply IHl0; auto.
Qed.


Definition list_subset {A: Type} (l0 l1: list A): Prop :=
  forall r, In r l0 -> In r l1.

Lemma list_subset_refl:
  forall {A: Type} (l: list A),
    list_subset l l.
Proof.
  unfold list_subset.
  intros.
  assumption.
Qed.

Lemma list_subset_in:
  forall {A: Type} l0 l1 (r: A)
    (Hldr_sub : list_subset l0 l1)
    (H : In r l0),
      In r l1.
Proof.
  unfold list_subset.
  intros.
  auto.
Qed.

Lemma list_subset_app_no_repeat_in_l:
  forall l0 l1 l2 r
    (Hldr_sub : list_subset (app_no_repeat reg_eqb l0 l1) l2)
    (H : In r l0),
      In r l2.
Proof.
  intros.
  apply list_in_app_no_repeat_l with (l1 := l1) in H.
  eapply list_subset_in; eauto.
Qed.

Lemma list_subset_app_no_repeat_in_r:
  forall l0 l1 l2 r
    (Hldr_sub : list_subset (app_no_repeat reg_eqb l0 l1) l2)
    (H : In r l1),
      In r l2.
Proof.
  intros.
  apply list_in_app_no_repeat_r with (l0 := l0) in H.
  eapply list_subset_in; eauto.
Qed.

Lemma list_in_bool_app_no_repeat_r:
  forall l0 l1 r,
    list_in_bool reg_eqb r l1 = true ->
      list_in_bool reg_eqb r (app_no_repeat reg_eqb l0 l1) = true.
Proof.
  induction l0; simpl; intros.
  assumption.

  destruct (list_in_bool reg_eqb a l1) eqn: Hb.
  - apply IHl0; auto.
  - simpl.
    destruct reg_eqb.
    + reflexivity.
    + apply IHl0; auto.
Qed.

Lemma list_in_bool_in_r:
  forall a l0 l1,
    list_in_bool reg_eqb a (app_no_repeat reg_eqb l0 (a :: l1)) = true.
Proof.
  intros.
  apply list_in_bool_app_no_repeat_r.
  simpl.
  rewrite reg_eqb_refl.
  reflexivity.
Qed.

Lemma app_no_repeat_nil_r:
  forall l,
    app_no_repeat reg_eqb l [] = l.
Proof.
  induction l; simpl; intros.
  - reflexivity.
  - rewrite IHl.
    reflexivity.
Qed.



Definition whole_reg_list: list reg := [R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10].

Definition complementary_reg_list (l0 l1: list reg): list reg :=
  List.filter (fun r => negb (list_in_bool reg_eqb r l0)) l1.


Lemma whole_reg_list_is_nodup:
  NoDup whole_reg_list.
Proof.
  unfold whole_reg_list.
  repeat (constructor; [
  intro HF;
  repeat (destruct HF as [HF | HF]; [inversion HF | ]);
  apply in_nil in HF; assumption | ]).
  constructor.
Qed.

Lemma list_in_bool_tl:
  forall {A:Type} l (eq: A -> A -> bool) r a,
    list_in_bool eq r (a :: l) = eq r a || list_in_bool eq r l.
Proof.
  intros.
  simpl.
  destruct (eq r a).
  - rewrite Bool.orb_true_l.
    reflexivity.
  - destruct (list_in_bool eq r l).
    + rewrite Bool.orb_true_r.
      reflexivity.
    + rewrite Bool.orb_false_l.
      reflexivity.
Qed.

Lemma NoDup_app_l:
  forall {A:Type} (l: list A) l',
    NoDup (l ++ l') -> NoDup l.
Proof.
  induction l'.
  - intros.
    rewrite app_nil_r in H.
    assumption.
  - intros.
    apply IHl'.
    eapply NoDup_remove_1; eauto.
Qed.

Lemma NoDup_app_r:
  forall {A:Type} (l: list A) l',
    NoDup (l ++ l') -> NoDup l'.
Proof.
  induction l.
  - intros.
    rewrite app_nil_l in H.
    assumption.
  - intros.
    apply IHl.
    assert (Heq: (a :: l) ++ l' = a :: (l ++ l')) by auto.
    rewrite Heq in H; clear Heq.
    apply NoDup_cons_iff in H.
    destruct H as (_ & H); assumption.
Qed.

Lemma complementary_reg_list_is_nodup:
  forall l0 l1
    (Hnodup_l0: NoDup l0)
    (Hnodup_l1: NoDup l1),
      NoDup (complementary_reg_list l0 l1).
Proof.
  intros.
  induction l1.
  - unfold complementary_reg_list; simpl.
    assumption.
  - apply NoDup_cons_iff in Hnodup_l1.
    destruct Hnodup_l1 as (Hno_in & Hnodup_l1).
    specialize (IHl1 Hnodup_l1).
    unfold complementary_reg_list in *.
    assert (Heq: a :: l1 = [a] ++ l1).
    { simpl. reflexivity. }
    rewrite Heq; clear Heq.
    rewrite filter_app.
    simpl.
    destruct (negb (list_in_bool reg_eqb a l0)) eqn: Heq.
    2:{
      rewrite app_nil_l. assumption.
    }
    simpl.
    rewrite NoDup_cons_iff.
    split; [| assumption].
    rewrite Bool.negb_true_iff in Heq.
    apply List_in_bool_false in Heq.
    intro HF.
    apply Hno_in.
    rewrite filter_In in HF.
    destruct HF as (HT & _); assumption.
Qed.


Lemma complementary_reg_list_whole:
  forall l1 l0 r
    (Hsubset: list_subset l0 l1),
      List.In r ((complementary_reg_list l0 l1) ++ l0) <-> List.In r l1.
Proof.
  unfold complementary_reg_list, list_subset.
  induction l1.
  - split; intros.
    + apply Hsubset.
      simpl in H.
      assumption.
    + inversion H.
  - split; intros.
    + rewrite in_app_iff in H.
      destruct H.
      * rewrite filter_In in H.
        destruct H as (HT & _); assumption.
      * apply Hsubset; auto.
    + destruct H as [Heq | Hin].
      { subst a.
        rewrite in_app_iff.
        destruct (list_in_bool reg_eqb r l0) eqn: Heq.
        - apply List_in_bool_true in Heq.
          right; assumption.
        - left.
          rewrite filter_In.
          split; [left; reflexivity | ].
          rewrite Heq.
          auto.
      }
      { rewrite in_app_iff.
        destruct (list_in_bool reg_eqb r l0) eqn: Heq.
        - apply List_in_bool_true in Heq.
          right; assumption.
        - left.
          rewrite filter_In.
          split; [right; assumption | ].
          rewrite Heq.
          auto.
      }
Qed.

Lemma filter_true_same:
  forall {A:Type} (l0:list A) l1,
    List.filter (fun _ => true) l0 = l1 -> l0 = l1.
Proof.
  induction l0; intros.
  - simpl in H.
    assumption.
  - simpl in H.
    destruct l1.
    + inversion H.
    + injection H as Heq_a Heq_l.
      subst a0.
      erewrite IHl0; eauto.
Qed.