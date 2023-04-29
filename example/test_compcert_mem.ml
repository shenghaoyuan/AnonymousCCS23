
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type comparison =
| Eq
| Lt
| Gt

module Coq__1 = struct
 (** val add : int -> int -> int **)let rec add = (+)
end
include Coq__1

module Nat =
 struct
 end

module Pos =
 struct
  type mask =
  | IsNul
  | IsPos of int
  | IsNeg
 end

module Coq_Pos =
 struct
  (** val succ : int -> int **)

  let rec succ = Stdlib.succ

  (** val add : int -> int -> int **)

  let rec add = (+)

  (** val add_carry : int -> int -> int **)

  and add_carry x y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (fun p->1+2*p) (add_carry p q))
        (fun q -> (fun p->2*p) (add_carry p q))
        (fun _ -> (fun p->1+2*p) (succ p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (fun p->2*p) (add_carry p q))
        (fun q -> (fun p->1+2*p) (add p q))
        (fun _ -> (fun p->2*p) (succ p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (fun p->1+2*p) (succ q))
        (fun q -> (fun p->2*p) (succ q))
        (fun _ -> (fun p->1+2*p) 1)
        y)
      x

  (** val pred_double : int -> int **)

  let rec pred_double x =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p -> (fun p->1+2*p) ((fun p->2*p) p))
      (fun p -> (fun p->1+2*p) (pred_double p))
      (fun _ -> 1)
      x

  (** val pred_N : int -> int **)

  let pred_N x =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p -> ((fun p->2*p) p))
      (fun p -> (pred_double p))
      (fun _ -> 0)
      x

  type mask = Pos.mask =
  | IsNul
  | IsPos of int
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos 1
  | IsPos p -> IsPos ((fun p->1+2*p) p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos ((fun p->2*p) p)
  | x0 -> x0

  (** val double_pred_mask : int -> mask **)

  let double_pred_mask x =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p -> IsPos ((fun p->2*p) ((fun p->2*p) p)))
      (fun p -> IsPos ((fun p->2*p) (pred_double p)))
      (fun _ -> IsNul)
      x

  (** val sub_mask : int -> int -> mask **)

  let rec sub_mask x y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> double_mask (sub_mask p q))
        (fun q -> succ_double_mask (sub_mask p q))
        (fun _ -> IsPos ((fun p->2*p) p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> succ_double_mask (sub_mask_carry p q))
        (fun q -> double_mask (sub_mask p q))
        (fun _ -> IsPos (pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> IsNeg)
        (fun _ -> IsNeg)
        (fun _ -> IsNul)
        y)
      x

  (** val sub_mask_carry : int -> int -> mask **)

  and sub_mask_carry x y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> succ_double_mask (sub_mask_carry p q))
        (fun q -> double_mask (sub_mask p q))
        (fun _ -> IsPos (pred_double p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> double_mask (sub_mask_carry p q))
        (fun q -> succ_double_mask (sub_mask_carry p q))
        (fun _ -> double_pred_mask p)
        y)
      (fun _ -> IsNeg)
      x

  (** val mul : int -> int -> int **)

  let rec mul = ( * )

  (** val iter : ('a1 -> 'a1) -> 'a1 -> int -> 'a1 **)

  let rec iter f x n0 =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun n' -> f (iter f (iter f x n') n'))
      (fun n' -> iter f (iter f x n') n')
      (fun _ -> f x)
      n0

  (** val div2 : int -> int **)

  let div2 p =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 -> p0)
      (fun p0 -> p0)
      (fun _ -> 1)
      p

  (** val div2_up : int -> int **)

  let div2_up p =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 -> succ p0)
      (fun p0 -> p0)
      (fun _ -> 1)
      p

  (** val size : int -> int **)

  let rec size p =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 -> succ (size p0))
      (fun p0 -> succ (size p0))
      (fun _ -> 1)
      p

  (** val compare_cont : comparison -> int -> int -> comparison **)

  let rec compare_cont = fun c x y -> if x=y then c else if x<y then Lt else Gt

  (** val compare : int -> int -> comparison **)

  let compare = fun x y -> if x=y then Eq else if x<y then Lt else Gt

  (** val coq_Nsucc_double : int -> int **)

  let coq_Nsucc_double x =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 1)
      (fun p -> ((fun p->1+2*p) p))
      x

  (** val coq_Ndouble : int -> int **)

  let coq_Ndouble n0 =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 0)
      (fun p -> ((fun p->2*p) p))
      n0

  (** val coq_lor : int -> int -> int **)

  let rec coq_lor p q =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> (fun p->1+2*p) (coq_lor p0 q0))
        (fun q0 -> (fun p->1+2*p) (coq_lor p0 q0))
        (fun _ -> p)
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> (fun p->1+2*p) (coq_lor p0 q0))
        (fun q0 -> (fun p->2*p) (coq_lor p0 q0))
        (fun _ -> (fun p->1+2*p) p0)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> q)
        (fun q0 -> (fun p->1+2*p) q0)
        (fun _ -> q)
        q)
      p

  (** val coq_land : int -> int -> int **)

  let rec coq_land p q =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> coq_Nsucc_double (coq_land p0 q0))
        (fun q0 -> coq_Ndouble (coq_land p0 q0))
        (fun _ -> 1)
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> coq_Ndouble (coq_land p0 q0))
        (fun q0 -> coq_Ndouble (coq_land p0 q0))
        (fun _ -> 0)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> 1)
        (fun _ -> 0)
        (fun _ -> 1)
        q)
      p

  (** val ldiff : int -> int -> int **)

  let rec ldiff p q =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> coq_Ndouble (ldiff p0 q0))
        (fun q0 -> coq_Nsucc_double (ldiff p0 q0))
        (fun _ -> ((fun p->2*p) p0))
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> coq_Ndouble (ldiff p0 q0))
        (fun q0 -> coq_Ndouble (ldiff p0 q0))
        (fun _ -> p)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> 0)
        (fun _ -> 1)
        (fun _ -> 0)
        q)
      p

  (** val coq_lxor : int -> int -> int **)

  let rec coq_lxor p q =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> coq_Ndouble (coq_lxor p0 q0))
        (fun q0 -> coq_Nsucc_double (coq_lxor p0 q0))
        (fun _ -> ((fun p->2*p) p0))
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> coq_Nsucc_double (coq_lxor p0 q0))
        (fun q0 -> coq_Ndouble (coq_lxor p0 q0))
        (fun _ -> ((fun p->1+2*p) p0))
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> ((fun p->2*p) q0))
        (fun q0 -> ((fun p->1+2*p) q0))
        (fun _ -> 0)
        q)
      p

  (** val testbit : int -> int -> bool **)

  let rec testbit p n0 =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> true)
        (fun n1 -> testbit p0 (pred_N n1))
        n0)
      (fun p0 ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> false)
        (fun n1 -> testbit p0 (pred_N n1))
        n0)
      (fun _ ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> true)
        (fun _ -> false)
        n0)
      p

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> int -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 -> op a (iter_op op p0 (op a a)))
      (fun p0 -> iter_op op p0 (op a a))
      (fun _ -> a)
      p

  (** val to_nat : int -> int **)

  let to_nat x =
    iter_op Coq__1.add x (Stdlib.succ 0)

  (** val of_succ_nat : int -> int **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 1)
      (fun x -> succ (of_succ_nat x))
      n0

  (** val eq_dec : int -> int -> bool **)

  let rec eq_dec p x0 =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p1 -> eq_dec p0 p1)
        (fun _ -> false)
        (fun _ -> false)
        x0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> false)
        (fun p1 -> eq_dec p0 p1)
        (fun _ -> false)
        x0)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> false)
        (fun _ -> false)
        (fun _ -> true)
        x0)
      p
 end

module N =
 struct
  (** val succ_double : int -> int **)

  let succ_double x =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 1)
      (fun p -> ((fun p->1+2*p) p))
      x

  (** val double : int -> int **)

  let double n0 =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 0)
      (fun p -> ((fun p->2*p) p))
      n0

  (** val succ_pos : int -> int **)

  let succ_pos n0 =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 1)
      (fun p -> Coq_Pos.succ p)
      n0

  (** val sub : int -> int -> int **)

  let sub = fun n m -> Stdlib.max 0 (n-m)

  (** val compare : int -> int -> comparison **)

  let compare = fun x y -> if x=y then Eq else if x<y then Lt else Gt

  (** val leb : int -> int -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val pos_div_eucl : int -> int -> int * int **)

  let rec pos_div_eucl a b =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = succ_double r in
      if leb b r' then ((succ_double q), (sub r' b)) else ((double q), r'))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = double r in
      if leb b r' then ((succ_double q), (sub r' b)) else ((double q), r'))
      (fun _ ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> (0, 1))
        (fun p ->
        (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
          (fun _ -> (0, 1))
          (fun _ -> (0, 1))
          (fun _ -> (1, 0))
          p)
        b)
      a

  (** val coq_lor : int -> int -> int **)

  let coq_lor n0 m =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> m)
      (fun p ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> n0)
        (fun q -> (Coq_Pos.coq_lor p q))
        m)
      n0

  (** val coq_land : int -> int -> int **)

  let coq_land n0 m =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 0)
      (fun p ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> 0)
        (fun q -> Coq_Pos.coq_land p q)
        m)
      n0

  (** val ldiff : int -> int -> int **)

  let ldiff n0 m =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 0)
      (fun p ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> n0)
        (fun q -> Coq_Pos.ldiff p q)
        m)
      n0

  (** val coq_lxor : int -> int -> int **)

  let coq_lxor n0 m =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> m)
      (fun p ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> n0)
        (fun q -> Coq_Pos.coq_lxor p q)
        m)
      n0

  (** val testbit : int -> int -> bool **)

  let testbit a n0 =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> false)
      (fun p -> Coq_Pos.testbit p n0)
      a
 end

module Z =
 struct
  (** val double : int -> int **)

  let double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> ((fun p->2*p) p))
      (fun p -> (~-) ((fun p->2*p) p))
      x

  (** val succ_double : int -> int **)

  let succ_double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 1)
      (fun p -> ((fun p->1+2*p) p))
      (fun p -> (~-) (Coq_Pos.pred_double p))
      x

  (** val pred_double : int -> int **)

  let pred_double x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (~-) 1)
      (fun p -> (Coq_Pos.pred_double p))
      (fun p -> (~-) ((fun p->1+2*p) p))
      x

  (** val pos_sub : int -> int -> int **)

  let rec pos_sub x y =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> double (pos_sub p q))
        (fun q -> succ_double (pos_sub p q))
        (fun _ -> ((fun p->2*p) p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> pred_double (pos_sub p q))
        (fun q -> double (pos_sub p q))
        (fun _ -> (Coq_Pos.pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q -> (~-) ((fun p->2*p) q))
        (fun q -> (~-) (Coq_Pos.pred_double q))
        (fun _ -> 0)
        y)
      x

  (** val add : int -> int -> int **)

  let add = (+)

  (** val opp : int -> int **)

  let opp = (~-)

  (** val pred : int -> int **)

  let pred = Stdlib.pred

  (** val sub : int -> int -> int **)

  let sub = (-)

  (** val mul : int -> int -> int **)

  let mul = ( * )

  (** val pow_pos : int -> int -> int **)

  let pow_pos z0 =
    Coq_Pos.iter (mul z0) 1

  (** val pow : int -> int -> int **)

  let pow x y =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 1)
      (fun p -> pow_pos x p)
      (fun _ -> 0)
      y

  (** val compare : int -> int -> comparison **)

  let compare = fun x y -> if x=y then Eq else if x<y then Lt else Gt

  (** val leb : int -> int -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : int -> int -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val to_nat : int -> int **)

  let to_nat z0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> Coq_Pos.to_nat p)
      (fun _ -> 0)
      z0

  (** val of_nat : int -> int **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n1 -> (Coq_Pos.of_succ_nat n1))
      n0

  (** val of_N : int -> int **)

  let of_N = fun p -> p

  (** val iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

  let iter n0 f x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> x)
      (fun p -> Coq_Pos.iter f x p)
      (fun _ -> x)
      n0

  (** val pos_div_eucl : int -> int -> int * int **)

  let rec pos_div_eucl a b =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = add (mul ((fun p->2*p) 1) r) 1 in
      if ltb r' b
      then ((mul ((fun p->2*p) 1) q), r')
      else ((add (mul ((fun p->2*p) 1) q) 1), (sub r' b)))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = mul ((fun p->2*p) 1) r in
      if ltb r' b
      then ((mul ((fun p->2*p) 1) q), r')
      else ((add (mul ((fun p->2*p) 1) q) 1), (sub r' b)))
      (fun _ -> if leb ((fun p->2*p) 1) b then (0, 1) else (1, 0))
      a

  (** val div_eucl : int -> int -> int * int **)

  let div_eucl a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (0, 0))
      (fun a' ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, 0))
        (fun _ -> pos_div_eucl a' b)
        (fun b' ->
        let (q, r) = pos_div_eucl a' b' in
        ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
           (fun _ -> ((opp q), 0))
           (fun _ -> ((opp (add q 1)), (add b r)))
           (fun _ -> ((opp (add q 1)), (add b r)))
           r))
        b)
      (fun a' ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, 0))
        (fun _ ->
        let (q, r) = pos_div_eucl a' b in
        ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
           (fun _ -> ((opp q), 0))
           (fun _ -> ((opp (add q 1)), (sub b r)))
           (fun _ -> ((opp (add q 1)), (sub b r)))
           r))
        (fun b' -> let (q, r) = pos_div_eucl a' b' in (q, (opp r)))
        b)
      a

  (** val div : int -> int -> int **)

  let div a b =
    let (q, _) = div_eucl a b in q

  (** val modulo : int -> int -> int **)

  let modulo a b =
    let (_, r) = div_eucl a b in r

  (** val quotrem : int -> int -> int * int **)

  let quotrem a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> (0, 0))
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, a))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((of_N q), (of_N r)))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((opp (of_N q)), (of_N r)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, a))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((opp (of_N q)), (opp (of_N r))))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((of_N q), (opp (of_N r))))
        b)
      a

  (** val quot : int -> int -> int **)

  let quot a b =
    fst (quotrem a b)

  (** val rem : int -> int -> int **)

  let rem a b =
    snd (quotrem a b)

  (** val odd : int -> bool **)

  let odd z0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> false)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> true)
        (fun _ -> false)
        (fun _ -> true)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> true)
        (fun _ -> false)
        (fun _ -> true)
        p)
      z0

  (** val div2 : int -> int **)

  let div2 z0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> (Coq_Pos.div2 p))
        (fun _ -> (Coq_Pos.div2 p))
        (fun _ -> 0)
        p)
      (fun p -> (~-) (Coq_Pos.div2_up p))
      z0

  (** val log2 : int -> int **)

  let log2 z0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p -> (Coq_Pos.size p))
        (fun p -> (Coq_Pos.size p))
        (fun _ -> 0)
        p0)
      (fun _ -> 0)
      z0

  (** val testbit : int -> int -> bool **)

  let testbit a n0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> odd a)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> false)
        (fun a0 -> Coq_Pos.testbit a0 p)
        (fun a0 -> negb (N.testbit (Coq_Pos.pred_N a0) p))
        a)
      (fun _ -> false)
      n0

  (** val shiftl : int -> int -> int **)

  let shiftl a n0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> a)
      (fun p -> Coq_Pos.iter (mul ((fun p->2*p) 1)) a p)
      (fun p -> Coq_Pos.iter div2 a p)
      n0

  (** val shiftr : int -> int -> int **)

  let shiftr a n0 =
    shiftl a (opp n0)

  (** val coq_lor : int -> int -> int **)

  let coq_lor a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> (Coq_Pos.coq_lor a0 b0))
        (fun b0 -> (~-) (N.succ_pos (N.ldiff (Coq_Pos.pred_N b0) a0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> (~-)
        (N.succ_pos (N.ldiff (Coq_Pos.pred_N a0) b0)))
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_land (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0))))
        b)
      a

  (** val coq_land : int -> int -> int **)

  let coq_land a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> 0)
        (fun b0 -> of_N (Coq_Pos.coq_land a0 b0))
        (fun b0 -> of_N (N.ldiff a0 (Coq_Pos.pred_N b0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> 0)
        (fun b0 -> of_N (N.ldiff b0 (Coq_Pos.pred_N a0)))
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_lor (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0))))
        b)
      a

  (** val coq_lxor : int -> int -> int **)

  let coq_lxor a b =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> of_N (Coq_Pos.coq_lxor a0 b0))
        (fun b0 -> (~-) (N.succ_pos (N.coq_lxor a0 (Coq_Pos.pred_N b0))))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> (~-)
        (N.succ_pos (N.coq_lxor (Coq_Pos.pred_N a0) b0)))
        (fun b0 -> of_N (N.coq_lxor (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0)))
        b)
      a

  (** val eq_dec : int -> int -> bool **)

  let eq_dec x y =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> true)
        (fun _ -> false)
        (fun _ -> false)
        y)
      (fun x0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> false)
        (fun p0 -> Coq_Pos.eq_dec x0 p0)
        (fun _ -> false)
        y)
      (fun x0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> false)
        (fun _ -> false)
        (fun p0 -> Coq_Pos.eq_dec x0 p0)
        y)
      x
 end

(** val z_lt_dec : int -> int -> bool **)

let z_lt_dec x y =
  match Z.compare x y with
  | Lt -> true
  | _ -> false

(** val z_le_dec : int -> int -> bool **)

let z_le_dec x y =
  match Z.compare x y with
  | Gt -> false
  | _ -> true

(** val z_le_gt_dec : int -> int -> bool **)

let z_le_gt_dec =
  z_le_dec

(** val zeq_bool : int -> int -> bool **)

let zeq_bool x y =
  match Z.compare x y with
  | Eq -> true
  | _ -> false

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| [] -> []
| x :: l' -> app (rev l') (x :: [])

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t0 -> (f a) :: (map f t0)

(** val repeat : 'a1 -> int -> 'a1 list **)

let rec repeat x n0 =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun k -> x :: (repeat x k))
    n0

(** val zdivide_dec : int -> int -> bool **)

let zdivide_dec a b =
  let s = Z.eq_dec a 0 in
  if s then Z.eq_dec b 0 else Z.eq_dec (Z.modulo b a) 0

(** val shift_nat : int -> int -> int **)

let rec shift_nat n0 z0 =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> z0)
    (fun n1 -> (fun p->2*p) (shift_nat n1 z0))
    n0

(** val shift_pos : int -> int -> int **)

let shift_pos n0 z0 =
  Coq_Pos.iter (fun x -> (fun p->2*p) x) z0 n0

(** val two_power_nat : int -> int **)

let two_power_nat n0 =
  (shift_nat n0 1)

(** val two_power_pos : int -> int **)

let two_power_pos x =
  (shift_pos x 1)

(** val two_p : int -> int **)

let two_p x =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> 1)
    (fun y -> two_power_pos y)
    (fun _ -> 0)
    x

(** val peq : int -> int -> bool **)

let peq =
  Coq_Pos.eq_dec

(** val zeq : int -> int -> bool **)

let zeq =
  Z.eq_dec

(** val zlt : int -> int -> bool **)

let zlt =
  z_lt_dec

(** val zle : int -> int -> bool **)

let zle =
  z_le_gt_dec

(** val proj_sumbool : bool -> bool **)

let proj_sumbool = function
| true -> true
| false -> false

(** val p_mod_two_p : int -> int -> int **)

let rec p_mod_two_p p n0 =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> 0)
    (fun m ->
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q -> Z.succ_double (p_mod_two_p q m))
      (fun q -> Z.double (p_mod_two_p q m))
      (fun _ -> 1)
      p)
    n0

(** val zshiftin : bool -> int -> int **)

let zshiftin b x =
  if b then Z.succ_double x else Z.double x

(** val zzero_ext : int -> int -> int **)

let zzero_ext n0 x =
  Z.iter n0 (fun rec0 x0 -> zshiftin (Z.odd x0) (rec0 (Z.div2 x0))) (fun _ ->
    0) x

(** val zsign_ext : int -> int -> int **)

let zsign_ext n0 x =
  Z.iter (Z.pred n0) (fun rec0 x0 -> zshiftin (Z.odd x0) (rec0 (Z.div2 x0)))
    (fun x0 ->
    if (&&) (Z.odd x0) (proj_sumbool (zlt 0 n0)) then (~-) 1 else 0) x

(** val z_one_bits : int -> int -> int -> int list **)

let rec z_one_bits n0 x i =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m ->
    if Z.odd x
    then i :: (z_one_bits m (Z.div2 x) (Z.add i 1))
    else z_one_bits m (Z.div2 x) (Z.add i 1))
    n0

(** val p_is_power2 : int -> bool **)

let rec p_is_power2 p =
  (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
    (fun _ -> false)
    (fun q -> p_is_power2 q)
    (fun _ -> true)
    p

(** val z_is_power2 : int -> int option **)

let z_is_power2 x =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> None)
    (fun p -> if p_is_power2 p then Some (Z.log2 x) else None)
    (fun _ -> None)
    x

(** val zsize : int -> int **)

let zsize x =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> 0)
    (fun p -> (Coq_Pos.size p))
    (fun _ -> 0)
    x

(** val emin : int -> int -> int **)

let emin prec emax =
  Z.sub (Z.sub ((fun p->1+2*p) 1) emax) prec

type full_float =
| F754_zero of bool
| F754_infinity of bool
| F754_nan of bool * int
| F754_finite of bool * int * int

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan of bool * int
| B754_finite of bool * int * int

(** val fF2B : int -> int -> full_float -> binary_float **)

let fF2B _ _ = function
| F754_zero s -> B754_zero s
| F754_infinity s -> B754_infinity s
| F754_nan (b, pl) -> B754_nan (b, pl)
| F754_finite (s, m, e) -> B754_finite (s, m, e)

(** val join_bits : int -> int -> bool -> int -> int -> int **)

let join_bits mw ew s m e =
  Z.add (Z.shiftl (Z.add (if s then Z.pow ((fun p->2*p) 1) ew else 0) e) mw) m

(** val split_bits : int -> int -> int -> (bool * int) * int **)

let split_bits mw ew x =
  let mm = Z.pow ((fun p->2*p) 1) mw in
  let em = Z.pow ((fun p->2*p) 1) ew in
  (((Z.leb (Z.mul mm em) x), (Z.modulo x mm)), (Z.modulo (Z.div x mm) em))

(** val bits_of_binary_float : int -> int -> binary_float -> int **)

let bits_of_binary_float mw ew =
  let prec = Z.add mw 1 in
  let emax = Z.pow ((fun p->2*p) 1) (Z.sub ew 1) in
  (fun x ->
  match x with
  | B754_zero sx -> join_bits mw ew sx 0 0
  | B754_infinity sx ->
    join_bits mw ew sx 0 (Z.sub (Z.pow ((fun p->2*p) 1) ew) 1)
  | B754_nan (sx, plx) ->
    join_bits mw ew sx plx (Z.sub (Z.pow ((fun p->2*p) 1) ew) 1)
  | B754_finite (sx, mx, ex) ->
    let m = Z.sub mx (Z.pow ((fun p->2*p) 1) mw) in
    if Z.leb 0 m
    then join_bits mw ew sx m (Z.add (Z.sub ex (emin prec emax)) 1)
    else join_bits mw ew sx mx 0)

(** val binary_float_of_bits_aux : int -> int -> int -> full_float **)

let binary_float_of_bits_aux mw ew =
  let prec = Z.add mw 1 in
  let emax = Z.pow ((fun p->2*p) 1) (Z.sub ew 1) in
  (fun x ->
  let (p, ex) = split_bits mw ew x in
  let (sx, mx) = p in
  if zeq_bool ex 0
  then ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
          (fun _ -> F754_zero sx)
          (fun px -> F754_finite (sx, px, (emin prec emax)))
          (fun _ -> F754_nan (false, 1))
          mx)
  else if zeq_bool ex (Z.sub (Z.pow ((fun p->2*p) 1) ew) 1)
       then ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
               (fun _ -> F754_infinity sx)
               (fun plx -> F754_nan (sx, plx))
               (fun _ -> F754_nan (false, 1))
               mx)
       else ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
               (fun _ -> F754_nan (false, 1))
               (fun px -> F754_finite (sx, px,
               (Z.sub (Z.add ex (emin prec emax)) 1)))
               (fun _ -> F754_nan (false,
               1))
               (Z.add mx (Z.pow ((fun p->2*p) 1) mw))))

(** val binary_float_of_bits : int -> int -> int -> binary_float **)

let binary_float_of_bits mw ew x =
  let prec = Z.add mw 1 in
  let emax = Z.pow ((fun p->2*p) 1) (Z.sub ew 1) in
  fF2B prec emax (binary_float_of_bits_aux mw ew x)

type binary32 = binary_float

(** val b32_of_bits : int -> binary32 **)

let b32_of_bits =
  binary_float_of_bits ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->2*p) 1)))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))

(** val bits_of_b32 : binary32 -> int **)

let bits_of_b32 =
  bits_of_binary_float ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->2*p) 1)))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))

type binary64 = binary_float

(** val b64_of_bits : int -> binary64 **)

let b64_of_bits =
  binary_float_of_bits ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->2*p) ((fun p->1+2*p) 1))))) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->2*p) 1)))

(** val bits_of_b64 : binary64 -> int **)

let bits_of_b64 =
  bits_of_binary_float ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->2*p) ((fun p->1+2*p) 1))))) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->2*p) 1)))

(** val ptr64 : bool **)

let ptr64 =
  false

(** val big_endian : bool **)

let big_endian = false

type comparison0 =
| Ceq
| Cne
| Clt
| Cle
| Cgt
| Cge

module type WORDSIZE =
 sig
  val wordsize : int
 end

module Make =
 functor (WS:WORDSIZE) ->
 struct
  (** val wordsize : int **)

  let wordsize =
    WS.wordsize

  (** val zwordsize : int **)

  let zwordsize =
    Z.of_nat wordsize

  (** val modulus : int **)

  let modulus =
    two_power_nat wordsize

  (** val half_modulus : int **)

  let half_modulus =
    Z.div modulus ((fun p->2*p) 1)

  (** val max_unsigned : int **)

  let max_unsigned =
    Z.sub modulus 1

  (** val max_signed : int **)

  let max_signed =
    Z.sub half_modulus 1

  (** val min_signed : int **)

  let min_signed =
    Z.opp half_modulus

  (** val intval : int -> int **)

  let intval i =
    i

  (** val coq_Z_mod_modulus : int -> int **)

  let coq_Z_mod_modulus x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> p_mod_two_p p wordsize)
      (fun p ->
      let r = p_mod_two_p p wordsize in if zeq r 0 then 0 else Z.sub modulus r)
      x

  (** val unsigned : int -> int **)

  let unsigned =
    intval

  (** val signed : int -> int **)

  let signed n0 =
    let x = unsigned n0 in if zlt x half_modulus then x else Z.sub x modulus

  (** val repr : int -> int **)

  let repr =
    coq_Z_mod_modulus

  (** val zero : int **)

  let zero =
    repr 0

  (** val one : int **)

  let one =
    repr 1

  (** val mone : int **)

  let mone =
    repr ((~-) 1)

  (** val iwordsize : int **)

  let iwordsize =
    repr zwordsize

  (** val eq_dec : int -> int -> bool **)

  let eq_dec =
    zeq

  (** val eq : int -> int -> bool **)

  let eq x y =
    if zeq (unsigned x) (unsigned y) then true else false

  (** val lt : int -> int -> bool **)

  let lt x y =
    if zlt (signed x) (signed y) then true else false

  (** val ltu : int -> int -> bool **)

  let ltu x y =
    if zlt (unsigned x) (unsigned y) then true else false

  (** val neg : int -> int **)

  let neg x =
    repr (Z.opp (unsigned x))

  (** val add : int -> int -> int **)

  let add x y =
    repr (Z.add (unsigned x) (unsigned y))

  (** val sub : int -> int -> int **)

  let sub x y =
    repr (Z.sub (unsigned x) (unsigned y))

  (** val mul : int -> int -> int **)

  let mul x y =
    repr (Z.mul (unsigned x) (unsigned y))

  (** val divs : int -> int -> int **)

  let divs x y =
    repr (Z.quot (signed x) (signed y))

  (** val mods : int -> int -> int **)

  let mods x y =
    repr (Z.rem (signed x) (signed y))

  (** val divu : int -> int -> int **)

  let divu x y =
    repr (Z.div (unsigned x) (unsigned y))

  (** val modu : int -> int -> int **)

  let modu x y =
    repr (Z.modulo (unsigned x) (unsigned y))

  (** val coq_and : int -> int -> int **)

  let coq_and x y =
    repr (Z.coq_land (unsigned x) (unsigned y))

  (** val coq_or : int -> int -> int **)

  let coq_or x y =
    repr (Z.coq_lor (unsigned x) (unsigned y))

  (** val xor : int -> int -> int **)

  let xor x y =
    repr (Z.coq_lxor (unsigned x) (unsigned y))

  (** val not : int -> int **)

  let not x =
    xor x mone

  (** val shl : int -> int -> int **)

  let shl x y =
    repr (Z.shiftl (unsigned x) (unsigned y))

  (** val shru : int -> int -> int **)

  let shru x y =
    repr (Z.shiftr (unsigned x) (unsigned y))

  (** val shr : int -> int -> int **)

  let shr x y =
    repr (Z.shiftr (signed x) (unsigned y))

  (** val rol : int -> int -> int **)

  let rol x y =
    let n0 = Z.modulo (unsigned y) zwordsize in
    repr
      (Z.coq_lor (Z.shiftl (unsigned x) n0)
        (Z.shiftr (unsigned x) (Z.sub zwordsize n0)))

  (** val ror : int -> int -> int **)

  let ror x y =
    let n0 = Z.modulo (unsigned y) zwordsize in
    repr
      (Z.coq_lor (Z.shiftr (unsigned x) n0)
        (Z.shiftl (unsigned x) (Z.sub zwordsize n0)))

  (** val rolm : int -> int -> int -> int **)

  let rolm x a m =
    coq_and (rol x a) m

  (** val shrx : int -> int -> int **)

  let shrx x y =
    divs x (shl one y)

  (** val mulhu : int -> int -> int **)

  let mulhu x y =
    repr (Z.div (Z.mul (unsigned x) (unsigned y)) modulus)

  (** val mulhs : int -> int -> int **)

  let mulhs x y =
    repr (Z.div (Z.mul (signed x) (signed y)) modulus)

  (** val negative : int -> int **)

  let negative x =
    if lt x zero then one else zero

  (** val add_carry : int -> int -> int -> int **)

  let add_carry x y cin =
    if zlt (Z.add (Z.add (unsigned x) (unsigned y)) (unsigned cin)) modulus
    then zero
    else one

  (** val add_overflow : int -> int -> int -> int **)

  let add_overflow x y cin =
    let s = Z.add (Z.add (signed x) (signed y)) (signed cin) in
    if (&&) (proj_sumbool (zle min_signed s))
         (proj_sumbool (zle s max_signed))
    then zero
    else one

  (** val sub_borrow : int -> int -> int -> int **)

  let sub_borrow x y bin =
    if zlt (Z.sub (Z.sub (unsigned x) (unsigned y)) (unsigned bin)) 0
    then one
    else zero

  (** val sub_overflow : int -> int -> int -> int **)

  let sub_overflow x y bin =
    let s = Z.sub (Z.sub (signed x) (signed y)) (signed bin) in
    if (&&) (proj_sumbool (zle min_signed s))
         (proj_sumbool (zle s max_signed))
    then zero
    else one

  (** val shr_carry : int -> int -> int **)

  let shr_carry x y =
    if (&&) (lt x zero) (negb (eq (coq_and x (sub (shl one y) one)) zero))
    then one
    else zero

  (** val zero_ext : int -> int -> int **)

  let zero_ext n0 x =
    repr (zzero_ext n0 (unsigned x))

  (** val sign_ext : int -> int -> int **)

  let sign_ext n0 x =
    repr (zsign_ext n0 (unsigned x))

  (** val one_bits : int -> int list **)

  let one_bits x =
    map repr (z_one_bits wordsize (unsigned x) 0)

  (** val is_power2 : int -> int option **)

  let is_power2 x =
    match z_is_power2 (unsigned x) with
    | Some i -> Some (repr i)
    | None -> None

  (** val cmp : comparison0 -> int -> int -> bool **)

  let cmp c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> lt x y
    | Cle -> negb (lt y x)
    | Cgt -> lt y x
    | Cge -> negb (lt x y)

  (** val cmpu : comparison0 -> int -> int -> bool **)

  let cmpu c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> ltu x y
    | Cle -> negb (ltu y x)
    | Cgt -> ltu y x
    | Cge -> negb (ltu x y)

  (** val notbool : int -> int **)

  let notbool x =
    if eq x zero then one else zero

  (** val divmodu2 : int -> int -> int -> (int * int) option **)

  let divmodu2 nhi nlo d =
    if eq_dec d zero
    then None
    else let (q, r) =
           Z.div_eucl (Z.add (Z.mul (unsigned nhi) modulus) (unsigned nlo))
             (unsigned d)
         in
         if zle q max_unsigned then Some ((repr q), (repr r)) else None

  (** val divmods2 : int -> int -> int -> (int * int) option **)

  let divmods2 nhi nlo d =
    if eq_dec d zero
    then None
    else let (q, r) =
           Z.quotrem (Z.add (Z.mul (signed nhi) modulus) (unsigned nlo))
             (signed d)
         in
         if (&&) (proj_sumbool (zle min_signed q))
              (proj_sumbool (zle q max_signed))
         then Some ((repr q), (repr r))
         else None

  (** val testbit : int -> int -> bool **)

  let testbit x i =
    Z.testbit (unsigned x) i

  (** val int_of_one_bits : int list -> int **)

  let rec int_of_one_bits = function
  | [] -> zero
  | a :: b -> add (shl one a) (int_of_one_bits b)

  (** val no_overlap : int -> int -> int -> int -> bool **)

  let no_overlap ofs1 sz1 ofs2 sz2 =
    let x1 = unsigned ofs1 in
    let x2 = unsigned ofs2 in
    (&&)
      ((&&) (proj_sumbool (zlt (Z.add x1 sz1) modulus))
        (proj_sumbool (zlt (Z.add x2 sz2) modulus)))
      ((||) (proj_sumbool (zle (Z.add x1 sz1) x2))
        (proj_sumbool (zle (Z.add x2 sz2) x1)))

  (** val size : int -> int **)

  let size x =
    zsize (unsigned x)

  (** val unsigned_bitfield_extract : int -> int -> int -> int **)

  let unsigned_bitfield_extract pos width n0 =
    zero_ext width (shru n0 (repr pos))

  (** val signed_bitfield_extract : int -> int -> int -> int **)

  let signed_bitfield_extract pos width n0 =
    sign_ext width (shru n0 (repr pos))

  (** val bitfield_insert : int -> int -> int -> int -> int **)

  let bitfield_insert pos width n0 p =
    let mask0 = shl (repr (Z.sub (two_p width) 1)) (repr pos) in
    coq_or (shl (zero_ext width p) (repr pos)) (coq_and n0 (not mask0))
 end

module Wordsize_32 =
 struct
  (** val wordsize : int **)

  let wordsize =
    Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0)))))))))))))))))))))))))))))))
 end

module Int = Make(Wordsize_32)

module Wordsize_8 =
 struct
  (** val wordsize : int **)

  let wordsize =
    Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0)))))))
 end

module Byte = Make(Wordsize_8)

module Wordsize_64 =
 struct
  (** val wordsize : int **)

  let wordsize =
    Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 end

module Int64 =
 struct
  (** val wordsize : int **)

  let wordsize =
    Wordsize_64.wordsize

  (** val modulus : int **)

  let modulus =
    two_power_nat wordsize

  (** val intval : int -> int **)

  let intval = function
  | intval0 -> intval0

  (** val coq_Z_mod_modulus : int -> int **)

  let coq_Z_mod_modulus x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> p_mod_two_p p wordsize)
      (fun p ->
      let r = p_mod_two_p p wordsize in if zeq r 0 then 0 else Z.sub modulus r)
      x

  (** val unsigned : int -> int **)

  let unsigned =
    intval

  (** val repr : int -> int **)

  let repr x =
    (coq_Z_mod_modulus x)

  (** val eq_dec : int -> int -> bool **)

  let eq_dec x y =
    let intval0 = x in let intval1 = y in zeq intval0 intval1
 end

module Wordsize_Ptrofs =
 struct
  (** val wordsize : int **)

  let wordsize =
    if ptr64
    then Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    else Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ 0)))))))))))))))))))))))))))))))
 end

module Ptrofs =
 struct
  (** val wordsize : int **)

  let wordsize =
    Wordsize_Ptrofs.wordsize

  (** val modulus : int **)

  let modulus =
    two_power_nat wordsize

  (** val intval : int -> int **)

  let intval = function
  | intval0 -> intval0

  (** val coq_Z_mod_modulus : int -> int **)

  let coq_Z_mod_modulus x =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> p_mod_two_p p wordsize)
      (fun p ->
      let r = p_mod_two_p p wordsize in if zeq r 0 then 0 else Z.sub modulus r)
      x

  (** val unsigned : int -> int **)

  let unsigned =
    intval

  (** val repr : int -> int **)

  let repr x =
    (coq_Z_mod_modulus x)

  (** val zero : int **)

  let zero =
    repr 0

  (** val eq_dec : int -> int -> bool **)

  let eq_dec x y =
    let intval0 = x in let intval1 = y in zeq intval0 intval1
 end

module PTree =
 struct
  type 'a tree' =
  | Node001 of 'a tree'
  | Node010 of 'a
  | Node011 of 'a * 'a tree'
  | Node100 of 'a tree'
  | Node101 of 'a tree' * 'a tree'
  | Node110 of 'a tree' * 'a
  | Node111 of 'a tree' * 'a * 'a tree'

  type 'a tree =
  | Empty
  | Nodes of 'a tree'

  type 'a t = 'a tree

  (** val empty : 'a1 t **)

  let empty =
    Empty

  (** val get' : int -> 'a1 tree' -> 'a1 option **)

  let rec get' p m =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q ->
      match m with
      | Node001 m' -> get' q m'
      | Node011 (_, m') -> get' q m'
      | Node101 (_, m') -> get' q m'
      | Node111 (_, _, m') -> get' q m'
      | _ -> None)
      (fun q ->
      match m with
      | Node100 m' -> get' q m'
      | Node101 (m', _) -> get' q m'
      | Node110 (m', _) -> get' q m'
      | Node111 (m', _, _) -> get' q m'
      | _ -> None)
      (fun _ ->
      match m with
      | Node010 x -> Some x
      | Node011 (x, _) -> Some x
      | Node110 (_, x) -> Some x
      | Node111 (_, x, _) -> Some x
      | _ -> None)
      p

  (** val get : int -> 'a1 tree -> 'a1 option **)

  let get p = function
  | Empty -> None
  | Nodes m' -> get' p m'

  (** val set0 : int -> 'a1 -> 'a1 tree' **)

  let rec set0 p x =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q -> Node001 (set0 q x))
      (fun q -> Node100 (set0 q x))
      (fun _ -> Node010 x)
      p

  (** val set' : int -> 'a1 -> 'a1 tree' -> 'a1 tree' **)

  let rec set' p x m =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q ->
      match m with
      | Node001 r -> Node001 (set' q x r)
      | Node010 y -> Node011 (y, (set0 q x))
      | Node011 (y, r) -> Node011 (y, (set' q x r))
      | Node100 l -> Node101 (l, (set0 q x))
      | Node101 (l, r) -> Node101 (l, (set' q x r))
      | Node110 (l, y) -> Node111 (l, y, (set0 q x))
      | Node111 (l, y, r) -> Node111 (l, y, (set' q x r)))
      (fun q ->
      match m with
      | Node001 r -> Node101 ((set0 q x), r)
      | Node010 y -> Node110 ((set0 q x), y)
      | Node011 (y, r) -> Node111 ((set0 q x), y, r)
      | Node100 l -> Node100 (set' q x l)
      | Node101 (l, r) -> Node101 ((set' q x l), r)
      | Node110 (l, y) -> Node110 ((set' q x l), y)
      | Node111 (l, y, r) -> Node111 ((set' q x l), y, r))
      (fun _ ->
      match m with
      | Node001 r -> Node011 (x, r)
      | Node010 _ -> Node010 x
      | Node011 (_, r) -> Node011 (x, r)
      | Node100 l -> Node110 (l, x)
      | Node101 (l, r) -> Node111 (l, x, r)
      | Node110 (l, _) -> Node110 (l, x)
      | Node111 (l, _, r) -> Node111 (l, x, r))
      p

  (** val set : int -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let set p x = function
  | Empty -> Nodes (set0 p x)
  | Nodes m' -> Nodes (set' p x m')

  (** val map1' : ('a1 -> 'a2) -> 'a1 tree' -> 'a2 tree' **)

  let rec map1' f = function
  | Node001 r -> Node001 (map1' f r)
  | Node010 x -> Node010 (f x)
  | Node011 (x, r) -> Node011 ((f x), (map1' f r))
  | Node100 l -> Node100 (map1' f l)
  | Node101 (l, r) -> Node101 ((map1' f l), (map1' f r))
  | Node110 (l, x) -> Node110 ((map1' f l), (f x))
  | Node111 (l, x, r) -> Node111 ((map1' f l), (f x), (map1' f r))

  (** val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map1 f = function
  | Empty -> Empty
  | Nodes m0 -> Nodes (map1' f m0)
 end

module PMap =
 struct
  type 'a t = 'a * 'a PTree.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init x =
    (x, PTree.empty)

  (** val get : int -> 'a1 t -> 'a1 **)

  let get i m =
    match PTree.get i (snd m) with
    | Some x -> x
    | None -> fst m

  (** val set : int -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree **)

  let set i x m =
    ((fst m), (PTree.set i x (snd m)))

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m =
    ((f (fst m)), (PTree.map1 f (snd m)))
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> int

  val eq : t -> t -> bool
 end

module IMap =
 functor (X:INDEXED_TYPE) ->
 struct
  type elt = X.t

  (** val elt_eq : X.t -> X.t -> bool **)

  let elt_eq =
    X.eq

  type 'x t = 'x PMap.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init =
    PMap.init

  (** val get : X.t -> 'a1 t -> 'a1 **)

  let get i m =
    PMap.get (X.index i) m

  (** val set : X.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree **)

  let set i v m =
    PMap.set (X.index i) v m

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map =
    PMap.map
 end

module ZIndexed =
 struct
  type t = int

  (** val index : int -> int **)

  let index z0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 1)
      (fun p -> (fun p->2*p) p)
      (fun p -> (fun p->1+2*p) p)
      z0

  (** val eq : int -> int -> bool **)

  let eq =
    zeq
 end

module ZMap = IMap(ZIndexed)

(** val beq_dec : int -> int -> binary_float -> binary_float -> bool **)

let beq_dec _ _ f1 f2 =
  match f1 with
  | B754_zero s1 ->
    (match f2 with
     | B754_zero s2 ->
       if s1 then if s2 then true else false else if s2 then false else true
     | _ -> false)
  | B754_infinity s1 ->
    (match f2 with
     | B754_infinity s2 ->
       if s1 then if s2 then true else false else if s2 then false else true
     | _ -> false)
  | B754_nan (s1, p1) ->
    (match f2 with
     | B754_nan (s2, p2) ->
       if s1
       then if s2 then Coq_Pos.eq_dec p1 p2 else false
       else if s2 then false else Coq_Pos.eq_dec p1 p2
     | _ -> false)
  | B754_finite (s1, m1, e1) ->
    (match f2 with
     | B754_finite (s2, m2, e2) ->
       if s1
       then if s2
            then let s = Coq_Pos.eq_dec m1 m2 in
                 if s then Z.eq_dec e1 e2 else false
            else false
       else if s2
            then false
            else let s = Coq_Pos.eq_dec m1 m2 in
                 if s then Z.eq_dec e1 e2 else false
     | _ -> false)

type float = binary64

type float32 = binary32

module Float =
 struct
  (** val eq_dec : float -> float -> bool **)

  let eq_dec =
    beq_dec ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))))))

  (** val to_bits : float -> int **)

  let to_bits f =
    Int64.repr (bits_of_b64 f)

  (** val of_bits : int -> float **)

  let of_bits b =
    b64_of_bits (Int64.unsigned b)
 end

module Float32 =
 struct
  (** val eq_dec : float32 -> float32 -> bool **)

  let eq_dec =
    beq_dec ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))

  (** val to_bits : float32 -> int **)

  let to_bits f =
    Int.repr (bits_of_b32 f)

  (** val of_bits : int -> float32 **)

  let of_bits b =
    b32_of_bits (Int.unsigned b)
 end

type memory_chunk =
| Mint8signed
| Mint8unsigned
| Mint16signed
| Mint16unsigned
| Mint32
| Mint64
| Mfloat32
| Mfloat64
| Many32
| Many64

type block = int

(** val eq_block : int -> int -> bool **)

let eq_block =
  peq

type val0 =
| Vundef
| Vint of int
| Vlong of int
| Vfloat of float
| Vsingle of float32
| Vptr of block * int

let string_of_val0_hex v =
  match v with
  | Vundef -> "undefied value"
  | Vint x0 -> "Int32: " ^ (Printf.sprintf "0x%02x" x0)
  | Vlong x0 -> "Int64: " ^ (Printf.sprintf "0x%02x" x0)
  | Vptr (b, ofs) -> (Printf.sprintf "0x%02x" b) ^ "," ^ (Printf.sprintf "0x%02x" ofs)
  | _ -> "unexpected cases"

let print_val0_hex v = print_endline (string_of_val0_hex v)

module Val =
 struct
  (** val eq : val0 -> val0 -> bool **)

  let eq x y =
    match x with
    | Vundef -> (match y with
                 | Vundef -> true
                 | _ -> false)
    | Vint x0 -> (match y with
                  | Vint i0 -> Int.eq_dec x0 i0
                  | _ -> false)
    | Vlong x0 -> (match y with
                   | Vlong i0 -> Int64.eq_dec x0 i0
                   | _ -> false)
    | Vfloat x0 -> (match y with
                    | Vfloat f0 -> Float.eq_dec x0 f0
                    | _ -> false)
    | Vsingle x0 ->
      (match y with
       | Vsingle f0 -> Float32.eq_dec x0 f0
       | _ -> false)
    | Vptr (x0, x1) ->
      (match y with
       | Vptr (b0, i0) ->
         if eq_block x0 b0 then Ptrofs.eq_dec x1 i0 else false
       | _ -> false)

  (** val load_result : memory_chunk -> val0 -> val0 **)

  let load_result chunk v =
    match chunk with
    | Mint8signed ->
      (match v with
       | Vint n0 ->
         Vint (Int.sign_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))) n0)
       | _ -> Vundef)
    | Mint8unsigned ->
      (match v with
       | Vint n0 ->
         Vint (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))) n0)
       | _ -> Vundef)
    | Mint16signed ->
      (match v with
       | Vint n0 ->
         Vint
           (Int.sign_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
             ((fun p->2*p) 1)))) n0)
       | _ -> Vundef)
    | Mint16unsigned ->
      (match v with
       | Vint n0 ->
         Vint
           (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
             ((fun p->2*p) 1)))) n0)
       | _ -> Vundef)
    | Mint32 ->
      (match v with
       | Vint n0 -> Vint n0
       | Vptr (b, ofs) -> if ptr64 then Vundef else Vptr (b, ofs)
       | _ -> Vundef)
    | Mint64 ->
      (match v with
       | Vlong n0 -> Vlong n0
       | Vptr (b, ofs) -> if ptr64 then Vptr (b, ofs) else Vundef
       | _ -> Vundef)
    | Mfloat32 -> (match v with
                   | Vsingle f -> Vsingle f
                   | _ -> Vundef)
    | Mfloat64 -> (match v with
                   | Vfloat f -> Vfloat f
                   | _ -> Vundef)
    | Many32 ->
      (match v with
       | Vint _ -> v
       | Vsingle _ -> v
       | Vptr (_, _) -> if ptr64 then Vundef else v
       | _ -> Vundef)
    | Many64 -> v
 end

(** val size_chunk : memory_chunk -> int **)

let size_chunk = function
| Mint8signed -> 1
| Mint8unsigned -> 1
| Mint16signed -> ((fun p->2*p) 1)
| Mint16unsigned -> ((fun p->2*p) 1)
| Mint32 -> ((fun p->2*p) ((fun p->2*p) 1))
| Mfloat32 -> ((fun p->2*p) ((fun p->2*p) 1))
| Many32 -> ((fun p->2*p) ((fun p->2*p) 1))
| _ -> ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))

(** val size_chunk_nat : memory_chunk -> int **)

let size_chunk_nat chunk =
  Z.to_nat (size_chunk chunk)

(** val align_chunk : memory_chunk -> int **)

let align_chunk = function
| Mint8signed -> 1
| Mint8unsigned -> 1
| Mint16signed -> ((fun p->2*p) 1)
| Mint16unsigned -> ((fun p->2*p) 1)
| Mint64 -> ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
| _ -> ((fun p->2*p) ((fun p->2*p) 1))

type quantity =
| Q32
| Q64

(** val quantity_eq : quantity -> quantity -> bool **)

let quantity_eq q1 q2 =
  match q1 with
  | Q32 -> (match q2 with
            | Q32 -> true
            | Q64 -> false)
  | Q64 -> (match q2 with
            | Q32 -> false
            | Q64 -> true)

(** val size_quantity_nat : quantity -> int **)

let size_quantity_nat = function
| Q32 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ 0)))
| Q64 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0)))))))

type memval =
| Undef
| Byte of int
| Fragment of val0 * quantity * int

(** val bytes_of_int : int -> int -> int list **)

let rec bytes_of_int n0 x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m ->
    (Byte.repr x) :: (bytes_of_int m
                       (Z.div x ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                         ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                         ((fun p->2*p) ((fun p->2*p) 1)))))))))))
    n0

(** val int_of_bytes : int list -> int **)

let rec int_of_bytes = function
| [] -> 0
| b :: l' ->
  Z.add (Byte.unsigned b)
    (Z.mul (int_of_bytes l') ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1)))))))))

(** val rev_if_be : int list -> int list **)

let rev_if_be l =
  if big_endian then rev l else l

(** val encode_int : int -> int -> int list **)

let encode_int sz x =
  rev_if_be (bytes_of_int sz x)

(** val decode_int : int list -> int **)

let decode_int b =
  int_of_bytes (rev_if_be b)

(** val inj_bytes : int list -> memval list **)

let inj_bytes bl =
  map (fun x -> Byte x) bl

(** val proj_bytes : memval list -> int list option **)

let rec proj_bytes = function
| [] -> Some []
| m :: vl' ->
  (match m with
   | Byte b ->
     (match proj_bytes vl' with
      | Some bl -> Some (b :: bl)
      | None -> None)
   | _ -> None)

(** val inj_value_rec : int -> val0 -> quantity -> memval list **)

let rec inj_value_rec n0 v q =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m -> (Fragment (v, q, m)) :: (inj_value_rec m v q))
    n0

(** val inj_value : quantity -> val0 -> memval list **)

let inj_value q v =
  inj_value_rec (size_quantity_nat q) v q

(** val check_value : int -> val0 -> quantity -> memval list -> bool **)

let rec check_value n0 v q vl =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> match vl with
              | [] -> true
              | _ :: _ -> false)
    (fun m ->
    match vl with
    | [] -> false
    | m0 :: vl' ->
      (match m0 with
       | Fragment (v', q', m') ->
         (&&)
           ((&&)
             ((&&) (proj_sumbool (Val.eq v v'))
               (proj_sumbool (quantity_eq q q'))) ((=) m m'))
           (check_value m v q vl')
       | _ -> false))
    n0

(** val proj_value : quantity -> memval list -> val0 **)

let proj_value q vl = match vl with
| [] -> Vundef
| m :: _ ->
  (match m with
   | Fragment (v, _, _) ->
     if check_value (size_quantity_nat q) v q vl then v else Vundef
   | _ -> Vundef)

(** val encode_val : memory_chunk -> val0 -> memval list **)

let encode_val chunk v = match v with
| Vundef ->
  (match chunk with
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> repeat Undef (size_chunk_nat chunk))
| Vint n0 ->
  (match chunk with
   | Mint8signed ->
     inj_bytes (encode_int (Stdlib.succ 0) (Int.unsigned n0))
   | Mint8unsigned ->
     inj_bytes (encode_int (Stdlib.succ 0) (Int.unsigned n0))
   | Mint16signed ->
     inj_bytes
       (encode_int (Stdlib.succ (Stdlib.succ 0)) (Int.unsigned n0))
   | Mint16unsigned ->
     inj_bytes
       (encode_int (Stdlib.succ (Stdlib.succ 0)) (Int.unsigned n0))
   | Mint32 ->
     inj_bytes
       (encode_int (Stdlib.succ (Stdlib.succ (Stdlib.succ
         (Stdlib.succ 0)))) (Int.unsigned n0))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> repeat Undef (size_chunk_nat chunk))
| Vlong n0 ->
  (match chunk with
   | Mint64 ->
     inj_bytes
       (encode_int (Stdlib.succ (Stdlib.succ (Stdlib.succ
         (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
         (Stdlib.succ 0)))))))) (Int64.unsigned n0))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> repeat Undef (size_chunk_nat chunk))
| Vfloat n0 ->
  (match chunk with
   | Mfloat64 ->
     inj_bytes
       (encode_int (Stdlib.succ (Stdlib.succ (Stdlib.succ
         (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
         (Stdlib.succ 0)))))))) (Int64.unsigned (Float.to_bits n0)))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> repeat Undef (size_chunk_nat chunk))
| Vsingle n0 ->
  (match chunk with
   | Mfloat32 ->
     inj_bytes
       (encode_int (Stdlib.succ (Stdlib.succ (Stdlib.succ
         (Stdlib.succ 0)))) (Int.unsigned (Float32.to_bits n0)))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> repeat Undef (size_chunk_nat chunk))
| Vptr (_, _) ->
  (match chunk with
   | Mint32 ->
     if ptr64
     then repeat Undef (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ 0))))
     else inj_value Q32 v
   | Mint64 ->
     if ptr64
     then inj_value Q64 v
     else repeat Undef (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ 0))))))))
   | Many32 -> inj_value Q32 v
   | Many64 -> inj_value Q64 v
   | _ -> repeat Undef (size_chunk_nat chunk))

(** val decode_val : memory_chunk -> memval list -> val0 **)

let decode_val chunk vl =
  match proj_bytes vl with
  | Some bl ->
    (match chunk with
     | Mint8signed ->
       Vint
         (Int.sign_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
           (Int.repr (decode_int bl)))
     | Mint8unsigned ->
       Vint
         (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
           (Int.repr (decode_int bl)))
     | Mint16signed ->
       Vint
         (Int.sign_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
           ((fun p->2*p) 1)))) (Int.repr (decode_int bl)))
     | Mint16unsigned ->
       Vint
         (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
           ((fun p->2*p) 1)))) (Int.repr (decode_int bl)))
     | Mint32 -> Vint (Int.repr (decode_int bl))
     | Mint64 -> Vlong (Int64.repr (decode_int bl))
     | Mfloat32 -> Vsingle (Float32.of_bits (Int.repr (decode_int bl)))
     | Mfloat64 -> Vfloat (Float.of_bits (Int64.repr (decode_int bl)))
     | _ -> Vundef)
  | None ->
    (match chunk with
     | Mint32 ->
       if ptr64 then Vundef else Val.load_result chunk (proj_value Q32 vl)
     | Mint64 ->
       if ptr64 then Val.load_result chunk (proj_value Q64 vl) else Vundef
     | Many32 -> Val.load_result chunk (proj_value Q32 vl)
     | Many64 -> Val.load_result chunk (proj_value Q64 vl)
     | _ -> Vundef)

type permission =
| Freeable
| Writable
| Readable
| Nonempty

type perm_kind =
| Max
| Cur

module Mem =
 struct
  type mem' = { mem_contents : memval ZMap.t PMap.t;
                mem_access : (int -> perm_kind -> permission option) PMap.t;
                nextblock : block }

  (** val mem_contents : mem' -> memval ZMap.t PMap.t **)

  let mem_contents m =
    m.mem_contents

  (** val mem_access :
      mem' -> (int -> perm_kind -> permission option) PMap.t **)

  let mem_access m =
    m.mem_access

  (** val nextblock : mem' -> block **)

  let nextblock m =
    m.nextblock

  type mem = mem'

  (** val perm_order_dec : permission -> permission -> bool **)

  let perm_order_dec p1 p2 =
    match p1 with
    | Freeable -> true
    | Writable -> (match p2 with
                   | Freeable -> false
                   | _ -> true)
    | Readable ->
      (match p2 with
       | Freeable -> false
       | Writable -> false
       | _ -> true)
    | Nonempty -> (match p2 with
                   | Nonempty -> true
                   | _ -> false)

  (** val perm_order'_dec : permission option -> permission -> bool **)

  let perm_order'_dec op p =
    match op with
    | Some p0 -> perm_order_dec p0 p
    | None -> false

  (** val perm_dec :
      mem -> block -> int -> perm_kind -> permission -> bool **)

  let perm_dec m b ofs k p =
    perm_order'_dec (PMap.get b m.mem_access ofs k) p

  (** val range_perm_dec :
      mem -> block -> int -> int -> perm_kind -> permission -> bool **)

  let rec range_perm_dec m b lo hi k p =
    let s = zlt lo hi in
    if s
    then let s0 = perm_dec m b lo k p in
         if s0
         then let y = Z.add lo 1 in range_perm_dec m b y hi k p
         else false
    else true

  (** val valid_access_dec :
      mem -> memory_chunk -> block -> int -> permission -> bool **)

  let valid_access_dec m chunk b ofs p =
    let s = range_perm_dec m b ofs (Z.add ofs (size_chunk chunk)) Cur p in
    if s then zdivide_dec (align_chunk chunk) ofs else false

  (** val empty : mem **)

  let empty =
    { mem_contents = (PMap.init (ZMap.init Undef)); mem_access =
      (PMap.init (fun _ _ -> None)); nextblock = 1 }

  (** val alloc : mem -> int -> int -> mem' * block **)

  let alloc m lo hi =
    ({ mem_contents =
      (PMap.set m.nextblock (ZMap.init Undef) m.mem_contents); mem_access =
      (PMap.set m.nextblock (fun ofs _ ->
        if (&&) (proj_sumbool (zle lo ofs)) (proj_sumbool (zlt ofs hi))
        then Some Freeable
        else None) m.mem_access); nextblock = (Coq_Pos.succ m.nextblock) },
      m.nextblock)

  (** val getN : int -> int -> memval ZMap.t -> memval list **)

  let rec getN n0 p c =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> [])
      (fun n' -> (ZMap.get p c) :: (getN n' (Z.add p 1) c))
      n0

  (** val load : memory_chunk -> mem -> block -> int -> val0 option **)

  let load chunk m b ofs =
    if valid_access_dec m chunk b ofs Readable
    then Some
           (decode_val chunk
             (getN (size_chunk_nat chunk) ofs (PMap.get b m.mem_contents)))
    else None

  (** val loadv : memory_chunk -> mem -> val0 -> val0 option **)

  let loadv chunk m = function
  | Vptr (b, ofs) -> load chunk m b (Ptrofs.unsigned ofs)
  | _ -> None

  (** val setN : memval list -> int -> memval ZMap.t -> memval ZMap.t **)

  let rec setN vl p c =
    match vl with
    | [] -> c
    | v :: vl' -> setN vl' (Z.add p 1) (ZMap.set p v c)

  (** val store :
      memory_chunk -> mem -> block -> int -> val0 -> mem option **)

  let store chunk m b ofs v =
    if valid_access_dec m chunk b ofs Writable
    then Some { mem_contents =
           (PMap.set b
             (setN (encode_val chunk v) ofs (PMap.get b m.mem_contents))
             m.mem_contents); mem_access = m.mem_access; nextblock =
           m.nextblock }
    else None

  (** val storev : memory_chunk -> mem -> val0 -> val0 -> mem option **)

  let storev chunk m addr v =
    match addr with
    | Vptr (b, ofs) -> store chunk m b (Ptrofs.unsigned ofs) v
    | _ -> None
 end

(** val init_mem : Mem.mem' * block **)

let init_mem =
  Mem.alloc Mem.empty 0 ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->2*p) 1))))

(** val test_store : Mem.mem **)

let test_store =
  match Mem.storev Mint16unsigned (fst init_mem) (Vptr ((snd init_mem),
          Ptrofs.zero)) (Vint
             (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
               ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
               ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
               ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
               1))))))))))))))))) with
  | Some m ->
    (match Mem.storev Mint16unsigned m (Vptr ((snd init_mem),
             (Ptrofs.repr ((fun p->2*p) 1)))) (Vint
          (Int.repr ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
            ((fun p->1+2*p) ((fun p->2*p) 1))))))) with
     | Some m0 ->
       (match Mem.storev Mint16unsigned m0 (Vptr ((snd init_mem),
                (Ptrofs.repr ((fun p->2*p) ((fun p->2*p) 1))))) (Vint
                (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                  ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                  ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                  ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                  1)))))))))))))))) with
        | Some m1 -> m1
        | None -> Mem.empty)
     | None -> Mem.empty)
  | None -> Mem.empty

(** val test_compcert_mem : val0 list **)

let test_compcert_mem =
  let t0 =
    match Mem.loadv Mint8unsigned test_store (Vptr ((snd init_mem),
            Ptrofs.zero)) with
    | Some r -> r
    | None -> Vundef
  in
  let t1 =
    match Mem.loadv Mint8unsigned test_store (Vptr ((snd init_mem),
            (Ptrofs.repr 1))) with
    | Some r -> r
    | None -> Vundef
  in
  let t01 =
    match Mem.loadv Mint16unsigned test_store (Vptr ((snd init_mem),
            Ptrofs.zero)) with
    | Some r -> r
    | None -> Vundef
  in
  let t2 =
    match Mem.loadv Mint8unsigned test_store (Vptr ((snd init_mem),
            (Ptrofs.repr ((fun p->2*p) 1)))) with
    | Some r -> r
    | None -> Vundef
  in
  let t3 =
    match Mem.loadv Mint8unsigned test_store (Vptr ((snd init_mem),
            (Ptrofs.repr ((fun p->1+2*p) 1)))) with
    | Some r -> r
    | None -> Vundef
  in
  let t23 =
    match Mem.loadv Mint16unsigned test_store (Vptr ((snd init_mem),
            (Ptrofs.repr ((fun p->2*p) 1)))) with
    | Some r -> r
    | None -> Vundef
  in
  let t03 =
    match Mem.loadv Mint32 test_store (Vptr ((snd init_mem), Ptrofs.zero)) with
    | Some r -> r
    | None -> Vundef
  in let _ = print_val0_hex t0
  in let _ = print_val0_hex t1
  in let _ = print_val0_hex t01
  in let _ = print_val0_hex t2
  in let _ = print_val0_hex t3
  in let _ = print_val0_hex t23
  in let _ = print_val0_hex t03 in
  t0 :: (t1 :: (t01 :: (t2 :: (t3 :: (t23 :: (t03 :: []))))))
