
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

(** val length : 'a1 list -> int **)

let rec length = function
| [] -> 0
| _ :: l' -> Stdlib.succ (length l')

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

(** val eqb : bool -> bool -> bool **)

let eqb b1 b2 =
  if b1 then b2 else if b2 then false else true

module Nat =
 struct
  (** val add : int -> int -> int **)

  let rec add n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> m)
      (fun p -> Stdlib.succ (add p m))
      n0

  (** val mul : int -> int -> int **)

  let rec mul n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun p -> add m (mul p m))
      n0

  (** val sub : int -> int -> int **)

  let rec sub n0 m =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> n0)
      (fun k ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> n0)
        (fun l -> sub k l)
        m)
      n0

  (** val even : int -> bool **)

  let rec even n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> true)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> false)
        (fun n' -> even n')
        n1)
      n0

  (** val odd : int -> bool **)

  let odd n0 =
    negb (even n0)

  (** val div2 : int -> int **)

  let rec div2 = fun n -> n/2

  (** val bitwise : (bool -> bool -> bool) -> int -> int -> int -> int **)

  let rec bitwise op n0 a b =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n' ->
      add (if op (odd a) (odd b) then Stdlib.succ 0 else 0)
        (mul (Stdlib.succ (Stdlib.succ 0))
          (bitwise op n' (div2 a) (div2 b))))
      n0

  (** val coq_land : int -> int -> int **)

  let coq_land a b =
    bitwise (&&) a a b
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

  (** val eqb : int -> int -> bool **)

  let rec eqb p q =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun q0 -> eqb p0 q0)
        (fun _ -> false)
        (fun _ -> false)
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> false)
        (fun q0 -> eqb p0 q0)
        (fun _ -> false)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> false)
        (fun _ -> false)
        (fun _ -> true)
        q)
      p

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
  (** val of_nat : int -> int **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun n' -> (Coq_Pos.of_succ_nat n'))
      n0
 end

module Coq_N =
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

  (** val eqb : int -> int -> bool **)

  let eqb x y =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> true)
        (fun _ -> false)
        (fun _ -> false)
        y)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> false)
        (fun q -> Coq_Pos.eqb p q)
        (fun _ -> false)
        y)
      (fun p ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> false)
        (fun _ -> false)
        (fun q -> Coq_Pos.eqb p q)
        y)
      x

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
        let (q, r) = Coq_N.pos_div_eucl a0 b0 in ((of_N q), (of_N r)))
        (fun b0 ->
        let (q, r) = Coq_N.pos_div_eucl a0 b0 in ((opp (of_N q)), (of_N r)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> (0, a))
        (fun b0 ->
        let (q, r) = Coq_N.pos_div_eucl a0 b0 in
        ((opp (of_N q)), (opp (of_N r))))
        (fun b0 ->
        let (q, r) = Coq_N.pos_div_eucl a0 b0 in ((of_N q), (opp (of_N r))))
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
        (fun a0 -> negb (Coq_N.testbit (Coq_Pos.pred_N a0) p))
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
        (fun b0 -> (~-)
        (Coq_N.succ_pos (Coq_N.ldiff (Coq_Pos.pred_N b0) a0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> (~-)
        (Coq_N.succ_pos (Coq_N.ldiff (Coq_Pos.pred_N a0) b0)))
        (fun b0 -> (~-)
        (Coq_N.succ_pos
          (Coq_N.coq_land (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0))))
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
        (fun b0 -> of_N (Coq_N.ldiff a0 (Coq_Pos.pred_N b0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> 0)
        (fun b0 -> of_N (Coq_N.ldiff b0 (Coq_Pos.pred_N a0)))
        (fun b0 -> (~-)
        (Coq_N.succ_pos
          (Coq_N.coq_lor (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0))))
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
        (fun b0 -> (~-)
        (Coq_N.succ_pos (Coq_N.coq_lxor a0 (Coq_Pos.pred_N b0))))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
        (fun _ -> a)
        (fun b0 -> (~-)
        (Coq_N.succ_pos (Coq_N.coq_lxor (Coq_Pos.pred_N a0) b0)))
        (fun b0 ->
        of_N (Coq_N.coq_lxor (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0)))
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

(** val nth_error : 'a1 list -> int -> 'a1 option **)

let rec nth_error l n0 =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> match l with
              | [] -> None
              | x :: _ -> Some x)
    (fun n1 -> match l with
               | [] -> None
               | _ :: l0 -> nth_error l0 n1)
    n0

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t0 -> (f a) :: (map f t0)

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

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan of bool * int
| B754_finite of bool * int * int

type binary32 = binary_float

type binary64 = binary_float

(** val ptr64 : bool **)

let ptr64 =
  false

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

  (** val zero : int **)

  let zero =
    repr 0

  (** val coq_and : int -> int -> int **)

  let coq_and x y =
    repr (Z.coq_land (unsigned x) (unsigned y))

  (** val coq_or : int -> int -> int **)

  let coq_or x y =
    repr (Z.coq_lor (unsigned x) (unsigned y))

  (** val shl : int -> int -> int **)

  let shl x y =
    repr (Z.shiftl (unsigned x) (unsigned y))

  (** val shru : int -> int -> int **)

  let shru x y =
    repr (Z.shiftr (unsigned x) (unsigned y))
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

  (** val half_modulus : int **)

  let half_modulus =
    Z.div modulus ((fun p->2*p) 1)

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

  (** val signed : int -> int **)

  let signed n0 =
    let x = unsigned n0 in if zlt x half_modulus then x else Z.sub x modulus

  (** val repr : int -> int **)

  let repr x =
    (coq_Z_mod_modulus x)

  (** val of_ints : int -> int **)

  let of_ints x =
    repr (Int.signed x)
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

type float = binary64

type float32 = binary32

type block = int

type val0 =
| Vundef
| Vint of int
| Vlong of int
| Vfloat of float
| Vsingle of float32
| Vptr of block * int

let string_of_val0 v =
  match v with
  | Vundef -> "undefied value"
  | Vint x0 -> "Int32: " ^ (string_of_int x0)
  | Vlong x0 -> "Int64: " ^ (string_of_int x0)
  | Vptr (b, ofs) -> (string_of_int b) ^ "," ^ (string_of_int ofs)
  | _ -> "unexpected cases"

type quantity =
| Q32
| Q64

type memval =
| Undef
| Byte of int
| Fragment of val0 * quantity * int

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

  type mem = mem'

  (** val empty : mem **)

  let empty =
    { mem_contents = (PMap.init (ZMap.init Undef)); mem_access =
      (PMap.init (fun _ _ -> None)); nextblock = 1 }
 end

type ireg =
| IR0
| IR1
| IR2
| IR3
| IR4
| IR5
| IR6
| IR7
| IR8
| IR9
| IR10
| IR11
| IR12
| IR13
| IR14

let string_of_ireg ir =
  match ir with
  | IR0 -> "IR0 "
  | IR1 -> "IR1 "
  | IR2 -> "IR2 "
  | IR3 -> "IR3 "
  | IR4 -> "IR4 "
  | IR5 -> "IR5 "
  | IR6 -> "IR6 "
  | IR7 -> "IR7 "
  | IR8 -> "IR8 "
  | IR9 -> "IR9 "
  | IR10 -> "IR10 "
  | IR11 -> "FP "
  | IR12 -> "IP "
  | IR13 -> "SP "
  | IR14 -> "RA "

type freg =
| FR0
| FR1
| FR2
| FR3
| FR4
| FR5
| FR6
| FR7
| FR8
| FR9
| FR10
| FR11
| FR12
| FR13
| FR14
| FR15

type shift_op =
| SOimm of int
| SOreg of ireg
| SOlsl of ireg * int
| SOlsr of ireg * int
| SOasr of ireg * int
| SOror of ireg * int

let string_of_shift_op sop =
  match sop with
  | SOimm i  -> string_of_int i
  | SOreg ir -> string_of_ireg ir
  | _ -> "shift_op not yet"

type testcond =
| TCeq
| TCne
| TChs
| TClo
| TCmi
| TCpl
| TChi
| TCls
| TCge
| TClt
| TCgt
| TCle

type instruction =
| Padd of ireg * ireg * shift_op
| Pand of ireg * ireg * shift_op
| Pasr of ireg * ireg * ireg
| Pb of int
| Pbc of testcond * int
| Pbreg of ireg
| Pblreg of ireg
| Pbic of ireg * ireg * shift_op
| Pcmp of ireg * shift_op
| Pcmn of ireg * shift_op
| Peor of ireg * ireg * shift_op
| Pldr of ireg * ireg * shift_op
| Pldr_a of ireg * ireg * shift_op
| Pldrb of ireg * ireg * shift_op
| Pldrh of ireg * ireg * shift_op
| Pldrsb of ireg * ireg * shift_op
| Pldrsh of ireg * ireg * shift_op
| Plsl of ireg * ireg * ireg
| Plsr of ireg * ireg * ireg
| Pmla of ireg * ireg * ireg * ireg
| Pmov of ireg * shift_op
| Pmovw of ireg * int
| Pmovt of ireg * int
| Pmul of ireg * ireg * ireg
| Pmvn of ireg * shift_op
| Porr of ireg * ireg * shift_op
| Prsb of ireg * ireg * shift_op
| Psbfx of ireg * ireg * int * int
| Pstr of ireg * ireg * shift_op
| Pstr_a of ireg * ireg * shift_op
| Pstrb of ireg * ireg * shift_op
| Pstrh of ireg * ireg * shift_op
| Psdiv
| Psmull of ireg * ireg * ireg * ireg
| Psub of ireg * ireg * shift_op
| Pudiv
| Pumull of ireg * ireg * ireg * ireg
| Pfcpyd of freg * freg
| Pfabsd of freg * freg
| Pfnegd of freg * freg
| Pfaddd of freg * freg * freg
| Pfdivd of freg * freg * freg
| Pfmuld of freg * freg * freg
| Pfsubd of freg * freg * freg
| Pflid of freg * float
| Pfcmpd of freg * freg
| Pfcmpzd of freg
| Pfsitod of freg * ireg
| Pfuitod of freg * ireg
| Pftosizd of ireg * freg
| Pftouizd of ireg * freg
| Pfabss of freg * freg
| Pfnegs of freg * freg
| Pfadds of freg * freg * freg
| Pfdivs of freg * freg * freg
| Pfmuls of freg * freg * freg
| Pfsubs of freg * freg * freg
| Pflis of freg * float32
| Pfcmps of freg * freg
| Pfcmpzs of freg
| Pfsitos of freg * ireg
| Pfuitos of freg * ireg
| Pftosizs of ireg * freg
| Pftouizs of ireg * freg
| Pfcvtsd of freg * freg
| Pfcvtds of freg * freg
| Pfldd of freg * ireg * int
| Pfldd_a of freg * ireg * int
| Pflds of freg * ireg * int
| Pfstd of freg * ireg * int
| Pfstd_a of freg * ireg * int
| Pfsts of freg * ireg * int

let string_of_instruction ins =
  match ins with
  | Padd (dst, src, sop) -> "Padd " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_shift_op sop)
  | Pand (dst, src, sop) -> "Pand " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_shift_op sop)
  | Pasr (dst, src, r) -> "Pasr " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_ireg r)
  | Pb ofs -> "Pb " ^ (string_of_int ofs)
  | Pbreg r -> "Pbreg " ^ (string_of_ireg r)
  | Pcmp (r, sop) -> "Pcmp " ^ (string_of_ireg r) ^ (string_of_shift_op sop)
  | Peor (dst, src, sop) -> "Peor " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_shift_op sop)
  | Pldr (dst, src, sop) -> "Pldr " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_shift_op sop)
  | Plsl (dst, src, r) -> "Plsl " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_ireg r)
  | Plsr (dst, src, r) -> "Plsr " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_ireg r)
  | Pmov (r, sop) -> "Pmov " ^ (string_of_ireg r) ^ (string_of_shift_op sop)
  | Pmovw (r, i) -> "Pmovw " ^ (string_of_ireg r) ^ (string_of_int i)
  | Pmovt (r, i) -> "Pmovt " ^ (string_of_ireg r) ^ (string_of_int i)
  | Pmul (dst, src, r) -> "Pmul " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_ireg r)
  | Porr (dst, src, sop) -> "Porr " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_shift_op sop)
  | Prsb (dst, src, sop) -> "Prsb " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_shift_op sop)    
  | Pstr (dst, src, sop) -> "Pstr " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_shift_op sop)  
  | Psub (dst, src, sop) -> "Psub " ^ (string_of_ireg dst) ^ (string_of_ireg src) ^ (string_of_shift_op sop)
  | Pudiv -> "Pudiv R0 R0 R1 "
  | _ -> "instruction: not yet"

(** val decode_arm32 : int -> int -> int -> int **)

let decode_arm32 ins from size0 =
  Int.shru
    (Int.coq_and ins (Int.repr (Z.sub (two_p (Z.of_nat (add from size0))) 1)))
    (Int.repr (Z.of_nat from))

(** val int2ireg : int -> ireg option **)

let int2ireg r =
  if Int.eq r (Int.repr 0)
  then Some IR0
  else if Int.eq r (Int.repr 1)
       then Some IR1
       else if Int.eq r (Int.repr ((fun p->2*p) 1))
            then Some IR2
            else if Int.eq r (Int.repr ((fun p->1+2*p) 1))
                 then Some IR3
                 else if Int.eq r (Int.repr ((fun p->2*p) ((fun p->2*p) 1)))
                      then Some IR4
                      else if Int.eq r
                                (Int.repr ((fun p->1+2*p) ((fun p->2*p) 1)))
                           then Some IR5
                           else if Int.eq r
                                     (Int.repr ((fun p->2*p) ((fun p->1+2*p)
                                       1)))
                                then Some IR6
                                else if Int.eq r
                                          (Int.repr ((fun p->1+2*p)
                                            ((fun p->1+2*p) 1)))
                                     then Some IR7
                                     else if Int.eq r
                                               (Int.repr ((fun p->2*p)
                                                 ((fun p->2*p) ((fun p->2*p)
                                                 1))))
                                          then Some IR8
                                          else if Int.eq r
                                                    (Int.repr ((fun p->1+2*p)
                                                      ((fun p->2*p)
                                                      ((fun p->2*p) 1))))
                                               then Some IR9
                                               else if Int.eq r
                                                         (Int.repr
                                                           ((fun p->2*p)
                                                           ((fun p->1+2*p)
                                                           ((fun p->2*p) 1))))
                                                    then Some IR10
                                                    else if Int.eq r
                                                              (Int.repr
                                                                ((fun p->1+2*p)
                                                                ((fun p->1+2*p)
                                                                ((fun p->2*p)
                                                                1))))
                                                         then Some IR11
                                                         else if Int.eq r
                                                                   (Int.repr
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    1))))
                                                              then Some IR12
                                                              else if 
                                                                    Int.eq r
                                                                    (Int.repr
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    1))))
                                                                   then 
                                                                    Some IR13
                                                                   else 
                                                                    if 
                                                                    Int.eq r
                                                                    (Int.repr
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->1+2*p)
                                                                    1))))
                                                                    then 
                                                                    Some IR14
                                                                    else None

(** val decode_alu_reg : int -> instruction option **)

let decode_alu_reg ins =
  let op11_4 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0)))) (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))
  in
  let op =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0)))))))))))))))))))))
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))
  in
  if Int.eq op11_4 Int.zero
  then if Int.eq op Int.zero
       then let rn =
              int2ireg
                (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ 0))))))))))))))))
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ 0)))))
            in
            let rd =
              int2ireg
                (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ 0)))))))))))) (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ 0)))))
            in
            let rm =
              int2ireg
                (decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ 0)))))
            in
            (match rn with
             | Some rn0 ->
               (match rd with
                | Some rd0 ->
                  (match rm with
                   | Some rm0 -> Some (Pand (rd0, rn0, (SOreg rm0)))
                   | None -> None)
                | None -> None)
             | None -> None)
       else if Int.eq op Int.one
            then let rn =
                   int2ireg
                     (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ 0))))))))))))))))
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ 0)))))
                 in
                 let rd =
                   int2ireg
                     (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ 0)))))))))))) (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       0)))))
                 in
                 let rm =
                   int2ireg
                     (decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ 0)))))
                 in
                 (match rn with
                  | Some rn0 ->
                    (match rd with
                     | Some rd0 ->
                       (match rm with
                        | Some rm0 -> Some (Peor (rd0, rn0, (SOreg rm0)))
                        | None -> None)
                     | None -> None)
                  | None -> None)
            else if Int.eq op (Int.repr ((fun p->2*p) 1))
                 then let rn =
                        int2ireg
                          (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            0)))))))))))))))) (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ 0)))))
                      in
                      let rd =
                        int2ireg
                          (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ 0))))))))))))
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ 0)))))
                      in
                      let rm =
                        int2ireg
                          (decode_arm32 ins 0 (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ 0)))))
                      in
                      (match rn with
                       | Some rn0 ->
                         (match rd with
                          | Some rd0 ->
                            (match rm with
                             | Some rm0 -> Some (Psub (rd0, rn0, (SOreg rm0)))
                             | None -> None)
                          | None -> None)
                       | None -> None)
                 else if Int.eq op (Int.repr ((fun p->1+2*p) 1))
                      then let rn =
                             int2ireg
                               (decode_arm32 ins (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ 0))))))))))))))))
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ 0)))))
                           in
                           let rd =
                             int2ireg
                               (decode_arm32 ins (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ 0))))))))))))
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ 0)))))
                           in
                           let rm =
                             int2ireg
                               (decode_arm32 ins 0 (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ 0)))))
                           in
                           (match rn with
                            | Some rn0 ->
                              (match rd with
                               | Some rd0 ->
                                 (match rm with
                                  | Some rm0 ->
                                    Some (Prsb (rd0, rn0, (SOreg rm0)))
                                  | None -> None)
                               | None -> None)
                            | None -> None)
                      else if Int.eq op
                                (Int.repr ((fun p->2*p) ((fun p->2*p) 1)))
                           then let rn =
                                  int2ireg
                                    (decode_arm32 ins (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ 0))))))))))))))))
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ 0)))))
                                in
                                let rd =
                                  int2ireg
                                    (decode_arm32 ins (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ 0))))))))))))
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ 0)))))
                                in
                                let rm =
                                  int2ireg
                                    (decode_arm32 ins 0 (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ 0)))))
                                in
                                (match rn with
                                 | Some rn0 ->
                                   (match rd with
                                    | Some rd0 ->
                                      (match rm with
                                       | Some rm0 ->
                                         Some (Padd (rd0, rn0, (SOreg rm0)))
                                       | None -> None)
                                    | None -> None)
                                 | None -> None)
                           else if Int.eq op
                                     (Int.repr ((fun p->2*p) ((fun p->2*p)
                                       ((fun p->1+2*p) 1))))
                                then let rn =
                                       int2ireg
                                         (decode_arm32 ins (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ 0))))))))))))))))
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           0)))))
                                     in
                                     let rd =
                                       int2ireg
                                         (decode_arm32 ins (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ 0))))))))))))
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           0)))))
                                     in
                                     let rm =
                                       int2ireg
                                         (decode_arm32 ins 0 (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ 0)))))
                                     in
                                     (match rn with
                                      | Some rn0 ->
                                        (match rd with
                                         | Some rd0 ->
                                           (match rm with
                                            | Some rm0 ->
                                              Some (Porr (rd0, rn0, (SOreg
                                                rm0)))
                                            | None -> None)
                                         | None -> None)
                                      | None -> None)
                                else if Int.eq op
                                          (Int.repr ((fun p->1+2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            1))))
                                     then let rd =
                                            int2ireg
                                              (decode_arm32 ins
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                0))))))))))))
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ 0)))))
                                          in
                                          let rm =
                                            int2ireg
                                              (decode_arm32 ins 0
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ 0)))))
                                          in
                                          (match rd with
                                           | Some rd0 ->
                                             (match rm with
                                              | Some rm0 ->
                                                Some (Pmov (rd0, (SOreg rm0)))
                                              | None -> None)
                                           | None -> None)
                                     else None
  else if Int.eq op Int.zero
       then let op15_12 =
              decode_arm32 ins (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ 0)))))))))))) (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))
            in
            let op7_4 =
              decode_arm32 ins (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ 0)))) (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))
            in
            if (&&) (Int.eq op15_12 (Int.repr 0))
                 (Int.eq op7_4
                   (Int.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1)))))
            then let rd =
                   int2ireg
                     (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ 0))))))))))))))))
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ 0)))))
                 in
                 let rm =
                   int2ireg
                     (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       0)))))))) (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ 0)))))
                 in
                 let rn =
                   int2ireg
                     (decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ 0)))))
                 in
                 (match rd with
                  | Some rd0 ->
                    (match rm with
                     | Some rm0 ->
                       (match rn with
                        | Some rn0 -> Some (Pand (rd0, rn0, (SOreg rm0)))
                        | None -> None)
                     | None -> None)
                  | None -> None)
            else None
       else if Int.eq op
                 (Int.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1))))
            then let op19_4 =
                   decode_arm32 ins (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ 0)))) (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     0))))))))))))))))
                 in
                 if Int.eq op19_4
                      (Int.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                        1))))))))))))))))
                 then let rm =
                        int2ireg
                          (decode_arm32 ins 0 (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ 0)))))
                      in
                      (match rm with
                       | Some rm0 -> Some (Pbreg rm0)
                       | None -> None)
                 else None
            else if Int.eq op
                      (Int.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                        1))))
                 then let op19_16 =
                        decode_arm32 ins (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ (Stdlib.succ 0))))))))))))))))
                          (Stdlib.succ (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ 0))))
                      in
                      if Int.eq op19_16 Int.zero
                      then let op7_4 =
                             decode_arm32 ins (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ 0)))) (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ 0))))
                           in
                           if Int.eq op7_4 Int.one
                           then let rd =
                                  int2ireg
                                    (decode_arm32 ins (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ 0))))))))))))
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ 0)))))
                                in
                                let rm =
                                  int2ireg
                                    (decode_arm32 ins (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ 0))))))))
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ 0)))))
                                in
                                let rn =
                                  int2ireg
                                    (decode_arm32 ins 0 (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ 0)))))
                                in
                                (match rd with
                                 | Some rd0 ->
                                   (match rm with
                                    | Some rm0 ->
                                      (match rn with
                                       | Some rn0 ->
                                         Some (Plsl (rd0, rn0, rm0))
                                       | None -> None)
                                    | None -> None)
                                 | None -> None)
                           else if Int.eq op7_4 (Int.repr ((fun p->1+2*p) 1))
                                then let rd =
                                       int2ireg
                                         (decode_arm32 ins (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ 0))))))))))))
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           0)))))
                                     in
                                     let rm =
                                       int2ireg
                                         (decode_arm32 ins (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ 0))))))))
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           0)))))
                                     in
                                     let rn =
                                       int2ireg
                                         (decode_arm32 ins 0 (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ 0)))))
                                     in
                                     (match rd with
                                      | Some rd0 ->
                                        (match rm with
                                         | Some rm0 ->
                                           (match rn with
                                            | Some rn0 ->
                                              Some (Plsr (rd0, rn0, rm0))
                                            | None -> None)
                                         | None -> None)
                                      | None -> None)
                                else if Int.eq op7_4
                                          (Int.repr ((fun p->1+2*p)
                                            ((fun p->2*p) 1)))
                                     then let rd =
                                            int2ireg
                                              (decode_arm32 ins
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                0))))))))))))
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ 0)))))
                                          in
                                          let rm =
                                            int2ireg
                                              (decode_arm32 ins
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ 0))))))))
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ 0)))))
                                          in
                                          let rn =
                                            int2ireg
                                              (decode_arm32 ins 0
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ 0)))))
                                          in
                                          (match rd with
                                           | Some rd0 ->
                                             (match rm with
                                              | Some rm0 ->
                                                (match rn with
                                                 | Some rn0 ->
                                                   Some (Pasr (rd0, rn0, rm0))
                                                 | None -> None)
                                              | None -> None)
                                           | None -> None)
                                     else None
                      else None
                 else None

(** val decode_cmp_reg : int -> instruction option **)

let decode_cmp_reg ins =
  let op24_21 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0)))))))))))))))))))))
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))
  in
  let op15_4 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0)))) (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))
  in
  if (&&)
       (Int.eq op24_21
         (Int.repr ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))))
       (Int.eq op15_4 Int.zero)
  then let rn =
         int2ireg
           (decode_arm32 ins (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))))))
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))
       in
       let rm =
         int2ireg
           (decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))))))))))
       in
       (match rn with
        | Some rn0 ->
          (match rm with
           | Some rm0 -> Some (Pcmp (rn0, (SOreg rm0)))
           | None -> None)
        | None -> None)
  else None

(** val decode_alu_imm : int -> instruction option **)

let decode_alu_imm ins =
  let op =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0)))))))))))))))))))))
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))
  in
  if Int.eq op Int.zero
  then let rn =
         int2ireg
           (decode_arm32 ins (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))))))
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))
       in
       let rd =
         int2ireg
           (decode_arm32 ins (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))))))))) (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0)))))
       in
       let imm12 =
         decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ 0))))))))))))
       in
       (match rn with
        | Some rn0 ->
          (match rd with
           | Some rd0 -> Some (Pand (rd0, rn0, (SOimm imm12)))
           | None -> None)
        | None -> None)
  else if Int.eq op Int.one
       then let rn =
              int2ireg
                (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ 0))))))))))))))))
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ 0)))))
            in
            let rd =
              int2ireg
                (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ
                  (Stdlib.succ 0)))))))))))) (Stdlib.succ
                  (Stdlib.succ (Stdlib.succ (Stdlib.succ 0)))))
            in
            let imm12 =
              decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ 0))))))))))))
            in
            (match rn with
             | Some rn0 ->
               (match rd with
                | Some rd0 -> Some (Peor (rd0, rn0, (SOimm imm12)))
                | None -> None)
             | None -> None)
       else if Int.eq op (Int.repr ((fun p->2*p) 1))
            then let rn =
                   int2ireg
                     (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ 0))))))))))))))))
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ 0)))))
                 in
                 let rd =
                   int2ireg
                     (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       (Stdlib.succ 0)))))))))))) (Stdlib.succ
                       (Stdlib.succ (Stdlib.succ (Stdlib.succ
                       0)))))
                 in
                 let imm12 =
                   decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ 0))))))))))))
                 in
                 (match rn with
                  | Some rn0 ->
                    (match rd with
                     | Some rd0 -> Some (Psub (rd0, rn0, (SOimm imm12)))
                     | None -> None)
                  | None -> None)
            else if Int.eq op (Int.repr ((fun p->1+2*p) 1))
                 then let rn =
                        int2ireg
                          (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            0)))))))))))))))) (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ 0)))))
                      in
                      let rd =
                        int2ireg
                          (decode_arm32 ins (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ 0))))))))))))
                            (Stdlib.succ (Stdlib.succ
                            (Stdlib.succ (Stdlib.succ 0)))))
                      in
                      let imm12 =
                        decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ (Stdlib.succ (Stdlib.succ
                          (Stdlib.succ 0))))))))))))
                      in
                      (match rn with
                       | Some rn0 ->
                         (match rd with
                          | Some rd0 -> Some (Prsb (rd0, rn0, (SOimm imm12)))
                          | None -> None)
                       | None -> None)
                 else if Int.eq op (Int.repr ((fun p->2*p) ((fun p->2*p) 1)))
                      then let rn =
                             int2ireg
                               (decode_arm32 ins (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ 0))))))))))))))))
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ 0)))))
                           in
                           let rd =
                             int2ireg
                               (decode_arm32 ins (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ 0))))))))))))
                                 (Stdlib.succ (Stdlib.succ
                                 (Stdlib.succ (Stdlib.succ 0)))))
                           in
                           let imm12 =
                             decode_arm32 ins 0 (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ 0))))))))))))
                           in
                           (match rn with
                            | Some rn0 ->
                              (match rd with
                               | Some rd0 ->
                                 Some (Padd (rd0, rn0, (SOimm imm12)))
                               | None -> None)
                            | None -> None)
                      else if Int.eq op
                                (Int.repr ((fun p->2*p) ((fun p->2*p)
                                  ((fun p->2*p) 1))))
                           then let imm4 =
                                  decode_arm32 ins (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ 0))))))))))))))))
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ 0))))
                                in
                                let rd =
                                  int2ireg
                                    (decode_arm32 ins (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ 0))))))))))))
                                      (Stdlib.succ (Stdlib.succ
                                      (Stdlib.succ (Stdlib.succ 0)))))
                                in
                                let imm12 =
                                  decode_arm32 ins 0 (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ (Stdlib.succ
                                    (Stdlib.succ 0))))))))))))
                                in
                                let imm16 =
                                  Int.coq_or
                                    (Int.shl imm4
                                      (Int.repr ((fun p->2*p) ((fun p->2*p)
                                        ((fun p->1+2*p) 1))))) imm12
                                in
                                (match rd with
                                 | Some rd0 -> Some (Pmovw (rd0, imm16))
                                 | None -> None)
                           else if Int.eq op
                                     (Int.repr ((fun p->2*p) ((fun p->1+2*p)
                                       ((fun p->2*p) 1))))
                                then let imm4 =
                                       decode_arm32 ins (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ 0))))))))))))))))
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         0))))
                                     in
                                     let rd =
                                       int2ireg
                                         (decode_arm32 ins (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ 0))))))))))))
                                           (Stdlib.succ (Stdlib.succ
                                           (Stdlib.succ (Stdlib.succ
                                           0)))))
                                     in
                                     let imm12 =
                                       decode_arm32 ins 0 (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ (Stdlib.succ
                                         (Stdlib.succ 0))))))))))))
                                     in
                                     let imm16 =
                                       Int.coq_or
                                         (Int.shl imm4
                                           (Int.repr ((fun p->2*p)
                                             ((fun p->2*p) ((fun p->1+2*p)
                                             1))))) imm12
                                     in
                                     (match rd with
                                      | Some rd0 -> Some (Pmovt (rd0, imm16))
                                      | None -> None)
                                else if Int.eq op
                                          (Int.repr ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            1))))
                                     then let rn =
                                            int2ireg
                                              (decode_arm32 ins
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                0))))))))))))))))
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ 0)))))
                                          in
                                          let rd =
                                            int2ireg
                                              (decode_arm32 ins
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                0))))))))))))
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ
                                                (Stdlib.succ 0)))))
                                          in
                                          let imm12 =
                                            decode_arm32 ins 0
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ
                                              (Stdlib.succ 0))))))))))))
                                          in
                                          (match rn with
                                           | Some rn0 ->
                                             (match rd with
                                              | Some rd0 ->
                                                Some (Porr (rd0, rn0, (SOimm
                                                  imm12)))
                                              | None -> None)
                                           | None -> None)
                                     else if Int.eq op
                                               (Int.repr ((fun p->1+2*p)
                                                 ((fun p->2*p)
                                                 ((fun p->1+2*p) 1))))
                                          then let rd =
                                                 int2ireg
                                                   (decode_arm32 ins
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     0))))))))))))
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ
                                                     (Stdlib.succ 0)))))
                                               in
                                               let imm12 =
                                                 decode_arm32 ins 0
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   (Stdlib.succ
                                                   0))))))))))))
                                               in
                                               (match rd with
                                                | Some rd0 ->
                                                  Some (Pmov (rd0, (SOimm
                                                    imm12)))
                                                | None -> None)
                                          else None

(** val decode_cmp_imm : int -> instruction option **)

let decode_cmp_imm ins =
  let op24_21 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0)))))))))))))))))))))
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))
  in
  let op15_12 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0)))))))))))) (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))
  in
  if (&&)
       (Int.eq op24_21
         (Int.repr ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))))
       (Int.eq op15_12 Int.zero)
  then let rn =
         int2ireg
           (decode_arm32 ins (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))))))
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))
       in
       let imm12 =
         decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ 0))))))))))))
       in
       (match rn with
        | Some rn0 -> Some (Pcmp (rn0, (SOimm imm12)))
        | None -> None)
  else None

(** val decode_mem_imm : int -> instruction option **)

let decode_mem_imm ins =
  let op24_21 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0)))))))))))))))))))))
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))
  in
  let op20 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0)))))))))))))))))))) (Stdlib.succ 0)
  in
  if Int.eq op24_21
       (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
  then let rn =
         int2ireg
           (decode_arm32 ins (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))))))
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))
       in
       let rt =
         int2ireg
           (decode_arm32 ins (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))))))))) (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0)))))
       in
       let imm12 =
         decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ 0))))))))))))
       in
       (match rn with
        | Some rn0 ->
          (match rt with
           | Some rt0 ->
             if Int.eq op20 Int.zero
             then Some (Pstr (rt0, rn0, (SOimm imm12)))
             else Some (Pldr (rt0, rn0, (SOimm imm12)))
           | None -> None)
        | None -> None)
  else None

(** val decode_mem_reg : int -> instruction option **)

let decode_mem_reg ins =
  let op24_21 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0)))))))))))))))))))))
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))
  in
  let op20 =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0)))))))))))))))))))) (Stdlib.succ 0)
  in
  if Int.eq op24_21
       (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
  then let rn =
         int2ireg
           (decode_arm32 ins (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))))))
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))
       in
       let rt =
         int2ireg
           (decode_arm32 ins (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0)))))))))))) (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0)))))
       in
       let rm =
         int2ireg
           (decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0)))))
       in
       (match rn with
        | Some rn0 ->
          (match rt with
           | Some rt0 ->
             (match rm with
              | Some rm0 ->
                if Int.eq op20 Int.zero
                then Some (Pstr (rt0, rn0, (SOreg rm0)))
                else Some (Pldr (rt0, rn0, (SOreg rm0)))
              | None -> None)
           | None -> None)
        | None -> None)
  else if Int.eq op24_21
            (Int.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1))))
       then if Int.eq op20 Int.one
            then let rd =
                   decode_arm32 ins (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ 0))))))))))))))))
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ 0))))
                 in
                 let op15_12 =
                   decode_arm32 ins (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ 0)))))))))))) (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))
                 in
                 let rm =
                   decode_arm32 ins (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ
                     0)))))))) (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ 0))))
                 in
                 let op7_4 =
                   decode_arm32 ins (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ 0)))) (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))
                 in
                 let rn =
                   decode_arm32 ins 0 (Stdlib.succ (Stdlib.succ
                     (Stdlib.succ (Stdlib.succ 0))))
                 in
                 if (&&)
                      ((&&)
                        ((&&)
                          ((&&)
                            (Int.eq op15_12
                              (Int.repr ((fun p->1+2*p) ((fun p->1+2*p)
                                ((fun p->1+2*p) 1))))) (Int.eq op7_4 Int.one))
                          (Int.eq rd Int.zero)) (Int.eq rm Int.zero))
                      (Int.eq rn Int.one)
                 then Some Pudiv
                 else None
            else None
       else None

(** val decode : int -> instruction option **)

let decode ins =
  let op =
    decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0)))))))))))))))))))))))))
      (Stdlib.succ (Stdlib.succ (Stdlib.succ 0)))
  in
  if Int.eq op Int.zero
  then let op31_28 =
         decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ 0)))))))))))))))))))))))))))) (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))
       in
       let op20 =
         decode_arm32 ins (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ (Stdlib.succ
           (Stdlib.succ (Stdlib.succ 0))))))))))))))))))))
           (Stdlib.succ 0)
       in
       if (&&) (Int.eq op31_28 Int.zero) (Int.eq op20 Int.zero)
       then decode_alu_reg ins
       else if (&&)
                 (Int.eq op31_28
                   (Int.repr ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                     1))))) (Int.eq op20 Int.one)
            then decode_cmp_reg ins
            else None
  else if Int.eq op Int.one
       then let op31_28 =
              decode_arm32 ins (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ
                0)))))))))))))))))))))))))))) (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))
            in
            let op20 =
              decode_arm32 ins (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                (Stdlib.succ (Stdlib.succ (Stdlib.succ
                0)))))))))))))))))))) (Stdlib.succ 0)
            in
            if (&&) (Int.eq op31_28 Int.zero) (Int.eq op20 Int.zero)
            then decode_alu_imm ins
            else if (&&)
                      (Int.eq op31_28
                        (Int.repr ((fun p->2*p) ((fun p->1+2*p)
                          ((fun p->1+2*p) 1))))) (Int.eq op20 Int.one)
                 then decode_cmp_imm ins
                 else None
       else if Int.eq op (Int.repr ((fun p->2*p) 1))
            then decode_mem_imm ins
            else if Int.eq op (Int.repr ((fun p->1+2*p) 1))
                 then decode_mem_reg ins
                 else if Int.eq op
                           (Int.repr ((fun p->1+2*p) ((fun p->2*p) 1)))
                      then let op24 =
                             decode_arm32 ins (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ 0))))))))))))))))))))))))
                               (Stdlib.succ 0)
                           in
                           let imm24 =
                             decode_arm32 ins 0 (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ (Stdlib.succ
                               (Stdlib.succ 0))))))))))))))))))))))))
                           in
                           if Int.eq op24 Int.zero
                           then Some (Pb (Ptrofs.of_ints imm24))
                           else None
                      else None

module List64AsArray =
 struct
  type t = int list

  (** val index : t -> int -> int **)

  let index l idx =
    match nth_error l (Z.to_nat (Int.unsigned idx)) with
    | Some i -> i
    | None -> Int64.zero

  (** val assign' : t -> int -> int -> t option **)

  let rec assign' l cur v =
    match l with
    | [] -> None
    | hd :: tl ->
      ((fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ -> Some (v :: tl))
         (fun n0 ->
         match assign' tl n0 v with
         | Some nl -> Some (hd :: nl)
         | None -> None)
         cur)

  (** val assign : t -> int -> int -> t **)

  let assign l cur v =
    match assign' l cur v with
    | Some nl -> nl
    | None -> []
 end

type bpf_flag =
| BPF_SUCC_RETURN
| BPF_OK
| BPF_ILLEGAL_INSTRUCTION
| BPF_ILLEGAL_MEM
| BPF_ILLEGAL_JUMP
| BPF_ILLEGAL_CALL
| BPF_ILLEGAL_LEN
| BPF_ILLEGAL_REGISTER
| BPF_NO_RETURN
| BPF_OUT_OF_BRANCHES
| BPF_ILLEGAL_DIV
| BPF_ILLEGAL_SHIFT
| BPF_ILLEGAL_ALU

(** val z_of_flag : bpf_flag -> int **)

let z_of_flag = function
| BPF_SUCC_RETURN -> 1
| BPF_OK -> 0
| BPF_ILLEGAL_INSTRUCTION -> ((fun p->2*p) 1)
| BPF_ILLEGAL_MEM -> ((fun p->1+2*p) 1)
| BPF_ILLEGAL_JUMP -> ((fun p->2*p) ((fun p->2*p) 1))
| BPF_ILLEGAL_CALL -> ((fun p->1+2*p) ((fun p->2*p) 1))
| BPF_ILLEGAL_LEN -> ((fun p->2*p) ((fun p->1+2*p) 1))
| BPF_ILLEGAL_REGISTER -> ((fun p->1+2*p) ((fun p->1+2*p) 1))
| BPF_NO_RETURN -> ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
| BPF_OUT_OF_BRANCHES -> ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1)))
| BPF_ILLEGAL_DIV -> ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))
| BPF_ILLEGAL_SHIFT -> ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))
| BPF_ILLEGAL_ALU -> ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1)))

(** val int_of_flag : bpf_flag -> int **)

let int_of_flag f =
  Int.repr (z_of_flag f)

(** val int64_to_sint32 : int -> int **)

let int64_to_sint32 x =
  Int.repr (Int64.unsigned x)

type reg =
| R0
| R1
| R2
| R3
| R4
| R5
| R6
| R7
| R8
| R9
| R10

(** val reg_eqb : reg -> reg -> bool **)

let reg_eqb r0 r1 =
  match r0 with
  | R0 -> (match r1 with
           | R0 -> true
           | _ -> false)
  | R1 -> (match r1 with
           | R1 -> true
           | _ -> false)
  | R2 -> (match r1 with
           | R2 -> true
           | _ -> false)
  | R3 -> (match r1 with
           | R3 -> true
           | _ -> false)
  | R4 -> (match r1 with
           | R4 -> true
           | _ -> false)
  | R5 -> (match r1 with
           | R5 -> true
           | _ -> false)
  | R6 -> (match r1 with
           | R6 -> true
           | _ -> false)
  | R7 -> (match r1 with
           | R7 -> true
           | _ -> false)
  | R8 -> (match r1 with
           | R8 -> true
           | _ -> false)
  | R9 -> (match r1 with
           | R9 -> true
           | _ -> false)
  | R10 -> (match r1 with
            | R10 -> true
            | _ -> false)

(** val id_of_reg : reg -> int **)

let id_of_reg = function
| R0 -> 0
| R1 -> 1
| R2 -> ((fun p->2*p) 1)
| R3 -> ((fun p->1+2*p) 1)
| R4 -> ((fun p->2*p) ((fun p->2*p) 1))
| R5 -> ((fun p->1+2*p) ((fun p->2*p) 1))
| R6 -> ((fun p->2*p) ((fun p->1+2*p) 1))
| R7 -> ((fun p->1+2*p) ((fun p->1+2*p) 1))
| R8 -> ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
| R9 -> ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1)))
| R10 -> ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))

(** val reg2nat : reg -> int **)

let reg2nat = function
| R0 -> 0
| R1 -> Stdlib.succ 0
| R2 -> Stdlib.succ (Stdlib.succ 0)
| R3 -> Stdlib.succ (Stdlib.succ (Stdlib.succ 0))
| R4 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ 0)))
| R5 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ 0))))
| R6 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ 0)))))
| R7 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))
| R8 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0)))))))
| R9 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ 0))))))))
| R10 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ 0)))))))))

(** val z_to_reg : int -> reg option **)

let z_to_reg z0 =
  if Z.eqb z0 0
  then Some R0
  else if Z.eqb z0 1
       then Some R1
       else if Z.eqb z0 ((fun p->2*p) 1)
            then Some R2
            else if Z.eqb z0 ((fun p->1+2*p) 1)
                 then Some R3
                 else if Z.eqb z0 ((fun p->2*p) ((fun p->2*p) 1))
                      then Some R4
                      else if Z.eqb z0 ((fun p->1+2*p) ((fun p->2*p) 1))
                           then Some R5
                           else if Z.eqb z0 ((fun p->2*p) ((fun p->1+2*p) 1))
                                then Some R6
                                else if Z.eqb z0 ((fun p->1+2*p)
                                          ((fun p->1+2*p) 1))
                                     then Some R7
                                     else if Z.eqb z0 ((fun p->2*p)
                                               ((fun p->2*p) ((fun p->2*p)
                                               1)))
                                          then Some R8
                                          else if Z.eqb z0 ((fun p->1+2*p)
                                                    ((fun p->2*p)
                                                    ((fun p->2*p) 1)))
                                               then Some R9
                                               else if Z.eqb z0 ((fun p->2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->2*p) 1)))
                                                    then Some R10
                                                    else None

(** val get_dst : int -> int **)

let get_dst i =
  Int64.unsigned
    (Int64.shru
      (Int64.coq_and i
        (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          1)))))))))))))
      (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))))

(** val get_src : int -> int **)

let get_src i =
  Int64.unsigned
    (Int64.shru
      (Int64.coq_and i
        (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          1)))))))))))))))))
      (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1)))))

(** val get_opcode : int -> int **)

let get_opcode ins =
  Z.to_nat
    (Int64.unsigned
      (Int64.coq_and ins
        (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
          1))))))))))

(** val get_offset : int -> int **)

let get_offset i =
  Int.sign_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))
    (Int.repr
      (Int64.unsigned
        (Int64.shru
          (Int64.shl i
            (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
              ((fun p->2*p) ((fun p->2*p) 1)))))))
          (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
            ((fun p->1+2*p) 1)))))))))

(** val get_immediate : int -> int **)

let get_immediate i1 =
  int64_to_sint32
    (Int64.shru i1
      (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
        ((fun p->2*p) 1)))))))

(** val encode_bpf64 : int -> int -> int -> int **)

let encode_bpf64 v ins from =
  Int64.coq_or (Int64.shl v (Int64.repr (Z.of_nat from))) ins

module Raw =
 struct
  (** val of_pos : int -> string -> string **)

  let rec of_pos p rest =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p1 ->
        (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('f', rest)))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('7', rest)))
            (fun _ ->
            (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

            ('f', rest))
            p2)
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('b', rest)))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('3', rest)))
            (fun _ ->
            (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

            ('b', rest))
            p2)
          (fun _ ->
          (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

          ('7', rest))
          p1)
        (fun p1 ->
        (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('d', rest)))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('5', rest)))
            (fun _ ->
            (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

            ('d', rest))
            p2)
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('9', rest)))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('1', rest)))
            (fun _ ->
            (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

            ('9', rest))
            p2)
          (fun _ ->
          (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

          ('5', rest))
          p1)
        (fun _ ->
        (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

        ('3', rest))
        p0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun p1 ->
        (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('e', rest)))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('6', rest)))
            (fun _ ->
            (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

            ('e', rest))
            p2)
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('a', rest)))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('2', rest)))
            (fun _ ->
            (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

            ('a', rest))
            p2)
          (fun _ ->
          (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

          ('6', rest))
          p1)
        (fun p1 ->
        (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('c', rest)))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('4', rest)))
            (fun _ ->
            (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

            ('c', rest))
            p2)
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('8', rest)))
            (fun p' ->
            of_pos p'
              ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

              ('0', rest)))
            (fun _ ->
            (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

            ('8', rest))
            p2)
          (fun _ ->
          (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

          ('4', rest))
          p1)
        (fun _ ->
        (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

        ('2', rest))
        p0)
      (fun _ ->
      (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

      ('1', rest))
      p
 end

(** val of_pos0 : int -> string **)

let of_pos0 p =
  (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

    ('0',
    ((* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

    ('x', (Raw.of_pos p ""))))

(** val of_N0 : int -> string **)

let of_N0 n0 =
  (fun f0 fp n -> if n=0 then f0 () else fp n)
    (fun _ -> "0x0")
    (fun p -> of_pos0 p)
    n0

(** val of_Z : int -> string **)

let of_Z z0 =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> "0x0")
    (fun p -> of_pos0 p)
    (fun p ->
    (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

    ('-', (of_pos0 p)))
    z0

(** val of_nat0 : int -> string **)

let of_nat0 n0 =
  of_N0 (N.of_nat n0)

(** val string_of_int_unsigned : int -> string **)

let string_of_int_unsigned i =
  of_Z (Int.unsigned i)

module List32 =
 struct
  type t = int list

  (** val index : t -> int -> int **)

  let index l idx =
    match nth_error l (Z.to_nat (Int.unsigned idx)) with
    | Some i -> i
    | None -> Int.zero

  (** val assign' : t -> int -> int -> t option **)

  let rec assign' l cur v =
    match l with
    | [] -> None
    | hd :: tl ->
      ((fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ -> Some (v :: tl))
         (fun n0 ->
         match assign' tl n0 v with
         | Some nl -> Some (hd :: nl)
         | None -> None)
         cur)

  (** val assign : t -> int -> int -> t **)

  let assign l cur v =
  let _ = print_endline ("loc_"^ (string_of_int cur) ^ ": " ^ (match decode v with | Some ins -> string_of_instruction ins | None -> "assgin decode error")) in
    match assign' l cur v with
    | Some nl -> nl
    | None -> []

  (** val create_int_list : int -> t **)

  let rec create_int_list l =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> [])
      (fun n0 -> Int.zero :: (create_int_list n0))
      l
 end

module ListNat =
 struct
  type t = int list

  (** val index : t -> int -> int **)

  let index l idx =
    match nth_error l idx with
    | Some i -> i
    | None -> 0

  (** val assign' : t -> int -> int -> t option **)

  let rec assign' l cur v =
    match l with
    | [] -> None
    | hd :: tl ->
      ((fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ -> Some (v :: tl))
         (fun n0 ->
         match assign' tl n0 v with
         | Some nl -> Some (hd :: nl)
         | None -> None)
         cur)

  (** val assign : t -> int -> int -> t **)

  let assign l cur v =
    match assign' l cur v with
    | Some nl -> nl
    | None -> []

  (** val is_exists : t -> int -> int -> bool **)

  let rec is_exists l cur v =
    match l with
    | [] -> false
    | hd :: tl ->
      ((fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ -> false)
         (fun n0 -> if (=) hd v then true else is_exists tl n0 v)
         cur)

  (** val create_int_list : int -> t **)

  let rec create_int_list l =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> [])
      (fun n0 -> 0 :: (create_int_list n0))
      l
 end

type memory_region = { start_addr : val0; block_size : val0;
                       block_perm : permission; block_ptr : val0 }

module Memory_regions =
 struct
  type t = memory_region list
 end

type myMemRegionsType = Memory_regions.t

type loadStorePerm =
| NonPerm
| LoadPerm
| StorePerm
| LoadAndStore

let string_of_loadStorePerm ls =
  match ls with
  | NonPerm -> "NonPerm"
  | LoadPerm -> "LoadPerm"
  | StorePerm -> "StorePerm"
  | LoadAndStore -> "LoadAndStore"

(** val loadStorePerm_eq : loadStorePerm -> loadStorePerm -> bool **)

let loadStorePerm_eq x y =
  match x with
  | NonPerm -> (match y with
                | NonPerm -> true
                | _ -> false)
  | LoadPerm -> (match y with
                 | LoadPerm -> true
                 | _ -> false)
  | StorePerm -> (match y with
                  | StorePerm -> true
                  | _ -> false)
  | LoadAndStore -> (match y with
                     | LoadAndStore -> true
                     | _ -> false)

(** val loadStorePerm_eqb : loadStorePerm -> loadStorePerm -> bool **)

let loadStorePerm_eqb x y =
  if loadStorePerm_eq x y then true else false

(** val upd_LoadStorePerm :
    loadStorePerm -> loadStorePerm -> loadStorePerm option **)

let upd_LoadStorePerm history = function
| LoadPerm -> (match history with
               | NonPerm -> Some LoadPerm
               | x -> Some x)
| StorePerm ->
  (match history with
   | NonPerm -> Some StorePerm
   | LoadPerm -> Some LoadAndStore
   | x -> Some x)
| _ -> None

type loadStoreRegs = { is_R0 : loadStorePerm; is_R1 : loadStorePerm;
                       is_R2 : loadStorePerm; is_R3 : loadStorePerm;
                       is_R4 : loadStorePerm; is_R5 : loadStorePerm;
                       is_R6 : loadStorePerm; is_R7 : loadStorePerm;
                       is_R8 : loadStorePerm; is_R9 : loadStorePerm;
                       is_R10 : loadStorePerm }

let print_load_store_regs ls =
  let _ = print_string ("is_R0= " ^ (string_of_loadStorePerm ls.is_R0) ^ "; ") in
  let _ = print_string ("is_R1= " ^ (string_of_loadStorePerm ls.is_R1) ^ "; ") in
  let _ = print_string ("is_R2= " ^ (string_of_loadStorePerm ls.is_R2) ^ "; ") in
  let _ = print_string ("is_R3= " ^ (string_of_loadStorePerm ls.is_R3) ^ "; ") in
  let _ = print_string ("is_R4= " ^ (string_of_loadStorePerm ls.is_R4) ^ "; ") in
  let _ = print_string ("is_R5= " ^ (string_of_loadStorePerm ls.is_R5) ^ ";\n") in
  let _ = print_string ("is_R6= " ^ (string_of_loadStorePerm ls.is_R6) ^ "; ") in
  let _ = print_string ("is_R7= " ^ (string_of_loadStorePerm ls.is_R7) ^ "; ") in
  let _ = print_string ("is_R8= " ^ (string_of_loadStorePerm ls.is_R8) ^ "; ") in
  let _ = print_string ("is_R9= " ^ (string_of_loadStorePerm ls.is_R9) ^ "; ") in
    print_string ("is_R10= " ^ (string_of_loadStorePerm ls.is_R10) ^ "\n")

(** val init_LoadStoreRegs : loadStoreRegs **)

let init_LoadStoreRegs =
  { is_R0 = NonPerm; is_R1 = NonPerm; is_R2 = NonPerm; is_R3 = NonPerm;
    is_R4 = NonPerm; is_R5 = NonPerm; is_R6 = NonPerm; is_R7 = NonPerm;
    is_R8 = NonPerm; is_R9 = NonPerm; is_R10 = NonPerm }

(** val eval_LoadStoreRegs : loadStoreRegs -> reg -> loadStorePerm **)

let eval_LoadStoreRegs ls = function
| R0 -> ls.is_R0
| R1 -> ls.is_R1
| R2 -> ls.is_R2
| R3 -> ls.is_R3
| R4 -> ls.is_R4
| R5 -> ls.is_R5
| R6 -> ls.is_R6
| R7 -> ls.is_R7
| R8 -> ls.is_R8
| R9 -> ls.is_R9
| R10 -> ls.is_R10

(** val upd_LoadStoreRegs :
    loadStoreRegs -> reg -> loadStorePerm -> loadStoreRegs option **)

let upd_LoadStoreRegs ls r cur =
  let history = eval_LoadStoreRegs ls r in
  let new0 = upd_LoadStorePerm history cur in
  (match new0 with
   | Some p ->
     Some
       (match r with
        | R0 ->
          { is_R0 = p; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 = ls.is_R3;
            is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R1 ->
          { is_R0 = ls.is_R0; is_R1 = p; is_R2 = ls.is_R2; is_R3 = ls.is_R3;
            is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R2 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = p; is_R3 = ls.is_R3;
            is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R3 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 = p;
            is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R4 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = p; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R5 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = p; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R6 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = p; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R7 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6;
            is_R7 = p; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 =
            ls.is_R10 }
        | R8 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R7;
            is_R7 = ls.is_R7; is_R8 = p; is_R9 = ls.is_R9; is_R10 =
            ls.is_R10 }
        | R9 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R7;
            is_R7 = ls.is_R7; is_R8 = ls.is_R8; is_R9 = p; is_R10 =
            ls.is_R10 }
        | R10 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R7;
            is_R7 = ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = p })
   | None -> None)

(** val is_load_reg : reg -> loadStoreRegs -> bool **)

let is_load_reg r ls =
  let perm = eval_LoadStoreRegs ls r in
  if loadStorePerm_eqb perm LoadPerm
  then true
  else loadStorePerm_eqb perm LoadAndStore

(** val is_store_reg : reg -> loadStoreRegs -> bool **)

let is_store_reg r ls =
  let perm = eval_LoadStoreRegs ls r in
  if loadStorePerm_eqb perm StorePerm
  then true
  else loadStorePerm_eqb perm LoadAndStore

(** val is_non_reg : reg -> loadStoreRegs -> bool **)

let is_non_reg r ls =
  let perm = eval_LoadStoreRegs ls r in loadStorePerm_eqb perm NonPerm

type jittedarm32 = { is_ip : bool; load_store_regs : loadStoreRegs;
                     offset : int; arm32_len : int; arm32 : List32.t }

(** val jITTED_LIST_MAX_LENGTH : int **)

let jITTED_LIST_MAX_LENGTH =
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
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val init_jittedarm32 : jittedarm32 **)

let init_jittedarm32 =
  { is_ip = false; load_store_regs = init_LoadStoreRegs; offset = 0;
    arm32_len = 0; arm32 = (List32.create_int_list jITTED_LIST_MAX_LENGTH) }

(** val upd_ip_jittedarm32 : bool -> jittedarm32 -> jittedarm32 **)

let upd_ip_jittedarm32 ip st =
  { is_ip = ip; load_store_regs = st.load_store_regs; offset = st.offset;
    arm32_len = st.arm32_len; arm32 = st.arm32 }

(** val add_ins_jittedarm32 : int -> jittedarm32 -> jittedarm32 **)

let add_ins_jittedarm32 ins st =
  { is_ip = st.is_ip; load_store_regs = st.load_store_regs; offset =
    (Stdlib.succ st.offset); arm32_len = (Stdlib.succ st.arm32_len);
    arm32 = (List32.assign st.arm32 st.arm32_len ins) }

(** val upd_load_store_regs_jittedarm32 :
    loadStoreRegs -> jittedarm32 -> jittedarm32 **)

let upd_load_store_regs_jittedarm32 lsr0 st =
  { is_ip = st.is_ip; load_store_regs = lsr0; offset = st.offset; arm32_len =
    st.arm32_len; arm32 = st.arm32 }

type entry_point = { entry_len : int; entry_ps : ListNat.t }

(** val init_entry_point : entry_point **)

let init_entry_point =
  { entry_len = 0; entry_ps =
    (ListNat.create_int_list jITTED_LIST_MAX_LENGTH) }

(** val add_new_entry_point : int -> entry_point -> entry_point **)

let add_new_entry_point v epl =
  { entry_len = (Stdlib.succ epl.entry_len); entry_ps =
    (ListNat.assign epl.entry_ps epl.entry_len v) }

type jit_state = { pc_loc : int; flag : val0; regs_st : val0; mrs_num : 
                   int; bpf_mrs : myMemRegionsType; ins_len : int;
                   ibpf : List64AsArray.t; stack_len : int; stack_ofs : 
                   int; jitted_len : int; jitted_list : List32.t;
                   jit_mem : Mem.mem }

(** val init_fp_push_stack : jit_state -> jit_state **)

let init_fp_push_stack st =
  { pc_loc = st.pc_loc; flag = st.flag; regs_st = st.regs_st; mrs_num =
    st.mrs_num; bpf_mrs = st.bpf_mrs; ins_len = st.ins_len; ibpf = st.ibpf;
    stack_len =
    (Nat.add st.stack_len (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0))))); stack_ofs =
    (Nat.add st.stack_ofs (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0))))); jitted_len = st.jitted_len; jitted_list =
    st.jitted_list; jit_mem = st.jit_mem }

(** val add_ins_to_push_stack : jit_state -> int -> jit_state **)

let add_ins_to_push_stack st ins =
  { pc_loc = st.pc_loc; flag = st.flag; regs_st = st.regs_st; mrs_num =
    st.mrs_num; bpf_mrs = st.bpf_mrs; ins_len = st.ins_len; ibpf = st.ibpf;
    stack_len =
    (Nat.add st.stack_len (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0))))); stack_ofs =
    (Nat.add st.stack_ofs (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0))))); jitted_len = (Stdlib.succ st.jitted_len);
    jitted_list = (List32.assign st.jitted_list st.jitted_len ins); jit_mem =
    st.jit_mem }

(** val add_ins_to_pop_stack : jit_state -> int -> jit_state **)

let add_ins_to_pop_stack st ins =
  { pc_loc = st.pc_loc; flag = st.flag; regs_st = st.regs_st; mrs_num =
    st.mrs_num; bpf_mrs = st.bpf_mrs; ins_len = st.ins_len; ibpf = st.ibpf;
    stack_len = st.stack_len; stack_ofs =
    (Nat.sub st.stack_ofs (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0))))); jitted_len = (Stdlib.succ st.jitted_len);
    jitted_list = (List32.assign st.jitted_list st.jitted_len ins); jit_mem =
    st.jit_mem }

(** val upd_jitted_list : jit_state -> int -> jit_state **)

let upd_jitted_list st ins =
  { pc_loc = st.pc_loc; flag = st.flag; regs_st = st.regs_st; mrs_num =
    st.mrs_num; bpf_mrs = st.bpf_mrs; ins_len = st.ins_len; ibpf = st.ibpf;
    stack_len = st.stack_len; stack_ofs = st.stack_ofs; jitted_len =
    (Stdlib.succ st.jitted_len); jitted_list =
    (List32.assign st.jitted_list st.jitted_len ins); jit_mem = st.jit_mem }

(** val upd_ibpf : jit_state -> int -> int -> jit_state **)

let upd_ibpf st ins ep =
  { pc_loc = st.pc_loc; flag = st.flag; regs_st = st.regs_st; mrs_num =
    st.mrs_num; bpf_mrs = st.bpf_mrs; ins_len = st.ins_len; ibpf =
    (List64AsArray.assign st.ibpf ep ins); stack_len = st.stack_len;
    stack_ofs = st.stack_ofs; jitted_len = st.jitted_len; jitted_list =
    st.jitted_list; jit_mem = st.jit_mem }

(** val encode_arm32 : int -> int -> int -> int **)

let encode_arm32 v ins from =
  Int.coq_or (Int.shl v (Int.repr (Z.of_nat from))) ins

(** val ireg_eqb : ireg -> ireg -> bool **)

let ireg_eqb r0 r1 =
  match r0 with
  | IR0 -> (match r1 with
            | IR0 -> true
            | _ -> false)
  | IR1 -> (match r1 with
            | IR1 -> true
            | _ -> false)
  | IR2 -> (match r1 with
            | IR2 -> true
            | _ -> false)
  | IR3 -> (match r1 with
            | IR3 -> true
            | _ -> false)
  | IR4 -> (match r1 with
            | IR4 -> true
            | _ -> false)
  | IR5 -> (match r1 with
            | IR5 -> true
            | _ -> false)
  | IR6 -> (match r1 with
            | IR6 -> true
            | _ -> false)
  | IR7 -> (match r1 with
            | IR7 -> true
            | _ -> false)
  | IR8 -> (match r1 with
            | IR8 -> true
            | _ -> false)
  | IR9 -> (match r1 with
            | IR9 -> true
            | _ -> false)
  | IR10 -> (match r1 with
             | IR10 -> true
             | _ -> false)
  | IR11 -> (match r1 with
             | IR11 -> true
             | _ -> false)
  | IR12 -> (match r1 with
             | IR12 -> true
             | _ -> false)
  | IR13 -> (match r1 with
             | IR13 -> true
             | _ -> false)
  | IR14 -> (match r1 with
             | IR14 -> true
             | _ -> false)

(** val ireg2nat : ireg -> int **)

let ireg2nat = function
| IR0 -> 0
| IR1 -> Stdlib.succ 0
| IR2 -> Stdlib.succ (Stdlib.succ 0)
| IR3 -> Stdlib.succ (Stdlib.succ (Stdlib.succ 0))
| IR4 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ 0)))
| IR5 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ 0))))
| IR6 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ 0)))))
| IR7 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))
| IR8 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0)))))))
| IR9 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ 0))))))))
| IR10 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ 0)))))))))
| IR11 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))
| IR12 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0)))))))))))
| IR13 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ 0))))))))))))
| IR14 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ 0)))))))))))))

(** val int_of_ireg : ireg -> int **)

let int_of_ireg r =
  Int.repr (Z.of_nat (ireg2nat r))

type opcode_alu32_reg =
| BPF_ADD32_REG
| BPF_SUB32_REG
| BPF_MUL32_REG
| BPF_DIV32_REG
| BPF_OR32_REG
| BPF_AND32_REG
| BPF_LSH32_REG
| BPF_RSH32_REG
| BPF_MOD32_REG
| BPF_XOR32_REG
| BPF_MOV32_REG
| BPF_ARSH32_REG
| BPF_ALU32_REG_ILLEGAL_INS

type opcode_alu32_imm =
| BPF_ADD32_IMM
| BPF_SUB32_IMM
| BPF_MUL32_IMM
| BPF_DIV32_IMM
| BPF_OR32_IMM
| BPF_AND32_IMM
| BPF_LSH32_IMM
| BPF_RSH32_IMM
| BPF_MOD32_IMM
| BPF_XOR32_IMM
| BPF_MOV32_IMM
| BPF_ARSH32_IMM
| BPF_ALU32_IMM_ILLEGAL_INS

(** val nat_to_opcode_alu32_reg : int -> opcode_alu32_reg **)

let nat_to_opcode_alu32_reg op =
  if (=) op (Stdlib.succ (Stdlib.succ (Stdlib.succ
       (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
       (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
       (Stdlib.succ 0))))))))))))
  then BPF_ADD32_REG
  else if (=) op (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ 0))))))))))))))))))))))))))))
       then BPF_SUB32_REG
       else if (=) op (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
                 (Stdlib.succ (Stdlib.succ
                 0))))))))))))))))))))))))))))))))))))))))))))
            then BPF_MUL32_REG
            else if (=) op (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
                      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                 then BPF_DIV32_REG
                 else if (=) op (Stdlib.succ (Stdlib.succ
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
                           (Stdlib.succ (Stdlib.succ (Stdlib.succ
                           (Stdlib.succ (Stdlib.succ (Stdlib.succ
                           (Stdlib.succ (Stdlib.succ (Stdlib.succ
                           (Stdlib.succ (Stdlib.succ (Stdlib.succ
                           (Stdlib.succ (Stdlib.succ
                           0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                      then BPF_OR32_REG
                      else if (=) op (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                           then BPF_AND32_REG
                           else if (=) op (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                then BPF_LSH32_REG
                                else if (=) op (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ
                                          0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                     then BPF_RSH32_REG
                                     else if (=) op (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                          then BPF_MOD32_REG
                                          else if (=) op (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                               then BPF_XOR32_REG
                                               else if (=) op
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                    then BPF_MOV32_REG
                                                    else if (=) op
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                         then BPF_ARSH32_REG
                                                         else BPF_ALU32_REG_ILLEGAL_INS

(** val nat_to_opcode_alu32_imm : int -> opcode_alu32_imm **)

let nat_to_opcode_alu32_imm op =
  if (=) op (Stdlib.succ (Stdlib.succ (Stdlib.succ
       (Stdlib.succ 0))))
  then BPF_ADD32_IMM
  else if (=) op (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ (Stdlib.succ
            (Stdlib.succ (Stdlib.succ 0))))))))))))))))))))
       then BPF_SUB32_IMM
       else if (=) op (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
                 0))))))))))))))))))))))))))))))))))))
            then BPF_MUL32_IMM
            else if (=) op (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
                      (Stdlib.succ
                      0))))))))))))))))))))))))))))))))))))))))))))))))))))
                 then BPF_DIV32_IMM
                 else if (=) op (Stdlib.succ (Stdlib.succ
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
                           (Stdlib.succ (Stdlib.succ (Stdlib.succ
                           (Stdlib.succ (Stdlib.succ (Stdlib.succ
                           0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                      then BPF_OR32_IMM
                      else if (=) op (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                (Stdlib.succ (Stdlib.succ
                                0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                           then BPF_AND32_IMM
                           else if (=) op (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     (Stdlib.succ (Stdlib.succ
                                     0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                then BPF_LSH32_IMM
                                else if (=) op (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ (Stdlib.succ
                                          (Stdlib.succ
                                          0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                     then BPF_RSH32_IMM
                                     else if (=) op (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               (Stdlib.succ
                                               0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                          then BPF_MOD32_IMM
                                          else if (=) op (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    (Stdlib.succ
                                                    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                               then BPF_XOR32_IMM
                                               else if (=) op
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         (Stdlib.succ
                                                         0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                    then BPF_MOV32_IMM
                                                    else if (=) op
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              (Stdlib.succ
                                                              0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                         then BPF_ARSH32_IMM
                                                         else BPF_ALU32_IMM_ILLEGAL_INS

type opcode_alu32 =
| ALU32_REG of opcode_alu32_reg
| ALU32_IMM of opcode_alu32_imm
| ALU32_ILLEGAL_INS

(** val nat_to_opcode_alu32 : int -> opcode_alu32 **)

let nat_to_opcode_alu32 op =
  let opc =
    Nat.coq_land op (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0)))))))
  in
  if (=) opc (Stdlib.succ (Stdlib.succ (Stdlib.succ
       (Stdlib.succ 0))))
  then if Int.eq Int.zero
            (Int.coq_and (Int.repr (Z.of_nat op))
              (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))))
       then ALU32_IMM (nat_to_opcode_alu32_imm op)
       else ALU32_REG (nat_to_opcode_alu32_reg op)
  else ALU32_ILLEGAL_INS

(** val ins_is_bpf_alu32 : int -> bool **)

let ins_is_bpf_alu32 ins =
  let op = get_opcode ins in
  if (||) ((=) op (Z.to_nat ((fun p->2*p) ((fun p->2*p) 1))))
       ((=) op (Z.to_nat ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1)))))
  then true
  else if (||)
            ((=) op
              (Z.to_nat ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                ((fun p->2*p) 1))))))
            ((=) op
              (Z.to_nat ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                ((fun p->1+2*p) 1))))))
       then true
       else if (||)
                 ((=) op
                   (Z.to_nat ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                     ((fun p->2*p) ((fun p->2*p) 1)))))))
                 ((=) op
                   (Z.to_nat ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                     ((fun p->1+2*p) ((fun p->2*p) 1)))))))
            then true
            else if (||)
                      ((=) op
                        (Z.to_nat ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                          ((fun p->2*p) ((fun p->1+2*p) 1)))))))
                      ((=) op
                        (Z.to_nat ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                          ((fun p->1+2*p) ((fun p->1+2*p) 1)))))))
                 then true
                 else if (||)
                           ((=) op
                             (Z.to_nat ((fun p->2*p) ((fun p->2*p)
                               ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
                               ((fun p->2*p) 1))))))))
                           ((=) op
                             (Z.to_nat ((fun p->2*p) ((fun p->2*p)
                               ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
                               ((fun p->2*p) 1))))))))
                      then true
                      else if (||)
                                ((=) op
                                  (Z.to_nat ((fun p->2*p) ((fun p->2*p)
                                    ((fun p->1+2*p) ((fun p->2*p)
                                    ((fun p->1+2*p) ((fun p->2*p) 1))))))))
                                ((=) op
                                  (Z.to_nat ((fun p->2*p) ((fun p->2*p)
                                    ((fun p->1+2*p) ((fun p->1+2*p)
                                    ((fun p->1+2*p) ((fun p->2*p) 1))))))))
                           then true
                           else if (||)
                                     ((=) op
                                       (Z.to_nat ((fun p->2*p) ((fun p->2*p)
                                         ((fun p->1+2*p) ((fun p->2*p)
                                         ((fun p->2*p) ((fun p->1+2*p)
                                         1))))))))
                                     ((=) op
                                       (Z.to_nat ((fun p->2*p) ((fun p->2*p)
                                         ((fun p->1+2*p) ((fun p->1+2*p)
                                         ((fun p->2*p) ((fun p->1+2*p)
                                         1))))))))
                                then true
                                else if (||)
                                          ((=) op
                                            (Z.to_nat ((fun p->2*p)
                                              ((fun p->2*p) ((fun p->1+2*p)
                                              ((fun p->2*p) ((fun p->1+2*p)
                                              ((fun p->1+2*p) 1))))))))
                                          ((=) op
                                            (Z.to_nat ((fun p->2*p)
                                              ((fun p->2*p) ((fun p->1+2*p)
                                              ((fun p->1+2*p) ((fun p->1+2*p)
                                              ((fun p->1+2*p) 1))))))))
                                     then true
                                     else if (=) op
                                               (Z.to_nat ((fun p->2*p)
                                                 ((fun p->2*p)
                                                 ((fun p->1+2*p)
                                                 ((fun p->2*p) ((fun p->2*p)
                                                 ((fun p->2*p) ((fun p->2*p)
                                                 1))))))))
                                          then true
                                          else if (||)
                                                    ((=) op
                                                      (Z.to_nat ((fun p->2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->2*p)
                                                        1)))))))))
                                                    ((=) op
                                                      (Z.to_nat ((fun p->2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->2*p)
                                                        1)))))))))
                                               then true
                                               else if (||)
                                                         ((=) op
                                                           (Z.to_nat
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             1)))))))))
                                                         ((=) op
                                                           (Z.to_nat
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             1)))))))))
                                                    then true
                                                    else if (||)
                                                              ((=) op
                                                                (Z.to_nat
                                                                  ((fun p->2*p)
                                                                  ((fun p->2*p)
                                                                  ((fun p->1+2*p)
                                                                  ((fun p->2*p)
                                                                  ((fun p->1+2*p)
                                                                  ((fun p->1+2*p)
                                                                  ((fun p->2*p)
                                                                  1)))))))))
                                                              ((=) op
                                                                (Z.to_nat
                                                                  ((fun p->2*p)
                                                                  ((fun p->2*p)
                                                                  ((fun p->1+2*p)
                                                                  ((fun p->1+2*p)
                                                                  ((fun p->1+2*p)
                                                                  ((fun p->1+2*p)
                                                                  ((fun p->2*p)
                                                                  1)))))))))
                                                         then true
                                                         else (||)
                                                                ((=) op
                                                                  (Z.to_nat
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    1)))))))))
                                                                ((=) op
                                                                  (Z.to_nat
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    1)))))))))

(** val ins_is_bpf_jump : int -> bool **)

let ins_is_bpf_jump ins =
  let op = get_opcode ins in
  if (=) op (Z.to_nat ((fun p->1+2*p) ((fun p->2*p) 1)))
  then true
  else if (||)
            ((=) op
              (Z.to_nat ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                ((fun p->2*p) 1))))))
            ((=) op
              (Z.to_nat ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                ((fun p->1+2*p) 1))))))
       then true
       else if (||)
                 ((=) op
                   (Z.to_nat ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                     ((fun p->2*p) ((fun p->2*p) 1)))))))
                 ((=) op
                   (Z.to_nat ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                     ((fun p->1+2*p) ((fun p->2*p) 1)))))))
            then true
            else if (||)
                      ((=) op
                        (Z.to_nat ((fun p->1+2*p) ((fun p->2*p)
                          ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                          1)))))))
                      ((=) op
                        (Z.to_nat ((fun p->1+2*p) ((fun p->2*p)
                          ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                          1)))))))
                 then true
                 else if (||)
                           ((=) op
                             (Z.to_nat ((fun p->1+2*p) ((fun p->2*p)
                               ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
                               ((fun p->2*p) 1))))))))
                           ((=) op
                             (Z.to_nat ((fun p->1+2*p) ((fun p->2*p)
                               ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
                               ((fun p->2*p) 1))))))))
                      then true
                      else if (||)
                                ((=) op
                                  (Z.to_nat ((fun p->1+2*p) ((fun p->2*p)
                                    ((fun p->1+2*p) ((fun p->2*p)
                                    ((fun p->1+2*p) ((fun p->2*p) 1))))))))
                                ((=) op
                                  (Z.to_nat ((fun p->1+2*p) ((fun p->2*p)
                                    ((fun p->1+2*p) ((fun p->1+2*p)
                                    ((fun p->1+2*p) ((fun p->2*p) 1))))))))
                           then true
                           else if (||)
                                     ((=) op
                                       (Z.to_nat ((fun p->1+2*p)
                                         ((fun p->2*p) ((fun p->1+2*p)
                                         ((fun p->2*p) ((fun p->2*p)
                                         ((fun p->1+2*p) 1))))))))
                                     ((=) op
                                       (Z.to_nat ((fun p->1+2*p)
                                         ((fun p->2*p) ((fun p->1+2*p)
                                         ((fun p->1+2*p) ((fun p->2*p)
                                         ((fun p->1+2*p) 1))))))))
                                then true
                                else if (||)
                                          ((=) op
                                            (Z.to_nat ((fun p->1+2*p)
                                              ((fun p->2*p) ((fun p->1+2*p)
                                              ((fun p->2*p) ((fun p->1+2*p)
                                              ((fun p->1+2*p) 1))))))))
                                          ((=) op
                                            (Z.to_nat ((fun p->1+2*p)
                                              ((fun p->2*p) ((fun p->1+2*p)
                                              ((fun p->1+2*p) ((fun p->1+2*p)
                                              ((fun p->1+2*p) 1))))))))
                                     then true
                                     else if (||)
                                               ((=) op
                                                 (Z.to_nat ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p) 1)))))))))
                                               ((=) op
                                                 (Z.to_nat ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p) 1)))))))))
                                          then true
                                          else if (||)
                                                    ((=) op
                                                      (Z.to_nat
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        1)))))))))
                                                    ((=) op
                                                      (Z.to_nat
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        1)))))))))
                                               then true
                                               else if (||)
                                                         ((=) op
                                                           (Z.to_nat
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             1)))))))))
                                                         ((=) op
                                                           (Z.to_nat
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             1)))))))))
                                                    then true
                                                    else (||)
                                                           ((=) op
                                                             (Z.to_nat
                                                               ((fun p->1+2*p)
                                                               ((fun p->2*p)
                                                               ((fun p->1+2*p)
                                                               ((fun p->2*p)
                                                               ((fun p->1+2*p)
                                                               ((fun p->2*p)
                                                               ((fun p->1+2*p)
                                                               1)))))))))
                                                           ((=) op
                                                             (Z.to_nat
                                                               ((fun p->1+2*p)
                                                               ((fun p->2*p)
                                                               ((fun p->1+2*p)
                                                               ((fun p->1+2*p)
                                                               ((fun p->1+2*p)
                                                               ((fun p->2*p)
                                                               ((fun p->1+2*p)
                                                               1)))))))))

(** val cOND_NE : int **)

let cOND_NE =
  Int.repr 1

(** val cOND_GE : int **)

let cOND_GE =
  Int.repr ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))

(** val aDD_R_OP : int **)

let aDD_R_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    1)))))))))))))))))))))))

(** val aDD_I_OP : int **)

let aDD_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->2*p) 1)))))))))))))))))))))))))

(** val aND_R_OP : int **)

let aND_R_OP =
  Int.repr 0

(** val aSR_R_OP : int **)

let aSR_R_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    1))))))))))))))))))))))))

(** val aSR_I_OP : int **)

let aSR_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    1))))))))))))))))))))))))

(** val b_OP : int **)

let b_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))))))))))))))))))))))))))

(** val cMP_I_OP : int **)

let cMP_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) 1)))))))))))))))))))))))))))))))

(** val eOR_R_OP : int **)

let eOR_R_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) 1)))))))))))))))))))))

(** val eOR_I_OP : int **)

let eOR_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) 1)))))))))))))))))))))))))

(** val lSL_R_OP : int **)

let lSL_R_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    1))))))))))))))))))))))))

(** val lSL_I_OP : int **)

let lSL_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    1))))))))))))))))))))))))

(** val lSR_R_OP : int **)

let lSR_R_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    1))))))))))))))))))))))))

(** val lSR_I_OP : int **)

let lSR_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    1))))))))))))))))))))))))

(** val mOV_I_OP : int **)

let mOV_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) 1)))))))))))))))))))))))))

(** val mOVW_OP : int **)

let mOVW_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) 1)))))))))))))))))))))))))

(** val mOVT_OP : int **)

let mOVT_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
    ((fun p->1+2*p) 1)))))))))))))))))))))))))

(** val mOV_R_OP : int **)

let mOV_R_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    1))))))))))))))))))))))))

(** val mUL_OP : int **)

let mUL_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1)))))))

(** val oRR_R_OP : int **)

let oRR_R_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    1))))))))))))))))))))))))

(** val oRR_I_OP : int **)

let oRR_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) 1)))))))))))))))))))))))))

(** val sUB_R_OP : int **)

let sUB_R_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))))))))))))))))))

(** val sUB_I_OP : int **)

let sUB_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
    ((fun p->2*p) 1)))))))))))))))))))))))))

(** val uDIV_OP : int **)

let uDIV_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) 1))))))))))))))))))))))))))

(** val bX_OP : int **)

let bX_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1))))))))))))))))))))))))

(** val lDR_I_OP : int **)

let lDR_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->2*p) 1))))))))))))))))))))))))))

(** val sTR_I_OP : int **)

let sTR_I_OP =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->2*p) 1))))))))))))))))))))))))))

(** val rBPF_State_REG : ireg **)

let rBPF_State_REG =
  IR11

(** val jit_alu32_pre : jit_state -> jit_state **)

let jit_alu32_pre st =
  let st0 = init_fp_push_stack st in
  let ins_src = encode_arm32 Int.one mOV_R_OP 0 in
  let ins_dst =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat rBPF_State_REG))) ins_src
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))))))))))
  in
  upd_jitted_list st0 ins_dst

(** val jit_alu32_save_register : int -> int -> int **)

let jit_alu32_save_register r ofs =
  let ins_imm12 = encode_arm32 (Int.repr (Z.of_nat ofs)) sTR_I_OP 0 in
  let ins_rt =
    encode_arm32 r ins_imm12 (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))
  in
  encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR13))) ins_rt (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))

(** val jit_alu32_arm32_upd_save :
    reg -> jit_state -> loadStoreRegs -> jit_state **)

let jit_alu32_arm32_upd_save r st ls =
  if is_non_reg r ls
  then st
  else let ins =
         jit_alu32_save_register (Int.repr (Z.of_nat (reg2nat r)))
           st.stack_ofs
       in
       add_ins_to_push_stack st ins

(** val jit_alu32_arm32_save_IP : jit_state -> jit_state **)

let jit_alu32_arm32_save_IP st =
  let ins =
    jit_alu32_save_register (Int.repr (Z.of_nat (ireg2nat IR12))) st.stack_ofs
  in
  add_ins_to_push_stack st ins

(** val jit_alu32_arm32_save :
    jit_state -> loadStoreRegs -> bool -> jit_state **)

let jit_alu32_arm32_save st ls f =
  let r1_st = jit_alu32_arm32_upd_save R1 st ls in
  let r2_st = jit_alu32_arm32_upd_save R2 r1_st ls in
  let r3_st = jit_alu32_arm32_upd_save R3 r2_st ls in
  let r4_st = jit_alu32_arm32_upd_save R4 r3_st ls in
  let r5_st = jit_alu32_arm32_upd_save R5 r4_st ls in
  let r6_st = jit_alu32_arm32_upd_save R6 r5_st ls in
  let r7_st = jit_alu32_arm32_upd_save R7 r6_st ls in
  let r8_st = jit_alu32_arm32_upd_save R8 r7_st ls in
  let r9_st = jit_alu32_arm32_upd_save R9 r8_st ls in
  let r10_st = jit_alu32_arm32_upd_save R10 r9_st ls in
  if f then jit_alu32_arm32_save_IP r10_st else r10_st

(** val bPF_R0_OFS : int **)

let bPF_R0_OFS =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))

(** val jit_alu32_load_register : int -> int **)

let jit_alu32_load_register r =
  let ins_imm12 =
    encode_arm32
      (Int.add bPF_R0_OFS
        (Int.mul r (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))
      lDR_I_OP 0
  in
  let ins_rt =
    encode_arm32 r ins_imm12 (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))
  in
  encode_arm32 (Int.repr (Z.of_nat (ireg2nat rBPF_State_REG))) ins_rt
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0))))))))))))))))

(** val jit_alu32_arm32_upd_load :
    reg -> jit_state -> loadStoreRegs -> jit_state **)

let jit_alu32_arm32_upd_load r st ls =
  if is_load_reg r ls
  then let ins = jit_alu32_load_register (Int.repr (Z.of_nat (reg2nat r))) in
       upd_jitted_list st ins
  else st

(** val jit_alu32_arm32_load : jit_state -> loadStoreRegs -> jit_state **)

let jit_alu32_arm32_load st ls =
  let r0_st = jit_alu32_arm32_upd_load R0 st ls in
  let r1_st = jit_alu32_arm32_upd_load R1 r0_st ls in
  let r2_st = jit_alu32_arm32_upd_load R2 r1_st ls in
  let r3_st = jit_alu32_arm32_upd_load R3 r2_st ls in
  let r4_st = jit_alu32_arm32_upd_load R4 r3_st ls in
  let r5_st = jit_alu32_arm32_upd_load R5 r4_st ls in
  let r6_st = jit_alu32_arm32_upd_load R6 r5_st ls in
  let r7_st = jit_alu32_arm32_upd_load R7 r6_st ls in
  let r8_st = jit_alu32_arm32_upd_load R8 r7_st ls in
  let r9_st = jit_alu32_arm32_upd_load R9 r8_st ls in
  jit_alu32_arm32_upd_load R10 r9_st ls

(** val bAL_0 : int **)

let bAL_0 =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) 1)))))))))))))))))))))))))))))))

(** val reg_of_ireg : ireg -> reg option **)

let reg_of_ireg = function
| IR0 -> Some R0
| IR1 -> Some R1
| IR2 -> Some R2
| IR3 -> Some R3
| IR4 -> Some R4
| IR5 -> Some R5
| IR6 -> Some R6
| IR7 -> Some R7
| IR8 -> Some R8
| IR9 -> Some R9
| IR10 -> Some R10
| _ -> None

(** val reg_ireg_eqb : reg -> ireg -> bool **)

let reg_ireg_eqb r0 r1 =
  match r0 with
  | R0 -> (match r1 with
           | IR0 -> true
           | _ -> false)
  | R1 -> (match r1 with
           | IR1 -> true
           | _ -> false)
  | R2 -> (match r1 with
           | IR2 -> true
           | _ -> false)
  | R3 -> (match r1 with
           | IR3 -> true
           | _ -> false)
  | R4 -> (match r1 with
           | IR4 -> true
           | _ -> false)
  | R5 -> (match r1 with
           | IR5 -> true
           | _ -> false)
  | R6 -> (match r1 with
           | IR6 -> true
           | _ -> false)
  | R7 -> (match r1 with
           | IR7 -> true
           | _ -> false)
  | R8 -> (match r1 with
           | IR8 -> true
           | _ -> false)
  | R9 -> (match r1 with
           | IR9 -> true
           | _ -> false)
  | R10 -> (match r1 with
            | IR10 -> true
            | _ -> false)

(** val bpf_alu32_to_arm32_reg :
    opcode_alu32_reg -> reg -> ireg -> jittedarm32 -> jittedarm32
    option * string **)

let bpf_alu32_to_arm32_reg op dst src j =
  match op with
  | BPF_ADD32_REG ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) aDD_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_rm = encode_arm32 (int_of_ireg src) ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_rm j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str ins_st)), "OK")
                | None -> (None, "add32_reg LoadStore error Store"))
             | None -> (None, "add32_reg error: reg_of_ireg src"))
     | None -> (None, "add32_reg LoadStore error Load"))
  | BPF_SUB32_REG ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) sUB_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_rm = encode_arm32 (int_of_ireg src) ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_rm j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str ins_st)), "OK")
                | None -> (None, "sub32_reg LoadStore error Store"))
             | None -> (None, "sub32_reg error: reg_of_ireg src"))
     | None -> (None, "sub32_reg LoadStore error Load"))
  | BPF_MUL32_REG ->
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) mUL_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rm =
      encode_arm32 (int_of_ireg src) ins_rd (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ 0))))))))
    in
    let ins_rn = encode_arm32 (Int.repr (id_of_reg dst)) ins_rm 0 in
    let ins_st = add_ins_jittedarm32 ins_rn j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str ins_st)), "OK")
                | None -> (None, "mul32_reg LoadStore error Store"))
             | None -> (None, "mul32_reg error: reg_of_ireg src"))
     | None -> (None, "mul32_reg LoadStore error Load"))
  | BPF_DIV32_REG ->
    if (&&) (reg_eqb dst R0) (ireg_eqb src IR1)
    then let st1 = upd_ip_jittedarm32 true j in
         let cmp_ins =
           encode_arm32 (int_of_ireg src) cMP_I_OP (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             0))))))))))))))))
         in
         let cmp_st = add_ins_jittedarm32 cmp_ins st1 in
         let b_ne =
           encode_arm32 cOND_NE b_OP (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))))))))))))))))))
         in
         let b_ins = encode_arm32 (Int.repr ((fun p->2*p) 1)) b_ne 0 in
         let b_st = add_ins_jittedarm32 b_ins cmp_st in
         let div_rd =
           encode_arm32 (Int.repr (id_of_reg dst)) uDIV_OP (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             0))))))))))))))))
         in
         let div_rm =
           encode_arm32 (int_of_ireg src) div_rd (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0))))))))
         in
         let div_rn = encode_arm32 (Int.repr (id_of_reg dst)) div_rm 0 in
         let div_st = add_ins_jittedarm32 div_rn b_st in
         let mov_rd =
           encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) mOV_I_OP
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
         in
         let mov_ins =
           encode_arm32
             (Int.repr ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1))))
             mov_rd 0
         in
         let mov_st = add_ins_jittedarm32 mov_ins div_st in
         let str_imm12 =
           encode_arm32 (Int.repr ((fun p->2*p) ((fun p->2*p) 1))) sTR_I_OP 0
         in
         let str_rt =
           encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) str_imm12
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
         in
         let str_ins =
           encode_arm32 (Int.repr (Z.of_nat (ireg2nat rBPF_State_REG)))
             str_rt (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ 0))))))))))))))))
         in
         let str_st = add_ins_jittedarm32 str_ins mov_st in
         let bal_st = add_ins_jittedarm32 bAL_0 str_st in
         (match upd_LoadStoreRegs bal_st.load_store_regs dst StorePerm with
          | Some ldr ->
            if ireg_eqb src IR12
            then ((Some (upd_load_store_regs_jittedarm32 ldr bal_st)), "OK")
            else (match reg_of_ireg src with
                  | Some r ->
                    (match upd_LoadStoreRegs ldr r LoadPerm with
                     | Some str ->
                       ((Some (upd_load_store_regs_jittedarm32 str bal_st)),
                         "OK")
                     | None -> (None, "div32_reg LoadStore error Store"))
                  | None -> (None, "div32_reg error: reg_of_ireg src"))
          | None -> (None, "div32_reg LoadStore error Load"))
    else (None, "Not UDIV R0 R0 R1")
  | BPF_OR32_REG ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) oRR_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_rm = encode_arm32 (int_of_ireg src) ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_rm j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str ins_st)), "OK")
                | None -> (None, "or32_reg LoadStore error Store"))
             | None -> (None, "or32_reg error: reg_of_ireg src"))
     | None -> (None, "or32_reg LoadStore error Load"))
  | BPF_AND32_REG ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) aND_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_rm = encode_arm32 (int_of_ireg src) ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_rm j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str ins_st)), "OK")
                | None -> (None, "and32_reg LoadStore error Store"))
             | None -> (None, "and32_reg error: reg_of_ireg src"))
     | None -> (None, "and32_reg LoadStore error Load"))
  | BPF_LSH32_REG ->
    let st1 = upd_ip_jittedarm32 true j in
    let cmp_rm =
      encode_arm32 (int_of_ireg src) cMP_I_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let cmp_ins =
      encode_arm32
        (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
          ((fun p->2*p) 1)))))) cmp_rm 0
    in
    let cmp_st = add_ins_jittedarm32 cmp_ins st1 in
    let b_ge =
      encode_arm32 cOND_GE b_OP (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ 0))))))))))))))))))))))))))))
    in
    let b_ins = encode_arm32 (Int.repr ((fun p->2*p) 1)) b_ge 0 in
    let b_st = add_ins_jittedarm32 b_ins cmp_st in
    let lsl_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) lSL_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let lsl_rm =
      encode_arm32 (int_of_ireg src) lsl_rd (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ 0))))))))
    in
    let lsl_rn = encode_arm32 (Int.repr (id_of_reg dst)) lsl_rm 0 in
    let lsl_st = add_ins_jittedarm32 lsl_rn b_st in
    let mov_rd =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) mOV_I_OP
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))
    in
    let mov_ins =
      encode_arm32
        (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))) mov_rd 0
    in
    let mov_st = add_ins_jittedarm32 mov_ins lsl_st in
    let str_imm12 =
      encode_arm32 (Int.repr ((fun p->2*p) ((fun p->2*p) 1))) sTR_I_OP 0
    in
    let str_rt =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) str_imm12
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))
    in
    let str_ins =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat rBPF_State_REG))) str_rt
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))))))
    in
    let str_st = add_ins_jittedarm32 str_ins mov_st in
    let bal_st = add_ins_jittedarm32 bAL_0 str_st in
    (match upd_LoadStoreRegs bal_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr bal_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str bal_st)), "OK")
                | None -> (None, "lsl32_reg LoadStore error Store"))
             | None -> (None, "lsl32_reg error: reg_of_ireg src"))
     | None -> (None, "lsl32_reg LoadStore error Load"))
  | BPF_RSH32_REG ->
    let st1 = upd_ip_jittedarm32 true j in
    let cmp_rm =
      encode_arm32 (int_of_ireg src) cMP_I_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let cmp_ins =
      encode_arm32
        (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
          ((fun p->2*p) 1)))))) cmp_rm 0
    in
    let cmp_st = add_ins_jittedarm32 cmp_ins st1 in
    let b_ge =
      encode_arm32 cOND_GE b_OP (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ 0))))))))))))))))))))))))))))
    in
    let b_ins = encode_arm32 (Int.repr ((fun p->2*p) 1)) b_ge 0 in
    let b_st = add_ins_jittedarm32 b_ins cmp_st in
    let lsr_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) lSR_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let lsr_rm =
      encode_arm32 (int_of_ireg src) lsr_rd (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ 0))))))))
    in
    let lsr_rn = encode_arm32 (Int.repr (id_of_reg dst)) lsr_rm 0 in
    let lsr_st = add_ins_jittedarm32 lsr_rn b_st in
    let mov_rd =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) mOV_I_OP
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))
    in
    let mov_ins =
      encode_arm32
        (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))) mov_rd 0
    in
    let mov_st = add_ins_jittedarm32 mov_ins lsr_st in
    let str_imm12 =
      encode_arm32 (Int.repr ((fun p->2*p) ((fun p->2*p) 1))) sTR_I_OP 0
    in
    let str_rt =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) str_imm12
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))
    in
    let str_ins =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat rBPF_State_REG))) str_rt
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))))))
    in
    let str_st = add_ins_jittedarm32 str_ins mov_st in
    let bal_st = add_ins_jittedarm32 bAL_0 str_st in
    (match upd_LoadStoreRegs bal_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr bal_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str bal_st)), "OK")
                | None -> (None, "lsr32_reg LoadStore error Store"))
             | None -> (None, "lsr32_reg error: reg_of_ireg src"))
     | None -> (None, "lsr32_reg LoadStore error Load"))
  | BPF_MOD32_REG -> (None, "MOD32 Not Ready: bpf_alu32_to_arm32")
  | BPF_XOR32_REG ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) eOR_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_rm = encode_arm32 (int_of_ireg src) ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_rm j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str ins_st)), "OK")
                | None -> (None, "xor32_reg LoadStore error Store"))
             | None -> (None, "xor32_reg error: reg_of_ireg src"))
     | None -> (None, "xor32_reg LoadStore error Load"))
  | BPF_MOV32_REG ->
    if reg_ireg_eqb dst src
    then ((Some j), "OK")
    else let ins_rd =
           encode_arm32 (Int.repr (id_of_reg dst)) mOV_R_OP (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))
         in
         let ins_rm = encode_arm32 (int_of_ireg src) ins_rd 0 in
         let ins_st = add_ins_jittedarm32 ins_rm j in
         (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
          | Some ldr ->
            if ireg_eqb src IR12
            then ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
            else (match reg_of_ireg src with
                  | Some r ->
                    (match upd_LoadStoreRegs ldr r LoadPerm with
                     | Some str ->
                       ((Some (upd_load_store_regs_jittedarm32 str ins_st)),
                         "OK")
                     | None -> (None, "mov32_reg LoadStore error Store"))
                  | None -> (None, "mov32_reg error: reg_of_ireg src"))
          | None -> (None, "mov32_reg LoadStore error Load"))
  | BPF_ARSH32_REG ->
    let st1 = upd_ip_jittedarm32 true j in
    let cmp_rm =
      encode_arm32 (int_of_ireg src) cMP_I_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let cmp_ins =
      encode_arm32
        (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
          ((fun p->2*p) 1)))))) cmp_rm 0
    in
    let cmp_st = add_ins_jittedarm32 cmp_ins st1 in
    let b_ge =
      encode_arm32 cOND_GE b_OP (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ 0))))))))))))))))))))))))))))
    in
    let b_ins = encode_arm32 (Int.repr ((fun p->2*p) 1)) b_ge 0 in
    let b_st = add_ins_jittedarm32 b_ins cmp_st in
    let asr_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) aSR_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let asr_rm =
      encode_arm32 (int_of_ireg src) asr_rd (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ 0))))))))
    in
    let asr_rn = encode_arm32 (Int.repr (id_of_reg dst)) asr_rm 0 in
    let asr_st = add_ins_jittedarm32 asr_rn b_st in
    let mov_rd =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) mOV_I_OP
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))
    in
    let mov_ins =
      encode_arm32
        (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))) mov_rd 0
    in
    let mov_st = add_ins_jittedarm32 mov_ins asr_st in
    let str_imm12 =
      encode_arm32 (Int.repr ((fun p->2*p) ((fun p->2*p) 1))) sTR_I_OP 0
    in
    let str_rt =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) str_imm12
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))
    in
    let str_ins =
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat rBPF_State_REG))) str_rt
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        0))))))))))))))))
    in
    let str_st = add_ins_jittedarm32 str_ins mov_st in
    let bal_st = add_ins_jittedarm32 bAL_0 str_st in
    (match upd_LoadStoreRegs bal_st.load_store_regs dst StorePerm with
     | Some ldr ->
       if ireg_eqb src IR12
       then ((Some (upd_load_store_regs_jittedarm32 ldr bal_st)), "OK")
       else (match reg_of_ireg src with
             | Some r ->
               (match upd_LoadStoreRegs ldr r LoadPerm with
                | Some str ->
                  ((Some (upd_load_store_regs_jittedarm32 str bal_st)), "OK")
                | None -> (None, "asr32_reg LoadStore error Store"))
             | None -> (None, "asr32_reg error: reg_of_ireg src"))
     | None -> (None, "asr32_reg LoadStore error Load"))
  | BPF_ALU32_REG_ILLEGAL_INS -> (None, "ERROR: BPF_ALU32_REG_ILLEGAL_INS")

(** val bpf_alu32_to_arm32_imm :
    opcode_alu32_imm -> reg -> int -> jittedarm32 -> jittedarm32
    option * string **)

let bpf_alu32_to_arm32_imm op dst imm8 j =
  match op with
  | BPF_ADD32_IMM ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) aDD_I_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_imm12 = encode_arm32 imm8 ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_imm12 j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr -> ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
     | None -> (None, "add32_reg LoadStore error Load"))
  | BPF_SUB32_IMM ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) sUB_I_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_imm12 = encode_arm32 imm8 ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_imm12 j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr -> ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
     | None -> (None, "sub32_reg LoadStore error Load"))
  | BPF_OR32_IMM ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) oRR_I_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_imm12 = encode_arm32 imm8 ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_imm12 j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr -> ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
     | None -> (None, "add32_reg LoadStore error Load"))
  | BPF_AND32_IMM ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) aND_R_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_imm12 = encode_arm32 imm8 ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_imm12 j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr -> ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
     | None -> (None, "add32_reg LoadStore error Load"))
  | BPF_LSH32_IMM ->
    if (&&) (Int.cmp Cle Int.zero imm8)
         (Int.cmp Clt imm8
           (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
             ((fun p->2*p) 1)))))))
    then let ins_rd =
           encode_arm32 (Int.repr (id_of_reg dst)) lSL_I_OP (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))
         in
         let ins_imm5 =
           encode_arm32 imm8 ins_rd (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0)))))))
         in
         let ins_rm = encode_arm32 (Int.repr (id_of_reg dst)) ins_imm5 0 in
         let ins_st = add_ins_jittedarm32 ins_rm j in
         (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
          | Some ldr ->
            ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
          | None -> (None, "lsl32_imm LoadStore error Load"))
    else (None, "Verifier SHIFT ERROR")
  | BPF_RSH32_IMM ->
    if (&&) (Int.cmp Cle Int.zero imm8)
         (Int.cmp Clt imm8
           (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
             ((fun p->2*p) 1)))))))
    then let ins_rd =
           encode_arm32 (Int.repr (id_of_reg dst)) lSR_I_OP (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))
         in
         let ins_imm5 =
           encode_arm32 imm8 ins_rd (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0)))))))
         in
         let ins_rm = encode_arm32 (Int.repr (id_of_reg dst)) ins_imm5 0 in
         let ins_st = add_ins_jittedarm32 ins_rm j in
         (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
          | Some ldr ->
            ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
          | None -> (None, "lsr32_imm LoadStore error Load"))
    else (None, "Verifier SHIFT ERROR")
  | BPF_MOD32_IMM -> (None, "MOD32_IMM Not Ready: bpf_alu32_to_arm32")
  | BPF_XOR32_IMM ->
    let ins_rn =
      encode_arm32 (Int.repr (id_of_reg dst)) eOR_I_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
    in
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) ins_rn (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_imm12 = encode_arm32 imm8 ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_imm12 j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr -> ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
     | None -> (None, "add32_reg LoadStore error Load"))
  | BPF_MOV32_IMM ->
    let ins_rd =
      encode_arm32 (Int.repr (id_of_reg dst)) mOV_I_OP (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))
    in
    let ins_imm12 = encode_arm32 imm8 ins_rd 0 in
    let ins_st = add_ins_jittedarm32 ins_imm12 j in
    (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
     | Some ldr -> ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
     | None -> (None, "mov32_reg LoadStore error Load"))
  | BPF_ARSH32_IMM ->
    if (&&) (Int.cmp Cle Int.zero imm8)
         (Int.cmp Clt imm8
           (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
             ((fun p->2*p) 1)))))))
    then let ins_rd =
           encode_arm32 (Int.repr (id_of_reg dst)) aSR_I_OP (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0))))))))))))
         in
         let ins_imm5 =
           encode_arm32 imm8 ins_rd (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0)))))))
         in
         let ins_rm = encode_arm32 (Int.repr (id_of_reg dst)) ins_imm5 0 in
         let ins_st = add_ins_jittedarm32 ins_rm j in
         (match upd_LoadStoreRegs ins_st.load_store_regs dst StorePerm with
          | Some ldr ->
            ((Some (upd_load_store_regs_jittedarm32 ldr ins_st)), "OK")
          | None -> (None, "asr32_imm LoadStore error Load"))
    else (None, "Verifier SHIFT ERROR")
  | _ -> (None, "ERROR: BPF_ALU32_IMM_ILLEGAL_INS")

(** val ireg_of_reg : reg -> ireg **)

let ireg_of_reg = function
| R0 -> IR0
| R1 -> IR1
| R2 -> IR2
| R3 -> IR3
| R4 -> IR4
| R5 -> IR5
| R6 -> IR6
| R7 -> IR7
| R8 -> IR8
| R9 -> IR9
| R10 -> IR10

(** val opcode_reg_of_imm : opcode_alu32_imm -> opcode_alu32_reg **)

let opcode_reg_of_imm = function
| BPF_ADD32_IMM -> BPF_ADD32_REG
| BPF_SUB32_IMM -> BPF_SUB32_REG
| BPF_MUL32_IMM -> BPF_MUL32_REG
| BPF_DIV32_IMM -> BPF_DIV32_REG
| BPF_OR32_IMM -> BPF_OR32_REG
| BPF_AND32_IMM -> BPF_AND32_REG
| BPF_LSH32_IMM -> BPF_LSH32_REG
| BPF_RSH32_IMM -> BPF_RSH32_REG
| BPF_MOD32_IMM -> BPF_MOD32_REG
| BPF_XOR32_IMM -> BPF_XOR32_REG
| BPF_MOV32_IMM -> BPF_MOV32_REG
| BPF_ARSH32_IMM -> BPF_ARSH32_REG
| BPF_ALU32_IMM_ILLEGAL_INS -> BPF_ALU32_REG_ILLEGAL_INS

(** val mov_int_to_reg_binary : int -> ireg -> jittedarm32 -> jittedarm32 **)

let mov_int_to_reg_binary i r j =
  let hi_32 =
    Int.shru i
      (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))))
  in
  let lo_32 =
    Int.coq_and i
      (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        1))))))))))))))))
  in
  let imm12w =
    Int.coq_and lo_32
      (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        1))))))))))))
  in
  let imm4w =
    Int.shru lo_32 (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
  in
  let imm12t =
    Int.coq_and hi_32
      (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        1))))))))))))
  in
  let imm4t =
    Int.shru hi_32 (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
  in
  let movw_imm4 =
    encode_arm32 imm4w mOVW_OP (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))))))
  in
  let movw_rd =
    encode_arm32 (int_of_ireg r) movw_imm4 (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))
  in
  let movw_imm12 = encode_arm32 imm12w movw_rd 0 in
  let movw_st = add_ins_jittedarm32 movw_imm12 j in
  let movt_imm4 =
    encode_arm32 imm4t mOVT_OP (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))))))
  in
  let movt_rd =
    encode_arm32 (int_of_ireg r) movt_imm4 (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))
  in
  let movt_imm12 = encode_arm32 imm12t movt_rd 0 in
  add_ins_jittedarm32 movt_imm12 movw_st

(** val bpf_alu32_to_arm32 :
    int -> jittedarm32 -> jittedarm32 option * string **)

let bpf_alu32_to_arm32 ins j =
  let op = nat_to_opcode_alu32 (get_opcode ins) in
  let dst = get_dst ins in
  let src = get_src ins in
  let imm32 = get_immediate ins in
  (match z_to_reg dst with
   | Some d ->
     (match op with
      | ALU32_REG opr ->
        (match z_to_reg src with
         | Some s -> bpf_alu32_to_arm32_reg opr d (ireg_of_reg s) j
         | None -> (None, "Verifier ERROR"))
      | ALU32_IMM opi ->
        (match opi with
         | BPF_MUL32_IMM ->
           let st1 = upd_ip_jittedarm32 true j in
           let st2 = mov_int_to_reg_binary imm32 IR12 st1 in
           bpf_alu32_to_arm32_reg BPF_MUL32_REG d IR12 st2
         | BPF_DIV32_IMM ->
           (None, "BPF backend ERROR: it should guarantee no div_imm")
         | _ ->
           if (&&) (Int.cmp Cle Int.zero imm32)
                (Int.cmp Cle imm32
                  (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                    ((fun p->1+2*p) 1)))))))))
           then bpf_alu32_to_arm32_imm opi d imm32 j
           else let st1 = upd_ip_jittedarm32 true j in
                let st2 = mov_int_to_reg_binary imm32 IR12 st1 in
                bpf_alu32_to_arm32_reg (opcode_reg_of_imm opi) d IR12 st2)
      | ALU32_ILLEGAL_INS -> (None, "ERROR: ALU32_ILLEGAL_INS"))
   | None -> (None, "Verifier ERROR"))

(** val jit_alu32_aux_arm32 :
    int -> int -> List64AsArray.t -> jittedarm32 -> jittedarm32
    option * string **)

let rec jit_alu32_aux_arm32 fuel entry_point0 l j =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> ((Some j), "OK"))
    (fun n0 ->
    let ins = List64AsArray.index l (Int.repr (Z.of_nat entry_point0)) in
    if ins_is_bpf_alu32 ins
    then let (o, str) = bpf_alu32_to_arm32 ins j in
         (match o with
          | Some arm33 ->
            jit_alu32_aux_arm32 n0 (Stdlib.succ entry_point0) l arm33
          | None -> (None, str))
    else ((Some j), "OK"))
    fuel

(** val jit_alu32_entry_points_list :
    int -> int -> bool -> List64AsArray.t -> entry_point -> entry_point **)

let rec jit_alu32_entry_points_list fuel pc is_alu32 l epl =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> epl)
    (fun n0 ->
    let ins = List64AsArray.index l (Int.repr (Z.of_nat pc)) in
    if ins_is_bpf_alu32 ins
    then if eqb is_alu32 false
         then let is_pc_repeat =
                ListNat.is_exists epl.entry_ps epl.entry_len pc
              in
              if is_pc_repeat
              then jit_alu32_entry_points_list n0 (Stdlib.succ pc) true l
                     epl
              else let new_epl = add_new_entry_point pc epl in
                   jit_alu32_entry_points_list n0 (Stdlib.succ pc) true l
                     new_epl
         else jit_alu32_entry_points_list n0 (Stdlib.succ pc) true l epl
    else if ins_is_bpf_jump ins
         then let ofs = get_offset ins in
              let next_pc =
                Int.add (Int.add (Int.repr (Z.of_nat pc)) ofs) Int.one
              in
              let next_ins = List64AsArray.index l next_pc in
              if ins_is_bpf_alu32 next_ins
              then let new_epl =
                     add_new_entry_point (Z.to_nat (Int.unsigned next_pc)) epl
                   in
                   jit_alu32_entry_points_list n0 (Stdlib.succ pc) false
                     l new_epl
              else jit_alu32_entry_points_list n0 (Stdlib.succ pc) false
                     l epl
         else jit_alu32_entry_points_list n0 (Stdlib.succ pc) false l epl)
    fuel

(** val jit_alu32_store_register : int -> int **)

let jit_alu32_store_register r =
  let ins_imm12 =
    encode_arm32
      (Int.add bPF_R0_OFS
        (Int.mul r (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))
      sTR_I_OP 0
  in
  let ins_rt =
    encode_arm32 r ins_imm12 (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))
  in
  encode_arm32 (Int.repr (Z.of_nat (ireg2nat rBPF_State_REG))) ins_rt
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0))))))))))))))))

(** val jit_alu32_arm32_upd_store :
    reg -> jit_state -> loadStoreRegs -> jit_state **)

let jit_alu32_arm32_upd_store r st ls =
  if is_store_reg r ls
  then let ins = jit_alu32_store_register (Int.repr (Z.of_nat (reg2nat r))) in
       upd_jitted_list st ins
  else st

(** val jit_alu32_arm32_store : jit_state -> loadStoreRegs -> jit_state **)

let jit_alu32_arm32_store st ls =
  let r0_st = jit_alu32_arm32_upd_store R0 st ls in
  let r1_st = jit_alu32_arm32_upd_store R1 r0_st ls in
  let r2_st = jit_alu32_arm32_upd_store R2 r1_st ls in
  let r3_st = jit_alu32_arm32_upd_store R3 r2_st ls in
  let r4_st = jit_alu32_arm32_upd_store R4 r3_st ls in
  let r5_st = jit_alu32_arm32_upd_store R5 r4_st ls in
  let r6_st = jit_alu32_arm32_upd_store R6 r5_st ls in
  let r7_st = jit_alu32_arm32_upd_store R7 r6_st ls in
  let r8_st = jit_alu32_arm32_upd_store R8 r7_st ls in
  let r9_st = jit_alu32_arm32_upd_store R9 r8_st ls in
  jit_alu32_arm32_upd_store R10 r9_st ls

(** val jit_alu32_store_flag : jit_state -> int -> jit_state **)

let jit_alu32_store_flag st f =
  let ins_rd =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) mOV_I_OP
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))))))))))
  in
  let ins0 = encode_arm32 f ins_rd 0 in
  let st0 = upd_jitted_list st ins0 in
  let ins_imm12 =
    encode_arm32 (Int.repr ((fun p->2*p) ((fun p->2*p) 1))) sTR_I_OP 0
  in
  let ins_rt =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR12))) ins_imm12
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))))))))))
  in
  let ins1 =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat rBPF_State_REG))) ins_rt
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))))))))))))))
  in
  upd_jitted_list st0 ins1

(** val jit_alu32_arm32_store_succ_R0 : jit_state -> jit_state **)

let jit_alu32_arm32_store_succ_R0 st =
  let st0 = jit_alu32_store_flag st (int_of_flag BPF_SUCC_RETURN) in
  let ins_r0 = jit_alu32_store_register (Int.repr (Z.of_nat (reg2nat R0))) in
  upd_jitted_list st0 ins_r0

(** val jit_alu32_reset : int -> int -> int **)

let jit_alu32_reset r ofs =
  let ins_imm12 = encode_arm32 (Int.repr (Z.of_nat ofs)) lDR_I_OP 0 in
  let ins_rt =
    encode_arm32 r ins_imm12 (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0))))))))))))
  in
  encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR13))) ins_rt (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))

(** val jit_alu32_arm32_upd_reset :
    reg -> jit_state -> loadStoreRegs -> jit_state **)

let jit_alu32_arm32_upd_reset r st ls =
  if is_non_reg r ls
  then st
  else let ins =
         jit_alu32_reset (Int.repr (Z.of_nat (reg2nat r)))
           (Nat.sub st.stack_ofs (Stdlib.succ (Stdlib.succ
             (Stdlib.succ (Stdlib.succ 0)))))
       in
       add_ins_to_pop_stack st ins

(** val jit_alu32_arm32_reset :
    jit_state -> loadStoreRegs -> bool -> jit_state **)

let jit_alu32_arm32_reset st ls _ =
  let r10_st = jit_alu32_arm32_upd_reset R10 st ls in
  let r9_st = jit_alu32_arm32_upd_reset R9 r10_st ls in
  let r8_st = jit_alu32_arm32_upd_reset R8 r9_st ls in
  let r7_st = jit_alu32_arm32_upd_reset R7 r8_st ls in
  let r6_st = jit_alu32_arm32_upd_reset R6 r7_st ls in
  let r5_st = jit_alu32_arm32_upd_reset R5 r6_st ls in
  let r4_st = jit_alu32_arm32_upd_reset R4 r5_st ls in
  let r3_st = jit_alu32_arm32_upd_reset R3 r4_st ls in
  let r2_st = jit_alu32_arm32_upd_reset R2 r3_st ls in
  jit_alu32_arm32_upd_reset R1 r2_st ls

(** val jit_alu32_post : jit_state -> jit_state **)

let jit_alu32_post st =
  let ins_imm12 = encode_arm32 (Int.repr (Z.of_nat 0)) lDR_I_OP 0 in
  let ins_rt =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR11))) ins_imm12
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))))))))))
  in
  let ins_rn =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR13))) ins_rt
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))))))))))))))
  in
  let st0 = add_ins_to_pop_stack st ins_rn in
  let ins_src = encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR11))) mOV_R_OP 0
  in
  let ins_dst =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR13))) ins_src
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0))))))))))))
  in
  let st1 = upd_jitted_list st0 ins_dst in
  let ins_rm = encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR14))) bX_OP 0 in
  upd_jitted_list st1 ins_rm

(** val get_store_ins_num : loadStoreRegs -> int **)

let get_store_ins_num ls =
  let n0 = if is_store_reg R0 ls then Stdlib.succ 0 else 0 in
  let n1 = if is_store_reg R1 ls then Stdlib.succ n0 else n0 in
  let n2 = if is_store_reg R2 ls then Stdlib.succ n1 else n1 in
  let n3 = if is_store_reg R3 ls then Stdlib.succ n2 else n2 in
  let n4 = if is_store_reg R4 ls then Stdlib.succ n3 else n3 in
  let n5 = if is_store_reg R5 ls then Stdlib.succ n4 else n4 in
  let n6 = if is_store_reg R6 ls then Stdlib.succ n5 else n5 in
  let n7 = if is_store_reg R7 ls then Stdlib.succ n6 else n6 in
  let n8 = if is_store_reg R8 ls then Stdlib.succ n7 else n7 in
  let n9 = if is_store_reg R9 ls then Stdlib.succ n8 else n8 in
  if is_store_reg R10 ls then Stdlib.succ n9 else n9

(** val copy_arm32_list_from_to_aux :
    int -> int -> List32.t -> jit_state -> loadStoreRegs -> jit_state **)

let rec copy_arm32_list_from_to_aux fuel pc from to0 ls =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> to0)
    (fun n0 ->
    let ins0 = List32.index from (Int.repr (Z.of_nat pc)) in
    let ins =
      if Int.eq ins0 bAL_0
      then let store_len = get_store_ins_num ls in let _ = print_endline ("fuel = " ^ (string_of_int fuel)) in let _ = print_endline ("store_len = " ^ (string_of_int store_len)) in
           let ofs = Int.repr (Z.of_nat (Nat.add fuel store_len)) in
           encode_arm32 ofs ins0 0
      else ins0
    in
    let st0 = upd_jitted_list to0 ins in
    copy_arm32_list_from_to_aux n0 (Stdlib.succ pc) from st0 ls)
    fuel

(** val copy_arm32_list_from_to :
    jittedarm32 -> jit_state -> loadStoreRegs -> jit_state **)

let copy_arm32_list_from_to from to0 ls =
  copy_arm32_list_from_to_aux from.arm32_len 0 from.arm32 to0 ls

(** val construct_iBPF_ins : jit_state -> int -> int -> int **)

let construct_iBPF_ins st entry_point0 ofs =
  let rbpf_ins =
    List64AsArray.index st.ibpf (Int.repr (Z.of_nat entry_point0))
  in
  let ins_set =
    Int64.coq_and rbpf_ins
      (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        1))))))))))))))))
  in
  let ibpf_0 =
    encode_bpf64 (Int64.repr (Z.of_nat ofs)) ins_set (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))))))))))))
  in
  encode_bpf64 (Int64.repr (Z.of_nat st.jitted_len)) ibpf_0 (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0))))))))))))))))))))))))))))))))

(** val jit_alu32_aux :
    int -> int -> ListNat.t -> jit_state -> jit_state option * string **)

let rec jit_alu32_aux fuel pc epl st = let _ = print_endline "****************jit alu32***************" in
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> ((Some st), "OK"))
    (fun n0 ->
    let entry_point0 = ListNat.index epl pc in let _ = print_endline ("entry_point: " ^ string_of_int entry_point0) in
    let (o, str) =
      jit_alu32_aux_arm32 (length st.ibpf) entry_point0 st.ibpf
        init_jittedarm32
    in
    (match o with
     | Some pre_jarm32 ->
       let ls_reg = pre_jarm32.load_store_regs in let _ = print_load_store_regs ls_reg in
       let ibpf_ins = construct_iBPF_ins st entry_point0 pre_jarm32.offset in let _ = print_endline "arm32_pre" in
       let arm32_pre = jit_alu32_pre st in let _ = print_endline "arm32_save" in
       let arm32_save = jit_alu32_arm32_save arm32_pre ls_reg pre_jarm32.is_ip
       in let _ = print_endline "arm32_load" in
       let arm32_load = jit_alu32_arm32_load arm32_save ls_reg in let _ = print_endline "arm32_core" in
       let arm32_core = copy_arm32_list_from_to pre_jarm32 arm32_load ls_reg
       in let _ = print_endline "arm32_store" in
       let arm32_store = jit_alu32_arm32_store arm32_core ls_reg in let _ = print_endline "arm32_reset" in
       let arm32_reset =
         jit_alu32_arm32_reset arm32_store ls_reg pre_jarm32.is_ip
       in let _ = print_endline "arm32_post" in
       let arm32_post = jit_alu32_post arm32_reset in let _ = print_endline "upd_ibpf" in
       let next_st = upd_ibpf arm32_post ibpf_ins entry_point0 in
       let _ = print_endline "****************jit alu32***************\n\n" in
       jit_alu32_aux n0 (Stdlib.succ pc) epl next_st
     | None -> (None, str)))
    fuel


(** val jit_alu32 : jit_state -> jit_state option * string **)

let jit_alu32 st =
  let epl =
    jit_alu32_entry_points_list (length st.ibpf) 0 false st.ibpf
      init_entry_point
  in
  if (&&) ((=) epl.entry_len (Stdlib.succ 0))
       ((=) (ListNat.index epl.entry_ps 0) 0)
  then let (o, str) =
         jit_alu32_aux_arm32 (length st.ibpf) 0 st.ibpf init_jittedarm32
       in
       (match o with
        | Some pre_jarm32 ->
          let ls_reg = pre_jarm32.load_store_regs in
          let arm32_pre = jit_alu32_pre st in
          let arm32_save = jit_alu32_arm32_save arm32_pre ls_reg true in
          let arm32_load = jit_alu32_arm32_load arm32_save ls_reg in
          let arm32_core =
            copy_arm32_list_from_to pre_jarm32 arm32_load ls_reg
          in
          let arm32_store =
            if (=) pre_jarm32.offset
                 (Nat.sub (length st.ibpf) (Stdlib.succ 0))
            then jit_alu32_arm32_store_succ_R0 arm32_core
            else jit_alu32_arm32_store arm32_core ls_reg
          in
          let arm32_reset = jit_alu32_arm32_reset arm32_store ls_reg true in
          let arm32_post = jit_alu32_post arm32_reset in
          ((Some arm32_post), "OK")
        | None -> (None, str))
  else jit_alu32_aux epl.entry_len 0 epl.entry_ps st

(** val arm32_decode_prog_aux : int -> int -> List32.t -> instruction list **)

let rec arm32_decode_prog_aux fuel pc l =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n0 ->
    match decode (List32.index l (Int.repr (Z.of_nat pc))) with
    | Some ins ->
      app (ins :: []) (arm32_decode_prog_aux n0 (Stdlib.succ pc) l)
    | None -> [])
    fuel

(** val arm32_decode_prog : List32.t -> int -> instruction list **)

let arm32_decode_prog l len =
  arm32_decode_prog_aux len 0 l

(** val string_of_ireg : ireg -> string **)

let string_of_ireg = function
| IR0 -> "r0"
| IR1 -> "r1"
| IR2 -> "r2"
| IR3 -> "r3"
| IR4 -> "r4"
| IR5 -> "r5"
| IR6 -> "r6"
| IR7 -> "r7"
| IR8 -> "r8"
| IR9 -> "r9"
| IR10 -> "r10"
| IR11 -> "fp"
| IR12 -> "ip"
| IR13 -> "sp"
| IR14 -> "lr"

(** val string_of_ptrofs_signed : int -> string **)

let string_of_ptrofs_signed ofs =
  of_Z (Ptrofs.signed ofs)

(** val string_of_shift_op : shift_op -> string **)

let string_of_shift_op = function
| SOimm i -> (^) "#" (string_of_int_unsigned i)
| SOreg r -> string_of_ireg r
| _ -> "shift op error: not now"

(** val print_arm32_instruction : instruction -> string **)

let print_arm32_instruction = function
| Padd (dst, src, sop) ->
  (^) (string_of_ireg dst)
    ((^) " := "
      ((^) (string_of_ireg src) ((^) " + " (string_of_shift_op sop))))
| Pand (dst, src, sop) ->
  (^) (string_of_ireg dst)
    ((^) " := "
      ((^) (string_of_ireg src) ((^) " & " (string_of_shift_op sop))))
| Pasr (dst, src, rop) ->
  (^) (string_of_ireg dst)
    ((^) " := "
      ((^) (string_of_ireg src) ((^) " >> (signed) " (string_of_ireg rop))))
| Pb imm24 -> (^) "goto " (string_of_ptrofs_signed imm24)
| Pbreg dst -> (^) "goto " (string_of_ireg dst)
| Pcmp (dst, sop) ->
  (^) "cmp "
    ((^) (string_of_ireg dst)
      ((^) " and " ((^) (string_of_shift_op sop) " => update flags")))
| Peor (dst, src, sop) ->
  (^) (string_of_ireg dst)
    ((^) " := "
      ((^) (string_of_ireg src) ((^) " ^ " (string_of_shift_op sop))))
| Pldr (dst, src, sop) ->
  (^) (string_of_ireg dst)
    ((^) " := ["
      ((^) (string_of_ireg src)
        ((^) " + " ((^) (string_of_shift_op sop) "]"))))
| Plsl (dst, src, rop) ->
  (^) (string_of_ireg dst)
    ((^) " := " ((^) (string_of_ireg src) ((^) " << " (string_of_ireg rop))))
| Plsr (dst, src, rop) ->
  (^) (string_of_ireg dst)
    ((^) " := " ((^) (string_of_ireg src) ((^) " >> " (string_of_ireg rop))))
| Pmov (dst, sop) ->
  (^) (string_of_ireg dst) ((^) " := " (string_of_shift_op sop))
| Pmovw (dst, imm16) ->
  (^) (string_of_ireg dst) ((^) " := #" (string_of_int_unsigned imm16))
| Pmovt (dst, imm16) ->
  (^) (string_of_ireg dst)
    ((^) " := (#" ((^) (string_of_int_unsigned imm16) ") << 16"))
| Porr (dst, src, sop) ->
  (^) (string_of_ireg dst)
    ((^) " := "
      ((^) (string_of_ireg src) ((^) " | " (string_of_shift_op sop))))
| Prsb (dst, src, sop) ->
  (^) (string_of_ireg dst)
    ((^) " := "
      ((^) (string_of_shift_op sop) ((^) " - " (string_of_ireg src))))
| Pstr (dst, src, sop) ->
  (^) "["
    ((^) (string_of_ireg src)
      ((^) " + "
        ((^) (string_of_shift_op sop) ((^) "] := " (string_of_ireg dst)))))
| Psub (dst, src, sop) ->
  (^) (string_of_ireg dst)
    ((^) " := "
      ((^) (string_of_ireg src) ((^) " - " (string_of_shift_op sop))))
| Pudiv -> "UDIV R0 R0 R1"
| _ -> "not yet"

(** val print_arm32_prog_aux : int -> instruction list -> string list **)

let rec print_arm32_prog_aux pc = function
| [] -> []
| hd :: tl ->
  let ins_string = print_arm32_instruction hd in
  app (((^) (of_nat0 pc) ((^) ": " ins_string)) :: [])
    (print_arm32_prog_aux (Stdlib.succ pc) tl)

(** val print_arm32_prog : instruction list -> string list **)

let print_arm32_prog l =
  print_arm32_prog_aux 0 l

(** val test_fletcher32_int64 : int list **)

let test_fletcher32_int64 =
  (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->2*p) 1)))))))))))))))) :: ((Int64.repr
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->1+2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->1+2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->1+2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->1+2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          ((fun p->2*p)
                                                          1))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))))))))))) :: ((Int64.repr
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->1+2*p)
                                                                    ((fun p->2*p)
                                                                    ((fun p->2*p)
                                                                    1))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      1)))))))))))))))))))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              1))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) 1))))))))))))))))))))) :: ((Int64.repr ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   1))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      1))))))))))))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                       ((fun p->1+2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->1+2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       ((fun p->2*p)
                                                       1))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1)))))))))))))) :: ((Int64.repr ((fun p->2*p) ((fun p->2*p)
                            ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) 1)))))))))) :: ((Int64.repr
                                                            ((fun p->2*p)
                                                            ((fun p->2*p)
                                                            ((fun p->1+2*p)
                                                            ((fun p->1+2*p)
                                                            ((fun p->1+2*p)
                                                            ((fun p->1+2*p)
                                                            ((fun p->2*p)
                                                            ((fun p->1+2*p)
                                                            ((fun p->2*p)
                                                            ((fun p->2*p)
                                                            ((fun p->1+2*p)
                                                            ((fun p->2*p)
                                                            1))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) 1))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           1)))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) 1))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) 1))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) 1))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           1)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) 1)))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              1))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p)
      1)))))))))))))))))))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              1))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) 1))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->1+2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->1+2*p) 1))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))))))))))) :: ((Int64.repr
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->2*p)
                                                         1))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1)))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
                                            1)))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1))))))))))))))))))))))))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   1)))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                             ((fun p->2*p) ((fun p->1+2*p)
                                             ((fun p->1+2*p) ((fun p->1+2*p)
                                             ((fun p->1+2*p) ((fun p->2*p)
                                             ((fun p->1+2*p) ((fun p->1+2*p)
                                             ((fun p->1+2*p) ((fun p->2*p)
                                             ((fun p->2*p) ((fun p->2*p)
                                             ((fun p->2*p) 1))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      1)))))))))) :: ((Int64.repr ((fun p->1+2*p) ((fun p->2*p)
                        ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                        ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                        ((fun p->1+2*p) ((fun p->1+2*p)
                        1)))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                                ((fun p->2*p)
                                                                ((fun p->2*p)
                                                                ((fun p->1+2*p)
                                                                ((fun p->1+2*p)
                                                                ((fun p->1+2*p)
                                                                ((fun p->1+2*p)
                                                                ((fun p->2*p)
                                                                ((fun p->1+2*p)
                                                                ((fun p->1+2*p)
                                                                ((fun p->2*p)
                                                                ((fun p->2*p)
                                                                ((fun p->2*p)
                                                                ((fun p->1+2*p)
                                                                ((fun p->2*p)
                                                                1))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p)
      1)))))))))))))))))))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              1))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))))))))))) :: ((Int64.repr
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->2*p)
                                                         1))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      1)))))))))))))) :: ((Int64.repr ((fun p->2*p) ((fun p->2*p)
                            ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                            ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                            ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                            ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                            ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                            ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                            1)))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1))))))))))))))))))))))))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   1)))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      1)))))))))))))) :: ((Int64.repr ((fun p->2*p) ((fun p->2*p)
                            ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                            ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p)
                            1))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) 1))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->1+2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->1+2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) ((fun p->2*p)
                                           ((fun p->2*p) 1))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))))))))))))))) :: ((Int64.repr
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->1+2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             ((fun p->2*p)
                                                             1))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1)))))))) :: []))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val fletcher32_init_jitted_list : List32.t **)

let fletcher32_init_jitted_list =
  List32.create_int_list jITTED_LIST_MAX_LENGTH

(** val fletcher32_init_jit_state : jit_state **)

let fletcher32_init_jit_state =
  { pc_loc = Int.zero; flag = Vundef; regs_st = Vundef; mrs_num = 0;
    bpf_mrs = []; ins_len = (length test_fletcher32_int64); ibpf =
    test_fletcher32_int64; stack_len = 0; stack_ofs = 0; jitted_len = 0;
    jitted_list = fletcher32_init_jitted_list; jit_mem = Mem.empty }

(** val fletcher32_main : string list **)

let fletcher32_main =
  let final_state = jit_alu32 fletcher32_init_jit_state in
  (match fst final_state with
   | Some st ->
     print_arm32_prog (arm32_decode_prog st.jitted_list st.jitted_len)
   | None -> [])
