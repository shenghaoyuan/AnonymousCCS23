
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

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

let rec app l m0 =
  match l with
  | [] -> m0
  | a :: l1 -> a :: (app l1 m0)

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
  (** val add : int -> int -> int **)

  let rec add n0 m0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> m0)
      (fun p -> Stdlib.succ (add p m0))
      n0

  (** val mul : int -> int -> int **)

  let rec mul n0 m0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> 0)
      (fun p -> add m0 (mul p m0))
      n0

  (** val ltb : int -> int -> bool **)

  let ltb n0 m0 =
    (<=) (Stdlib.succ n0) m0

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

  (** val add : int -> int -> int **)

  let add = (+)

  (** val sub : int -> int -> int **)

  let sub = fun n m -> Stdlib.max 0 (n-m)

  (** val mul : int -> int -> int **)

  let mul = ( * )

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

  let coq_lor n0 m0 =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> m0)
      (fun p ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> n0)
        (fun q -> (Coq_Pos.coq_lor p q))
        m0)
      n0

  (** val coq_land : int -> int -> int **)

  let coq_land n0 m0 =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 0)
      (fun p ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> 0)
        (fun q -> Coq_Pos.coq_land p q)
        m0)
      n0

  (** val ldiff : int -> int -> int **)

  let ldiff n0 m0 =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> 0)
      (fun p ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> n0)
        (fun q -> Coq_Pos.ldiff p q)
        m0)
      n0

  (** val coq_lxor : int -> int -> int **)

  let coq_lxor n0 m0 =
    (fun f0 fp n -> if n=0 then f0 () else fp n)
      (fun _ -> m0)
      (fun p ->
      (fun f0 fp n -> if n=0 then f0 () else fp n)
        (fun _ -> n0)
        (fun q -> Coq_Pos.coq_lxor p q)
        m0)
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

(** val n_of_digits : bool list -> int **)

let rec n_of_digits = function
| [] -> 0
| b :: l' ->
  N.add (if b then 1 else 0) (N.mul ((fun p->2*p) 1) (n_of_digits l'))

(** val n_of_ascii : char -> int **)

let n_of_ascii a =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun a0 a1 a2 a3 a4 a5 a6 a7 ->
    n_of_digits
      (a0 :: (a1 :: (a2 :: (a3 :: (a4 :: (a5 :: (a6 :: (a7 :: [])))))))))
    a

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
    (fun m0 ->
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q -> Z.succ_double (p_mod_two_p q m0))
      (fun q -> Z.double (p_mod_two_p q m0))
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
    (fun m0 ->
    if Z.odd x
    then i :: (z_one_bits m0 (Z.div2 x) (Z.add i 1))
    else z_one_bits m0 (Z.div2 x) (Z.add i 1))
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
| F754_finite (s, m0, e) -> B754_finite (s, m0, e)

(** val join_bits : int -> int -> bool -> int -> int -> int **)

let join_bits mw ew s m0 e =
  Z.add (Z.shiftl (Z.add (if s then Z.pow ((fun p->2*p) 1) ew else 0) e) mw)
    m0

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
    let m0 = Z.sub mx (Z.pow ((fun p->2*p) 1) mw) in
    if Z.leb 0 m0
    then join_bits mw ew sx m0 (Z.add (Z.sub ex (emin prec emax)) 1)
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

  let rolm x a m0 =
    coq_and (rol x a) m0

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

  (** val zwordsize : int **)

  let zwordsize =
    Z.of_nat wordsize

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

  (** val zero : int **)

  let zero =
    repr 0

  (** val eq_dec : int -> int -> bool **)

  let eq_dec x y =
    let intval0 = x in let intval1 = y in zeq intval0 intval1

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

  (** val shl : int -> int -> int **)

  let shl x y =
    repr (Z.shiftl (unsigned x) (unsigned y))

  (** val shru : int -> int -> int **)

  let shru x y =
    repr (Z.shiftr (unsigned x) (unsigned y))

  (** val iwordsize' : int **)

  let iwordsize' =
    Int.repr zwordsize

  (** val shl' : int -> int -> int **)

  let shl' x y =
    repr (Z.shiftl (unsigned x) (Int.unsigned y))

  (** val shru' : int -> int -> int **)

  let shru' x y =
    repr (Z.shiftr (unsigned x) (Int.unsigned y))

  (** val shr' : int -> int -> int **)

  let shr' x y =
    repr (Z.shiftr (signed x) (Int.unsigned y))
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

  (** val eq : int -> int -> bool **)

  let eq x y =
    if zeq (unsigned x) (unsigned y) then true else false

  (** val ltu : int -> int -> bool **)

  let ltu x y =
    if zlt (unsigned x) (unsigned y) then true else false

  (** val add : int -> int -> int **)

  let add x y =
    repr (Z.add (unsigned x) (unsigned y))

  (** val sub : int -> int -> int **)

  let sub x y =
    repr (Z.sub (unsigned x) (unsigned y))

  (** val cmpu : comparison0 -> int -> int -> bool **)

  let cmpu c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> ltu x y
    | Cle -> negb (ltu y x)
    | Cgt -> ltu y x
    | Cge -> negb (ltu x y)

  (** val to_int : int -> int **)

  let to_int x =
    Int.repr (unsigned x)

  (** val to_int64 : int -> int **)

  let to_int64 x =
    Int64.repr (unsigned x)

  (** val of_int : int -> int **)

  let of_int x =
    repr (Int.unsigned x)

  (** val of_int64 : int -> int **)

  let of_int64 x =
    repr (Int64.unsigned x)
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

  let rec get' p m0 =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q ->
      match m0 with
      | Node001 m' -> get' q m'
      | Node011 (_, m') -> get' q m'
      | Node101 (_, m') -> get' q m'
      | Node111 (_, _, m') -> get' q m'
      | _ -> None)
      (fun q ->
      match m0 with
      | Node100 m' -> get' q m'
      | Node101 (m', _) -> get' q m'
      | Node110 (m', _) -> get' q m'
      | Node111 (m', _, _) -> get' q m'
      | _ -> None)
      (fun _ ->
      match m0 with
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

  let rec set' p x m0 =
    (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
      (fun q ->
      match m0 with
      | Node001 r -> Node001 (set' q x r)
      | Node010 y -> Node011 (y, (set0 q x))
      | Node011 (y, r) -> Node011 (y, (set' q x r))
      | Node100 l -> Node101 (l, (set0 q x))
      | Node101 (l, r) -> Node101 (l, (set' q x r))
      | Node110 (l, y) -> Node111 (l, y, (set0 q x))
      | Node111 (l, y, r) -> Node111 (l, y, (set' q x r)))
      (fun q ->
      match m0 with
      | Node001 r -> Node101 ((set0 q x), r)
      | Node010 y -> Node110 ((set0 q x), y)
      | Node011 (y, r) -> Node111 ((set0 q x), y, r)
      | Node100 l -> Node100 (set' q x l)
      | Node101 (l, r) -> Node101 ((set' q x l), r)
      | Node110 (l, y) -> Node110 ((set' q x l), y)
      | Node111 (l, y, r) -> Node111 ((set' q x l), y, r))
      (fun _ ->
      match m0 with
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
  | Nodes m1 -> Nodes (map1' f m1)
 end

module PMap =
 struct
  type 'a t = 'a * 'a PTree.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init x =
    (x, PTree.empty)

  (** val get : int -> 'a1 t -> 'a1 **)

  let get i m0 =
    match PTree.get i (snd m0) with
    | Some x -> x
    | None -> fst m0

  (** val set : int -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree **)

  let set i x m0 =
    ((fst m0), (PTree.set i x (snd m0)))

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m0 =
    ((f (fst m0)), (PTree.map1 f (snd m0)))
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

  let get i m0 =
    PMap.get (X.index i) m0

  (** val set : X.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree **)

  let set i v m0 =
    PMap.set (X.index i) v m0

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

let string_of_memory_chunk mc =
  match mc with
  | Mint32          -> "32"
  | Mint16unsigned  -> "16"
  | Mint8unsigned   -> "8"
  | Mint64          -> "64"
  | _               -> "error"

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

let string_of_val0 v =
  match v with
  | Vundef -> "undefied value"
  | Vint x0 -> "Int32: " ^ (string_of_int x0)
  | Vlong x0 -> "Int64: " ^ (string_of_int x0)
  | Vptr (b, ofs) -> (string_of_int b) ^ "," ^ (string_of_int ofs)
  | _ -> "unexpected cases"

let print_val0 v = print_endline (string_of_val0 v)

(** val vzero : val0 **)

let vzero =
  Vint Int.zero

(** val vnullptr : val0 **)

let vnullptr =
  if ptr64 then Vlong Int64.zero else Vint Int.zero

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

  (** val neg : val0 -> val0 **)

  let neg = function
  | Vint n0 -> Vint (Int.neg n0)
  | _ -> Vundef

  (** val add : val0 -> val0 -> val0 **)

  let add v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 -> Vint (Int.add n1 n2)
       | Vptr (b2, ofs2) ->
         if ptr64
         then Vundef
         else Vptr (b2, (Ptrofs.add ofs2 (Ptrofs.of_int n1)))
       | _ -> Vundef)
    | Vptr (b1, ofs1) ->
      (match v2 with
       | Vint n2 ->
         if ptr64
         then Vundef
         else Vptr (b1, (Ptrofs.add ofs1 (Ptrofs.of_int n2)))
       | _ -> Vundef)
    | _ -> Vundef

  (** val sub : val0 -> val0 -> val0 **)

  let sub v1 v2 =
    match v1 with
    | Vint n1 -> (match v2 with
                  | Vint n2 -> Vint (Int.sub n1 n2)
                  | _ -> Vundef)
    | Vptr (b1, ofs1) ->
      (match v2 with
       | Vint n2 ->
         if ptr64
         then Vundef
         else Vptr (b1, (Ptrofs.sub ofs1 (Ptrofs.of_int n2)))
       | Vptr (b2, ofs2) ->
         if ptr64
         then Vundef
         else if eq_block b1 b2
              then Vint (Ptrofs.to_int (Ptrofs.sub ofs1 ofs2))
              else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val mul : val0 -> val0 -> val0 **)

  let mul v1 v2 =
    match v1 with
    | Vint n1 -> (match v2 with
                  | Vint n2 -> Vint (Int.mul n1 n2)
                  | _ -> Vundef)
    | _ -> Vundef

  (** val divu : val0 -> val0 -> val0 option **)

  let divu v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.eq n2 Int.zero then None else Some (Vint (Int.divu n1 n2))
       | _ -> None)
    | _ -> None

  (** val modu : val0 -> val0 -> val0 option **)

  let modu v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.eq n2 Int.zero then None else Some (Vint (Int.modu n1 n2))
       | _ -> None)
    | _ -> None

  (** val coq_and : val0 -> val0 -> val0 **)

  let coq_and v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 -> Vint (Int.coq_and n1 n2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val coq_or : val0 -> val0 -> val0 **)

  let coq_or v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 -> Vint (Int.coq_or n1 n2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val xor : val0 -> val0 -> val0 **)

  let xor v1 v2 =
    match v1 with
    | Vint n1 -> (match v2 with
                  | Vint n2 -> Vint (Int.xor n1 n2)
                  | _ -> Vundef)
    | _ -> Vundef

  (** val shl : val0 -> val0 -> val0 **)

  let shl v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.ltu n2 Int.iwordsize then Vint (Int.shl n1 n2) else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val shr : val0 -> val0 -> val0 **)

  let shr v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.ltu n2 Int.iwordsize then Vint (Int.shr n1 n2) else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val shru : val0 -> val0 -> val0 **)

  let shru v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.ltu n2 Int.iwordsize then Vint (Int.shru n1 n2) else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val negl : val0 -> val0 **)

  let negl = function
  | Vlong n0 -> Vlong (Int64.neg n0)
  | _ -> Vundef

  (** val longofint : val0 -> val0 **)

  let longofint = function
  | Vint n0 -> Vlong (Int64.repr (Int.signed n0))
  | _ -> Vundef

  (** val longofintu : val0 -> val0 **)

  let longofintu = function
  | Vint n0 -> Vlong (Int64.repr (Int.unsigned n0))
  | _ -> Vundef

  (** val addl : val0 -> val0 -> val0 **)

  let addl v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vlong n2 -> Vlong (Int64.add n1 n2)
       | Vptr (b2, ofs2) ->
         if ptr64
         then Vptr (b2, (Ptrofs.add ofs2 (Ptrofs.of_int64 n1)))
         else Vundef
       | _ -> Vundef)
    | Vptr (b1, ofs1) ->
      (match v2 with
       | Vlong n2 ->
         if ptr64
         then Vptr (b1, (Ptrofs.add ofs1 (Ptrofs.of_int64 n2)))
         else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val subl : val0 -> val0 -> val0 **)

  let subl v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vlong n2 -> Vlong (Int64.sub n1 n2)
       | _ -> Vundef)
    | Vptr (b1, ofs1) ->
      (match v2 with
       | Vlong n2 ->
         if ptr64
         then Vptr (b1, (Ptrofs.sub ofs1 (Ptrofs.of_int64 n2)))
         else Vundef
       | Vptr (b2, ofs2) ->
         if negb ptr64
         then Vundef
         else if eq_block b1 b2
              then Vlong (Ptrofs.to_int64 (Ptrofs.sub ofs1 ofs2))
              else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val mull : val0 -> val0 -> val0 **)

  let mull v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vlong n2 -> Vlong (Int64.mul n1 n2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val divlu : val0 -> val0 -> val0 option **)

  let divlu v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vlong n2 ->
         if Int64.eq n2 Int64.zero
         then None
         else Some (Vlong (Int64.divu n1 n2))
       | _ -> None)
    | _ -> None

  (** val modlu : val0 -> val0 -> val0 option **)

  let modlu v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vlong n2 ->
         if Int64.eq n2 Int64.zero
         then None
         else Some (Vlong (Int64.modu n1 n2))
       | _ -> None)
    | _ -> None

  (** val andl : val0 -> val0 -> val0 **)

  let andl v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vlong n2 -> Vlong (Int64.coq_and n1 n2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val orl : val0 -> val0 -> val0 **)

  let orl v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vlong n2 -> Vlong (Int64.coq_or n1 n2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val xorl : val0 -> val0 -> val0 **)

  let xorl v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vlong n2 -> Vlong (Int64.xor n1 n2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val shll : val0 -> val0 -> val0 **)

  let shll v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.ltu n2 Int64.iwordsize'
         then Vlong (Int64.shl' n1 n2)
         else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val shrl : val0 -> val0 -> val0 **)

  let shrl v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.ltu n2 Int64.iwordsize'
         then Vlong (Int64.shr' n1 n2)
         else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val shrlu : val0 -> val0 -> val0 **)

  let shrlu v1 v2 =
    match v1 with
    | Vlong n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.ltu n2 Int64.iwordsize'
         then Vlong (Int64.shru' n1 n2)
         else Vundef
       | _ -> Vundef)
    | _ -> Vundef

  (** val cmp_different_blocks : comparison0 -> bool option **)

  let cmp_different_blocks = function
  | Ceq -> Some false
  | Cne -> Some true
  | _ -> None

  (** val cmpu_bool :
      (block -> int -> bool) -> comparison0 -> val0 -> val0 -> bool option **)

  let cmpu_bool valid_ptr =
    let weak_valid_ptr = fun b ofs ->
      (||) (valid_ptr b ofs) (valid_ptr b (Z.sub ofs 1))
    in
    (fun c v1 v2 ->
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 -> Some (Int.cmpu c n1 n2)
       | Vptr (b2, ofs2) ->
         if ptr64
         then None
         else if (&&) (Int.eq n1 Int.zero)
                   (weak_valid_ptr b2 (Ptrofs.unsigned ofs2))
              then cmp_different_blocks c
              else None
       | _ -> None)
    | Vptr (b1, ofs1) ->
      (match v2 with
       | Vint n2 ->
         if ptr64
         then None
         else if (&&) (Int.eq n2 Int.zero)
                   (weak_valid_ptr b1 (Ptrofs.unsigned ofs1))
              then cmp_different_blocks c
              else None
       | Vptr (b2, ofs2) ->
         if ptr64
         then None
         else if eq_block b1 b2
              then if (&&) (weak_valid_ptr b1 (Ptrofs.unsigned ofs1))
                        (weak_valid_ptr b2 (Ptrofs.unsigned ofs2))
                   then Some (Ptrofs.cmpu c ofs1 ofs2)
                   else None
              else if (&&) (valid_ptr b1 (Ptrofs.unsigned ofs1))
                        (valid_ptr b2 (Ptrofs.unsigned ofs2))
                   then cmp_different_blocks c
                   else None
       | _ -> None)
    | _ -> None)

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
    (fun m0 ->
    (Byte.repr x) :: (bytes_of_int m0
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
| m0 :: vl' ->
  (match m0 with
   | Byte b ->
     (match proj_bytes vl' with
      | Some bl -> Some (b :: bl)
      | None -> None)
   | _ -> None)

(** val inj_value_rec : int -> val0 -> quantity -> memval list **)

let rec inj_value_rec n0 v q =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m0 -> (Fragment (v, q, m0)) :: (inj_value_rec m0 v q))
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
    (fun m0 ->
    match vl with
    | [] -> false
    | m1 :: vl' ->
      (match m1 with
       | Fragment (v', q', m') ->
         (&&)
           ((&&)
             ((&&) (proj_sumbool (Val.eq v v'))
               (proj_sumbool (quantity_eq q q'))) ((=) m0 m'))
           (check_value m0 v q vl')
       | _ -> false))
    n0

(** val proj_value : quantity -> memval list -> val0 **)

let proj_value q vl = match vl with
| [] -> Vundef
| m0 :: _ ->
  (match m0 with
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

  let mem_contents m0 =
    m0.mem_contents

  (** val mem_access :
      mem' -> (int -> perm_kind -> permission option) PMap.t **)

  let mem_access m0 =
    m0.mem_access

  (** val nextblock : mem' -> block **)

  let nextblock m0 =
    m0.nextblock

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

  let perm_dec m0 b ofs k p =
    perm_order'_dec (PMap.get b m0.mem_access ofs k) p

  (** val range_perm_dec :
      mem -> block -> int -> int -> perm_kind -> permission -> bool **)

  let rec range_perm_dec m0 b lo hi k p =
    let s = zlt lo hi in
    if s
    then let s0 = perm_dec m0 b lo k p in
         if s0
         then let y = Z.add lo 1 in range_perm_dec m0 b y hi k p
         else false
    else true

  (** val valid_access_dec :
      mem -> memory_chunk -> block -> int -> permission -> bool **)

  let valid_access_dec m0 chunk b ofs p =
    let s = range_perm_dec m0 b ofs (Z.add ofs (size_chunk chunk)) Cur p in
    if s then zdivide_dec (align_chunk chunk) ofs else false

  (** val valid_pointer : mem -> block -> int -> bool **)

  let valid_pointer m0 b ofs =
    proj_sumbool (perm_dec m0 b ofs Cur Nonempty)

  (** val empty : mem **)

  let empty =
    { mem_contents = (PMap.init (ZMap.init Undef)); mem_access =
      (PMap.init (fun _ _ -> None)); nextblock = 1 }

  (** val alloc : mem -> int -> int -> mem' * block **)

  let alloc m0 lo hi =
    ({ mem_contents =
      (PMap.set m0.nextblock (ZMap.init Undef) m0.mem_contents); mem_access =
      (PMap.set m0.nextblock (fun ofs _ ->
        if (&&) (proj_sumbool (zle lo ofs)) (proj_sumbool (zlt ofs hi))
        then Some Freeable
        else None) m0.mem_access); nextblock = (Coq_Pos.succ m0.nextblock) },
      m0.nextblock)

  (** val getN : int -> int -> memval ZMap.t -> memval list **)

  let rec getN n0 p c =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> [])
      (fun n' -> (ZMap.get p c) :: (getN n' (Z.add p 1) c))
      n0

  (** val load : memory_chunk -> mem -> block -> int -> val0 option **)

  let load chunk m0 b ofs =
    if valid_access_dec m0 chunk b ofs Readable
    then Some
           (decode_val chunk
             (getN (size_chunk_nat chunk) ofs (PMap.get b m0.mem_contents)))
    else None

  (** val loadv : memory_chunk -> mem -> val0 -> val0 option **)

  let loadv chunk m0 = function
  | Vptr (b, ofs) -> load chunk m0 b (Ptrofs.unsigned ofs)
  | _ -> None

  (** val setN : memval list -> int -> memval ZMap.t -> memval ZMap.t **)

  let rec setN vl p c =
    match vl with
    | [] -> c
    | v :: vl' -> setN vl' (Z.add p 1) (ZMap.set p v c)

  (** val store :
      memory_chunk -> mem -> block -> int -> val0 -> mem option **)

  let store chunk m0 b ofs v =
    if valid_access_dec m0 chunk b ofs Writable
    then Some { mem_contents =
           (PMap.set b
             (setN (encode_val chunk v) ofs (PMap.get b m0.mem_contents))
             m0.mem_contents); mem_access = m0.mem_access; nextblock =
           m0.nextblock }
    else None

  (** val storev : memory_chunk -> mem -> val0 -> val0 -> mem option **)

  let storev chunk m0 addr v =
    match addr with
    | Vptr (b, ofs) -> store chunk m0 b (Ptrofs.unsigned ofs) v
    | _ -> None

  (** val storebytes : mem -> block -> int -> memval list -> mem option **)

  let storebytes m0 b ofs bytes =
    if range_perm_dec m0 b ofs (Z.add ofs (Z.of_nat (length bytes))) Cur
         Writable
    then Some { mem_contents =
           (PMap.set b (setN bytes ofs (PMap.get b m0.mem_contents))
             m0.mem_contents); mem_access = m0.mem_access; nextblock =
           m0.nextblock }
    else None
 end

type signedness =
| Signed
| Unsigned

let string_of_signedness s =
  match s with
  | Signed -> "s"
  | Unsigned -> ""
  
module List64AsArray =
 struct
  type t = int list

  (** val index : t -> int -> int **)

  let index l idx =
    match nth_error l (Z.to_nat (Int.unsigned idx)) with
    | Some i -> i
    | None -> Int64.zero
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

let string_of_flag f =
  match f with
  | BPF_SUCC_RETURN -> "BPF_SUCC_RETURN"
  | BPF_OK -> "BPF_OK"
  | BPF_ILLEGAL_INSTRUCTION -> "BPF_ILLEGAL_INSTRUCTION"
  | BPF_ILLEGAL_MEM -> "BPF_ILLEGAL_MEM"
  | BPF_ILLEGAL_JUMP -> "BPF_ILLEGAL_JUMP"
  | BPF_ILLEGAL_CALL -> "BPF_ILLEGAL_CALL"
  | BPF_ILLEGAL_LEN -> "BPF_ILLEGAL_LEN"
  | BPF_ILLEGAL_REGISTER -> "BPF_ILLEGAL_REGISTER"
  | BPF_NO_RETURN -> "BPF_NO_RETURN"
  | BPF_OUT_OF_BRANCHES -> "BPF_OUT_OF_BRANCHES"
  | BPF_ILLEGAL_DIV -> "BPF_ILLEGAL_DIV"
  | BPF_ILLEGAL_SHIFT -> "BPF_ILLEGAL_SHIFT"
  | BPF_ILLEGAL_ALU -> "BPF_ILLEGAL_ALU"
  
(** val bpf_flag_eq : bpf_flag -> bpf_flag -> bool **)

let bpf_flag_eq x y =
  match x with
  | BPF_SUCC_RETURN -> (match y with
                        | BPF_SUCC_RETURN -> true
                        | _ -> false)
  | BPF_OK -> (match y with
               | BPF_OK -> true
               | _ -> false)
  | BPF_ILLEGAL_INSTRUCTION ->
    (match y with
     | BPF_ILLEGAL_INSTRUCTION -> true
     | _ -> false)
  | BPF_ILLEGAL_MEM -> (match y with
                        | BPF_ILLEGAL_MEM -> true
                        | _ -> false)
  | BPF_ILLEGAL_JUMP -> (match y with
                         | BPF_ILLEGAL_JUMP -> true
                         | _ -> false)
  | BPF_ILLEGAL_CALL -> (match y with
                         | BPF_ILLEGAL_CALL -> true
                         | _ -> false)
  | BPF_ILLEGAL_LEN -> (match y with
                        | BPF_ILLEGAL_LEN -> true
                        | _ -> false)
  | BPF_ILLEGAL_REGISTER ->
    (match y with
     | BPF_ILLEGAL_REGISTER -> true
     | _ -> false)
  | BPF_NO_RETURN -> (match y with
                      | BPF_NO_RETURN -> true
                      | _ -> false)
  | BPF_OUT_OF_BRANCHES ->
    (match y with
     | BPF_OUT_OF_BRANCHES -> true
     | _ -> false)
  | BPF_ILLEGAL_DIV -> (match y with
                        | BPF_ILLEGAL_DIV -> true
                        | _ -> false)
  | BPF_ILLEGAL_SHIFT -> (match y with
                          | BPF_ILLEGAL_SHIFT -> true
                          | _ -> false)
  | BPF_ILLEGAL_ALU -> (match y with
                        | BPF_ILLEGAL_ALU -> true
                        | _ -> false)

(** val flag_eq : bpf_flag -> bpf_flag -> bool **)

let flag_eq x y =
  if bpf_flag_eq x y then true else false

(** val cmp_ptr32_null : Mem.mem -> val0 -> bool option **)

let cmp_ptr32_null m0 v =
  Val.cmpu_bool (Mem.valid_pointer m0) Ceq v vnullptr

(** val val32_modu : val0 -> val0 -> val0 **)

let val32_modu x y =
  match Val.modu x y with
  | Some res -> res
  | None -> Vundef

(** val comp_eq_32 : val0 -> val0 -> bool **)

let comp_eq_32 x y =
  match x with
  | Vint n1 -> (match y with
                | Vint n2 -> Int.eq n1 n2
                | _ -> false)
  | _ -> false

(** val comp_ne_32 : val0 -> val0 -> bool **)

let comp_ne_32 x y =
  match x with
  | Vint n1 -> (match y with
                | Vint n2 -> negb (Int.eq n1 n2)
                | _ -> false)
  | _ -> false

(** val compu_lt_32 : val0 -> val0 -> bool **)

let compu_lt_32 x y =
  match x with
  | Vint n1 -> (match y with
                | Vint n2 -> Int.ltu n1 n2
                | _ -> false)
  | _ -> false

(** val compu_le_32 : val0 -> val0 -> bool **)

let compu_le_32 x y =
  match x with
  | Vint n1 -> (match y with
                | Vint n2 -> negb (Int.ltu n2 n1)
                | _ -> false)
  | _ -> false

(** val compl_eq : val0 -> val0 -> bool **)

let compl_eq x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> Int64.eq n1 n2
                 | _ -> false)
  | _ -> false

(** val compl_ne : val0 -> val0 -> bool **)

let compl_ne x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> negb (Int64.eq n1 n2)
                 | _ -> false)
  | _ -> false

(** val compl_lt : val0 -> val0 -> bool **)

let compl_lt x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> Int64.lt n1 n2
                 | _ -> false)
  | _ -> false

(** val compl_le : val0 -> val0 -> bool **)

let compl_le x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> negb (Int64.lt n2 n1)
                 | _ -> false)
  | _ -> false

(** val compl_gt : val0 -> val0 -> bool **)

let compl_gt x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> Int64.lt n2 n1
                 | _ -> false)
  | _ -> false

(** val compl_ge : val0 -> val0 -> bool **)

let compl_ge x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> negb (Int64.lt n1 n2)
                 | _ -> false)
  | _ -> false

(** val complu_lt : val0 -> val0 -> bool **)

let complu_lt x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> Int64.ltu n1 n2
                 | _ -> false)
  | _ -> false

(** val complu_le : val0 -> val0 -> bool **)

let complu_le x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> negb (Int64.ltu n2 n1)
                 | _ -> false)
  | _ -> false

(** val complu_gt : val0 -> val0 -> bool **)

let complu_gt x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> Int64.ltu n2 n1
                 | _ -> false)
  | _ -> false

(** val complu_ge : val0 -> val0 -> bool **)

let complu_ge x y =
  match x with
  | Vlong n1 -> (match y with
                 | Vlong n2 -> negb (Int64.ltu n1 n2)
                 | _ -> false)
  | _ -> false

(** val complu_set : val0 -> val0 -> bool **)

let complu_set x y =
  match x with
  | Vlong n1 ->
    (match y with
     | Vlong n2 -> negb (Int64.eq (Int64.coq_and n1 n2) Int64.zero)
     | _ -> false)
  | _ -> false

(** val val_intuoflongu : val0 -> val0 **)

let val_intuoflongu = function
| Vlong n0 -> Vint (Int.repr (Int64.unsigned n0))
| _ -> Vundef

(** val sint32_to_vint : int -> val0 **)

let sint32_to_vint v =
  Vint v

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

let string_of_reg r =
  match r with
  | R0 -> "R0"
  | R1 -> "R1"
  | R2 -> "R2"
  | R3 -> "R3"
  | R4 -> "R4"
  | R5 -> "R5"
  | R6 -> "R6"
  | R7 -> "R7"
  | R8 -> "R8"
  | R9 -> "R9"
  | R10 -> "R10"
  
type regmap = { r0_val : val0; r1_val : val0; r2_val : val0; r3_val : 
                val0; r4_val : val0; r5_val : val0; r6_val : val0;
                r7_val : val0; r8_val : val0; r9_val : val0; r10_val : 
                val0 }

let print_regmap rs =
  let _ = print_endline ("R0= " ^ (string_of_val0 rs.r0_val)) in
  let _ = print_endline ("R1= " ^ (string_of_val0 rs.r1_val)) in
  let _ = print_endline ("R2= " ^ (string_of_val0 rs.r2_val)) in
  let _ = print_endline ("R3= " ^ (string_of_val0 rs.r3_val)) in
  let _ = print_endline ("R4= " ^ (string_of_val0 rs.r4_val)) in
  let _ = print_endline ("R5= " ^ (string_of_val0 rs.r5_val)) in
  let _ = print_endline ("R6= " ^ (string_of_val0 rs.r6_val)) in
  let _ = print_endline ("R7= " ^ (string_of_val0 rs.r7_val)) in
  let _ = print_endline ("R8= " ^ (string_of_val0 rs.r8_val)) in
  let _ = print_endline ("R9= " ^ (string_of_val0 rs.r9_val)) in
    print_endline ("R10= " ^ (string_of_val0 rs.r10_val))
    
(** val eval_regmap : reg -> regmap -> val0 **)

let eval_regmap r regs =
  match r with
  | R0 -> regs.r0_val
  | R1 -> regs.r1_val
  | R2 -> regs.r2_val
  | R3 -> regs.r3_val
  | R4 -> regs.r4_val
  | R5 -> regs.r5_val
  | R6 -> regs.r6_val
  | R7 -> regs.r7_val
  | R8 -> regs.r8_val
  | R9 -> regs.r9_val
  | R10 -> regs.r10_val

(** val upd_regmap : reg -> val0 -> regmap -> regmap **)

let upd_regmap r v regs =
  match r with
  | R0 ->
    { r0_val = v; r1_val = regs.r1_val; r2_val = regs.r2_val; r3_val =
      regs.r3_val; r4_val = regs.r4_val; r5_val = regs.r5_val; r6_val =
      regs.r6_val; r7_val = regs.r7_val; r8_val = regs.r8_val; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R1 ->
    { r0_val = regs.r0_val; r1_val = v; r2_val = regs.r2_val; r3_val =
      regs.r3_val; r4_val = regs.r4_val; r5_val = regs.r5_val; r6_val =
      regs.r6_val; r7_val = regs.r7_val; r8_val = regs.r8_val; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R2 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = v; r3_val =
      regs.r3_val; r4_val = regs.r4_val; r5_val = regs.r5_val; r6_val =
      regs.r6_val; r7_val = regs.r7_val; r8_val = regs.r8_val; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R3 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = regs.r2_val;
      r3_val = v; r4_val = regs.r4_val; r5_val = regs.r5_val; r6_val =
      regs.r6_val; r7_val = regs.r7_val; r8_val = regs.r8_val; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R4 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = regs.r2_val;
      r3_val = regs.r3_val; r4_val = v; r5_val = regs.r5_val; r6_val =
      regs.r6_val; r7_val = regs.r7_val; r8_val = regs.r8_val; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R5 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = regs.r2_val;
      r3_val = regs.r3_val; r4_val = regs.r4_val; r5_val = v; r6_val =
      regs.r6_val; r7_val = regs.r7_val; r8_val = regs.r8_val; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R6 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = regs.r2_val;
      r3_val = regs.r3_val; r4_val = regs.r4_val; r5_val = regs.r5_val;
      r6_val = v; r7_val = regs.r7_val; r8_val = regs.r8_val; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R7 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = regs.r2_val;
      r3_val = regs.r3_val; r4_val = regs.r4_val; r5_val = regs.r5_val;
      r6_val = regs.r6_val; r7_val = v; r8_val = regs.r8_val; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R8 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = regs.r2_val;
      r3_val = regs.r3_val; r4_val = regs.r4_val; r5_val = regs.r5_val;
      r6_val = regs.r6_val; r7_val = regs.r7_val; r8_val = v; r9_val =
      regs.r9_val; r10_val = regs.r10_val }
  | R9 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = regs.r2_val;
      r3_val = regs.r3_val; r4_val = regs.r4_val; r5_val = regs.r5_val;
      r6_val = regs.r6_val; r7_val = regs.r7_val; r8_val = regs.r8_val;
      r9_val = v; r10_val = regs.r10_val }
  | R10 ->
    { r0_val = regs.r0_val; r1_val = regs.r1_val; r2_val = regs.r2_val;
      r3_val = regs.r3_val; r4_val = regs.r4_val; r5_val = regs.r5_val;
      r6_val = regs.r6_val; r7_val = regs.r7_val; r8_val = regs.r8_val;
      r9_val = regs.r9_val; r10_val = v }

(** val val64_zero : val0 **)

let val64_zero =
  Vlong Int64.zero

(** val perm_ge : permission -> permission -> bool **)

let perm_ge x y =
  if Mem.perm_order_dec x y then true else false

type memory_region = { start_addr : val0; block_size : val0;
                       block_perm : permission; block_ptr : val0 }

module Memory_regions =
 struct
  type t = memory_region list
 end

type myMemRegionsType = Memory_regions.t

(** val well_chunk_Z : memory_chunk -> int **)

let well_chunk_Z = function
| Mint8unsigned -> 1
| Mint16unsigned -> ((fun p->2*p) 1)
| Mint32 -> ((fun p->2*p) ((fun p->2*p) 1))
| Mint64 -> ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
| _ -> ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))

(** val memory_chunk_to_valu32 : memory_chunk -> val0 **)

let memory_chunk_to_valu32 chunk =
  Vint (Int.repr (well_chunk_Z chunk))

(** val memory_chunk_to_valu32_upbound : memory_chunk -> val0 **)

let memory_chunk_to_valu32_upbound chunk =
  Vint (Int.repr (Z.sub Int.max_unsigned (well_chunk_Z chunk)))

(** val _to_vlong : val0 -> val0 option **)

let _to_vlong = function
| Vint n0 -> Some (Vlong (Int64.repr (Int.unsigned n0)))
| Vlong n0 -> Some (Vlong n0)
| _ -> None

(** val vlong_to_vint_or_vlong : memory_chunk -> val0 -> val0 **)

let vlong_to_vint_or_vlong chunk = function
| Vlong n0 ->
  (match chunk with
   | Mint8unsigned ->
     Vint
       (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
         (Int.repr (Int64.unsigned n0)))
   | Mint16unsigned ->
     Vint
       (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
         1)))) (Int.repr (Int64.unsigned n0)))
   | Mint32 -> Vint (Int.repr (Int64.unsigned n0))
   | Mint64 -> Vlong n0
   | _ -> Vundef)
| _ -> Vundef

(** val vint_to_vint_or_vlong : memory_chunk -> val0 -> val0 **)

let vint_to_vint_or_vlong chunk = function
| Vint n0 ->
  (match chunk with
   | Mint8unsigned ->
     Vint (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))) n0)
   | Mint16unsigned ->
     Vint
       (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
         1)))) n0)
   | Mint32 -> Vint n0
   | Mint64 -> Vlong (Int64.repr (Int.signed n0))
   | _ -> Vundef)
| _ -> Vundef

type state = { pc_loc : int; flag : bpf_flag; regs_st : regmap;
               mrs_num : int; bpf_mrs : myMemRegionsType; ins_len : int;
               ins : List64AsArray.t; bpf_m : Mem.mem }
  
let print_state st =
  let _ = print_endline "****************bpf state***************" in
  let _ = print_endline ("pc:= " ^ string_of_int st.pc_loc) in
  let _ = print_endline ("flag:= " ^ string_of_flag st.flag) in
  let _ = print_regmap st.regs_st in
    print_endline "****************bpf state***************\n\n"
    
(** val eval_pc : state -> int **)

let eval_pc st =
  st.pc_loc

(** val upd_pc : int -> state -> state **)

let upd_pc p st =
  { pc_loc = p; flag = st.flag; regs_st = st.regs_st; mrs_num = st.mrs_num;
    bpf_mrs = st.bpf_mrs; ins_len = st.ins_len; ins = st.ins; bpf_m =
    st.bpf_m }

(** val upd_pc_incr : state -> state **)

let upd_pc_incr st =
  { pc_loc = (Int.add st.pc_loc Int.one); flag = st.flag; regs_st =
    st.regs_st; mrs_num = st.mrs_num; bpf_mrs = st.bpf_mrs; ins_len =
    st.ins_len; ins = st.ins; bpf_m = st.bpf_m }

(** val eval_flag : state -> bpf_flag **)

let eval_flag st =
  st.flag

(** val upd_flag : bpf_flag -> state -> state **)

let upd_flag f st =
  { pc_loc = st.pc_loc; flag = f; regs_st = st.regs_st; mrs_num = st.mrs_num;
    bpf_mrs = st.bpf_mrs; ins_len = st.ins_len; ins = st.ins; bpf_m =
    st.bpf_m }

(** val eval_mem_num : state -> int **)

let eval_mem_num st =
  st.mrs_num

(** val eval_reg : reg -> state -> val0 **)

let eval_reg r st =
  eval_regmap r st.regs_st

(** val upd_reg : reg -> val0 -> state -> state **)

let upd_reg r v st =
  { pc_loc = st.pc_loc; flag = st.flag; regs_st =
    (upd_regmap r v st.regs_st); mrs_num = st.mrs_num; bpf_mrs = st.bpf_mrs;
    ins_len = st.ins_len; ins = st.ins; bpf_m = st.bpf_m }

(** val eval_mem_regions : state -> myMemRegionsType **)

let eval_mem_regions st =
  st.bpf_mrs

(** val eval_mem : state -> Mem.mem **)

let eval_mem st =
  st.bpf_m

(** val upd_mem : Mem.mem -> state -> state **)

let upd_mem m0 st =
  { pc_loc = st.pc_loc; flag = st.flag; regs_st = st.regs_st; mrs_num =
    st.mrs_num; bpf_mrs = st.bpf_mrs; ins_len = st.ins_len; ins = st.ins;
    bpf_m = m0 }

(** val load_mem : memory_chunk -> val0 -> state -> val0 option **)

let load_mem chunk ptr st =
  match chunk with
  | Mint8unsigned ->
    (match Mem.loadv chunk st.bpf_m ptr with
     | Some res -> _to_vlong res
     | None -> None)
  | Mint16unsigned ->
    (match Mem.loadv chunk st.bpf_m ptr with
     | Some res -> _to_vlong res
     | None -> None)
  | Mint32 ->
    (match Mem.loadv chunk st.bpf_m ptr with
     | Some res -> _to_vlong res
     | None -> None)
  | Mint64 -> Mem.loadv chunk st.bpf_m ptr
  | _ -> None

(** val store_mem_imm :
    val0 -> memory_chunk -> val0 -> state -> state option **)

let store_mem_imm ptr chunk v st =
  match chunk with
  | Mint8unsigned ->
    let src = vint_to_vint_or_vlong chunk v in
    (match Mem.storev chunk st.bpf_m ptr src with
     | Some m0 -> Some (upd_mem m0 st)
     | None -> None)
  | Mint16unsigned ->
    let src = vint_to_vint_or_vlong chunk v in
    (match Mem.storev chunk st.bpf_m ptr src with
     | Some m0 -> Some (upd_mem m0 st)
     | None -> None)
  | Mint32 ->
    let src = vint_to_vint_or_vlong chunk v in
    (match Mem.storev chunk st.bpf_m ptr src with
     | Some m0 -> Some (upd_mem m0 st)
     | None -> None)
  | Mint64 ->
    let src = vint_to_vint_or_vlong chunk v in
    (match Mem.storev chunk st.bpf_m ptr src with
     | Some m0 -> Some (upd_mem m0 st)
     | None -> None)
  | _ -> None

(** val store_mem_reg :
    val0 -> memory_chunk -> val0 -> state -> state option **)

let store_mem_reg ptr chunk v st =
  match chunk with
  | Mint8unsigned ->
    let src = vlong_to_vint_or_vlong chunk v in
    (match Mem.storev chunk st.bpf_m ptr src with
     | Some m0 -> Some (upd_mem m0 st)
     | None -> None)
  | Mint16unsigned ->
    let src = vlong_to_vint_or_vlong chunk v in
    (match Mem.storev chunk st.bpf_m ptr src with
     | Some m0 -> Some (upd_mem m0 st)
     | None -> None)
  | Mint32 ->
    let src = vlong_to_vint_or_vlong chunk v in
    (match Mem.storev chunk st.bpf_m ptr src with
     | Some m0 -> Some (upd_mem m0 st)
     | None -> None)
  | Mint64 ->
    let src = vlong_to_vint_or_vlong chunk v in
    (match Mem.storev chunk st.bpf_m ptr src with
     | Some m0 -> Some (upd_mem m0 st)
     | None -> None)
  | _ -> None

(** val eval_ins_len : state -> int **)

let eval_ins_len st =
  Int.repr (Z.of_nat st.ins_len)

(** val eval_ins : int -> state -> int **)

let eval_ins idx st =
  List64AsArray.index st.ins idx

type ('st, 'a) m = 'st -> ('a * 'st) option

(** val returnM : 'a1 -> ('a2, 'a1) m **)

let returnM a st =
  Some (a, st)

(** val errorM : ('a2, 'a1) m **)

let errorM _ =
  None

(** val bindM : ('a3, 'a1) m -> ('a1 -> ('a3, 'a2) m) -> ('a3, 'a2) m **)

let bindM x f st =
  match x st with
  | Some p -> let (x', st') = p in f x' st'
  | None -> None

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

(** val int64_to_dst_reg' : int -> reg option **)

let int64_to_dst_reg' ins0 =
  z_to_reg (get_dst ins0)

(** val int64_to_src_reg' : int -> reg option **)

let int64_to_src_reg' ins0 =
  z_to_reg (get_src ins0)

(** val get_opcode : int -> int **)

let get_opcode ins0 =
  Z.to_nat
    (Int64.unsigned
      (Int64.coq_and ins0
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

type arch =
| A32
| A64

let string_of_arch a =
  match a with
  | A32 -> "32"
  | A64 -> "64"
  
type cond =
| Eq0
| Gt0 of signedness
| Ge of signedness
| Lt0 of signedness
| Le of signedness
| SEt
| Ne

let strng_of_cond cd =
  match cd with
  | Eq0 -> "eq"
  | Gt0 s -> (string_of_signedness s) ^ "gt"
  | Ge s -> (string_of_signedness s) ^ "ge"
  | Lt0 s -> (string_of_signedness s) ^ "lt"
  | Le s -> (string_of_signedness s) ^ "le"
  | SEt -> "set"
  | Ne ->"ne"
  
type off = int

type imm = int

type binOp =
| BPF_ADD
| BPF_SUB
| BPF_MUL
| BPF_DIV
| BPF_OR
| BPF_AND
| BPF_LSH
| BPF_RSH
| BPF_MOD
| BPF_XOR
| BPF_MOV
| BPF_ARSH

type instruction =
| BPF_NEG of arch * reg
| BPF_BINARY of arch * binOp * reg * (reg, imm) sum
| BPF_JA of off
| BPF_JUMP of cond * reg * (reg, imm) sum * off
| BPF_LDDW_low of reg * imm
| BPF_LDDW_high of reg * imm
| BPF_LDX of memory_chunk * reg * reg * off
| BPF_ST of memory_chunk * reg * (reg, imm) sum * off
| BPF_CALL of imm
| BPF_RET
| BPF_ERR

let string_of_instruction ins =
  match ins with
  | BPF_NEG (a, r) -> "neg" ^ (string_of_arch a) ^ " " ^ (string_of_reg r)
  | BPF_BINARY (a, bop, dst, src) ->
    (match bop with
    | BPF_ADD -> "add"
    | BPF_SUB -> "sub"
    | BPF_MUL -> "mul"
    | BPF_DIV -> "div"
    | BPF_OR  -> "or"
    | BPF_AND -> "and"
    | BPF_LSH -> "lsh"
    | BPF_RSH -> "rsh"
    | BPF_MOD -> "mod"
    | BPF_XOR -> "xor"
    | BPF_MOV -> "mov"
    | BPF_ARSH -> "arsh" ) ^ (string_of_arch a) ^ " " ^ (string_of_reg dst) ^ " " ^ 
    (match src with
    | Inl r -> string_of_reg r
    | Inr i -> string_of_int i)
    
  | BPF_JA o -> "JA " ^ (string_of_int o)
  | BPF_JUMP (cd, dst, src, o) ->
    "j" ^ (strng_of_cond cd) ^ " " ^ (string_of_reg dst) ^ " " ^ 
    (match src with
    | Inl r -> string_of_reg r
    | Inr i -> string_of_int i) ^ " " ^ (string_of_int o)
    
  | BPF_LDDW_low (dst, i) -> (string_of_reg dst) ^ " = " ^ (string_of_int i) ^ " (lddw)"
  | BPF_LDDW_high (dst, i) -> (string_of_reg dst) ^ " = (" ^ (string_of_int i) ^ " <<32)"
  | BPF_LDX (mc, dst, src, o) ->
    (string_of_reg dst) ^ " = *(u" ^ (string_of_memory_chunk mc) ^ " *)(" ^
        (string_of_reg src) ^ " + " ^ (string_of_int o) ^ ")"
  | BPF_ST (mc, dst, src, o) ->
    "*(u" ^ (string_of_memory_chunk mc) ^ " *)(" ^ (string_of_reg dst) ^ " + " ^ (string_of_int o) ^ ") = " ^
    (match src with
    | Inl r -> string_of_reg r
    | Inr i -> string_of_int i) 
  | BPF_CALL i -> "call " ^ (string_of_int i)
  | BPF_RET -> "exit"
  | BPF_ERR -> "error"
  
(** val get_instruction_alu64_imm :
    int -> reg -> int -> int -> instruction **)

let get_instruction_alu64_imm _ rd i op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_BINARY (A64, BPF_ADD, rd, (Inr i)))
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ -> BPF_BINARY (A64,
                                                  BPF_SUB, rd, (Inr
                                                  i)))
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_ERR)
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_ERR)
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_ERR)
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_MUL,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_DIV,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_OR,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_AND,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_LSH,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_RSH,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n121 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n122 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n123 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n124 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n125 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n126 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n127 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n128 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n129 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n130 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n131 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n132 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n133 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n134 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_NEG
                                                                    (A64,
                                                                    rd))
                                                                    (fun n135 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n136 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n137 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n138 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n139 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n140 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n141 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n142 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n143 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n144 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n145 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n146 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n147 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n148 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n149 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n150 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_MOD,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n151 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n152 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n153 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n154 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n155 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n156 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n157 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n158 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n159 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n160 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n161 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n162 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n163 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n164 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n165 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n166 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_XOR,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n167 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n168 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n169 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n170 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n171 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n172 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n173 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n174 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n175 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n176 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n177 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n178 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n179 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n180 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n181 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n182 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_MOV,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n183 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n184 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n185 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n186 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n187 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n188 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n189 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n190 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n191 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n192 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n193 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n194 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n195 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n196 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n197 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n198 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_ARSH,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n198)
                                                                    n197)
                                                                    n196)
                                                                    n195)
                                                                    n194)
                                                                    n193)
                                                                    n192)
                                                                    n191)
                                                                    n190)
                                                                    n189)
                                                                    n188)
                                                                    n187)
                                                                    n186)
                                                                    n185)
                                                                    n184)
                                                                    n183)
                                                                    n182)
                                                                    n181)
                                                                    n180)
                                                                    n179)
                                                                    n178)
                                                                    n177)
                                                                    n176)
                                                                    n175)
                                                                    n174)
                                                                    n173)
                                                                    n172)
                                                                    n171)
                                                                    n170)
                                                                    n169)
                                                                    n168)
                                                                    n167)
                                                                    n166)
                                                                    n165)
                                                                    n164)
                                                                    n163)
                                                                    n162)
                                                                    n161)
                                                                    n160)
                                                                    n159)
                                                                    n158)
                                                                    n157)
                                                                    n156)
                                                                    n155)
                                                                    n154)
                                                                    n153)
                                                                    n152)
                                                                    n151)
                                                                    n150)
                                                                    n149)
                                                                    n148)
                                                                    n147)
                                                                    n146)
                                                                    n145)
                                                                    n144)
                                                                    n143)
                                                                    n142)
                                                                    n141)
                                                                    n140)
                                                                    n139)
                                                                    n138)
                                                                    n137)
                                                                    n136)
                                                                    n135)
                                                                    n134)
                                                                    n133)
                                                                    n132)
                                                                    n131)
                                                                    n130)
                                                                    n129)
                                                                    n128)
                                                                    n127)
                                                                    n126)
                                                                    n125)
                                                                    n124)
                                                                    n123)
                                                                    n122)
                                                                    n121)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    op

(** val get_instruction_alu64_reg :
    int -> reg -> reg -> int -> instruction **)

let get_instruction_alu64_reg _ rd rs op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_BINARY (A64, BPF_ADD, rd,
                                  (Inl rs)))
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_ERR)
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_ERR)
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_BINARY
                                                                  (A64,
                                                                  BPF_SUB,
                                                                  rd, (Inl
                                                                  rs)))
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_MUL,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_DIV,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_OR,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_AND,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_LSH,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n121 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n122 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n123 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n124 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n125 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n126 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_RSH,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n127 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n128 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n129 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n130 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n131 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n132 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n133 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n134 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n135 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n136 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n137 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n138 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n139 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n140 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n141 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n142 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n143 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n144 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n145 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n146 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n147 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n148 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n149 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n150 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n151 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n152 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n153 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n154 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n155 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n156 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n157 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n158 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_MOD,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n159 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n160 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n161 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n162 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n163 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n164 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n165 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n166 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n167 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n168 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n169 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n170 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n171 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n172 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n173 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n174 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_XOR,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n175 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n176 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n177 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n178 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n179 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n180 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n181 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n182 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n183 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n184 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n185 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n186 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n187 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n188 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n189 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n190 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_MOV,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n191 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n192 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n193 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n194 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n195 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n196 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n197 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n198 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n199 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n200 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n201 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n202 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n203 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n204 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n205 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n206 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A64,
                                                                    BPF_ARSH,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n206)
                                                                    n205)
                                                                    n204)
                                                                    n203)
                                                                    n202)
                                                                    n201)
                                                                    n200)
                                                                    n199)
                                                                    n198)
                                                                    n197)
                                                                    n196)
                                                                    n195)
                                                                    n194)
                                                                    n193)
                                                                    n192)
                                                                    n191)
                                                                    n190)
                                                                    n189)
                                                                    n188)
                                                                    n187)
                                                                    n186)
                                                                    n185)
                                                                    n184)
                                                                    n183)
                                                                    n182)
                                                                    n181)
                                                                    n180)
                                                                    n179)
                                                                    n178)
                                                                    n177)
                                                                    n176)
                                                                    n175)
                                                                    n174)
                                                                    n173)
                                                                    n172)
                                                                    n171)
                                                                    n170)
                                                                    n169)
                                                                    n168)
                                                                    n167)
                                                                    n166)
                                                                    n165)
                                                                    n164)
                                                                    n163)
                                                                    n162)
                                                                    n161)
                                                                    n160)
                                                                    n159)
                                                                    n158)
                                                                    n157)
                                                                    n156)
                                                                    n155)
                                                                    n154)
                                                                    n153)
                                                                    n152)
                                                                    n151)
                                                                    n150)
                                                                    n149)
                                                                    n148)
                                                                    n147)
                                                                    n146)
                                                                    n145)
                                                                    n144)
                                                                    n143)
                                                                    n142)
                                                                    n141)
                                                                    n140)
                                                                    n139)
                                                                    n138)
                                                                    n137)
                                                                    n136)
                                                                    n135)
                                                                    n134)
                                                                    n133)
                                                                    n132)
                                                                    n131)
                                                                    n130)
                                                                    n129)
                                                                    n128)
                                                                    n127)
                                                                    n126)
                                                                    n125)
                                                                    n124)
                                                                    n123)
                                                                    n122)
                                                                    n121)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    op

(** val get_instruction_alu32_imm :
    int -> reg -> int -> int -> instruction **)

let get_instruction_alu32_imm _ rd i op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_BINARY (A32, BPF_ADD, rd, (Inr i)))
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_BINARY (A32,
                                            BPF_SUB, rd, (Inr i)))
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_ERR)
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_ERR)
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_ERR)
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_MUL,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_DIV,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_OR,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_AND,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_LSH,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_RSH,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n121 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n122 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n123 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n124 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n125 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n126 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n127 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n128 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n129 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n130 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n131 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_NEG
                                                                    (A32,
                                                                    rd))
                                                                    (fun n132 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n133 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n134 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n135 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n136 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n137 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n138 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n139 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n140 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n141 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n142 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n143 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n144 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n145 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n146 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n147 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_MOD,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n148 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n149 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n150 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n151 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n152 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n153 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n154 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n155 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n156 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n157 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n158 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n159 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n160 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n161 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n162 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n163 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_XOR,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n164 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n165 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n166 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n167 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n168 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n169 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n170 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n171 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n172 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n173 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n174 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n175 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n176 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n177 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n178 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n179 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_MOV,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun n180 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n181 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n182 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n183 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n184 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n185 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n186 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n187 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n188 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n189 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n190 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n191 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n192 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n193 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n194 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n195 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_ARSH,
                                                                    rd, (Inr
                                                                    i)))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n195)
                                                                    n194)
                                                                    n193)
                                                                    n192)
                                                                    n191)
                                                                    n190)
                                                                    n189)
                                                                    n188)
                                                                    n187)
                                                                    n186)
                                                                    n185)
                                                                    n184)
                                                                    n183)
                                                                    n182)
                                                                    n181)
                                                                    n180)
                                                                    n179)
                                                                    n178)
                                                                    n177)
                                                                    n176)
                                                                    n175)
                                                                    n174)
                                                                    n173)
                                                                    n172)
                                                                    n171)
                                                                    n170)
                                                                    n169)
                                                                    n168)
                                                                    n167)
                                                                    n166)
                                                                    n165)
                                                                    n164)
                                                                    n163)
                                                                    n162)
                                                                    n161)
                                                                    n160)
                                                                    n159)
                                                                    n158)
                                                                    n157)
                                                                    n156)
                                                                    n155)
                                                                    n154)
                                                                    n153)
                                                                    n152)
                                                                    n151)
                                                                    n150)
                                                                    n149)
                                                                    n148)
                                                                    n147)
                                                                    n146)
                                                                    n145)
                                                                    n144)
                                                                    n143)
                                                                    n142)
                                                                    n141)
                                                                    n140)
                                                                    n139)
                                                                    n138)
                                                                    n137)
                                                                    n136)
                                                                    n135)
                                                                    n134)
                                                                    n133)
                                                                    n132)
                                                                    n131)
                                                                    n130)
                                                                    n129)
                                                                    n128)
                                                                    n127)
                                                                    n126)
                                                                    n125)
                                                                    n124)
                                                                    n123)
                                                                    n122)
                                                                    n121)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    op

(** val get_instruction_alu32_reg :
    int -> reg -> reg -> int -> instruction **)

let get_instruction_alu32_reg _ rd rs op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_BINARY (A32, BPF_ADD, rd, (Inl
                            rs)))
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_BINARY (A32,
                                                            BPF_SUB, rd, (Inl
                                                            rs)))
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_ERR)
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_ERR)
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_MUL,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_DIV,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_OR,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_AND,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_LSH,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n121 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n122 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n123 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_RSH,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n124 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n125 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n126 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n127 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n128 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n129 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n130 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n131 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n132 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n133 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n134 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n135 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n136 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n137 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n138 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n139 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n140 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n141 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n142 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n143 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n144 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n145 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n146 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n147 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n148 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n149 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n150 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n151 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n152 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n153 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n154 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n155 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_MOD,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n156 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n157 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n158 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n159 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n160 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n161 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n162 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n163 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n164 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n165 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n166 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n167 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n168 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n169 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n170 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n171 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_XOR,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n172 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n173 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n174 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n175 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n176 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n177 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n178 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n179 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n180 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n181 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n182 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n183 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n184 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n185 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n186 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n187 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_MOV,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun n188 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n189 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n190 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n191 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n192 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n193 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n194 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n195 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n196 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n197 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n198 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n199 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n200 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n201 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n202 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n203 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_BINARY
                                                                    (A32,
                                                                    BPF_ARSH,
                                                                    rd, (Inl
                                                                    rs)))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n203)
                                                                    n202)
                                                                    n201)
                                                                    n200)
                                                                    n199)
                                                                    n198)
                                                                    n197)
                                                                    n196)
                                                                    n195)
                                                                    n194)
                                                                    n193)
                                                                    n192)
                                                                    n191)
                                                                    n190)
                                                                    n189)
                                                                    n188)
                                                                    n187)
                                                                    n186)
                                                                    n185)
                                                                    n184)
                                                                    n183)
                                                                    n182)
                                                                    n181)
                                                                    n180)
                                                                    n179)
                                                                    n178)
                                                                    n177)
                                                                    n176)
                                                                    n175)
                                                                    n174)
                                                                    n173)
                                                                    n172)
                                                                    n171)
                                                                    n170)
                                                                    n169)
                                                                    n168)
                                                                    n167)
                                                                    n166)
                                                                    n165)
                                                                    n164)
                                                                    n163)
                                                                    n162)
                                                                    n161)
                                                                    n160)
                                                                    n159)
                                                                    n158)
                                                                    n157)
                                                                    n156)
                                                                    n155)
                                                                    n154)
                                                                    n153)
                                                                    n152)
                                                                    n151)
                                                                    n150)
                                                                    n149)
                                                                    n148)
                                                                    n147)
                                                                    n146)
                                                                    n145)
                                                                    n144)
                                                                    n143)
                                                                    n142)
                                                                    n141)
                                                                    n140)
                                                                    n139)
                                                                    n138)
                                                                    n137)
                                                                    n136)
                                                                    n135)
                                                                    n134)
                                                                    n133)
                                                                    n132)
                                                                    n131)
                                                                    n130)
                                                                    n129)
                                                                    n128)
                                                                    n127)
                                                                    n126)
                                                                    n125)
                                                                    n124)
                                                                    n123)
                                                                    n122)
                                                                    n121)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    op

(** val get_instruction_ld : int -> reg -> int -> int -> instruction **)

let get_instruction_ld _ rd i op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_LDDW_high (rd,
                                    i))
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ -> BPF_LDDW_low
                                                    (rd, i))
                                                    (fun _ -> BPF_ERR)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    (Nat.coq_land op (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val get_instruction_ldx :
    int -> reg -> reg -> int -> int -> instruction **)

let get_instruction_ldx _ rd rs ofs op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_ERR)
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_ERR)
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_ERR)
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_LDX
                                                                    (Mint32,
                                                                    rd, rs,
                                                                    ofs))
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_LDX
                                                                    (Mint16unsigned,
                                                                    rd, rs,
                                                                    ofs))
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_LDX
                                                                    (Mint8unsigned,
                                                                    rd, rs,
                                                                    ofs))
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_LDX
                                                                    (Mint64,
                                                                    rd, rs,
                                                                    ofs))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    (Nat.coq_land op (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val get_instruction_st :
    int -> reg -> int -> int -> int -> instruction **)

let get_instruction_st _ rd ofs i op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_ERR)
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_ERR)
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_ERR)
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ST
                                                                    (Mint32,
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ST
                                                                    (Mint16unsigned,
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ST
                                                                    (Mint8unsigned,
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n121 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ST
                                                                    (Mint64,
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n121)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    (Nat.coq_land op (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val get_instruction_stx :
    int -> reg -> reg -> int -> int -> instruction **)

let get_instruction_stx _ rd rs ofs op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_ERR)
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_ERR)
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_ERR)
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ST
                                                                    (Mint32,
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ST
                                                                    (Mint16unsigned,
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ST
                                                                    (Mint8unsigned,
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n121 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n122 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ST
                                                                    (Mint64,
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n122)
                                                                    n121)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    (Nat.coq_land op (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val get_instruction_branch_imm :
    int -> reg -> int -> int -> int -> instruction **)

let get_instruction_branch_imm _ rd ofs i op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_JA ofs)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_JUMP (Eq0, rd,
                                              (Inr i), ofs))
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_ERR)
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_ERR)
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_ERR)
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Gt0
                                                                    Unsigned),
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Ge
                                                                    Unsigned),
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    (SEt, rd,
                                                                    (Inr i),
                                                                    ofs))
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    (Ne, rd,
                                                                    (Inr i),
                                                                    ofs))
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Gt0
                                                                    Signed),
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Ge
                                                                    Signed),
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n121 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n122 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n123 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n124 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n125 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n126 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n127 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n128 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n129 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n130 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n131 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n132 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_CALL
                                                                    i)
                                                                    (fun n133 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n134 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n135 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n136 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n137 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n138 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n139 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n140 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n141 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n142 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n143 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n144 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n145 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n146 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n147 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n148 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_RET)
                                                                    (fun n149 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n150 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n151 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n152 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n153 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n154 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n155 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n156 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n157 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n158 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n159 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n160 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n161 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n162 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n163 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n164 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Lt0
                                                                    Unsigned),
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n165 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n166 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n167 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n168 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n169 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n170 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n171 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n172 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n173 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n174 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n175 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n176 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n177 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n178 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n179 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n180 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Le
                                                                    Unsigned),
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n181 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n182 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n183 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n184 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n185 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n186 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n187 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n188 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n189 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n190 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n191 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n192 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n193 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n194 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n195 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n196 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Lt0
                                                                    Signed),
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun n197 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n198 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n199 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n200 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n201 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n202 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n203 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n204 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n205 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n206 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n207 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n208 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n209 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n210 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n211 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n212 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Le
                                                                    Signed),
                                                                    rd, (Inr
                                                                    i),
                                                                    ofs))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n212)
                                                                    n211)
                                                                    n210)
                                                                    n209)
                                                                    n208)
                                                                    n207)
                                                                    n206)
                                                                    n205)
                                                                    n204)
                                                                    n203)
                                                                    n202)
                                                                    n201)
                                                                    n200)
                                                                    n199)
                                                                    n198)
                                                                    n197)
                                                                    n196)
                                                                    n195)
                                                                    n194)
                                                                    n193)
                                                                    n192)
                                                                    n191)
                                                                    n190)
                                                                    n189)
                                                                    n188)
                                                                    n187)
                                                                    n186)
                                                                    n185)
                                                                    n184)
                                                                    n183)
                                                                    n182)
                                                                    n181)
                                                                    n180)
                                                                    n179)
                                                                    n178)
                                                                    n177)
                                                                    n176)
                                                                    n175)
                                                                    n174)
                                                                    n173)
                                                                    n172)
                                                                    n171)
                                                                    n170)
                                                                    n169)
                                                                    n168)
                                                                    n167)
                                                                    n166)
                                                                    n165)
                                                                    n164)
                                                                    n163)
                                                                    n162)
                                                                    n161)
                                                                    n160)
                                                                    n159)
                                                                    n158)
                                                                    n157)
                                                                    n156)
                                                                    n155)
                                                                    n154)
                                                                    n153)
                                                                    n152)
                                                                    n151)
                                                                    n150)
                                                                    n149)
                                                                    n148)
                                                                    n147)
                                                                    n146)
                                                                    n145)
                                                                    n144)
                                                                    n143)
                                                                    n142)
                                                                    n141)
                                                                    n140)
                                                                    n139)
                                                                    n138)
                                                                    n137)
                                                                    n136)
                                                                    n135)
                                                                    n134)
                                                                    n133)
                                                                    n132)
                                                                    n131)
                                                                    n130)
                                                                    n129)
                                                                    n128)
                                                                    n127)
                                                                    n126)
                                                                    n125)
                                                                    n124)
                                                                    n123)
                                                                    n122)
                                                                    n121)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    op

(** val get_instruction_branch_reg :
    int -> reg -> reg -> int -> int -> instruction **)

let get_instruction_branch_reg _ rd rs ofs op =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> BPF_ERR)
    (fun n0 ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> BPF_ERR)
      (fun n1 ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> BPF_ERR)
        (fun n2 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> BPF_ERR)
          (fun n3 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> BPF_ERR)
            (fun n4 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ -> BPF_ERR)
              (fun n5 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ -> BPF_ERR)
                (fun n6 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ -> BPF_ERR)
                  (fun n7 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> BPF_ERR)
                    (fun n8 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ -> BPF_ERR)
                      (fun n9 ->
                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                        (fun _ -> BPF_ERR)
                        (fun n10 ->
                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                          (fun _ -> BPF_ERR)
                          (fun n11 ->
                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                            (fun _ -> BPF_ERR)
                            (fun n12 ->
                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                              (fun _ -> BPF_ERR)
                              (fun n13 ->
                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                (fun _ -> BPF_ERR)
                                (fun n14 ->
                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                  (fun _ -> BPF_ERR)
                                  (fun n15 ->
                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                    (fun _ -> BPF_ERR)
                                    (fun n16 ->
                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                      (fun _ -> BPF_ERR)
                                      (fun n17 ->
                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                        (fun _ -> BPF_ERR)
                                        (fun n18 ->
                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                          (fun _ -> BPF_ERR)
                                          (fun n19 ->
                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                            (fun _ -> BPF_ERR)
                                            (fun n20 ->
                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                              (fun _ -> BPF_ERR)
                                              (fun n21 ->
                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                (fun _ -> BPF_ERR)
                                                (fun n22 ->
                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                  (fun _ ->
                                                  BPF_ERR)
                                                  (fun n23 ->
                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                    (fun _ ->
                                                    BPF_ERR)
                                                    (fun n24 ->
                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                      (fun _ ->
                                                      BPF_ERR)
                                                      (fun n25 ->
                                                      (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                        (fun _ ->
                                                        BPF_ERR)
                                                        (fun n26 ->
                                                        (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                          (fun _ ->
                                                          BPF_ERR)
                                                          (fun n27 ->
                                                          (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                            (fun _ ->
                                                            BPF_ERR)
                                                            (fun n28 ->
                                                            (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                              (fun _ ->
                                                              BPF_JUMP (Eq0,
                                                              rd, (Inl rs),
                                                              ofs))
                                                              (fun n29 ->
                                                              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                (fun _ ->
                                                                BPF_ERR)
                                                                (fun n30 ->
                                                                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                  (fun _ ->
                                                                  BPF_ERR)
                                                                  (fun n31 ->
                                                                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n32 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n33 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n34 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n35 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n36 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n37 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n38 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n39 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n40 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n41 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n42 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n43 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n44 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Gt0
                                                                    Unsigned),
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n45 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n46 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n47 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n48 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n49 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n50 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n51 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n52 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n53 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n54 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n55 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n56 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n57 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n58 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n59 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n60 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Ge
                                                                    Unsigned),
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n61 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n62 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n63 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n64 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n65 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n66 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n67 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n68 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n69 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n70 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n71 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n72 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n73 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n74 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n75 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n76 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    (SEt, rd,
                                                                    (Inl rs),
                                                                    ofs))
                                                                    (fun n77 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n78 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n79 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n80 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n81 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n82 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n83 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n84 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n85 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n86 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n87 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n88 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n89 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n90 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n91 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n92 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    (Ne, rd,
                                                                    (Inl rs),
                                                                    ofs))
                                                                    (fun n93 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n94 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n95 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n96 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n97 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n98 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n99 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n100 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n101 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n102 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n103 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n104 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n105 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n106 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n107 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n108 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Gt0
                                                                    Signed),
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n109 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n110 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n111 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n112 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n113 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n114 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n115 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n116 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n117 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n118 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n119 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n120 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n121 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n122 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n123 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n124 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Ge
                                                                    Signed),
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n125 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n126 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n127 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n128 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n129 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n130 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n131 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n132 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n133 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n134 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n135 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n136 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n137 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n138 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n139 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n140 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n141 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n142 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n143 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n144 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n145 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n146 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n147 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n148 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n149 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n150 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n151 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n152 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n153 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n154 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n155 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n156 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n157 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n158 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n159 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n160 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n161 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n162 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n163 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n164 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n165 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n166 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n167 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n168 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n169 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n170 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n171 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n172 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Lt0
                                                                    Unsigned),
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n173 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n174 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n175 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n176 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n177 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n178 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n179 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n180 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n181 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n182 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n183 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n184 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n185 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n186 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n187 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n188 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Le
                                                                    Unsigned),
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n189 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n190 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n191 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n192 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n193 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n194 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n195 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n196 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n197 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n198 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n199 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n200 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n201 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n202 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n203 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n204 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Lt0
                                                                    Signed),
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun n205 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n206 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n207 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n208 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n209 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n210 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n211 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n212 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n213 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n214 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n215 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n216 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n217 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n218 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n219 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    (fun n220 ->
                                                                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                                                                    (fun _ ->
                                                                    BPF_JUMP
                                                                    ((Le
                                                                    Signed),
                                                                    rd, (Inl
                                                                    rs),
                                                                    ofs))
                                                                    (fun _ ->
                                                                    BPF_ERR)
                                                                    n220)
                                                                    n219)
                                                                    n218)
                                                                    n217)
                                                                    n216)
                                                                    n215)
                                                                    n214)
                                                                    n213)
                                                                    n212)
                                                                    n211)
                                                                    n210)
                                                                    n209)
                                                                    n208)
                                                                    n207)
                                                                    n206)
                                                                    n205)
                                                                    n204)
                                                                    n203)
                                                                    n202)
                                                                    n201)
                                                                    n200)
                                                                    n199)
                                                                    n198)
                                                                    n197)
                                                                    n196)
                                                                    n195)
                                                                    n194)
                                                                    n193)
                                                                    n192)
                                                                    n191)
                                                                    n190)
                                                                    n189)
                                                                    n188)
                                                                    n187)
                                                                    n186)
                                                                    n185)
                                                                    n184)
                                                                    n183)
                                                                    n182)
                                                                    n181)
                                                                    n180)
                                                                    n179)
                                                                    n178)
                                                                    n177)
                                                                    n176)
                                                                    n175)
                                                                    n174)
                                                                    n173)
                                                                    n172)
                                                                    n171)
                                                                    n170)
                                                                    n169)
                                                                    n168)
                                                                    n167)
                                                                    n166)
                                                                    n165)
                                                                    n164)
                                                                    n163)
                                                                    n162)
                                                                    n161)
                                                                    n160)
                                                                    n159)
                                                                    n158)
                                                                    n157)
                                                                    n156)
                                                                    n155)
                                                                    n154)
                                                                    n153)
                                                                    n152)
                                                                    n151)
                                                                    n150)
                                                                    n149)
                                                                    n148)
                                                                    n147)
                                                                    n146)
                                                                    n145)
                                                                    n144)
                                                                    n143)
                                                                    n142)
                                                                    n141)
                                                                    n140)
                                                                    n139)
                                                                    n138)
                                                                    n137)
                                                                    n136)
                                                                    n135)
                                                                    n134)
                                                                    n133)
                                                                    n132)
                                                                    n131)
                                                                    n130)
                                                                    n129)
                                                                    n128)
                                                                    n127)
                                                                    n126)
                                                                    n125)
                                                                    n124)
                                                                    n123)
                                                                    n122)
                                                                    n121)
                                                                    n120)
                                                                    n119)
                                                                    n118)
                                                                    n117)
                                                                    n116)
                                                                    n115)
                                                                    n114)
                                                                    n113)
                                                                    n112)
                                                                    n111)
                                                                    n110)
                                                                    n109)
                                                                    n108)
                                                                    n107)
                                                                    n106)
                                                                    n105)
                                                                    n104)
                                                                    n103)
                                                                    n102)
                                                                    n101)
                                                                    n100)
                                                                    n99)
                                                                    n98)
                                                                    n97)
                                                                    n96)
                                                                    n95)
                                                                    n94)
                                                                    n93)
                                                                    n92)
                                                                    n91)
                                                                    n90)
                                                                    n89)
                                                                    n88)
                                                                    n87)
                                                                    n86)
                                                                    n85)
                                                                    n84)
                                                                    n83)
                                                                    n82)
                                                                    n81)
                                                                    n80)
                                                                    n79)
                                                                    n78)
                                                                    n77)
                                                                    n76)
                                                                    n75)
                                                                    n74)
                                                                    n73)
                                                                    n72)
                                                                    n71)
                                                                    n70)
                                                                    n69)
                                                                    n68)
                                                                    n67)
                                                                    n66)
                                                                    n65)
                                                                    n64)
                                                                    n63)
                                                                    n62)
                                                                    n61)
                                                                    n60)
                                                                    n59)
                                                                    n58)
                                                                    n57)
                                                                    n56)
                                                                    n55)
                                                                    n54)
                                                                    n53)
                                                                    n52)
                                                                    n51)
                                                                    n50)
                                                                    n49)
                                                                    n48)
                                                                    n47)
                                                                    n46)
                                                                    n45)
                                                                    n44)
                                                                    n43)
                                                                    n42)
                                                                    n41)
                                                                    n40)
                                                                    n39)
                                                                    n38)
                                                                    n37)
                                                                    n36)
                                                                    n35)
                                                                    n34)
                                                                    n33)
                                                                    n32)
                                                                    n31)
                                                                  n30)
                                                                n29)
                                                              n28)
                                                            n27)
                                                          n26)
                                                        n25)
                                                      n24)
                                                    n23)
                                                  n22)
                                                n21)
                                              n20)
                                            n19)
                                          n18)
                                        n17)
                                      n16)
                                    n15)
                                  n14)
                                n13)
                              n12)
                            n11)
                          n10)
                        n9)
                      n8)
                    n7)
                  n6)
                n5)
              n4)
            n3)
          n2)
        n1)
      n0)
    op

(** val decode : int -> instruction option **)

let decode ins0 =
  let opcode = get_opcode ins0 in
  (match int64_to_dst_reg' ins0 with
   | Some dst ->
     let opc =
       Nat.coq_land opcode (Stdlib.succ (Stdlib.succ (Stdlib.succ
         (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
         0)))))))
     in
     let n_ofs = get_offset ins0 in
     let n_imm = get_immediate ins0 in
     ((fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> Some (get_instruction_ld ins0 dst n_imm opcode))
        (fun n0 ->
        (fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ ->
          match int64_to_src_reg' ins0 with
          | Some src -> Some (get_instruction_ldx ins0 dst src n_ofs opcode)
          | None -> None)
          (fun n1 ->
          (fun fO fS n -> if n=0 then fO () else fS (n-1))
            (fun _ -> Some
            (get_instruction_st ins0 dst n_ofs n_imm opcode))
            (fun n2 ->
            (fun fO fS n -> if n=0 then fO () else fS (n-1))
              (fun _ ->
              match int64_to_src_reg' ins0 with
              | Some src ->
                Some (get_instruction_stx ins0 dst src n_ofs opcode)
              | None -> None)
              (fun n3 ->
              (fun fO fS n -> if n=0 then fO () else fS (n-1))
                (fun _ ->
                if Int.eq Int.zero
                     (Int.coq_and (Int.repr (Z.of_nat opcode))
                       (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                         1)))))
                then Some (get_instruction_alu32_imm ins0 dst n_imm opcode)
                else (match int64_to_src_reg' ins0 with
                      | Some src ->
                        Some (get_instruction_alu32_reg ins0 dst src opcode)
                      | None -> None))
                (fun n4 ->
                (fun fO fS n -> if n=0 then fO () else fS (n-1))
                  (fun _ ->
                  if Int.eq Int.zero
                       (Int.coq_and (Int.repr (Z.of_nat opcode))
                         (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                           1)))))
                  then Some
                         (get_instruction_branch_imm ins0 dst n_ofs n_imm
                           opcode)
                  else (match int64_to_src_reg' ins0 with
                        | Some src ->
                          Some
                            (get_instruction_branch_reg ins0 dst src n_ofs
                              opcode)
                        | None -> None))
                  (fun n5 ->
                  (fun fO fS n -> if n=0 then fO () else fS (n-1))
                    (fun _ -> Some BPF_ERR)
                    (fun n6 ->
                    (fun fO fS n -> if n=0 then fO () else fS (n-1))
                      (fun _ ->
                      if Int.eq Int.zero
                           (Int.coq_and (Int.repr (Z.of_nat opcode))
                             (Int.repr ((fun p->2*p) ((fun p->2*p)
                               ((fun p->2*p) 1)))))
                      then Some
                             (get_instruction_alu64_imm ins0 dst n_imm opcode)
                      else (match int64_to_src_reg' ins0 with
                            | Some src ->
                              Some
                                (get_instruction_alu64_reg ins0 dst src
                                  opcode)
                            | None -> None))
                      (fun _ -> Some BPF_ERR)
                      n6)
                    n5)
                  n4)
                n3)
              n2)
            n1)
          n0)
        opc)
   | None -> None)

(** val eval_pc0 : (state, int) m **)

let eval_pc0 st =
  Some ((eval_pc st), st)

(** val upd_pc0 : int -> (state, unit) m **)

let upd_pc0 p st =
  if Int.cmpu Cle p
       (Int.sub (Int.repr (Z.of_nat st.ins_len)) (Int.repr ((fun p->2*p) 1)))
  then Some ((), (upd_pc p st))
  else None

(** val upd_pc_incr0 : (state, unit) m **)

let upd_pc_incr0 st =
  if Int.cmpu Clt (Int.add st.pc_loc Int.one) (Int.repr (Z.of_nat st.ins_len))
  then Some ((), (upd_pc_incr st))
  else None

(** val eval_flag0 : (state, bpf_flag) m **)

let eval_flag0 st =
  Some ((eval_flag st), st)

(** val upd_flag0 : bpf_flag -> (state, unit) m **)

let upd_flag0 f st =
  Some ((), (upd_flag f st))

(** val eval_mrs_num : (state, int) m **)

let eval_mrs_num st =
  Some ((eval_mem_num st), st)

(** val eval_reg0 : reg -> (state, val0) m **)

let eval_reg0 r st =
  Some ((eval_reg r st), st)

(** val upd_reg0 : reg -> val0 -> (state, unit) m **)

let upd_reg0 r v st =
  match v with
  | Vlong _ -> Some ((), (upd_reg r v st))
  | _ -> None

(** val eval_mrs_regions : (state, myMemRegionsType) m **)

let eval_mrs_regions st =
  Some ((eval_mem_regions st), st)

(** val eval_mem_regions0 : (state, myMemRegionsType) m **)

let eval_mem_regions0 st =
  Some ((eval_mem_regions st), st)

(** val eval_mem0 : (state, Mem.mem) m **)

let eval_mem0 st =
  Some ((eval_mem st), st)

(** val load_mem0 : memory_chunk -> val0 -> (state, val0) m **)

let load_mem0 chunk ptr st =
  match load_mem chunk ptr st with
  | Some res -> (match res with
                 | Vundef -> None
                 | _ -> Some (res, st))
  | None -> None

(** val store_mem_imm0 : val0 -> memory_chunk -> val0 -> (state, unit) m **)

let store_mem_imm0 ptr chunk v st =
  match store_mem_imm ptr chunk v st with
  | Some res -> Some ((), res)
  | None -> None

(** val store_mem_reg0 : val0 -> memory_chunk -> val0 -> (state, unit) m **)

let store_mem_reg0 ptr chunk v st =
  match store_mem_reg ptr chunk v st with
  | Some res -> Some ((), res)
  | None -> None

(** val eval_ins_len0 : (state, int) m **)

let eval_ins_len0 st =
  Some ((eval_ins_len st), st)

(** val eval_ins0 : int -> (state, int) m **)

let eval_ins0 idx st =
  if Int.cmpu Clt idx (Int.repr (Z.of_nat st.ins_len))
  then Some ((eval_ins idx st), st)
  else None

(** val cmp_ptr32_nullM : val0 -> (state, bool) m **)

let cmp_ptr32_nullM v st =
  match cmp_ptr32_null (eval_mem st) v with
  | Some res -> Some (res, st)
  | None -> None

(** val get_mem_region :
    int -> myMemRegionsType -> (state, memory_region) m **)

let get_mem_region n0 mrs st =
  if Nat.ltb n0 st.mrs_num
  then (match nth_error mrs n0 with
        | Some mr -> Some (mr, st)
        | None -> None)
  else None

(** val _bpf_get_call : val0 -> (state, val0) m **)

let _bpf_get_call = function v -> returnM v

(** val exec_function : val0 -> (state, val0) m **)

let exec_function = function v -> returnM v

(** val eval_src : (reg, imm) sum -> (state, val0) m **)

let eval_src = function
| Inl r -> eval_reg0 r
| Inr i -> returnM (Val.longofint (sint32_to_vint i))

(** val eval_reg32 : reg -> (state, val0) m **)

let eval_reg32 r =
  bindM (eval_reg0 r) (fun v -> returnM (val_intuoflongu v))

(** val eval_src32 : (reg, imm) sum -> (state, val0) m **)

let eval_src32 = function
| Inl r -> eval_reg32 r
| Inr i -> returnM (sint32_to_vint i)

(** val step_alu_binary_operation :
    arch -> binOp -> reg -> (reg, imm) sum -> (state, unit) m **)

let step_alu_binary_operation a bop d s =
  match a with
  | A32 ->
    bindM (eval_reg32 d) (fun d32 ->
      bindM (eval_src32 s) (fun s32 ->
        match bop with
        | BPF_ADD -> upd_reg0 d (Val.longofintu (Val.add d32 s32))
        | BPF_SUB -> upd_reg0 d (Val.longofintu (Val.sub d32 s32))
        | BPF_MUL -> upd_reg0 d (Val.longofintu (Val.mul d32 s32))
        | BPF_DIV ->
          if comp_ne_32 s32 vzero
          then (match Val.divu d32 s32 with
                | Some res -> upd_reg0 d (Val.longofintu res)
                | None -> errorM)
          else upd_flag0 BPF_ILLEGAL_DIV
        | BPF_OR -> upd_reg0 d (Val.longofintu (Val.coq_or d32 s32))
        | BPF_AND -> upd_reg0 d (Val.longofintu (Val.coq_and d32 s32))
        | BPF_LSH ->
          if compu_lt_32 s32 (Vint
               (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                 ((fun p->2*p) ((fun p->2*p) 1)))))))
          then upd_reg0 d (Val.longofintu (Val.shl d32 s32))
          else upd_flag0 BPF_ILLEGAL_SHIFT
        | BPF_RSH ->
          if compu_lt_32 s32 (Vint
               (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                 ((fun p->2*p) ((fun p->2*p) 1)))))))
          then upd_reg0 d (Val.longofintu (Val.shru d32 s32))
          else upd_flag0 BPF_ILLEGAL_SHIFT
        | BPF_MOD ->
          if comp_ne_32 s32 vzero
          then (match Val.modu d32 s32 with
                | Some res -> upd_reg0 d (Val.longofintu res)
                | None -> errorM)
          else upd_flag0 BPF_ILLEGAL_DIV
        | BPF_XOR -> upd_reg0 d (Val.longofintu (Val.xor d32 s32))
        | BPF_MOV -> upd_reg0 d (Val.longofintu s32)
        | BPF_ARSH ->
          if compu_lt_32 s32 (Vint
               (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                 ((fun p->2*p) ((fun p->2*p) 1)))))))
          then upd_reg0 d (Val.longofint (Val.shr d32 s32))
          else upd_flag0 BPF_ILLEGAL_SHIFT))
  | A64 ->
    bindM (eval_reg0 d) (fun d64 ->
      bindM (eval_src s) (fun s64 ->
        match bop with
        | BPF_ADD -> upd_reg0 d (Val.addl d64 s64)
        | BPF_SUB -> upd_reg0 d (Val.subl d64 s64)
        | BPF_MUL -> upd_reg0 d (Val.mull d64 s64)
        | BPF_DIV ->
          if compl_ne s64 val64_zero
          then (match Val.divlu d64 s64 with
                | Some res -> upd_reg0 d res
                | None -> errorM)
          else upd_flag0 BPF_ILLEGAL_DIV
        | BPF_OR -> upd_reg0 d (Val.orl d64 s64)
        | BPF_AND -> upd_reg0 d (Val.andl d64 s64)
        | BPF_LSH ->
          if compu_lt_32 (val_intuoflongu s64) (Vint
               (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))))
          then upd_reg0 d (Val.shll d64 (val_intuoflongu s64))
          else upd_flag0 BPF_ILLEGAL_SHIFT
        | BPF_RSH ->
          if compu_lt_32 (val_intuoflongu s64) (Vint
               (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))))
          then upd_reg0 d (Val.shrlu d64 (val_intuoflongu s64))
          else upd_flag0 BPF_ILLEGAL_SHIFT
        | BPF_MOD ->
          if compl_ne s64 val64_zero
          then (match Val.modlu d64 s64 with
                | Some res -> upd_reg0 d res
                | None -> errorM)
          else upd_flag0 BPF_ILLEGAL_DIV
        | BPF_XOR -> upd_reg0 d (Val.xorl d64 s64)
        | BPF_MOV -> upd_reg0 d s64
        | BPF_ARSH ->
          if compu_lt_32 (val_intuoflongu s64) (Vint
               (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))))
          then upd_reg0 d (Val.shrl d64 (val_intuoflongu s64))
          else upd_flag0 BPF_ILLEGAL_SHIFT))

(** val step_branch_cond :
    cond -> reg -> (reg, imm) sum -> (state, bool) m **)

let step_branch_cond c d s =
  bindM (eval_reg0 d) (fun dst ->
    bindM (eval_src s) (fun src ->
      returnM
        (match c with
         | Eq0 -> compl_eq dst src
         | Gt0 sign ->
           (match sign with
            | Signed -> compl_gt dst src
            | Unsigned -> complu_gt dst src)
         | Ge sign ->
           (match sign with
            | Signed -> compl_ge dst src
            | Unsigned -> complu_ge dst src)
         | Lt0 sign ->
           (match sign with
            | Signed -> compl_lt dst src
            | Unsigned -> complu_lt dst src)
         | Le sign ->
           (match sign with
            | Signed -> compl_le dst src
            | Unsigned -> complu_le dst src)
         | SEt -> complu_set dst src
         | Ne -> compl_ne dst src)))

(** val get_add : val0 -> val0 -> (state, val0) m **)

let get_add x y =
  returnM (Val.add x y)

(** val get_sub : val0 -> val0 -> (state, val0) m **)

let get_sub x y =
  returnM (Val.sub x y)

(** val get_addr_ofs : val0 -> int -> (state, val0) m **)

let get_addr_ofs x ofs =
  returnM (val_intuoflongu (Val.addl x (Val.longofint (sint32_to_vint ofs))))

(** val get_start_addr : memory_region -> (state, val0) m **)

let get_start_addr mr =
  returnM mr.start_addr

(** val get_block_size : memory_region -> (state, val0) m **)

let get_block_size mr =
  returnM mr.block_size

(** val get_block_perm : memory_region -> (state, permission) m **)

let get_block_perm mr =
  returnM mr.block_perm

(** val is_well_chunk_bool : memory_chunk -> (state, bool) m **)

let is_well_chunk_bool = function
| Mint8unsigned -> returnM true
| Mint16unsigned -> returnM true
| Mint32 -> returnM true
| Mint64 -> returnM true
| _ -> returnM false

(** val check_mem_aux2 :
    memory_region -> permission -> val0 -> memory_chunk -> (state, val0) m **)

let check_mem_aux2 mr perm addr chunk =
  bindM (get_start_addr mr) (fun start ->
    bindM (get_block_size mr) (fun size0 ->
      bindM (get_block_perm mr) (fun mr_perm ->
        bindM (get_sub addr start) (fun lo_ofs ->
          bindM (get_add lo_ofs (memory_chunk_to_valu32 chunk))
            (fun hi_ofs ->
            if (&&)
                 ((&&) (compu_lt_32 hi_ofs size0)
                   ((&&)
                     (compu_le_32 lo_ofs
                       (memory_chunk_to_valu32_upbound chunk))
                     (comp_eq_32 vzero
                       (val32_modu lo_ofs (memory_chunk_to_valu32 chunk)))))
                 (perm_ge mr_perm perm)
            then returnM (Val.add mr.block_ptr lo_ofs)
            else returnM vnullptr)))))

(** val check_mem_aux :
    int -> permission -> memory_chunk -> val0 -> myMemRegionsType -> (state,
    val0) m **)

let rec check_mem_aux num perm chunk addr mrs =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> returnM vnullptr)
    (fun n0 ->
    bindM (get_mem_region n0 mrs) (fun cur_mr ->
      bindM (check_mem_aux2 cur_mr perm addr chunk) (fun check_mem0 ->
        bindM (cmp_ptr32_nullM check_mem0) (fun is_null ->
          if is_null
          then check_mem_aux n0 perm chunk addr mrs
          else returnM check_mem0))))
    num

(** val check_mem : permission -> memory_chunk -> val0 -> (state, val0) m **)

let check_mem perm chunk addr =
  bindM (is_well_chunk_bool chunk) (fun well_chunk ->
    if well_chunk
    then bindM eval_mrs_num (fun mem_reg_num ->
           bindM eval_mrs_regions (fun mrs ->
             bindM (check_mem_aux mem_reg_num perm chunk addr mrs)
               (fun check_mem0 ->
               bindM (cmp_ptr32_nullM check_mem0) (fun is_null ->
                 if is_null then returnM vnullptr else returnM check_mem0))))
    else returnM vnullptr)

(** val step_load_x_operation :
    memory_chunk -> reg -> reg -> off -> (state, unit) m **)

let step_load_x_operation chunk d s ofs =
  bindM eval_mem0 (fun _ ->
    bindM eval_mem_regions0 (fun _ ->
      bindM (eval_reg0 s) (fun sv ->
        bindM (get_addr_ofs sv ofs) (fun addr ->
          bindM (check_mem Readable chunk addr) (fun ptr ->
            bindM (cmp_ptr32_nullM ptr) (fun is_null ->
              if is_null
              then upd_flag0 BPF_ILLEGAL_MEM
              else bindM (load_mem0 chunk ptr) (fun v ->
                     bindM (upd_reg0 d v) (fun _ -> returnM ()))))))))

(** val step_store_operation :
    memory_chunk -> reg -> (reg, imm) sum -> off -> (state, unit) m **)

let step_store_operation chunk d s ofs =
  bindM eval_mem0 (fun _ ->
    bindM eval_mem_regions0 (fun _ ->
      bindM (eval_reg0 d) (fun dv ->
        bindM (get_addr_ofs dv ofs) (fun addr ->
          match s with
          | Inl r ->
            bindM (eval_reg0 r) (fun src ->
              bindM (check_mem Writable chunk addr) (fun ptr ->
                bindM (cmp_ptr32_nullM ptr) (fun is_null ->
                  if is_null
                  then upd_flag0 BPF_ILLEGAL_MEM
                  else bindM (store_mem_reg0 ptr chunk src) (fun _ ->
                         returnM ()))))
          | Inr i ->
            bindM (check_mem Writable chunk addr) (fun ptr ->
              bindM (cmp_ptr32_nullM ptr) (fun is_null ->
                if is_null
                then upd_flag0 BPF_ILLEGAL_MEM
                else bindM (store_mem_imm0 ptr chunk (sint32_to_vint i))
                       (fun _ -> returnM ())))))))

(** val decodeM : int -> (state, instruction) m **)

let decodeM i st =
  match decode i with
  | Some ins0 -> let _ = print_endline (string_of_instruction ins0) in let _ = print_state st in Some (ins0, st)
  | None -> None

(** val step : (state, unit) m **)

let step =
  bindM eval_pc0 (fun pc ->
    bindM (eval_ins0 pc) (fun ins64 ->
      bindM (decodeM ins64) (fun ins0 ->
        match ins0 with
        | BPF_NEG (a, d) ->
          (match a with
           | A32 ->
             bindM (eval_reg0 d) (fun d32 ->
               upd_reg0 d (Val.longofintu (Val.neg (val_intuoflongu d32))))
           | A64 -> bindM (eval_reg0 d) (fun d64 -> upd_reg0 d (Val.negl d64)))
        | BPF_BINARY (a, bop, d, s) -> step_alu_binary_operation a bop d s
        | BPF_JA ofs -> upd_pc0 (Int.add pc ofs)
        | BPF_JUMP (c, d, s, ofs) ->
          bindM (step_branch_cond c d s) (fun cond0 ->
            if cond0 then upd_pc0 (Int.add pc ofs) else returnM ())
        | BPF_LDDW_low (d, i) ->
          bindM (upd_reg0 d (Val.longofintu (sint32_to_vint i))) (fun _ ->
            returnM ())
        | BPF_LDDW_high (d, i) ->
          bindM (eval_reg0 d) (fun d64 ->
            bindM
              (upd_reg0 d
                (Val.orl d64
                  (Val.shll (Val.longofintu (sint32_to_vint i))
                    (sint32_to_vint
                      (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) 1)))))))))) (fun _ ->
              returnM ()))
        | BPF_LDX (chunk, d, s, ofs) -> step_load_x_operation chunk d s ofs
        | BPF_ST (chunk, d, s, ofs) -> step_store_operation chunk d s ofs
        | BPF_CALL i ->
          bindM
            (_bpf_get_call (Vint
              (Int.repr (Int64.unsigned (Int64.repr (Int.signed i))))))
            (fun f_ptr ->
            bindM (cmp_ptr32_nullM f_ptr) (fun is_null ->
              if is_null
              then upd_flag0 BPF_ILLEGAL_CALL
              else bindM (exec_function f_ptr) (fun res ->
                     upd_reg0 R0 (Val.longofintu res))))
        | BPF_RET -> upd_flag0 BPF_SUCC_RETURN
        | BPF_ERR -> upd_flag0 BPF_ILLEGAL_INSTRUCTION)))

(** val bpf_interpreter_aux : int -> (state, unit) m **)

let rec bpf_interpreter_aux fuel = let _ = print_endline ("fuel= " ^ (string_of_int fuel)) in
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> upd_flag0 BPF_ILLEGAL_LEN)
    (fun fuel0 ->
    bindM eval_ins_len0 (fun len ->
      bindM eval_pc0 (fun pc ->
        if Int.ltu pc len
        then bindM step (fun _ ->
               bindM eval_flag0 (fun f ->
                 if flag_eq f BPF_OK
                 then bindM eval_ins_len0 (fun len0 ->
                        bindM eval_pc0 (fun pc0 ->
                          if Int.ltu (Int.add pc0 Int.one) len0
                          then bindM upd_pc_incr0 (fun _ ->
                                 bpf_interpreter_aux fuel0)
                          else upd_flag0 BPF_ILLEGAL_LEN))
                 else returnM ()))
        else upd_flag0 BPF_ILLEGAL_LEN)))
    fuel

(** val bpf_interpreter : int -> (state, val0) m **)

let bpf_interpreter fuel =
  bindM (bpf_interpreter_aux fuel) (fun _ ->
    bindM eval_flag0 (fun f ->
      if flag_eq f BPF_SUCC_RETURN
      then bindM (eval_reg0 R0) returnM
      else returnM val64_zero))

(** val wrap_around_data_ascii : string **)

let wrap_around_data_ascii =
  (^)
    "AD3Awn4kb6FtcsyE0RU25U7f55Yncn3LP3oEx9Gl4qr7iDW7I8L6Pbw9jNnh0sE4DmCKuc"
    ((^)
      "d1J8I34vn31W924y5GMS74vUrZQc08805aj4Tf66HgL1cO94os10V2s2GDQ825yNh9Yuq3"
      ((^)
        "QHcA60xl31rdA7WskVtCXI7ruH1A4qaR6Uk454hm401lLmv2cGWt5KTJmr93d3JsGaRRPs"
        ((^)
          "4HqYi4mFGowo8fWv48IcA3N89Z99nf0A0H2R6P0uI4Tir682Of3Rk78DUB2dIGQRRpdqVT"
          ((^)
            "tLhgfET2gUGU65V3edSwADMqRttI9JPVz8JS37g5QZj4Ax56rU1u0m0K8YUs57UYG5645n"
            "byNy4yqxu7"))))

(** val wrap_around_data_byte : memval list **)

let wrap_around_data_byte =
  let ascii_list =
    (fun s ->
      Array.to_list (Array.init (String.length s) (fun i -> s.[i])))
      wrap_around_data_ascii
  in
  map (fun a -> Byte (Byte.repr (Z.of_N (n_of_ascii a)))) ascii_list

(** val test_fletcher32_int64 : int list **)

let test_fletcher32_int64 =
  (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    1)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      1)))))))))))))))))))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                              ((fun p->1+2*p)
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
                                                              1)))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
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
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              1)))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))))))) :: ((Int64.repr
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->2*p)
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
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->1+2*p)
                                                        ((fun p->2*p)
                                                        1))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      1))))))))))))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                       ((fun p->1+2*p)
                                                       ((fun p->1+2*p)
                                                       ((fun p->1+2*p)
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
                                                       1))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p)
      1)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1)))))))))))))))))))))))))))))))))))))) :: ((Int64.repr ((fun p->1+2*p)
                                                    ((fun p->1+2*p)
                                                    ((fun p->1+2*p)
                                                    ((fun p->2*p)
                                                    ((fun p->1+2*p)
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
                                                    1)))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->2*p) 1))))))))))))) :: ((Int64.repr ((fun p->1+2*p)
                                         ((fun p->2*p) ((fun p->2*p)
                                         ((fun p->1+2*p) ((fun p->2*p)
                                         ((fun p->1+2*p) ((fun p->1+2*p)
                                         ((fun p->2*p) ((fun p->2*p)
                                         ((fun p->2*p) ((fun p->2*p)
                                         ((fun p->2*p) ((fun p->1+2*p)
                                         ((fun p->1+2*p) 1))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      1)))))))))) :: ((Int64.repr ((fun p->1+2*p) ((fun p->1+2*p)
                        ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                        ((fun p->2*p) 1)))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      1)))))))))))))) :: ((Int64.repr ((fun p->2*p) ((fun p->2*p)
                            ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                            1))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
                                                   ((fun p->2*p)
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
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   ((fun p->1+2*p)
                                                   1)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))))))) :: ((Int64.repr
                                                     ((fun p->1+2*p)
                                                     ((fun p->2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->2*p)
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
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     1)))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))))))) :: ((Int64.repr ((fun p->2*p)
                                                     ((fun p->2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
                                                     ((fun p->1+2*p)
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
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
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
                                                              1))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1)))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
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
                                            ((fun p->2*p)
                                            1))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
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
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              1))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1))))))))))))))))))))))))))))))))) :: ((Int64.repr ((fun p->1+2*p)
                                               ((fun p->1+2*p)
                                               ((fun p->1+2*p)
                                               ((fun p->1+2*p) ((fun p->2*p)
                                               ((fun p->2*p) ((fun p->2*p)
                                               ((fun p->2*p) ((fun p->1+2*p)
                                               ((fun p->2*p) ((fun p->2*p)
                                               ((fun p->2*p) ((fun p->1+2*p)
                                               ((fun p->2*p) 1))))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1)))))))))))))))))))))))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                                ((fun p->2*p) ((fun p->1+2*p)
                                                ((fun p->1+2*p)
                                                ((fun p->1+2*p)
                                                ((fun p->1+2*p) ((fun p->2*p)
                                                ((fun p->1+2*p) ((fun p->2*p)
                                                ((fun p->2*p) 1))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) 1)))))))))))))))))))))))))))))))) :: ((Int64.repr
                                                              ((fun p->2*p)
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
                                                              1))))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      1)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1)))))))))))))) :: ((Int64.repr ((fun p->2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->2*p) ((fun p->1+2*p)
                                            ((fun p->1+2*p) ((fun p->1+2*p)
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
                                            ((fun p->2*p)
                                            1))))))))))))))))))))))))))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
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
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->1+2*p)
                                                              ((fun p->2*p)
                                                              ((fun p->2*p)
                                                              1))))))))))))) :: (
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
      1)))))))))))))) :: ((Int64.repr ((fun p->1+2*p) ((fun p->2*p)
                            ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
                            ((fun p->2*p) ((fun p->2*p) 1)))))))) :: []))))))))))))))))))))))))))))))))))))))))))

(** val init_mem : Mem.mem' * block **)

let init_mem =
  Mem.alloc Mem.empty 0 361

(** val input_blk : block **)

let input_blk =
  snd init_mem

(** val mem1 : Mem.mem **)

let mem1 =
  match Mem.storebytes (fst init_mem) input_blk 0 wrap_around_data_byte with
  | Some m0 -> m0
  | None -> Mem.empty

(** val fletcher32_init_regs : regmap **)

let fletcher32_init_regs =
  { r0_val = val64_zero; r1_val = (Vlong (Int64.repr 0)); r2_val =
    (Vlong
    (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) 1))))))))); r3_val =
    val64_zero; r4_val = val64_zero; r5_val = val64_zero; r6_val =
    val64_zero; r7_val = val64_zero; r8_val = val64_zero; r9_val =
    val64_zero; r10_val = val64_zero }

(** val fletcher32_init_memory_regions : memory_region list **)

let fletcher32_init_memory_regions =
  { start_addr = (Vint (Int.repr 0)); block_size = (Vint
    (Int.repr 361)); block_perm =
    Readable; block_ptr = (Vptr (input_blk, Ptrofs.zero)) } :: []

(** val fletcher32_init_bpf_state : state **)

let fletcher32_init_bpf_state =
  { pc_loc = Int.zero; flag = BPF_OK; regs_st = fletcher32_init_regs;
    mrs_num = (Stdlib.succ 0); bpf_mrs = fletcher32_init_memory_regions;
    ins_len = (length test_fletcher32_int64); ins = test_fletcher32_int64;
    bpf_m = mem1 }

(** val fletcher32_test_main : (val0 * state) option **)

let fletcher32_test_main =
  bpf_interpreter 5000 fletcher32_init_bpf_state
  
let main =
  match fletcher32_test_main with
  | Some (res, st) -> let _ = print_val0 res in let _ = print_endline "final state:" in print_state st
  | None -> print_endline "error fletcher32_test_main"
