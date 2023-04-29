
(** val xorb : bool -> bool -> bool **)

let xorb b1 b2 =
  if b1 then if b2 then false else true else b2

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

(** val compOpp : comparison -> comparison **)

let compOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



module Coq__1 = struct
 (** val add : int -> int -> int **)let rec add = (+)
end
include Coq__1

(** val bool_dec : bool -> bool -> bool **)

let bool_dec b1 b2 =
  if b1 then if b2 then true else false else if b2 then false else true

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

  (** val shiftl_nat : int -> int -> int **)

  let rec shiftl_nat p n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> p)
      (fun n1 -> (fun p->2*p) (shiftl_nat p n1))
      n0

  (** val shiftr_nat : int -> int -> int **)

  let rec shiftr_nat p n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> p)
      (fun n1 -> div2 (shiftr_nat p n1))
      n0

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

  (** val max : int -> int -> int **)

  let max = Stdlib.max

  (** val min : int -> int -> int **)

  let min = Stdlib.min

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

  (** val to_pos : int -> int **)

  let to_pos z0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 1)
      (fun p -> p)
      (fun _ -> 1)
      z0

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

  (** val even : int -> bool **)

  let even z0 =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> true)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> false)
        (fun _ -> true)
        (fun _ -> false)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
        (fun _ -> false)
        (fun _ -> true)
        (fun _ -> false)
        p)
      z0

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

(** val flat_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list **)

let rec flat_map f = function
| [] -> []
| x :: t0 -> app (f x) (flat_map f t0)

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| [] -> true
| a :: l0 -> (&&) (f a) (forallb f l0)

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

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some y -> Some (f y)
| None -> None

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

type spec_float =
| S754_zero of bool
| S754_infinity of bool
| S754_nan
| S754_finite of bool * int * int

(** val emin : int -> int -> int **)

let emin prec emax =
  Z.sub (Z.sub ((fun p->1+2*p) 1) emax) prec

(** val fexp : int -> int -> int -> int **)

let fexp prec emax e =
  Z.max (Z.sub e prec) (emin prec emax)

(** val digits2_pos : int -> int **)

let rec digits2_pos n0 =
  (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
    (fun p -> Coq_Pos.succ (digits2_pos p))
    (fun p -> Coq_Pos.succ (digits2_pos p))
    (fun _ -> 1)
    n0

(** val zdigits2 : int -> int **)

let zdigits2 n0 =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> n0)
    (fun p -> (digits2_pos p))
    (fun p -> (digits2_pos p))
    n0

(** val iter_pos : ('a1 -> 'a1) -> int -> 'a1 -> 'a1 **)

let rec iter_pos f n0 x =
  (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
    (fun n' -> iter_pos f n' (iter_pos f n' (f x)))
    (fun n' -> iter_pos f n' (iter_pos f n' x))
    (fun _ -> f x)
    n0

type location =
| Loc_Exact
| Loc_Inexact of comparison

type shr_record = { shr_m : int; shr_r : bool; shr_s : bool }

(** val shr_1 : shr_record -> shr_record **)

let shr_1 mrs =
  let { shr_m = m; shr_r = r; shr_s = s } = mrs in
  let s0 = (||) r s in
  ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
     (fun _ -> { shr_m = 0; shr_r = false; shr_s = s0 })
     (fun p0 ->
     (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
       (fun p -> { shr_m = p; shr_r = true; shr_s = s0 })
       (fun p -> { shr_m = p; shr_r = false; shr_s = s0 })
       (fun _ -> { shr_m = 0; shr_r = true; shr_s = s0 })
       p0)
     (fun p0 ->
     (fun f2p1 f2p f1 p ->
  if p<=1 then f1 () else if p mod 2 = 0 then f2p (p/2) else f2p1 (p/2))
       (fun p -> { shr_m = ((~-) p); shr_r = true; shr_s = s0 })
       (fun p -> { shr_m = ((~-) p); shr_r = false; shr_s = s0 })
       (fun _ -> { shr_m = 0; shr_r = true; shr_s = s0 })
       p0)
     m)

(** val loc_of_shr_record : shr_record -> location **)

let loc_of_shr_record mrs =
  let { shr_m = _; shr_r = shr_r0; shr_s = shr_s0 } = mrs in
  if shr_r0
  then if shr_s0 then Loc_Inexact Gt else Loc_Inexact Eq
  else if shr_s0 then Loc_Inexact Lt else Loc_Exact

(** val shr_record_of_loc : int -> location -> shr_record **)

let shr_record_of_loc m = function
| Loc_Exact -> { shr_m = m; shr_r = false; shr_s = false }
| Loc_Inexact c ->
  (match c with
   | Eq -> { shr_m = m; shr_r = true; shr_s = false }
   | Lt -> { shr_m = m; shr_r = false; shr_s = true }
   | Gt -> { shr_m = m; shr_r = true; shr_s = true })

(** val shr : shr_record -> int -> int -> shr_record * int **)

let shr mrs e n0 =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> (mrs, e))
    (fun p -> ((iter_pos shr_1 p mrs), (Z.add e n0)))
    (fun _ -> (mrs, e))
    n0

(** val shr_fexp :
    int -> int -> int -> int -> location -> shr_record * int **)

let shr_fexp prec emax m e l =
  shr (shr_record_of_loc m l) e
    (Z.sub (fexp prec emax (Z.add (zdigits2 m) e)) e)

(** val shl_align : int -> int -> int -> int * int **)

let shl_align mx ex ex' =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> (mx, ex))
    (fun _ -> (mx, ex))
    (fun d -> ((shift_pos d mx), ex'))
    (Z.sub ex' ex)

(** val sFcompare : spec_float -> spec_float -> comparison option **)

let sFcompare f1 f2 =
  match f1 with
  | S754_zero _ ->
    (match f2 with
     | S754_zero _ -> Some Eq
     | S754_infinity s -> Some (if s then Gt else Lt)
     | S754_nan -> None
     | S754_finite (s, _, _) -> Some (if s then Gt else Lt))
  | S754_infinity s ->
    (match f2 with
     | S754_infinity s0 ->
       Some (if s then if s0 then Eq else Lt else if s0 then Gt else Eq)
     | S754_nan -> None
     | _ -> Some (if s then Lt else Gt))
  | S754_nan -> None
  | S754_finite (s1, m1, e1) ->
    (match f2 with
     | S754_zero _ -> Some (if s1 then Lt else Gt)
     | S754_infinity s -> Some (if s then Gt else Lt)
     | S754_nan -> None
     | S754_finite (s2, m2, e2) ->
       Some
         (if s1
          then if s2
               then (match Z.compare e1 e2 with
                     | Eq -> compOpp (Coq_Pos.compare_cont Eq m1 m2)
                     | Lt -> Gt
                     | Gt -> Lt)
               else Lt
          else if s2
               then Gt
               else (match Z.compare e1 e2 with
                     | Eq -> Coq_Pos.compare_cont Eq m1 m2
                     | x -> x)))

(** val cond_Zopp : bool -> int -> int **)

let cond_Zopp b m =
  if b then Z.opp m else m

(** val new_location_even : int -> int -> location **)

let new_location_even nb_steps k =
  if zeq_bool k 0
  then Loc_Exact
  else Loc_Inexact (Z.compare (Z.mul ((fun p->2*p) 1) k) nb_steps)

(** val new_location_odd : int -> int -> location **)

let new_location_odd nb_steps k =
  if zeq_bool k 0
  then Loc_Exact
  else Loc_Inexact
         (match Z.compare (Z.add (Z.mul ((fun p->2*p) 1) k) 1) nb_steps with
          | Eq -> Lt
          | x -> x)

(** val new_location : int -> int -> location **)

let new_location nb_steps =
  if Z.even nb_steps
  then new_location_even nb_steps
  else new_location_odd nb_steps

(** val sFdiv_core_binary :
    int -> int -> int -> int -> int -> int -> (int * int) * location **)

let sFdiv_core_binary prec emax m1 e1 m2 e2 =
  let d1 = zdigits2 m1 in
  let d2 = zdigits2 m2 in
  let e' =
    Z.min (fexp prec emax (Z.sub (Z.add d1 e1) (Z.add d2 e2))) (Z.sub e1 e2)
  in
  let s = Z.sub (Z.sub e1 e2) e' in
  let m' =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> m1)
      (fun _ -> Z.shiftl m1 s)
      (fun _ -> 0)
      s
  in
  let (q, r) = Z.div_eucl m' m2 in ((q, e'), (new_location m2 r))

type radix = int
  (* singleton inductive, whose constructor was Build_radix *)

(** val radix_val : radix -> int **)

let radix_val r =
  r

(** val radix2 : radix **)

let radix2 =
  ((fun p->2*p) 1)

(** val iter_nat : ('a1 -> 'a1) -> int -> 'a1 -> 'a1 **)

let rec iter_nat f n0 x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> x)
    (fun n' -> iter_nat f n' (f x))
    n0

(** val cond_incr : bool -> int -> int **)

let cond_incr b m =
  if b then Z.add m 1 else m

(** val round_sign_DN : bool -> location -> bool **)

let round_sign_DN s = function
| Loc_Exact -> false
| Loc_Inexact _ -> s

(** val round_sign_UP : bool -> location -> bool **)

let round_sign_UP s = function
| Loc_Exact -> false
| Loc_Inexact _ -> negb s

(** val round_N : bool -> location -> bool **)

let round_N p = function
| Loc_Exact -> false
| Loc_Inexact c -> (match c with
                    | Eq -> p
                    | Lt -> false
                    | Gt -> true)

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan
| B754_finite of bool * int * int

(** val sF2B : int -> int -> spec_float -> binary_float **)

let sF2B _ _ = function
| S754_zero s -> B754_zero s
| S754_infinity s -> B754_infinity s
| S754_nan -> B754_nan
| S754_finite (s, m, e) -> B754_finite (s, m, e)

(** val b2SF : int -> int -> binary_float -> spec_float **)

let b2SF _ _ = function
| B754_zero s -> S754_zero s
| B754_infinity s -> S754_infinity s
| B754_nan -> S754_nan
| B754_finite (s, m, e) -> S754_finite (s, m, e)

(** val bcompare :
    int -> int -> binary_float -> binary_float -> comparison option **)

let bcompare prec emax f1 f2 =
  sFcompare (b2SF prec emax f1) (b2SF prec emax f2)

type mode =
| Mode_NE
| Mode_ZR
| Mode_DN
| Mode_UP
| Mode_NA

(** val choice_mode : mode -> bool -> int -> location -> int **)

let choice_mode m sx mx lx =
  match m with
  | Mode_NE -> cond_incr (round_N (negb (Z.even mx)) lx) mx
  | Mode_ZR -> mx
  | Mode_DN -> cond_incr (round_sign_DN sx lx) mx
  | Mode_UP -> cond_incr (round_sign_UP sx lx) mx
  | Mode_NA -> cond_incr (round_N true lx) mx

(** val overflow_to_inf : mode -> bool -> bool **)

let overflow_to_inf m s =
  match m with
  | Mode_ZR -> false
  | Mode_DN -> s
  | Mode_UP -> negb s
  | _ -> true

(** val binary_overflow : int -> int -> mode -> bool -> spec_float **)

let binary_overflow prec emax m s =
  if overflow_to_inf m s
  then S754_infinity s
  else S754_finite (s, (Z.to_pos (Z.sub (Z.pow ((fun p->2*p) 1) prec) 1)),
         (Z.sub emax prec))

(** val binary_fit_aux :
    int -> int -> mode -> bool -> int -> int -> spec_float **)

let binary_fit_aux prec emax mode0 sx mx ex =
  if Z.leb ex (Z.sub emax prec)
  then S754_finite (sx, mx, ex)
  else binary_overflow prec emax mode0 sx

(** val binary_round_aux :
    int -> int -> mode -> bool -> int -> int -> location -> spec_float **)

let binary_round_aux prec emax mode0 sx mx ex lx =
  let (mrs', e') = shr_fexp prec emax mx ex lx in
  let (mrs'', e'') =
    shr_fexp prec emax
      (choice_mode mode0 sx mrs'.shr_m (loc_of_shr_record mrs')) e' Loc_Exact
  in
  ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
     (fun _ -> S754_zero sx)
     (fun m -> binary_fit_aux prec emax mode0 sx m e'')
     (fun _ -> S754_nan)
     mrs''.shr_m)

(** val bmult :
    int -> int -> mode -> binary_float -> binary_float -> binary_float **)

let bmult prec emax m x y =
  match x with
  | B754_zero sx ->
    (match y with
     | B754_zero sy -> B754_zero (xorb sx sy)
     | B754_finite (sy, _, _) -> B754_zero (xorb sx sy)
     | _ -> B754_nan)
  | B754_infinity sx ->
    (match y with
     | B754_infinity sy -> B754_infinity (xorb sx sy)
     | B754_finite (sy, _, _) -> B754_infinity (xorb sx sy)
     | _ -> B754_nan)
  | B754_nan -> B754_nan
  | B754_finite (sx, mx, ex) ->
    (match y with
     | B754_zero sy -> B754_zero (xorb sx sy)
     | B754_infinity sy -> B754_infinity (xorb sx sy)
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) ->
       sF2B prec emax
         (binary_round_aux prec emax m (xorb sx sy) (Coq_Pos.mul mx my)
           (Z.add ex ey) Loc_Exact))

(** val shl_align_fexp : int -> int -> int -> int -> int * int **)

let shl_align_fexp prec emax mx ex =
  shl_align mx ex (fexp prec emax (Z.add (digits2_pos mx) ex))

(** val binary_round :
    int -> int -> mode -> bool -> int -> int -> spec_float **)

let binary_round prec emax m sx mx ex =
  let (mz, ez) = shl_align_fexp prec emax mx ex in
  binary_round_aux prec emax m sx mz ez Loc_Exact

(** val binary_normalize :
    int -> int -> mode -> int -> int -> bool -> binary_float **)

let binary_normalize prec emax mode0 m e szero =
  (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
    (fun _ -> B754_zero szero)
    (fun m0 ->
    sF2B prec emax (binary_round prec emax mode0 false m0 e))
    (fun m0 -> sF2B prec emax (binary_round prec emax mode0 true m0 e))
    m

(** val fplus_naive :
    bool -> int -> int -> bool -> int -> int -> int -> int **)

let fplus_naive sx mx ex sy my ey ez =
  Z.add (cond_Zopp sx (fst (shl_align mx ex ez)))
    (cond_Zopp sy (fst (shl_align my ey ez)))

(** val bplus :
    int -> int -> mode -> binary_float -> binary_float -> binary_float **)

let bplus prec emax m x y =
  match x with
  | B754_zero sx ->
    (match y with
     | B754_zero sy ->
       if eqb sx sy
       then x
       else (match m with
             | Mode_DN -> B754_zero true
             | _ -> B754_zero false)
     | B754_nan -> B754_nan
     | _ -> y)
  | B754_infinity sx ->
    (match y with
     | B754_infinity sy -> if eqb sx sy then x else B754_nan
     | B754_nan -> B754_nan
     | _ -> x)
  | B754_nan -> B754_nan
  | B754_finite (sx, mx, ex) ->
    (match y with
     | B754_zero _ -> x
     | B754_infinity _ -> y
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) ->
       let ez = Z.min ex ey in
       binary_normalize prec emax m (fplus_naive sx mx ex sy my ey ez) ez
         (match m with
          | Mode_DN -> true
          | _ -> false))

(** val bminus :
    int -> int -> mode -> binary_float -> binary_float -> binary_float **)

let bminus prec emax m x y =
  match x with
  | B754_zero sx ->
    (match y with
     | B754_zero sy ->
       if eqb sx (negb sy)
       then x
       else (match m with
             | Mode_DN -> B754_zero true
             | _ -> B754_zero false)
     | B754_infinity sy -> B754_infinity (negb sy)
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) -> B754_finite ((negb sy), my, ey))
  | B754_infinity sx ->
    (match y with
     | B754_infinity sy -> if eqb sx (negb sy) then x else B754_nan
     | B754_nan -> B754_nan
     | _ -> x)
  | B754_nan -> B754_nan
  | B754_finite (sx, mx, ex) ->
    (match y with
     | B754_zero _ -> x
     | B754_infinity sy -> B754_infinity (negb sy)
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) ->
       let ez = Z.min ex ey in
       binary_normalize prec emax m (fplus_naive sx mx ex (negb sy) my ey ez)
         ez (match m with
             | Mode_DN -> true
             | _ -> false))

(** val bdiv :
    int -> int -> mode -> binary_float -> binary_float -> binary_float **)

let bdiv prec emax m x y =
  match x with
  | B754_zero sx ->
    (match y with
     | B754_infinity sy -> B754_zero (xorb sx sy)
     | B754_finite (sy, _, _) -> B754_zero (xorb sx sy)
     | _ -> B754_nan)
  | B754_infinity sx ->
    (match y with
     | B754_zero sy -> B754_infinity (xorb sx sy)
     | B754_finite (sy, _, _) -> B754_infinity (xorb sx sy)
     | _ -> B754_nan)
  | B754_nan -> B754_nan
  | B754_finite (sx, mx, ex) ->
    (match y with
     | B754_zero sy -> B754_infinity (xorb sx sy)
     | B754_infinity sy -> B754_zero (xorb sx sy)
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) ->
       sF2B prec emax
         (let (p, lz) = sFdiv_core_binary prec emax mx ex my ey in
          let (mz, ez) = p in
          binary_round_aux prec emax m (xorb sx sy) mz ez lz))

type full_float =
| F754_zero of bool
| F754_infinity of bool
| F754_nan of bool * int
| F754_finite of bool * int * int

type binary_float0 =
| B754_zero0 of bool
| B754_infinity0 of bool
| B754_nan0 of bool * int
| B754_finite0 of bool * int * int

(** val b2BSN : int -> int -> binary_float0 -> binary_float **)

let b2BSN _ _ = function
| B754_zero0 s -> B754_zero s
| B754_infinity0 s -> B754_infinity s
| B754_nan0 (_, _) -> B754_nan
| B754_finite0 (s, m, e) -> B754_finite (s, m, e)

(** val fF2B : int -> int -> full_float -> binary_float0 **)

let fF2B _ _ = function
| F754_zero s -> B754_zero0 s
| F754_infinity s -> B754_infinity0 s
| F754_nan (b, pl) -> B754_nan0 (b, pl)
| F754_finite (s, m, e) -> B754_finite0 (s, m, e)

(** val bsign : int -> int -> binary_float0 -> bool **)

let bsign _ _ = function
| B754_zero0 s -> s
| B754_infinity0 s -> s
| B754_nan0 (s, _) -> s
| B754_finite0 (s, _, _) -> s

(** val get_nan_pl : int -> int -> binary_float0 -> int **)

let get_nan_pl _ _ = function
| B754_nan0 (_, pl) -> pl
| _ -> 1

(** val build_nan : int -> int -> binary_float0 -> binary_float0 **)

let build_nan prec emax x =
  B754_nan0 ((bsign prec emax x), (get_nan_pl prec emax x))

(** val bSN2B :
    int -> int -> binary_float0 -> binary_float -> binary_float0 **)

let bSN2B prec emax nan = function
| B754_zero s -> B754_zero0 s
| B754_infinity s -> B754_infinity0 s
| B754_nan -> build_nan prec emax nan
| B754_finite (s, m, e) -> B754_finite0 (s, m, e)

(** val bSN2B' : int -> int -> binary_float -> binary_float0 **)

let bSN2B' _ _ = function
| B754_zero sx -> B754_zero0 sx
| B754_infinity sx -> B754_infinity0 sx
| B754_nan -> assert false (* absurd case *)
| B754_finite (sx, mx, ex) -> B754_finite0 (sx, mx, ex)

(** val bopp :
    int -> int -> (binary_float0 -> binary_float0) -> binary_float0 ->
    binary_float0 **)

let bopp prec emax opp_nan x = match x with
| B754_zero0 sx -> B754_zero0 (negb sx)
| B754_infinity0 sx -> B754_infinity0 (negb sx)
| B754_nan0 (_, _) -> build_nan prec emax (opp_nan x)
| B754_finite0 (sx, mx, ex) -> B754_finite0 ((negb sx), mx, ex)

(** val babs :
    int -> int -> (binary_float0 -> binary_float0) -> binary_float0 ->
    binary_float0 **)

let babs prec emax abs_nan0 x = match x with
| B754_zero0 _ -> B754_zero0 false
| B754_infinity0 _ -> B754_infinity0 false
| B754_nan0 (_, _) -> build_nan prec emax (abs_nan0 x)
| B754_finite0 (_, mx, ex) -> B754_finite0 (false, mx, ex)

(** val bcompare0 :
    int -> int -> binary_float0 -> binary_float0 -> comparison option **)

let bcompare0 prec emax f1 f2 =
  bcompare prec emax (b2BSN prec emax f1) (b2BSN prec emax f2)

(** val bmult0 :
    int -> int -> (binary_float0 -> binary_float0 -> binary_float0) -> mode
    -> binary_float0 -> binary_float0 -> binary_float0 **)

let bmult0 prec emax mult_nan m x y =
  bSN2B prec emax (mult_nan x y)
    (bmult prec emax m (b2BSN prec emax x) (b2BSN prec emax y))

(** val binary_normalize0 :
    int -> int -> mode -> int -> int -> bool -> binary_float0 **)

let binary_normalize0 prec emax mode0 m e szero =
  bSN2B' prec emax (binary_normalize prec emax mode0 m e szero)

(** val bplus0 :
    int -> int -> (binary_float0 -> binary_float0 -> binary_float0) -> mode
    -> binary_float0 -> binary_float0 -> binary_float0 **)

let bplus0 prec emax plus_nan m x y =
  bSN2B prec emax (plus_nan x y)
    (bplus prec emax m (b2BSN prec emax x) (b2BSN prec emax y))

(** val bminus0 :
    int -> int -> (binary_float0 -> binary_float0 -> binary_float0) -> mode
    -> binary_float0 -> binary_float0 -> binary_float0 **)

let bminus0 prec emax minus_nan m x y =
  bSN2B prec emax (minus_nan x y)
    (bminus prec emax m (b2BSN prec emax x) (b2BSN prec emax y))

(** val bdiv0 :
    int -> int -> (binary_float0 -> binary_float0 -> binary_float0) -> mode
    -> binary_float0 -> binary_float0 -> binary_float0 **)

let bdiv0 prec emax div_nan m x y =
  bSN2B prec emax (div_nan x y)
    (bdiv prec emax m (b2BSN prec emax x) (b2BSN prec emax y))

(** val join_bits : int -> int -> bool -> int -> int -> int **)

let join_bits mw ew s m e =
  Z.add (Z.shiftl (Z.add (if s then Z.pow ((fun p->2*p) 1) ew else 0) e) mw) m

(** val split_bits : int -> int -> int -> (bool * int) * int **)

let split_bits mw ew x =
  let mm = Z.pow ((fun p->2*p) 1) mw in
  let em = Z.pow ((fun p->2*p) 1) ew in
  (((Z.leb (Z.mul mm em) x), (Z.modulo x mm)), (Z.modulo (Z.div x mm) em))

(** val bits_of_binary_float : int -> int -> binary_float0 -> int **)

let bits_of_binary_float mw ew =
  let prec = Z.add mw 1 in
  let emax = Z.pow ((fun p->2*p) 1) (Z.sub ew 1) in
  (fun x ->
  match x with
  | B754_zero0 sx -> join_bits mw ew sx 0 0
  | B754_infinity0 sx ->
    join_bits mw ew sx 0 (Z.sub (Z.pow ((fun p->2*p) 1) ew) 1)
  | B754_nan0 (sx, plx) ->
    join_bits mw ew sx plx (Z.sub (Z.pow ((fun p->2*p) 1) ew) 1)
  | B754_finite0 (sx, mx, ex) ->
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

(** val binary_float_of_bits : int -> int -> int -> binary_float0 **)

let binary_float_of_bits mw ew x =
  let prec = Z.add mw 1 in
  let emax = Z.pow ((fun p->2*p) 1) (Z.sub ew 1) in
  fF2B prec emax (binary_float_of_bits_aux mw ew x)

type binary32 = binary_float0

(** val b32_of_bits : int -> binary32 **)

let b32_of_bits =
  binary_float_of_bits ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->2*p) 1)))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))

(** val bits_of_b32 : binary32 -> int **)

let bits_of_b32 =
  bits_of_binary_float ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->2*p) 1)))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))

type binary64 = binary_float0

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

(** val default_nan_64 : bool * int **)

let default_nan_64 =
  (false,
    (let rec f n0 =
       (fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ -> 1)
         (fun n1 -> (fun p->2*p) (f n1))
         n0
     in f (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ
          0)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val default_nan_32 : bool * int **)

let default_nan_32 =
  (false,
    (let rec f n0 =
       (fun fO fS n -> if n=0 then fO () else fS (n-1))
         (fun _ -> 1)
         (fun n1 -> (fun p->2*p) (f n1))
         n0
     in f (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ 0))))))))))))))))))))))))

(** val choose_nan :
    (int -> bool) -> (bool * int) -> (bool * int) list -> bool * int **)

let choose_nan is_signaling default l0 =
  let rec choose_snan = function
  | [] -> (match l0 with
           | [] -> default
           | n0 :: _ -> n0)
  | n0 :: l2 ->
    let (_, p) = n0 in if is_signaling p then n0 else choose_snan l2
  in choose_snan l0

(** val choose_nan_64 : (bool * int) list -> bool * int **)

let choose_nan_64 =
  choose_nan (fun p ->
    negb
      (Coq_Pos.testbit p ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
        ((fun p->2*p) ((fun p->1+2*p) 1))))))) default_nan_64

(** val choose_nan_32 : (bool * int) list -> bool * int **)

let choose_nan_32 =
  choose_nan (fun p ->
    negb
      (Coq_Pos.testbit p ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
        ((fun p->2*p) 1)))))) default_nan_32

(** val float_of_single_preserves_sNaN : bool **)

let float_of_single_preserves_sNaN =
  false

(** val float_conversion_default_nan : bool **)

let float_conversion_default_nan =
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

  (** val eq_dec : int -> int -> bool **)

  let eq_dec x y =
    let intval0 = x in let intval1 = y in zeq intval0 intval1

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

  (** val of_int : int -> int **)

  let of_int x =
    repr (Int.unsigned x)

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

module type EQUALITY_TYPE =
 sig
  type t

  val eq : t -> t -> bool
 end

module EMap =
 functor (X:EQUALITY_TYPE) ->
 struct
  type elt = X.t

  (** val elt_eq : X.t -> X.t -> bool **)

  let elt_eq =
    X.eq

  type 'a t = X.t -> 'a

  (** val init : 'a1 -> X.t -> 'a1 **)

  let init v _ =
    v

  (** val get : X.t -> 'a1 t -> 'a1 **)

  let get x m =
    m x

  (** val set : X.t -> 'a1 -> 'a1 t -> X.t -> 'a1 **)

  let set x v m y =
    if X.eq y x then v else m y

  (** val map : ('a1 -> 'a2) -> 'a1 t -> X.t -> 'a2 **)

  let map f m x =
    f (m x)
 end

(** val beq_dec : int -> int -> binary_float0 -> binary_float0 -> bool **)

let beq_dec _ _ f1 f2 =
  match f1 with
  | B754_zero0 s1 ->
    (match f2 with
     | B754_zero0 s2 ->
       if s1 then if s2 then true else false else if s2 then false else true
     | _ -> false)
  | B754_infinity0 s1 ->
    (match f2 with
     | B754_infinity0 s2 ->
       if s1 then if s2 then true else false else if s2 then false else true
     | _ -> false)
  | B754_nan0 (s1, p1) ->
    (match f2 with
     | B754_nan0 (s2, p2) ->
       if s1
       then if s2 then Coq_Pos.eq_dec p1 p2 else false
       else if s2 then false else Coq_Pos.eq_dec p1 p2
     | _ -> false)
  | B754_finite0 (s1, m1, e1) ->
    (match f2 with
     | B754_finite0 (s2, m2, e2) ->
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

(** val bofZ : int -> int -> int -> binary_float0 **)

let bofZ prec emax n0 =
  binary_normalize0 prec emax Mode_NE n0 0 false

(** val zofB : int -> int -> binary_float0 -> int option **)

let zofB _ _ = function
| B754_zero0 _ -> Some 0
| B754_finite0 (s, m, e0) ->
  ((fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
     (fun _ -> Some (cond_Zopp s m))
     (fun e -> Some
     (Z.mul (cond_Zopp s m) (Z.pow_pos (radix_val radix2) e)))
     (fun e -> Some
     (cond_Zopp s (Z.div m (Z.pow_pos (radix_val radix2) e))))
     e0)
| _ -> None

(** val zofB_range :
    int -> int -> binary_float0 -> int -> int -> int option **)

let zofB_range prec emax f zmin zmax =
  match zofB prec emax f with
  | Some z0 -> if (&&) (Z.leb zmin z0) (Z.leb z0 zmax) then Some z0 else None
  | None -> None

(** val bconv :
    int -> int -> int -> int -> (binary_float0 -> binary_float0) -> mode ->
    binary_float0 -> binary_float0 **)

let bconv _ _ prec2 emax2 conv_nan md f = match f with
| B754_nan0 (_, _) -> build_nan prec2 emax2 (conv_nan f)
| B754_finite0 (s, m, e) ->
  binary_normalize0 prec2 emax2 md (cond_Zopp s m) e s
| x -> x

type float = binary64

type float32 = binary32

(** val cmp_of_comparison : comparison0 -> comparison option -> bool **)

let cmp_of_comparison c x =
  match c with
  | Ceq ->
    (match x with
     | Some c0 -> (match c0 with
                   | Eq -> true
                   | _ -> false)
     | None -> false)
  | Cne ->
    (match x with
     | Some c0 -> (match c0 with
                   | Eq -> false
                   | _ -> true)
     | None -> true)
  | Clt ->
    (match x with
     | Some c0 -> (match c0 with
                   | Lt -> true
                   | _ -> false)
     | None -> false)
  | Cle ->
    (match x with
     | Some c0 -> (match c0 with
                   | Gt -> false
                   | _ -> true)
     | None -> false)
  | Cgt ->
    (match x with
     | Some c0 -> (match c0 with
                   | Gt -> true
                   | _ -> false)
     | None -> false)
  | Cge ->
    (match x with
     | Some c0 -> (match c0 with
                   | Lt -> false
                   | _ -> true)
     | None -> false)

(** val quiet_nan_64_payload : int -> int **)

let quiet_nan_64_payload p =
  Z.to_pos
    (p_mod_two_p
      (Coq_Pos.coq_lor p
        (iter_nat (fun x -> (fun p->2*p) x) (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ
          0))))))))))))))))))))))))))))))))))))))))))))))))))) 1))
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val quiet_nan_64 : (bool * int) -> float **)

let quiet_nan_64 = function
| (s, p) -> B754_nan0 (s, (quiet_nan_64_payload p))

(** val default_nan_0 : float **)

let default_nan_0 =
  quiet_nan_64 default_nan_64

(** val quiet_nan_32_payload : int -> int **)

let quiet_nan_32_payload p =
  Z.to_pos
    (p_mod_two_p
      (Coq_Pos.coq_lor p
        (iter_nat (fun x -> (fun p->2*p) x) (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
          0)))))))))))))))))))))) 1)) (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ 0))))))))))))))))))))))))

(** val quiet_nan_32 : (bool * int) -> float32 **)

let quiet_nan_32 = function
| (s, p) -> B754_nan0 (s, (quiet_nan_32_payload p))

(** val default_nan_1 : float32 **)

let default_nan_1 =
  quiet_nan_32 default_nan_32

module Float =
 struct
  (** val expand_nan_payload : int -> int **)

  let expand_nan_payload p =
    Coq_Pos.shiftl_nat p (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ 0)))))))))))))))))))))))))))))

  (** val expand_nan : bool -> int -> binary_float0 **)

  let expand_nan s p =
    B754_nan0 (s, (expand_nan_payload p))

  (** val of_single_nan : float32 -> float **)

  let of_single_nan = function
  | B754_nan0 (s, p) ->
    if float_conversion_default_nan
    then default_nan_0
    else if float_of_single_preserves_sNaN
         then expand_nan s p
         else quiet_nan_64 (s, (expand_nan_payload p))
  | _ -> default_nan_0

  (** val reduce_nan_payload : int -> int **)

  let reduce_nan_payload p =
    Coq_Pos.shiftr_nat (quiet_nan_64_payload p) (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
      0)))))))))))))))))))))))))))))

  (** val to_single_nan : float -> float32 **)

  let to_single_nan = function
  | B754_nan0 (s, p) ->
    if float_conversion_default_nan
    then default_nan_1
    else quiet_nan_32 (s, (reduce_nan_payload p))
  | _ -> default_nan_1

  (** val neg_nan : float -> float **)

  let neg_nan = function
  | B754_nan0 (s, p) -> B754_nan0 ((negb s), p)
  | _ -> default_nan_0

  (** val abs_nan : float -> float **)

  let abs_nan = function
  | B754_nan0 (_, p) -> B754_nan0 (false, p)
  | _ -> default_nan_0

  (** val cons_pl : float -> (bool * int) list -> (bool * int) list **)

  let cons_pl x l =
    match x with
    | B754_nan0 (s, p) -> (s, p) :: l
    | _ -> l

  (** val binop_nan : float -> float -> float **)

  let binop_nan x y =
    quiet_nan_64 (choose_nan_64 (cons_pl x (cons_pl y [])))

  (** val zero : float **)

  let zero =
    B754_zero0 false

  (** val eq_dec : float -> float -> bool **)

  let eq_dec =
    beq_dec ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))))))

  (** val neg : float -> float **)

  let neg =
    bopp ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) neg_nan

  (** val abs : float -> float **)

  let abs =
    babs ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) abs_nan

  (** val add : float -> float -> float **)

  let add =
    bplus0 ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) binop_nan Mode_NE

  (** val sub : float -> float -> float **)

  let sub =
    bminus0 ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) binop_nan Mode_NE

  (** val mul : float -> float -> float **)

  let mul =
    bmult0 ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) binop_nan Mode_NE

  (** val div : float -> float -> float **)

  let div =
    bdiv0 ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) binop_nan Mode_NE

  (** val compare : float -> float -> comparison option **)

  let compare f1 f2 =
    bcompare0 ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) f1 f2

  (** val cmp : comparison0 -> float -> float -> bool **)

  let cmp c f1 f2 =
    cmp_of_comparison c (compare f1 f2)

  (** val of_single : float32 -> float **)

  let of_single =
    bconv ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))))))
      of_single_nan Mode_NE

  (** val to_single : float -> float32 **)

  let to_single =
    bconv ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->1+2*p) 1)))) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1))))))) to_single_nan Mode_NE

  (** val to_int : float -> int option **)

  let to_int f =
    option_map Int.repr
      (zofB_range ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
        ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
        ((fun p->2*p) ((fun p->2*p) 1)))))))))) f Int.min_signed
        Int.max_signed)

  (** val to_intu : float -> int option **)

  let to_intu f =
    option_map Int.repr
      (zofB_range ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
        ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
        ((fun p->2*p) ((fun p->2*p) 1)))))))))) f 0 Int.max_unsigned)

  (** val of_int : int -> float **)

  let of_int n0 =
    bofZ ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) (Int.signed n0)

  (** val of_intu : int -> float **)

  let of_intu n0 =
    bofZ ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) 1))))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))))) (Int.unsigned n0)

  (** val to_bits : float -> int **)

  let to_bits f =
    Int64.repr (bits_of_b64 f)

  (** val of_bits : int -> float **)

  let of_bits b =
    b64_of_bits (Int64.unsigned b)
 end

module Float32 =
 struct
  (** val neg_nan : float32 -> float32 **)

  let neg_nan = function
  | B754_nan0 (s, p) -> B754_nan0 ((negb s), p)
  | _ -> default_nan_1

  (** val abs_nan : float32 -> float32 **)

  let abs_nan = function
  | B754_nan0 (_, p) -> B754_nan0 (false, p)
  | _ -> default_nan_1

  (** val cons_pl : float32 -> (bool * int) list -> (bool * int) list **)

  let cons_pl x l =
    match x with
    | B754_nan0 (s, p) -> (s, p) :: l
    | _ -> l

  (** val binop_nan : float32 -> float32 -> float32 **)

  let binop_nan x y =
    quiet_nan_32 (choose_nan_32 (cons_pl x (cons_pl y [])))

  (** val zero : float32 **)

  let zero =
    B754_zero0 false

  (** val eq_dec : float32 -> float32 -> bool **)

  let eq_dec =
    beq_dec ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1)))))))

  (** val neg : float32 -> float32 **)

  let neg =
    bopp ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) neg_nan

  (** val abs : float32 -> float32 **)

  let abs =
    babs ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) abs_nan

  (** val add : float32 -> float32 -> float32 **)

  let add =
    bplus0 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) binop_nan Mode_NE

  (** val sub : float32 -> float32 -> float32 **)

  let sub =
    bminus0 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) binop_nan Mode_NE

  (** val mul : float32 -> float32 -> float32 **)

  let mul =
    bmult0 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) binop_nan Mode_NE

  (** val div : float32 -> float32 -> float32 **)

  let div =
    bdiv0 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) binop_nan Mode_NE

  (** val compare : float32 -> float32 -> comparison option **)

  let compare f1 f2 =
    bcompare0 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) f1 f2

  (** val cmp : comparison0 -> float32 -> float32 -> bool **)

  let cmp c f1 f2 =
    cmp_of_comparison c (compare f1 f2)

  (** val to_int : float32 -> int option **)

  let to_int f =
    option_map Int.repr
      (zofB_range ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
        1)))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))) f Int.min_signed
        Int.max_signed)

  (** val to_intu : float32 -> int option **)

  let to_intu f =
    option_map Int.repr
      (zofB_range ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
        1)))) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
        ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))))) f 0
        Int.max_unsigned)

  (** val of_int : int -> float32 **)

  let of_int n0 =
    bofZ ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) (Int.signed n0)

  (** val of_intu : int -> float32 **)

  let of_intu n0 =
    bofZ ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1))))
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) 1))))))) (Int.unsigned n0)

  (** val to_bits : float32 -> int **)

  let to_bits f =
    Int.repr (bits_of_b32 f)

  (** val of_bits : int -> float32 **)

  let of_bits b =
    b32_of_bits (Int.unsigned b)
 end

type typ =
| Tint
| Tfloat
| Tlong
| Tsingle
| Tany32
| Tany64

type rettype =
| Tret of typ
| Tint8signed
| Tint8unsigned
| Tint16signed
| Tint16unsigned
| Tvoid

type calling_convention = { cc_vararg : int option; cc_unproto : bool;
                            cc_structret : bool }

(** val cc_default : calling_convention **)

let cc_default =
  { cc_vararg = None; cc_unproto = false; cc_structret = false }

type signature = { sig_args : typ list; sig_res : rettype;
                   sig_cc : calling_convention }

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

let string_of_val0 v =
  match v with
  | Vundef -> "undefied value"
  | Vint x0 -> "Int32: " ^ (string_of_int x0)
  | Vlong x0 -> "Int64: " ^ (string_of_int x0)
  | Vptr (b, ofs) -> (string_of_int b) ^ "," ^ (string_of_int ofs)
  | _ -> "unexpected cases"

let print_val0 v = print_endline (string_of_val0 v)

(** val vtrue : val0 **)

let vtrue =
  Vint Int.one

(** val vfalse : val0 **)

let vfalse =
  Vint Int.zero

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

  (** val has_type_dec : val0 -> typ -> bool **)

  let has_type_dec v t0 =
    match v with
    | Vundef -> true
    | Vint _ ->
      (match t0 with
       | Tint -> true
       | Tany32 -> true
       | Tany64 -> true
       | _ -> false)
    | Vlong _ -> (match t0 with
                  | Tlong -> true
                  | Tany64 -> true
                  | _ -> false)
    | Vfloat _ -> (match t0 with
                   | Tfloat -> true
                   | Tany64 -> true
                   | _ -> false)
    | Vsingle _ ->
      (match t0 with
       | Tint -> false
       | Tfloat -> false
       | Tlong -> false
       | _ -> true)
    | Vptr (_, _) ->
      (match t0 with
       | Tint -> bool_dec ptr64 false
       | Tlong -> bool_dec ptr64 true
       | Tany32 -> bool_dec ptr64 false
       | Tany64 -> true
       | _ -> false)

  (** val neg : val0 -> val0 **)

  let neg = function
  | Vint n0 -> Vint (Int.neg n0)
  | _ -> Vundef

  (** val negf : val0 -> val0 **)

  let negf = function
  | Vfloat f -> Vfloat (Float.neg f)
  | _ -> Vundef

  (** val absf : val0 -> val0 **)

  let absf = function
  | Vfloat f -> Vfloat (Float.abs f)
  | _ -> Vundef

  (** val negfs : val0 -> val0 **)

  let negfs = function
  | Vsingle f -> Vsingle (Float32.neg f)
  | _ -> Vundef

  (** val absfs : val0 -> val0 **)

  let absfs = function
  | Vsingle f -> Vsingle (Float32.abs f)
  | _ -> Vundef

  (** val intoffloat : val0 -> val0 option **)

  let intoffloat = function
  | Vfloat f -> option_map (fun x -> Vint x) (Float.to_int f)
  | _ -> None

  (** val intuoffloat : val0 -> val0 option **)

  let intuoffloat = function
  | Vfloat f -> option_map (fun x -> Vint x) (Float.to_intu f)
  | _ -> None

  (** val floatofint : val0 -> val0 option **)

  let floatofint = function
  | Vint n0 -> Some (Vfloat (Float.of_int n0))
  | _ -> None

  (** val floatofintu : val0 -> val0 option **)

  let floatofintu = function
  | Vint n0 -> Some (Vfloat (Float.of_intu n0))
  | _ -> None

  (** val intofsingle : val0 -> val0 option **)

  let intofsingle = function
  | Vsingle f -> option_map (fun x -> Vint x) (Float32.to_int f)
  | _ -> None

  (** val intuofsingle : val0 -> val0 option **)

  let intuofsingle = function
  | Vsingle f -> option_map (fun x -> Vint x) (Float32.to_intu f)
  | _ -> None

  (** val singleofint : val0 -> val0 option **)

  let singleofint = function
  | Vint n0 -> Some (Vsingle (Float32.of_int n0))
  | _ -> None

  (** val singleofintu : val0 -> val0 option **)

  let singleofintu = function
  | Vint n0 -> Some (Vsingle (Float32.of_intu n0))
  | _ -> None

  (** val notint : val0 -> val0 **)

  let notint = function
  | Vint n0 -> Vint (Int.not n0)
  | _ -> Vundef

  (** val of_bool : bool -> val0 **)

  let of_bool = function
  | true -> vtrue
  | false -> vfalse

  (** val sign_ext : int -> val0 -> val0 **)

  let sign_ext nbits = function
  | Vint n0 -> Vint (Int.sign_ext nbits n0)
  | _ -> Vundef

  (** val singleoffloat : val0 -> val0 **)

  let singleoffloat = function
  | Vfloat f -> Vsingle (Float.to_single f)
  | _ -> Vundef

  (** val floatofsingle : val0 -> val0 **)

  let floatofsingle = function
  | Vsingle f -> Vfloat (Float.of_single f)
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

  (** val mulhs : val0 -> val0 -> val0 **)

  let mulhs v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 -> Vint (Int.mulhs n1 n2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val mulhu : val0 -> val0 -> val0 **)

  let mulhu v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 -> Vint (Int.mulhu n1 n2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val divs : val0 -> val0 -> val0 option **)

  let divs v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 ->
         if (||) (Int.eq n2 Int.zero)
              ((&&) (Int.eq n1 (Int.repr Int.min_signed))
                (Int.eq n2 Int.mone))
         then None
         else Some (Vint (Int.divs n1 n2))
       | _ -> None)
    | _ -> None

  (** val divu : val0 -> val0 -> val0 option **)

  let divu v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 ->
         if Int.eq n2 Int.zero then None else Some (Vint (Int.divu n1 n2))
       | _ -> None)
    | _ -> None

  (** val sub_overflow : val0 -> val0 -> val0 **)

  let sub_overflow v1 v2 =
    match v1 with
    | Vint n1 ->
      (match v2 with
       | Vint n2 -> Vint (Int.sub_overflow n1 n2 Int.zero)
       | _ -> Vundef)
    | _ -> Vundef

  (** val negative : val0 -> val0 **)

  let negative = function
  | Vint n0 -> Vint (Int.negative n0)
  | _ -> Vundef

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

  (** val ror : val0 -> val0 -> val0 **)

  let ror v1 v2 =
    match v1 with
    | Vint n1 -> (match v2 with
                  | Vint n2 -> Vint (Int.ror n1 n2)
                  | _ -> Vundef)
    | _ -> Vundef

  (** val addf : val0 -> val0 -> val0 **)

  let addf v1 v2 =
    match v1 with
    | Vfloat f1 ->
      (match v2 with
       | Vfloat f2 -> Vfloat (Float.add f1 f2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val subf : val0 -> val0 -> val0 **)

  let subf v1 v2 =
    match v1 with
    | Vfloat f1 ->
      (match v2 with
       | Vfloat f2 -> Vfloat (Float.sub f1 f2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val mulf : val0 -> val0 -> val0 **)

  let mulf v1 v2 =
    match v1 with
    | Vfloat f1 ->
      (match v2 with
       | Vfloat f2 -> Vfloat (Float.mul f1 f2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val divf : val0 -> val0 -> val0 **)

  let divf v1 v2 =
    match v1 with
    | Vfloat f1 ->
      (match v2 with
       | Vfloat f2 -> Vfloat (Float.div f1 f2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val addfs : val0 -> val0 -> val0 **)

  let addfs v1 v2 =
    match v1 with
    | Vsingle f1 ->
      (match v2 with
       | Vsingle f2 -> Vsingle (Float32.add f1 f2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val subfs : val0 -> val0 -> val0 **)

  let subfs v1 v2 =
    match v1 with
    | Vsingle f1 ->
      (match v2 with
       | Vsingle f2 -> Vsingle (Float32.sub f1 f2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val mulfs : val0 -> val0 -> val0 **)

  let mulfs v1 v2 =
    match v1 with
    | Vsingle f1 ->
      (match v2 with
       | Vsingle f2 -> Vsingle (Float32.mul f1 f2)
       | _ -> Vundef)
    | _ -> Vundef

  (** val divfs : val0 -> val0 -> val0 **)

  let divfs v1 v2 =
    match v1 with
    | Vsingle f1 ->
      (match v2 with
       | Vsingle f2 -> Vsingle (Float32.div f1 f2)
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

  (** val of_optbool : bool option -> val0 **)

  let of_optbool = function
  | Some b -> if b then vtrue else vfalse
  | None -> Vundef

  (** val cmpu :
      (block -> int -> bool) -> comparison0 -> val0 -> val0 -> val0 **)

  let cmpu valid_ptr c v1 v2 =
    of_optbool (cmpu_bool valid_ptr c v1 v2)

  (** val offset_ptr : val0 -> int -> val0 **)

  let offset_ptr v delta =
    match v with
    | Vptr (b, ofs) -> Vptr (b, (Ptrofs.add ofs delta))
    | _ -> Vundef

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

  (** val valid_pointer : mem -> block -> int -> bool **)

  let valid_pointer m b ofs =
    proj_sumbool (perm_dec m b ofs Cur Nonempty)

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

  (** val loadbytes : mem -> block -> int -> int -> memval list option **)

  let loadbytes m b ofs n0 =
    if range_perm_dec m b ofs (Z.add ofs n0) Cur Readable
    then Some (getN (Z.to_nat n0) ofs (PMap.get b m.mem_contents))
    else None

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

  (** val storebytes : mem -> block -> int -> memval list -> mem option **)

  let storebytes m b ofs bytes =
    if range_perm_dec m b ofs (Z.add ofs (Z.of_nat (length bytes))) Cur
         Writable
    then Some { mem_contents =
           (PMap.set b (setN bytes ofs (PMap.get b m.mem_contents))
             m.mem_contents); mem_access = m.mem_access; nextblock =
           m.nextblock }
    else None
 end

type mreg =
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
| R11
| R12
| F0
| F1
| F2
| F3
| F4
| F5
| F6
| F7
| F8
| F9
| F10
| F11
| F12
| F13
| F14
| F15

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

(** val ireg_eq : ireg -> ireg -> bool **)

let ireg_eq x y =
  match x with
  | IR0 -> (match y with
            | IR0 -> true
            | _ -> false)
  | IR1 -> (match y with
            | IR1 -> true
            | _ -> false)
  | IR2 -> (match y with
            | IR2 -> true
            | _ -> false)
  | IR3 -> (match y with
            | IR3 -> true
            | _ -> false)
  | IR4 -> (match y with
            | IR4 -> true
            | _ -> false)
  | IR5 -> (match y with
            | IR5 -> true
            | _ -> false)
  | IR6 -> (match y with
            | IR6 -> true
            | _ -> false)
  | IR7 -> (match y with
            | IR7 -> true
            | _ -> false)
  | IR8 -> (match y with
            | IR8 -> true
            | _ -> false)
  | IR9 -> (match y with
            | IR9 -> true
            | _ -> false)
  | IR10 -> (match y with
             | IR10 -> true
             | _ -> false)
  | IR11 -> (match y with
             | IR11 -> true
             | _ -> false)
  | IR12 -> (match y with
             | IR12 -> true
             | _ -> false)
  | IR13 -> (match y with
             | IR13 -> true
             | _ -> false)
  | IR14 -> (match y with
             | IR14 -> true
             | _ -> false)

(** val freg_eq : freg -> freg -> bool **)

let freg_eq x y =
  match x with
  | FR0 -> (match y with
            | FR0 -> true
            | _ -> false)
  | FR1 -> (match y with
            | FR1 -> true
            | _ -> false)
  | FR2 -> (match y with
            | FR2 -> true
            | _ -> false)
  | FR3 -> (match y with
            | FR3 -> true
            | _ -> false)
  | FR4 -> (match y with
            | FR4 -> true
            | _ -> false)
  | FR5 -> (match y with
            | FR5 -> true
            | _ -> false)
  | FR6 -> (match y with
            | FR6 -> true
            | _ -> false)
  | FR7 -> (match y with
            | FR7 -> true
            | _ -> false)
  | FR8 -> (match y with
            | FR8 -> true
            | _ -> false)
  | FR9 -> (match y with
            | FR9 -> true
            | _ -> false)
  | FR10 -> (match y with
             | FR10 -> true
             | _ -> false)
  | FR11 -> (match y with
             | FR11 -> true
             | _ -> false)
  | FR12 -> (match y with
             | FR12 -> true
             | _ -> false)
  | FR13 -> (match y with
             | FR13 -> true
             | _ -> false)
  | FR14 -> (match y with
             | FR14 -> true
             | _ -> false)
  | FR15 -> (match y with
             | FR15 -> true
             | _ -> false)

type crbit =
| CN
| CZ
| CC
| CV

(** val crbit_eq : crbit -> crbit -> bool **)

let crbit_eq x y =
  match x with
  | CN -> (match y with
           | CN -> true
           | _ -> false)
  | CZ -> (match y with
           | CZ -> true
           | _ -> false)
  | CC -> (match y with
           | CC -> true
           | _ -> false)
  | CV -> (match y with
           | CV -> true
           | _ -> false)

type preg =
| IR of ireg
| FR of freg
| CR of crbit
| PC

(** val preg_eq : preg -> preg -> bool **)

let preg_eq x y =
  match x with
  | IR x0 -> (match y with
              | IR i0 -> ireg_eq x0 i0
              | _ -> false)
  | FR x0 -> (match y with
              | FR f0 -> freg_eq x0 f0
              | _ -> false)
  | CR x0 -> (match y with
              | CR c0 -> crbit_eq x0 c0
              | _ -> false)
  | PC -> (match y with
           | PC -> true
           | _ -> false)

module PregEq =
 struct
  type t = preg

  (** val eq : preg -> preg -> bool **)

  let eq =
    preg_eq
 end

module Pregmap = EMap(PregEq)

type shift_op =
| SOimm of int
| SOreg of ireg
| SOlsl of ireg * int
| SOlsr of ireg * int
| SOasr of ireg * int
| SOror of ireg * int

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

(** val preg_of : mreg -> preg **)

let preg_of = function
| R0 -> IR IR0
| R1 -> IR IR1
| R2 -> IR IR2
| R3 -> IR IR3
| R4 -> IR IR4
| R5 -> IR IR5
| R6 -> IR IR6
| R7 -> IR IR7
| R8 -> IR IR8
| R9 -> IR IR9
| R10 -> IR IR10
| R11 -> IR IR11
| R12 -> IR IR12
| F0 -> FR FR0
| F1 -> FR FR1
| F2 -> FR FR2
| F3 -> FR FR3
| F4 -> FR FR4
| F5 -> FR FR5
| F6 -> FR FR6
| F7 -> FR FR7
| F8 -> FR FR8
| F9 -> FR FR9
| F10 -> FR FR10
| F11 -> FR FR11
| F12 -> FR FR12
| F13 -> FR FR13
| F14 -> FR FR14
| F15 -> FR FR15

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
  | IR14 -> "LR "

let string_of_shift_op sop =
  match sop with
  | SOimm i  -> string_of_int i
  | SOreg ir -> string_of_ireg ir
  | _ -> "shift_op not yet"

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

(** val int_callee_save_regs : mreg list **)

let int_callee_save_regs =
  R4 :: (R5 :: (R6 :: (R7 :: (R8 :: (R9 :: (R10 :: (R11 :: [])))))))

(** val float_callee_save_regs : mreg list **)

let float_callee_save_regs =
  F8 :: (F9 :: (F10 :: (F11 :: (F12 :: (F13 :: (F14 :: (F15 :: [])))))))

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

(** val isize : int **)

let isize =
  Ptrofs.repr ((fun p->2*p) ((fun p->2*p) 1))

type sreg =
| Sreg of preg
| Ssp

type sval =
| Sval of sreg * int

type aval =
| Cval of val0
| Aval of sval

type aregset = aval Pregmap.t

let string_of_preg pr =
  match pr with
  | IR ir -> string_of_ireg ir
  | PC -> "PC"
  | _ -> "preg: not yet"
  
let string_of_sreg sr =
  match sr with
  | Sreg pr -> string_of_preg pr
  | Ssp -> "New SP"

let string_of_reg_val a =
  match a with
  | Cval v -> string_of_val0 v
  | Aval s ->
    match s with
    | Sval (sr, i) -> ("SVal " ^ (string_of_sreg sr) ^ ", " ^ (string_of_int i))
    
let print_aregset rs =
  let _ = print_endline "****************arm32 regs***************" in
  let _ = print_endline ("R0=  " ^ (string_of_reg_val (rs (IR IR0)))) in
  let _ = print_endline ("R1=  " ^ (string_of_reg_val (rs (IR IR1)))) in
  let _ = print_endline ("R2=  " ^ (string_of_reg_val (rs (IR IR2)))) in
  let _ = print_endline ("R3=  " ^ (string_of_reg_val (rs (IR IR3)))) in
  let _ = print_endline ("R4=  " ^ (string_of_reg_val (rs (IR IR4)))) in
  let _ = print_endline ("R5=  " ^ (string_of_reg_val (rs (IR IR5)))) in
  let _ = print_endline ("R6=  " ^ (string_of_reg_val (rs (IR IR6)))) in
  let _ = print_endline ("R7=  " ^ (string_of_reg_val (rs (IR IR7)))) in
  let _ = print_endline ("R8=  " ^ (string_of_reg_val (rs (IR IR8)))) in
  let _ = print_endline ("R9=  " ^ (string_of_reg_val (rs (IR IR9)))) in
  let _ = print_endline ("R10= " ^ (string_of_reg_val (rs (IR IR10)))) in
  let _ = print_endline ("FP=  " ^ (string_of_reg_val (rs (IR IR11)))) in
  let _ = print_endline ("IP=  " ^ (string_of_reg_val (rs (IR IR12)))) in
  let _ = print_endline ("SP=  " ^ (string_of_reg_val (rs (IR IR13)))) in
  let _ = print_endline ("LR=  " ^ (string_of_reg_val (rs (IR IR14)))) in
  let _ = print_endline ("PC=  " ^ (string_of_reg_val (rs PC))) in
    print_endline "****************arm32 regs***************\n\n"

type amemval =
| Cmemval of memval
| Amemval of sval * int

(** val sreg_eq : sreg -> sreg -> bool **)

let sreg_eq r r' =
  match r with
  | Sreg r1 ->
    (match r' with
     | Sreg r2 -> proj_sumbool (preg_eq r1 r2)
     | Ssp -> false)
  | Ssp -> (match r' with
            | Sreg _ -> false
            | Ssp -> true)

(** val sval_eq : sval -> sval -> bool **)

let sval_eq sv sv' =
  let Sval (b, o) = sv in
  let Sval (b', o') = sv' in (&&) (sreg_eq b b') (Int.eq o o')

(** val aval_eq : aval -> aval -> bool **)

let aval_eq v1 v2 =
  match v1 with
  | Cval v3 ->
    (match v2 with
     | Cval v4 -> proj_sumbool (Val.eq v3 v4)
     | Aval _ -> false)
  | Aval v3 -> (match v2 with
                | Cval _ -> false
                | Aval v4 -> sval_eq v3 v4)

(** val find_instr : aval -> Mem.mem -> instruction option **)

let find_instr v m =
  match v with
  | Cval v0 -> (* let _ = print_val0 v0 in *)
    (match Mem.loadv Mint32 m v0 with
     | Some v1 -> (match v1 with
                   | Vint i -> (* let _ = print_endline ("ins: " ^ (string_of_int i)) in *)
                     let res = decode i in
                     let _ = match res with | Some t -> print_endline (string_of_instruction t) | None -> print_endline "decode error" in res
                   | _ -> None)
     | None -> let _ = print_endline "find_instr: Cval load None" in None)
  | Aval _ -> let _ = print_endline "find_instr: Aval" in None

(** val encode_sval_fragments : sval -> amemval list **)

let encode_sval_fragments r =
  (Amemval (r, (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0))))) :: ((Amemval (r, (Stdlib.succ (Stdlib.succ
    0)))) :: ((Amemval (r, (Stdlib.succ 0))) :: ((Amemval (r, 0)) :: [])))

(** val encode_sval : memory_chunk -> sval -> amemval list **)

let encode_sval chunk r =
  match chunk with
  | Mint32 -> encode_sval_fragments r
  | Mint64 ->
    map (fun x -> Cmemval x)
      (repeat Undef (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
        (Stdlib.succ 0)))))))))
  | Many32 -> encode_sval_fragments r
  | Many64 -> map (fun x -> Cmemval x) (inj_value Q64 Vundef)
  | _ -> map (fun x -> Cmemval x) (repeat Undef (size_chunk_nat chunk))

(** val encode_aval : memory_chunk -> aval -> amemval list **)

let encode_aval chunk = function
| Cval v0 -> map (fun x -> Cmemval x) (encode_val chunk v0)
| Aval r -> encode_sval chunk r

(** val setN0 : amemval list -> int -> amemval ZMap.t -> amemval ZMap.t **)

let rec setN0 vl p c =
  match vl with
  | [] -> c
  | v :: vl' -> setN0 vl' (Z.add p 1) (ZMap.set p v c)

(** val getN0 : int -> int -> amemval ZMap.t -> amemval list **)

let rec getN0 n0 p c =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n' -> (ZMap.get p c) :: (getN0 n' (Z.add p 1) c))
    n0

(** val astack_store :
    memory_chunk -> Mem.mem -> amemval ZMap.t -> block -> int -> aval ->
    amemval ZMap.t option **)

let astack_store chunk m mb b ofs v =
  if Mem.valid_access_dec m chunk b ofs Writable
  then Some (setN0 (encode_aval chunk v) ofs mb)
  else None

type amemSplit =
| SplitErr
| SplitNIL
| SplitC of memval list
| SplitA of (sval * int) list

(** val cons_amemval : amemval -> amemSplit -> amemSplit **)

let cons_amemval m = function
| SplitErr -> SplitErr
| SplitNIL ->
  (match m with
   | Cmemval x -> SplitC (x :: [])
   | Amemval (s0, o) -> SplitA ((s0, o) :: []))
| SplitC l ->
  (match m with
   | Cmemval x -> SplitC (x :: l)
   | Amemval (_, _) -> SplitErr)
| SplitA l ->
  (match m with
   | Cmemval _ -> SplitErr
   | Amemval (s0, o) -> SplitA ((s0, o) :: l))

(** val split_amemval : amemval list -> amemSplit **)

let rec split_amemval = function
| [] -> SplitNIL
| e :: l0 -> cons_amemval e (split_amemval l0)

(** val check_value0 : int -> sval -> (sval * int) list -> bool **)

let rec check_value0 n0 v vl =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> match vl with
              | [] -> true
              | _ :: _ -> false)
    (fun m ->
    match vl with
    | [] -> false
    | p :: vl' ->
      let (v', idx) = p in
      (&&) ((&&) (sval_eq v v') ((=) m idx)) (check_value0 m v vl'))
    n0

(** val proj_value0 : (sval * int) list -> aval **)

let proj_value0 vl = match vl with
| [] -> Cval Vundef
| p :: _ ->
  let (v, _) = p in
  if check_value0 (Stdlib.succ (Stdlib.succ (Stdlib.succ
       (Stdlib.succ 0)))) v vl
  then Aval v
  else Cval Vundef

(** val decode_val0 : memory_chunk -> amemval list -> aval **)

let decode_val0 chunk vl =
  match split_amemval vl with
  | SplitC l -> Cval (decode_val chunk l)
  | SplitA l -> proj_value0 l
  | _ -> Cval Vundef

(** val astack_load :
    memory_chunk -> Mem.mem -> amemval ZMap.t -> block -> int -> aval option **)

let astack_load chunk m mb b ofs =
  if Mem.valid_access_dec m chunk b ofs Readable
  then Some (decode_val0 chunk (getN0 (size_chunk_nat chunk) ofs mb))
  else None

type astack = amemval ZMap.t

type aoutcome =
| ANext of aregset * astack * block * Mem.mem
| AStuck

(** val ashl : aval -> int -> aval **)

let ashl v i =
  match v with
  | Cval cv -> Cval (Val.shl cv (Vint i))
  | Aval _ -> if Int.eq i Int.zero then v else Cval Vundef

(** val ashru : aval -> int -> aval **)

let ashru v i =
  match v with
  | Cval cv -> Cval (Val.shru cv (Vint i))
  | Aval _ -> if Int.eq i Int.zero then v else Cval Vundef

(** val ashr : aval -> int -> aval **)

let ashr v i =
  match v with
  | Cval cv -> Cval (Val.shr cv (Vint i))
  | Aval _ -> if Int.eq i Int.zero then v else Cval Vundef

(** val aror : aval -> int -> aval **)

let aror v i =
  match v with
  | Cval cv -> Cval (Val.ror cv (Vint i))
  | Aval _ -> if Int.eq i Int.zero then v else Cval Vundef

(** val aeval_shift_op : shift_op -> aregset -> aval **)

let aeval_shift_op so rs =
  match so with
  | SOimm n0 -> Cval (Vint n0)
  | SOreg r -> rs (IR r)
  | SOlsl (r, n0) -> ashl (rs (IR r)) n0
  | SOlsr (r, n0) -> ashru (rs (IR r)) n0
  | SOasr (r, n0) -> ashr (rs (IR r)) n0
  | SOror (r, n0) -> aror (rs (IR r)) n0

(** val aadd : aval -> aval -> aval option **)

let aadd v1 v2 =
  match v1 with
  | Cval v3 ->
    (match v3 with
     | Vint i ->
       (match v2 with
        | Cval v4 -> Some (Cval (Val.add v3 v4))
        | Aval v ->
          let Sval (r, o) = v in Some (Aval (Sval (r, (Int.add o i)))))
     | _ ->
       (match v2 with
        | Cval v4 -> Some (Cval (Val.add v3 v4))
        | Aval _ -> None))
  | Aval v ->
    let Sval (r, o) = v in
    (match v2 with
     | Cval v0 ->
       (match v0 with
        | Vint i -> Some (Aval (Sval (r, (Int.add o i))))
        | _ -> None)
     | Aval _ -> None)

(** val alift2 : (val0 -> val0 -> val0) -> aval -> aval -> aval **)

let alift2 f v1 v2 =
  match v1 with
  | Cval v3 ->
    (match v2 with
     | Cval v4 -> Cval (f v3 v4)
     | Aval _ -> Cval Vundef)
  | Aval _ -> Cval Vundef

(** val alift1 : (val0 -> val0) -> aval -> aval **)

let alift1 f = function
| Cval v0 -> Cval (f v0)
| Aval _ -> Cval Vundef

(** val aoffset_ptr : aval -> int -> aval **)

let aoffset_ptr v ofs =
  match v with
  | Cval v0 -> Cval (Val.offset_ptr v0 ofs)
  | Aval v0 ->
    let Sval (r, o) = v0 in Aval (Sval (r, (Int.add o (Ptrofs.to_int ofs))))

(** val agoto_label :
    int -> aregset -> block -> astack -> Mem.mem -> aoutcome **)

let agoto_label lbl rs b stk m =
  ANext ((Pregmap.set PC (aoffset_ptr (rs PC) lbl) rs), stk, b, m)

(** val anextinstr : aregset -> PregEq.t -> aval **)

let anextinstr rs =
  Pregmap.set PC (aoffset_ptr (rs PC) isize) rs

(** val aundef_flags : aregset -> aregset **)

let aundef_flags rs r = match r with
| CR _ -> Cval Vundef
| _ -> rs r

(** val anextinstr_nf : aregset -> PregEq.t -> aval **)

let anextinstr_nf rs =
  anextinstr (aundef_flags rs)

(** val acompare_int :
    aregset -> aval -> aval -> Mem.mem -> PregEq.t -> aval **)

let acompare_int rs v1 v2 m =
  Pregmap.set (CR CV) (alift2 Val.sub_overflow v1 v2)
    (Pregmap.set (CR CC) (alift2 (Val.cmpu (Mem.valid_pointer m) Cge) v1 v2)
      (Pregmap.set (CR CZ)
        (alift2 (Val.cmpu (Mem.valid_pointer m) Ceq) v1 v2)
        (Pregmap.set (CR CN) (alift1 Val.negative (alift2 Val.sub v1 v2)) rs)))

(** val aexec_load :
    memory_chunk -> aval option -> preg -> aregset -> astack -> block ->
    Mem.mem -> aoutcome **)

let aexec_load chunk addr r rs stk b m =
  match addr with
  | Some a ->
    (match a with
     | Cval addr' ->
       (match Mem.loadv chunk m addr' with
        | Some v ->
          ANext ((anextinstr (Pregmap.set r (Cval v) rs)), stk, b, m)
        | None -> AStuck)
     | Aval v ->
       let Sval (s, o) = v in
       if sreg_eq Ssp s
       then (match astack_load chunk m stk b (Int.unsigned o) with
             | Some v0 ->
               ANext ((anextinstr (Pregmap.set r v0 rs)), stk, b, m)
             | None -> AStuck)
       else AStuck)
  | None -> AStuck

(** val aexec_store :
    memory_chunk -> aval option -> preg -> aregset -> astack -> block ->
    Mem.mem -> aoutcome **)

let aexec_store chunk addr r rs stk b m =
  match addr with
  | Some a ->
    (match a with
     | Cval addr' ->
       (match rs r with
        | Cval v ->
          (match Mem.storev chunk m addr' v with
           | Some m' -> ANext ((anextinstr rs), stk, b, m')
           | None -> AStuck)
        | Aval _ -> AStuck)
     | Aval v ->
       let Sval (s, o) = v in
       if sreg_eq s Ssp
       then (match astack_store chunk m stk b (Int.unsigned o) (rs r) with
             | Some stk' -> ANext ((anextinstr rs), stk', b, m)
             | None -> AStuck)
       else AStuck)
  | None -> AStuck

(** val adivs : aval -> aval -> aval option **)

let adivs v1 v2 =
  match v1 with
  | Cval v3 ->
    (match v2 with
     | Cval v4 ->
       (match Val.divs v3 v4 with
        | Some v -> Some (Cval v)
        | None -> None)
     | Aval _ -> None)
  | Aval _ -> None

(** val adivu : aval -> aval -> aval option **)

let adivu v1 v2 =
  match v1 with
  | Cval v3 ->
    (match v2 with
     | Cval v4 ->
       (match Val.divu v3 v4 with
        | Some v -> Some (Cval v)
        | None -> None)
     | Aval _ -> None)
  | Aval _ -> None

(** val update_regset : (preg * aval) list -> aregset -> aregset **)

let rec update_regset vl rs =
  match vl with
  | [] -> rs
  | p :: vl0 -> let (r, v) = p in Pregmap.set r v (update_regset vl0 rs)

(** val init_regset : (preg * val0) list -> aregset -> aregset **)

let init_regset vl rs =
  update_regset (map (fun pat -> let (r, v) = pat in (r, (Cval v))) vl) rs

(** val acompare_float_update : aval -> aval -> (preg * aval) list **)

let acompare_float_update v1 v2 =
  match v1 with
  | Cval v ->
    (match v with
     | Vfloat f1 ->
       (match v2 with
        | Cval v0 ->
          (match v0 with
           | Vfloat f2 ->
             ((CR CN), (Cval (Val.of_bool (Float.cmp Clt f1 f2)))) :: (((CR
               CZ), (Cval (Val.of_bool (Float.cmp Ceq f1 f2)))) :: (((CR CC),
               (Cval (Val.of_bool (negb (Float.cmp Clt f1 f2))))) :: (((CR
               CV), (Cval
               (Val.of_bool
                 (negb
                   ((||) ((||) (Float.cmp Ceq f1 f2) (Float.cmp Clt f1 f2))
                     (Float.cmp Cgt f1 f2)))))) :: [])))
           | _ ->
             ((CR CN), (Cval Vundef)) :: (((CR CZ), (Cval Vundef)) :: (((CR
               CC), (Cval Vundef)) :: (((CR CV), (Cval Vundef)) :: []))))
        | Aval _ ->
          ((CR CN), (Cval Vundef)) :: (((CR CZ), (Cval Vundef)) :: (((CR CC),
            (Cval Vundef)) :: (((CR CV), (Cval Vundef)) :: []))))
     | _ ->
       ((CR CN), (Cval Vundef)) :: (((CR CZ), (Cval Vundef)) :: (((CR CC),
         (Cval Vundef)) :: (((CR CV), (Cval Vundef)) :: []))))
  | Aval _ ->
    ((CR CN), (Cval Vundef)) :: (((CR CZ), (Cval Vundef)) :: (((CR CC), (Cval
      Vundef)) :: (((CR CV), (Cval Vundef)) :: [])))

(** val acompare_float : aregset -> aval -> aval -> aregset **)

let acompare_float rs v1 v2 =
  update_regset (acompare_float_update v1 v2) rs

(** val acompare_float32_update : aval -> aval -> (preg * aval) list **)

let acompare_float32_update v1 v2 =
  match v1 with
  | Cval v ->
    (match v with
     | Vsingle f1 ->
       (match v2 with
        | Cval v0 ->
          (match v0 with
           | Vsingle f2 ->
             ((CR CN), (Cval (Val.of_bool (Float32.cmp Clt f1 f2)))) :: (((CR
               CZ), (Cval (Val.of_bool (Float32.cmp Ceq f1 f2)))) :: (((CR
               CC), (Cval
               (Val.of_bool (negb (Float32.cmp Clt f1 f2))))) :: (((CR CV),
               (Cval
               (Val.of_bool
                 (negb
                   ((||)
                     ((||) (Float32.cmp Ceq f1 f2) (Float32.cmp Clt f1 f2))
                     (Float32.cmp Cgt f1 f2)))))) :: [])))
           | _ ->
             ((CR CN), (Cval Vundef)) :: (((CR CZ), (Cval Vundef)) :: (((CR
               CC), (Cval Vundef)) :: (((CR CV), (Cval Vundef)) :: []))))
        | Aval _ ->
          ((CR CN), (Cval Vundef)) :: (((CR CZ), (Cval Vundef)) :: (((CR CC),
            (Cval Vundef)) :: (((CR CV), (Cval Vundef)) :: []))))
     | _ ->
       ((CR CN), (Cval Vundef)) :: (((CR CZ), (Cval Vundef)) :: (((CR CC),
         (Cval Vundef)) :: (((CR CV), (Cval Vundef)) :: []))))
  | Aval _ ->
    ((CR CN), (Cval Vundef)) :: (((CR CZ), (Cval Vundef)) :: (((CR CC), (Cval
      Vundef)) :: (((CR CV), (Cval Vundef)) :: [])))

(** val acompare_float32 : aregset -> aval -> aval -> aregset **)

let acompare_float32 rs v1 v2 =
  update_regset (acompare_float32_update v1 v2) rs

(** val amaketotal : aval option -> aval **)

let amaketotal = function
| Some v -> v
| None -> Cval Vundef

(** val alifto1 : (val0 -> val0 option) -> aval -> aval option **)

let alifto1 f = function
| Cval v0 -> option_map (fun x -> Cval x) (f v0)
| Aval _ -> None

(** val eval_unary_cond : (int -> bool) -> aregset -> preg -> bool option **)

let eval_unary_cond f rs r =
  match rs r with
  | Cval v -> (match v with
               | Vint n0 -> Some (f n0)
               | _ -> None)
  | Aval _ -> None

(** val eval_binary_cond :
    (int -> int -> bool) -> aregset -> preg -> preg -> bool option **)

let eval_binary_cond f rs r1 r2 =
  match rs r1 with
  | Cval v ->
    (match v with
     | Vint c ->
       (match rs r2 with
        | Cval v0 -> (match v0 with
                      | Vint z0 -> Some (f c z0)
                      | _ -> None)
        | Aval _ -> None)
     | _ -> None)
  | Aval _ -> None

(** val eval_ternary_cond :
    (int -> int -> int -> bool) -> aregset -> preg -> preg -> preg -> bool
    option **)

let eval_ternary_cond f rs r1 r2 r3 =
  match rs r1 with
  | Cval v ->
    (match v with
     | Vint c -> eval_binary_cond (f c) rs r2 r3
     | _ -> None)
  | Aval _ -> None

(** val aeval_testcond : testcond -> aregset -> bool option **)

let aeval_testcond c rs =
  match c with
  | TCeq -> eval_unary_cond (Int.eq Int.one) rs (CR CZ)
  | TCne -> eval_unary_cond (Int.eq Int.zero) rs (CR CZ)
  | TChs -> eval_unary_cond (Int.eq Int.one) rs (CR CC)
  | TClo -> eval_unary_cond (Int.eq Int.zero) rs (CR CC)
  | TCmi -> eval_unary_cond (Int.eq Int.one) rs (CR CN)
  | TCpl -> eval_unary_cond (Int.eq Int.zero) rs (CR CN)
  | TChi ->
    eval_binary_cond (fun c0 z0 ->
      (&&) (Int.eq c0 Int.one) (Int.eq z0 Int.zero)) rs (CR CC) (CR CZ)
  | TCls ->
    eval_binary_cond (fun c0 z0 ->
      (||) (Int.eq c0 Int.zero) (Int.eq z0 Int.one)) rs (CR CC) (CR CZ)
  | TCge ->
    eval_binary_cond (fun o s -> Int.eq (Int.xor o s) Int.zero) rs (CR CV)
      (CR CN)
  | TClt ->
    eval_binary_cond (fun o s -> Int.eq (Int.xor o s) Int.one) rs (CR CV) (CR
      CN)
  | TCgt ->
    eval_ternary_cond (fun o s z0 ->
      (&&) (Int.eq (Int.xor o s) Int.zero) (Int.eq z0 Int.zero)) rs (CR CV)
      (CR CN) (CR CZ)
  | TCle ->
    eval_ternary_cond (fun o s z0 ->
      (||) (Int.eq (Int.xor o s) Int.one) (Int.eq z0 Int.one)) rs (CR CV) (CR
      CN) (CR CZ)

(** val aexec_instr :
    instruction -> aregset -> astack -> block -> Mem.mem -> aoutcome **)

let aexec_instr i rs astk b m =
  match i with
  | Padd (r1, r2, so) ->
    (match aadd (rs (IR r2)) (aeval_shift_op so rs) with
     | Some v ->
       ANext ((anextinstr_nf (Pregmap.set (IR r1) v rs)), astk, b, m)
     | None -> AStuck)
  | Pand (r1, r2, so) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1)
           (alift2 Val.coq_and (rs (IR r2)) (aeval_shift_op so rs)) rs)),
      astk, b, m)
  | Pasr (r1, r2, r3) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1) (alift2 Val.shr (rs (IR r2)) (rs (IR r3))) rs)),
      astk, b, m)
  | Pb lbl -> agoto_label lbl rs b astk m
  | Pbc (cond, lbl) ->
    (match aeval_testcond cond rs with
     | Some b0 ->
       if b0
       then agoto_label lbl rs b astk m
       else ANext ((anextinstr rs), astk, b, m)
     | None -> AStuck)
  | Pbreg r -> ANext ((Pregmap.set PC (rs (IR r)) rs), astk, b, m)
  | Pblreg r ->
    ANext
      ((Pregmap.set PC (rs (IR r))
         (Pregmap.set (IR IR14) (aoffset_ptr (rs PC) isize) rs)), astk, b, m)
  | Pbic (r1, r2, so) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1)
           (alift2 Val.coq_and (rs (IR r2))
             (alift1 Val.notint (aeval_shift_op so rs))) rs)), astk, b, m)
  | Pcmp (r1, so) ->
    ANext
      ((anextinstr (acompare_int rs (rs (IR r1)) (aeval_shift_op so rs) m)),
      astk, b, m)
  | Pcmn (r1, so) ->
    ANext
      ((anextinstr
         (acompare_int rs (rs (IR r1))
           (alift1 Val.neg (aeval_shift_op so rs)) m)), astk, b, m)
  | Peor (r1, r2, so) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1)
           (alift2 Val.xor (rs (IR r2)) (aeval_shift_op so rs)) rs)), astk,
      b, m)
  | Pldr (r1, r2, sa) ->
    aexec_load Mint32 (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR r1) rs
      astk b m
  | Pldr_a (r1, r2, sa) ->
    aexec_load Many32 (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR r1) rs
      astk b m
  | Pldrb (r1, r2, sa) ->
    aexec_load Mint8unsigned (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR
      r1) rs astk b m
  | Pldrh (r1, r2, sa) ->
    aexec_load Mint16unsigned (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR
      r1) rs astk b m
  | Pldrsb (r1, r2, sa) ->
    aexec_load Mint8signed (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR r1)
      rs astk b m
  | Pldrsh (r1, r2, sa) ->
    aexec_load Mint16signed (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR
      r1) rs astk b m
  | Plsl (r1, r2, r3) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1) (alift2 Val.shl (rs (IR r2)) (rs (IR r3))) rs)),
      astk, b, m)
  | Plsr (r1, r2, r3) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1) (alift2 Val.shru (rs (IR r2)) (rs (IR r3))) rs)),
      astk, b, m)
  | Pmla (r1, r2, r3, r4) ->
    (match aadd (alift2 Val.mul (rs (IR r2)) (rs (IR r3))) (rs (IR r4)) with
     | Some v -> ANext ((anextinstr (Pregmap.set (IR r1) v rs)), astk, b, m)
     | None -> AStuck)
  | Pmov (r1, so) ->
    ANext ((anextinstr_nf (Pregmap.set (IR r1) (aeval_shift_op so rs) rs)),
      astk, b, m)
  | Pmovw (r, n0) ->
    ANext ((anextinstr (Pregmap.set (IR r) (Cval (Vint n0)) rs)), astk, b, m)
  | Pmovt (r, n0) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR r)
           (alift2 Val.coq_or
             (alift2 Val.coq_and (rs (IR r)) (Cval (Vint
               (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                 ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                 ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                 ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                 ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                 1))))))))))))))))))) (Cval (Vint
             (Int.shl n0
               (Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                 ((fun p->2*p) 1))))))))) rs)), astk, b, m)
  | Pmul (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR r1) (alift2 Val.mul (rs (IR r2)) (rs (IR r3))) rs)),
      astk, b, m)
  | Pmvn (r1, so) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1) (alift1 Val.notint (aeval_shift_op so rs)) rs)),
      astk, b, m)
  | Porr (r1, r2, so) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1)
           (alift2 Val.coq_or (rs (IR r2)) (aeval_shift_op so rs)) rs)),
      astk, b, m)
  | Prsb (r1, r2, so) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1)
           (alift2 Val.sub (aeval_shift_op so rs) (rs (IR r2))) rs)), astk,
      b, m)
  | Psbfx (r1, r2, lsb, sz) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR r1)
           (alift1 (Val.sign_ext (Int.unsigned sz))
             (alift2 Val.shru (rs (IR r2)) (Cval (Vint lsb)))) rs)), astk, b,
      m)
  | Pstr (r1, r2, sa) ->
    aexec_store Mint32 (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR r1) rs
      astk b m
  | Pstr_a (r1, r2, sa) ->
    aexec_store Many32 (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR r1) rs
      astk b m
  | Pstrb (r1, r2, sa) ->
    aexec_store Mint8unsigned (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR
      r1) rs astk b m
  | Pstrh (r1, r2, sa) ->
    aexec_store Mint16unsigned (aadd (rs (IR r2)) (aeval_shift_op sa rs)) (IR
      r1) rs astk b m
  | Psdiv ->
    (match adivs (rs (IR IR0)) (rs (IR IR1)) with
     | Some v ->
       ANext
         ((anextinstr
            (Pregmap.set (IR IR12) (Cval Vundef)
              (Pregmap.set (IR IR3) (Cval Vundef)
                (Pregmap.set (IR IR2) (Cval Vundef)
                  (Pregmap.set (IR IR1) (Cval Vundef)
                    (Pregmap.set (IR IR0) v rs)))))), astk, b, m)
     | None -> AStuck)
  | Psmull (rdl, rdh, r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR rdh) (alift2 Val.mulhs (rs (IR r1)) (rs (IR r2)))
           (Pregmap.set (IR rdl) (alift2 Val.mul (rs (IR r1)) (rs (IR r2)))
             rs))), astk, b, m)
  | Psub (r1, r2, so) ->
    ANext
      ((anextinstr_nf
         (Pregmap.set (IR r1)
           (alift2 Val.sub (rs (IR r2)) (aeval_shift_op so rs)) rs)), astk,
      b, m)
  | Pudiv ->
    (match adivu (rs (IR IR0)) (rs (IR IR1)) with
     | Some v ->
       ANext
         ((anextinstr
            (Pregmap.set (IR IR12) (Cval Vundef)
              (Pregmap.set (IR IR3) (Cval Vundef)
                (Pregmap.set (IR IR2) (Cval Vundef)
                  (Pregmap.set (IR IR1) (Cval Vundef)
                    (Pregmap.set (IR IR0) v rs)))))), astk, b, m)
     | None -> AStuck)
  | Pumull (rdl, rdh, r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR rdh) (alift2 Val.mulhu (rs (IR r1)) (rs (IR r2)))
           (Pregmap.set (IR rdl) (alift2 Val.mul (rs (IR r1)) (rs (IR r2)))
             rs))), astk, b, m)
  | Pfcpyd (r1, r2) ->
    ANext ((anextinstr (Pregmap.set (FR r1) (rs (FR r2)) rs)), astk, b, m)
  | Pfabsd (r1, r2) ->
    ANext
      ((anextinstr (Pregmap.set (FR r1) (alift1 Val.absf (rs (FR r2))) rs)),
      astk, b, m)
  | Pfnegd (r1, r2) ->
    ANext
      ((anextinstr (Pregmap.set (FR r1) (alift1 Val.negf (rs (FR r2))) rs)),
      astk, b, m)
  | Pfaddd (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift2 Val.addf (rs (FR r2)) (rs (FR r3))) rs)),
      astk, b, m)
  | Pfdivd (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift2 Val.divf (rs (FR r2)) (rs (FR r3))) rs)),
      astk, b, m)
  | Pfmuld (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift2 Val.mulf (rs (FR r2)) (rs (FR r3))) rs)),
      astk, b, m)
  | Pfsubd (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift2 Val.subf (rs (FR r2)) (rs (FR r3))) rs)),
      astk, b, m)
  | Pflid (r1, f) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (Cval (Vfloat f))
           (Pregmap.set (IR IR14) (Cval Vundef) rs))), astk, b, m)
  | Pfcmpd (r1, r2) ->
    ANext ((anextinstr (acompare_float rs (rs (FR r1)) (rs (FR r2)))), astk,
      b, m)
  | Pfcmpzd r1 ->
    ANext
      ((anextinstr
         (acompare_float rs (rs (FR r1)) (Cval (Vfloat Float.zero)))), astk,
      b, m)
  | Pfsitod (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1)
           (amaketotal (alifto1 Val.floatofint (rs (IR r2)))) rs)), astk, b,
      m)
  | Pfuitod (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1)
           (amaketotal (alifto1 Val.floatofintu (rs (IR r2)))) rs)), astk, b,
      m)
  | Pftosizd (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR r1)
           (amaketotal (alifto1 Val.intoffloat (rs (FR r2))))
           (Pregmap.set (FR FR6) (Cval Vundef) rs))), astk, b, m)
  | Pftouizd (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR r1)
           (amaketotal (alifto1 Val.intuoffloat (rs (FR r2))))
           (Pregmap.set (FR FR6) (Cval Vundef) rs))), astk, b, m)
  | Pfabss (r1, r2) ->
    ANext
      ((anextinstr (Pregmap.set (FR r1) (alift1 Val.absfs (rs (FR r2))) rs)),
      astk, b, m)
  | Pfnegs (r1, r2) ->
    ANext
      ((anextinstr (Pregmap.set (FR r1) (alift1 Val.negfs (rs (FR r2))) rs)),
      astk, b, m)
  | Pfadds (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift2 Val.addfs (rs (FR r2)) (rs (FR r3))) rs)),
      astk, b, m)
  | Pfdivs (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift2 Val.divfs (rs (FR r2)) (rs (FR r3))) rs)),
      astk, b, m)
  | Pfmuls (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift2 Val.mulfs (rs (FR r2)) (rs (FR r3))) rs)),
      astk, b, m)
  | Pfsubs (r1, r2, r3) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift2 Val.subfs (rs (FR r2)) (rs (FR r3))) rs)),
      astk, b, m)
  | Pflis (r1, f) ->
    ANext ((anextinstr (Pregmap.set (FR r1) (Cval (Vsingle f)) rs)), astk, b,
      m)
  | Pfcmps (r1, r2) ->
    ANext ((anextinstr (acompare_float32 rs (rs (FR r1)) (rs (FR r2)))),
      astk, b, m)
  | Pfcmpzs r1 ->
    ANext
      ((anextinstr
         (acompare_float32 rs (rs (FR r1)) (Cval (Vsingle Float32.zero)))),
      astk, b, m)
  | Pfsitos (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1)
           (amaketotal (alifto1 Val.singleofint (rs (IR r2)))) rs)), astk, b,
      m)
  | Pfuitos (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1)
           (amaketotal (alifto1 Val.singleofintu (rs (IR r2)))) rs)), astk,
      b, m)
  | Pftosizs (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR r1)
           (amaketotal (alifto1 Val.intofsingle (rs (FR r2))))
           (Pregmap.set (FR FR6) (Cval Vundef) rs))), astk, b, m)
  | Pftouizs (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (IR r1)
           (amaketotal (alifto1 Val.intuofsingle (rs (FR r2))))
           (Pregmap.set (FR FR6) (Cval Vundef) rs))), astk, b, m)
  | Pfcvtsd (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift1 Val.singleoffloat (rs (FR r2))) rs)),
      astk, b, m)
  | Pfcvtds (r1, r2) ->
    ANext
      ((anextinstr
         (Pregmap.set (FR r1) (alift1 Val.floatofsingle (rs (FR r2))) rs)),
      astk, b, m)
  | Pfldd (r1, r2, n0) ->
    aexec_load Mfloat64 (aadd (rs (IR r2)) (Cval (Vint n0))) (FR r1) rs astk
      b m
  | Pfldd_a (r1, r2, n0) ->
    aexec_load Many64 (aadd (rs (IR r2)) (Cval (Vint n0))) (FR r1) rs astk b m
  | Pflds (r1, r2, n0) ->
    aexec_load Mfloat32 (aadd (rs (IR r2)) (Cval (Vint n0))) (FR r1) rs astk
      b m
  | Pfstd (r1, r2, n0) ->
    aexec_store Mfloat64 (aadd (rs (IR r2)) (Cval (Vint n0))) (FR r1) rs astk
      b m
  | Pfstd_a (r1, r2, n0) ->
    aexec_store Many64 (aadd (rs (IR r2)) (Cval (Vint n0))) (FR r1) rs astk b
      m
  | Pfsts (r1, r2, n0) ->
    aexec_store Mfloat32 (aadd (rs (IR r2)) (Cval (Vint n0))) (FR r1) rs astk
      b m

(** val int_param_regs : preg list **)

let int_param_regs =
  map (fun x -> IR x) (IR0 :: (IR1 :: (IR2 :: (IR3 :: []))))

(** val alloc_arguments :
    val0 list -> typ list -> preg list -> (preg * val0) list option **)

let rec alloc_arguments lv lt0 lr =
  match lv with
  | [] -> (match lt0 with
           | [] -> Some []
           | _ :: _ -> None)
  | v1 :: lv1 ->
    (match lt0 with
     | [] -> None
     | t0 :: lt1 ->
       (match t0 with
        | Tint ->
          (match lr with
           | [] -> None
           | r1 :: lr1 ->
             (match alloc_arguments lv1 lt1 lr1 with
              | Some l -> Some ((r1, v1) :: l)
              | None -> None))
        | _ -> None))

(** val regset0 : aregset **)

let regset0 r =
  Aval (Sval ((Sreg r), Int.zero))

(** val allocframe : int -> int -> Mem.mem -> aregset -> aoutcome **)

let allocframe sz pos m rs = (* let _ = print_string "allocframe: PC= " in let _ = print_reg_val (rs PC) in let _ = print_string "IR0= " in let _ = print_reg_val (rs (IR IR0)) in *)
  let (m1, stkb) = Mem.alloc m 0 sz in
  let sp = Aval (Sval (Ssp, Int.zero)) in
  let stk = ZMap.init (Cmemval Undef) in
  (match aexec_store Mint32 (Some (aoffset_ptr sp pos)) (IR IR13) rs stk stkb
           m1 with
   | ANext (rs', stk', stkb', m1') -> (* let _ = print_string "after aexec_store: PC= " in let _ = print_reg_val (rs' PC) in let _ = print_string "IR0= " in let _ = print_reg_val (rs' (IR IR0)) in *)
     ANext
       ((anextinstr
          (Pregmap.set (IR IR13) sp
            (Pregmap.set (IR IR12) (rs (IR IR13)) rs'))), stk', stkb', m1')
   | AStuck -> AStuck)

(** val init_state :
    signature -> int -> int -> val0 list -> Mem.mem -> aoutcome **)

let init_state s sz pos l m =
  match alloc_arguments l s.sig_args int_param_regs with
  | Some l0 ->
    let rs = init_regset l0 regset0 in (* let _ = print_reg_val (rs PC) in let _ = print_string "IR0 " in let _ = print_reg_val (rs (IR IR0)) in *)
    (match allocframe sz pos m rs with
     | ANext (rs', stk', stkb', m1') ->
       ANext ((Pregmap.set PC (rs (IR IR0)) rs'), stk', stkb', m1')
     | AStuck -> AStuck)
  | None -> AStuck

(** val is_final_state : aregset -> bool **)

let is_final_state rs = (*let _ = print_string "is_final_state: PC=" in let _ = print_endline (string_of_reg_val (rs PC)) in let _ = print_string "LR= " in let _ = print_endline (string_of_reg_val (rs (IR IR14))) in let _ = print_string "LR'= " in let _ = print_endline (string_of_reg_val (Aval (Sval ((Sreg (IR IR14)), Int.zero)))) in
 let _ = print_string "SP= " in let _ = print_endline (string_of_reg_val (rs (IR IR13))) in let _ = print_string "SP' " in let _ = print_endline (string_of_reg_val (Aval (Sval ((Sreg (IR IR13)), Int.zero)))) in
 let _ = print_endline (string_of_bool (forallb (fun r ->
      aval_eq (rs (preg_of r)) (Aval (Sval ((Sreg (preg_of r)), Int.zero))))
      (app int_callee_save_regs float_callee_save_regs))) in *)
  (&&)
    ((&&) (aval_eq (rs PC) (Aval (Sval ((Sreg (IR IR14)), Int.zero))))
      (aval_eq (rs (IR IR13)) (Aval (Sval ((Sreg (IR IR13)), Int.zero)))))
    (forallb (fun r ->
      aval_eq (rs (preg_of r)) (Aval (Sval ((Sreg (preg_of r)), Int.zero))))
      (app int_callee_save_regs float_callee_save_regs))

(** val has_rettypeb : val0 -> rettype -> bool **)

let has_rettypeb v = function
| Tret t0 -> proj_sumbool (Val.has_type_dec v t0)
| Tint8signed ->
  (match v with
   | Vundef -> true
   | Vint n0 ->
     Int.eq n0
       (Int.sign_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))) n0)
   | _ -> false)
| Tint8unsigned ->
  (match v with
   | Vundef -> true
   | Vint n0 ->
     Int.eq n0
       (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))) n0)
   | _ -> false)
| Tint16signed ->
  (match v with
   | Vundef -> true
   | Vint n0 ->
     Int.eq n0
       (Int.sign_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
         1)))) n0)
   | _ -> false)
| Tint16unsigned ->
  (match v with
   | Vundef -> true
   | Vint n0 ->
     Int.eq n0
       (Int.zero_ext ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
         1)))) n0)
   | _ -> false)
| Tvoid -> (match v with
            | Vundef -> true
            | _ -> false)

(** val is_definedb : val0 -> bool **)

let is_definedb = function
| Vundef -> false
| _ -> true

(** val get_result : rettype -> aval -> Mem.mem -> (val0 * Mem.mem) option **)

let get_result r v m =
  match v with
  | Cval v0 ->
    if (&&) (is_definedb v0) (has_rettypeb v0 r) then Some (v0, m) else None
  | Aval _ -> None

(** val bin_interp :
    rettype -> int -> aregset -> astack -> block -> Mem.mem ->
    (val0 * Mem.mem) option **)

let rec bin_interp rt n0 rs stk stkb m =
  if is_final_state rs
  then let _ = print_endline "is_final_state:Yes" in get_result rt (rs (IR IR0)) m
  else ((fun fO fS n -> if n=0 then fO () else fS (n-1))
          (fun _ -> None)
          (fun n1 ->
          match find_instr (rs PC) m with
          | Some i ->  (*let _ = print_endline "find_instr:Some" in *)
            (match aexec_instr i rs stk stkb m with
             | ANext (rs', stk', stkb0, m') -> let _ = print_aregset rs' in 
               bin_interp rt n1 rs' stk' stkb0 m'
             | AStuck -> let _ = print_endline "aexec_instr:None" in None)
          | None -> let _ = print_endline "find_instr:None" in None)
          n0)

(** val bin_exec :
    int -> signature -> int -> int -> val0 list -> Mem.mem ->
    (val0 * Mem.mem) option **)

let bin_exec n0 s sz pos args m =
  match init_state s sz pos args m with
  | ANext (rs, stk, stkb, m1) -> let _ = print_aregset rs in (*let _ = print_endline "after init_state" in let _ = print_reg_val (rs PC) in let _ = print_string "IR0 " in let _ = print_reg_val (rs (IR IR0)) in*) bin_interp s.sig_res n0 rs stk stkb m1
  | AStuck -> let _ = print_endline "error init_state" in None

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
| R13
| R14
| R15
| R16
| R17
| R18
| R19
| R20
| R21
| R22
| R23

(** val reg_eqb : reg -> reg -> bool **)

let reg_eqb r0 r1 =
  match r0 with
  | R13 -> (match r1 with
            | R13 -> true
            | _ -> false)
  | R14 -> (match r1 with
            | R14 -> true
            | _ -> false)
  | R15 -> (match r1 with
            | R15 -> true
            | _ -> false)
  | R16 -> (match r1 with
            | R16 -> true
            | _ -> false)
  | R17 -> (match r1 with
            | R17 -> true
            | _ -> false)
  | R18 -> (match r1 with
            | R18 -> true
            | _ -> false)
  | R19 -> (match r1 with
            | R19 -> true
            | _ -> false)
  | R20 -> (match r1 with
            | R20 -> true
            | _ -> false)
  | R21 -> (match r1 with
            | R21 -> true
            | _ -> false)
  | R22 -> (match r1 with
            | R22 -> true
            | _ -> false)
  | R23 -> (match r1 with
            | R23 -> true
            | _ -> false)

(** val id_of_reg : reg -> int **)

let id_of_reg = function
| R13 -> 0
| R14 -> 1
| R15 -> ((fun p->2*p) 1)
| R16 -> ((fun p->1+2*p) 1)
| R17 -> ((fun p->2*p) ((fun p->2*p) 1))
| R18 -> ((fun p->1+2*p) ((fun p->2*p) 1))
| R19 -> ((fun p->2*p) ((fun p->1+2*p) 1))
| R20 -> ((fun p->1+2*p) ((fun p->1+2*p) 1))
| R21 -> ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))
| R22 -> ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1)))
| R23 -> ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))

(** val reg2nat : reg -> int **)

let reg2nat = function
| R13 -> 0
| R14 -> Stdlib.succ 0
| R15 -> Stdlib.succ (Stdlib.succ 0)
| R16 -> Stdlib.succ (Stdlib.succ (Stdlib.succ 0))
| R17 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ 0)))
| R18 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ 0))))
| R19 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ 0)))))
| R20 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ 0))))))
| R21 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0)))))))
| R22 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ 0))))))))
| R23 ->
  Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ 0)))))))))

(** val z_to_reg : int -> reg option **)

let z_to_reg z0 =
  if Z.eqb z0 0
  then Some R13
  else if Z.eqb z0 1
       then Some R14
       else if Z.eqb z0 ((fun p->2*p) 1)
            then Some R15
            else if Z.eqb z0 ((fun p->1+2*p) 1)
                 then Some R16
                 else if Z.eqb z0 ((fun p->2*p) ((fun p->2*p) 1))
                      then Some R17
                      else if Z.eqb z0 ((fun p->1+2*p) ((fun p->2*p) 1))
                           then Some R18
                           else if Z.eqb z0 ((fun p->2*p) ((fun p->1+2*p) 1))
                                then Some R19
                                else if Z.eqb z0 ((fun p->1+2*p)
                                          ((fun p->1+2*p) 1))
                                     then Some R20
                                     else if Z.eqb z0 ((fun p->2*p)
                                               ((fun p->2*p) ((fun p->2*p)
                                               1)))
                                          then Some R21
                                          else if Z.eqb z0 ((fun p->1+2*p)
                                                    ((fun p->2*p)
                                                    ((fun p->2*p) 1)))
                                               then Some R22
                                               else if Z.eqb z0 ((fun p->2*p)
                                                         ((fun p->1+2*p)
                                                         ((fun p->2*p) 1)))
                                                    then Some R23
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

(** val init_LoadStoreRegs : loadStoreRegs **)

let init_LoadStoreRegs =
  { is_R0 = NonPerm; is_R1 = NonPerm; is_R2 = NonPerm; is_R3 = NonPerm;
    is_R4 = NonPerm; is_R5 = NonPerm; is_R6 = NonPerm; is_R7 = NonPerm;
    is_R8 = NonPerm; is_R9 = NonPerm; is_R10 = NonPerm }

(** val eval_LoadStoreRegs : loadStoreRegs -> reg -> loadStorePerm **)

let eval_LoadStoreRegs ls = function
| R13 -> ls.is_R0
| R14 -> ls.is_R1
| R15 -> ls.is_R2
| R16 -> ls.is_R3
| R17 -> ls.is_R4
| R18 -> ls.is_R5
| R19 -> ls.is_R6
| R20 -> ls.is_R7
| R21 -> ls.is_R8
| R22 -> ls.is_R9
| R23 -> ls.is_R10

(** val upd_LoadStoreRegs :
    loadStoreRegs -> reg -> loadStorePerm -> loadStoreRegs option **)

let upd_LoadStoreRegs ls r cur =
  let history = eval_LoadStoreRegs ls r in
  let new0 = upd_LoadStorePerm history cur in
  (match new0 with
   | Some p ->
     Some
       (match r with
        | R13 ->
          { is_R0 = p; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 = ls.is_R3;
            is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R14 ->
          { is_R0 = ls.is_R0; is_R1 = p; is_R2 = ls.is_R2; is_R3 = ls.is_R3;
            is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R15 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = p; is_R3 = ls.is_R3;
            is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R16 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 = p;
            is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R17 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = p; is_R5 = ls.is_R5; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R18 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = p; is_R6 = ls.is_R6; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R19 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = p; is_R7 =
            ls.is_R7; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 = ls.is_R10 }
        | R20 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R6;
            is_R7 = p; is_R8 = ls.is_R8; is_R9 = ls.is_R9; is_R10 =
            ls.is_R10 }
        | R21 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R7;
            is_R7 = ls.is_R7; is_R8 = p; is_R9 = ls.is_R9; is_R10 =
            ls.is_R10 }
        | R22 ->
          { is_R0 = ls.is_R0; is_R1 = ls.is_R1; is_R2 = ls.is_R2; is_R3 =
            ls.is_R3; is_R4 = ls.is_R4; is_R5 = ls.is_R5; is_R6 = ls.is_R7;
            is_R7 = ls.is_R7; is_R8 = ls.is_R8; is_R9 = p; is_R10 =
            ls.is_R10 }
        | R23 ->
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

type jittedarm32 = { is_IR11 : bool; load_store_regs : loadStoreRegs;
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
  { is_IR11 = false; load_store_regs = init_LoadStoreRegs; offset = 0;
    arm32_len = 0; arm32 = (List32.create_int_list jITTED_LIST_MAX_LENGTH) }

(** val upd_IR11_jittedarm32 : bool -> jittedarm32 -> jittedarm32 **)

let upd_IR11_jittedarm32 f st =
  { is_IR11 = f; load_store_regs = st.load_store_regs; offset = st.offset;
    arm32_len = st.arm32_len; arm32 = st.arm32 }

(** val add_ins_jittedarm32 : int -> jittedarm32 -> jittedarm32 **)

let add_ins_jittedarm32 ins st =
  { is_IR11 = st.is_IR11; load_store_regs = st.load_store_regs; offset =
    (Stdlib.succ st.offset); arm32_len = (Stdlib.succ st.arm32_len);
    arm32 = (List32.assign st.arm32 st.arm32_len ins) }

(** val upd_load_store_regs_jittedarm32 :
    loadStoreRegs -> jittedarm32 -> jittedarm32 **)

let upd_load_store_regs_jittedarm32 lsr0 st =
  { is_IR11 = st.is_IR11; load_store_regs = lsr0; offset = st.offset;
    arm32_len = st.arm32_len; arm32 = st.arm32 }

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

(** val empty_jit_state : jit_state **)

let empty_jit_state =
  { pc_loc = Int.zero; flag = Vundef; regs_st = Vundef; mrs_num = 0;
    bpf_mrs = []; ins_len = 0; ibpf = []; stack_len = 0; stack_ofs = 0;
    jitted_len = 0; jitted_list = []; jit_mem = Mem.empty }

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
  IR12

(** val jIT_EXTRA_REG : ireg **)

let jIT_EXTRA_REG =
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

(** val jit_alu32_arm32_save_IR11 : jit_state -> jit_state **)

let jit_alu32_arm32_save_IR11 st =
  let ins =
    jit_alu32_save_register (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG)))
      st.stack_ofs
  in
  add_ins_to_push_stack st ins

(** val jit_alu32_arm32_save :
    jit_state -> loadStoreRegs -> bool -> jit_state **)

let jit_alu32_arm32_save st ls f =
  let r1_st = jit_alu32_arm32_upd_save R14 st ls in
  let r2_st = jit_alu32_arm32_upd_save R15 r1_st ls in
  let r3_st = jit_alu32_arm32_upd_save R16 r2_st ls in
  let r4_st = jit_alu32_arm32_upd_save R17 r3_st ls in
  let r5_st = jit_alu32_arm32_upd_save R18 r4_st ls in
  let r6_st = jit_alu32_arm32_upd_save R19 r5_st ls in
  let r7_st = jit_alu32_arm32_upd_save R20 r6_st ls in
  let r8_st = jit_alu32_arm32_upd_save R21 r7_st ls in
  let r9_st = jit_alu32_arm32_upd_save R22 r8_st ls in
  let r10_st = jit_alu32_arm32_upd_save R23 r9_st ls in
  if f then jit_alu32_arm32_save_IR11 r10_st else r10_st

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
  let r0_st = jit_alu32_arm32_upd_load R13 st ls in
  let r1_st = jit_alu32_arm32_upd_load R14 r0_st ls in
  let r2_st = jit_alu32_arm32_upd_load R15 r1_st ls in
  let r3_st = jit_alu32_arm32_upd_load R16 r2_st ls in
  let r4_st = jit_alu32_arm32_upd_load R17 r3_st ls in
  let r5_st = jit_alu32_arm32_upd_load R18 r4_st ls in
  let r6_st = jit_alu32_arm32_upd_load R19 r5_st ls in
  let r7_st = jit_alu32_arm32_upd_load R20 r6_st ls in
  let r8_st = jit_alu32_arm32_upd_load R21 r7_st ls in
  let r9_st = jit_alu32_arm32_upd_load R22 r8_st ls in
  jit_alu32_arm32_upd_load R23 r9_st ls

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
| IR0 -> Some R13
| IR1 -> Some R14
| IR2 -> Some R15
| IR3 -> Some R16
| IR4 -> Some R17
| IR5 -> Some R18
| IR6 -> Some R19
| IR7 -> Some R20
| IR8 -> Some R21
| IR9 -> Some R22
| IR10 -> Some R23
| _ -> None

(** val reg_ireg_eqb : reg -> ireg -> bool **)

let reg_ireg_eqb r0 r1 =
  match r0 with
  | R13 -> (match r1 with
            | IR0 -> true
            | _ -> false)
  | R14 -> (match r1 with
            | IR1 -> true
            | _ -> false)
  | R15 -> (match r1 with
            | IR2 -> true
            | _ -> false)
  | R16 -> (match r1 with
            | IR3 -> true
            | _ -> false)
  | R17 -> (match r1 with
            | IR4 -> true
            | _ -> false)
  | R18 -> (match r1 with
            | IR5 -> true
            | _ -> false)
  | R19 -> (match r1 with
            | IR6 -> true
            | _ -> false)
  | R20 -> (match r1 with
            | IR7 -> true
            | _ -> false)
  | R21 -> (match r1 with
            | IR8 -> true
            | _ -> false)
  | R22 -> (match r1 with
            | IR9 -> true
            | _ -> false)
  | R23 -> (match r1 with
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
       if ireg_eqb src jIT_EXTRA_REG
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
       if ireg_eqb src jIT_EXTRA_REG
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
       if ireg_eqb src jIT_EXTRA_REG
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
    if (&&) (reg_eqb dst R13) (ireg_eqb src IR1)
    then let st1 = upd_IR11_jittedarm32 true j in
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
           encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG)))
             mOV_I_OP (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
           encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG)))
             str_imm12 (Stdlib.succ (Stdlib.succ (Stdlib.succ
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
            if ireg_eqb src jIT_EXTRA_REG
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
       if ireg_eqb src jIT_EXTRA_REG
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
       if ireg_eqb src jIT_EXTRA_REG
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
    let st1 = upd_IR11_jittedarm32 true j in
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
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG))) mOV_I_OP
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
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG))) str_imm12
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
       if ireg_eqb src jIT_EXTRA_REG
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
    let st1 = upd_IR11_jittedarm32 true j in
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
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG))) mOV_I_OP
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
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG))) str_imm12
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
       if ireg_eqb src jIT_EXTRA_REG
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
       if ireg_eqb src jIT_EXTRA_REG
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
            if ireg_eqb src jIT_EXTRA_REG
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
    let st1 = upd_IR11_jittedarm32 true j in
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
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG))) mOV_I_OP
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
      encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG))) str_imm12
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
       if ireg_eqb src jIT_EXTRA_REG
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
| R13 -> IR0
| R14 -> IR1
| R15 -> IR2
| R16 -> IR3
| R17 -> IR4
| R18 -> IR5
| R19 -> IR6
| R20 -> IR7
| R21 -> IR8
| R22 -> IR9
| R23 -> IR10

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
           let st1 = upd_IR11_jittedarm32 true j in
           let st2 = mov_int_to_reg_binary imm32 jIT_EXTRA_REG st1 in
           bpf_alu32_to_arm32_reg BPF_MUL32_REG d jIT_EXTRA_REG st2
         | BPF_DIV32_IMM ->
           (None, "BPF backend ERROR: it should guarantee no div_imm")
         | _ ->
           if (&&) (Int.cmp Cle Int.zero imm32)
                (Int.cmp Cle imm32
                  (Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p)
                    ((fun p->1+2*p) 1)))))))))
           then bpf_alu32_to_arm32_imm opi d imm32 j
           else let st1 = upd_IR11_jittedarm32 true j in
                let st2 = mov_int_to_reg_binary imm32 jIT_EXTRA_REG st1 in
                bpf_alu32_to_arm32_reg (opcode_reg_of_imm opi) d
                  jIT_EXTRA_REG st2)
      | ALU32_ILLEGAL_INS -> (None, "ERROR: ALU32_ILLEGAL_INS"))
   | None -> (None, "Verifier ERROR"))

(** val jit_alu32_aux_arm32 :
    int -> int -> List64AsArray.t -> jittedarm32 -> jittedarm32
    option * string **)

let rec jit_alu32_aux_arm32 fuel0 entry_point0 l j =
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
    fuel0

(** val jit_alu32_entry_points_list :
    int -> int -> bool -> List64AsArray.t -> entry_point -> entry_point **)

let rec jit_alu32_entry_points_list fuel0 pc is_alu32 l epl =
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
    fuel0

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
  let r0_st = jit_alu32_arm32_upd_store R13 st ls in
  let r1_st = jit_alu32_arm32_upd_store R14 r0_st ls in
  let r2_st = jit_alu32_arm32_upd_store R15 r1_st ls in
  let r3_st = jit_alu32_arm32_upd_store R16 r2_st ls in
  let r4_st = jit_alu32_arm32_upd_store R17 r3_st ls in
  let r5_st = jit_alu32_arm32_upd_store R18 r4_st ls in
  let r6_st = jit_alu32_arm32_upd_store R19 r5_st ls in
  let r7_st = jit_alu32_arm32_upd_store R20 r6_st ls in
  let r8_st = jit_alu32_arm32_upd_store R21 r7_st ls in
  let r9_st = jit_alu32_arm32_upd_store R22 r8_st ls in
  jit_alu32_arm32_upd_store R23 r9_st ls

(** val jit_alu32_store_flag : jit_state -> int -> jit_state **)

let jit_alu32_store_flag st f =
  let ins_rd =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG))) mOV_I_OP
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
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG))) ins_imm12
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
  let ins_r0 = jit_alu32_store_register (Int.repr (Z.of_nat (reg2nat R13))) in
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

(** val jit_alu32_arm32_reset_IR11 : jit_state -> jit_state **)

let jit_alu32_arm32_reset_IR11 st =
  let ins =
    jit_alu32_reset (Int.repr (Z.of_nat (ireg2nat jIT_EXTRA_REG)))
      (Nat.sub st.stack_ofs (Stdlib.succ (Stdlib.succ
        (Stdlib.succ (Stdlib.succ 0)))))
  in
  add_ins_to_pop_stack st ins

(** val jit_alu32_arm32_reset :
    jit_state -> loadStoreRegs -> bool -> jit_state **)

let jit_alu32_arm32_reset st ls f =
  let r12_st = if f then jit_alu32_arm32_reset_IR11 st else st in
  let r10_st = jit_alu32_arm32_upd_reset R23 r12_st ls in
  let r9_st = jit_alu32_arm32_upd_reset R22 r10_st ls in
  let r8_st = jit_alu32_arm32_upd_reset R21 r9_st ls in
  let r7_st = jit_alu32_arm32_upd_reset R20 r8_st ls in
  let r6_st = jit_alu32_arm32_upd_reset R19 r7_st ls in
  let r5_st = jit_alu32_arm32_upd_reset R18 r6_st ls in
  let r4_st = jit_alu32_arm32_upd_reset R17 r5_st ls in
  let r3_st = jit_alu32_arm32_upd_reset R16 r4_st ls in
  let r2_st = jit_alu32_arm32_upd_reset R15 r3_st ls in
  jit_alu32_arm32_upd_reset R14 r2_st ls

(** val jit_alu32_post : jit_state -> jit_state **)

let jit_alu32_post st =
  let ins_imm12 = encode_arm32 (Int.repr (Z.of_nat 0)) lDR_I_OP 0 in
  let ins_rt =
    encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR13))) ins_imm12
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
  let ins_rm = encode_arm32 (Int.repr (Z.of_nat (ireg2nat IR14))) bX_OP 0 in
  upd_jitted_list st0 ins_rm

(** val get_store_ins_num : loadStoreRegs -> int **)

let get_store_ins_num ls =
  let n0 = if is_store_reg R13 ls then Stdlib.succ 0 else 0 in
  let n1 = if is_store_reg R14 ls then Stdlib.succ n0 else n0 in
  let n2 = if is_store_reg R15 ls then Stdlib.succ n1 else n1 in
  let n3 = if is_store_reg R16 ls then Stdlib.succ n2 else n2 in
  let n4 = if is_store_reg R17 ls then Stdlib.succ n3 else n3 in
  let n5 = if is_store_reg R18 ls then Stdlib.succ n4 else n4 in
  let n6 = if is_store_reg R19 ls then Stdlib.succ n5 else n5 in
  let n7 = if is_store_reg R20 ls then Stdlib.succ n6 else n6 in
  let n8 = if is_store_reg R21 ls then Stdlib.succ n7 else n7 in
  let n9 = if is_store_reg R22 ls then Stdlib.succ n8 else n8 in
  if is_store_reg R23 ls then Stdlib.succ n9 else n9

(** val copy_arm32_list_from_to_aux :
    int -> int -> List32.t -> jit_state -> loadStoreRegs -> jit_state **)

let rec copy_arm32_list_from_to_aux fuel0 pc from to0 ls =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> to0)
    (fun n0 ->
    let ins0 = List32.index from (Int.repr (Z.of_nat pc)) in
    let ins =
      if Int.eq ins0 bAL_0
      then let store_len = get_store_ins_num ls in
           let ofs = Int.repr (Z.of_nat (Nat.add fuel0 store_len)) in
           encode_arm32 ofs ins0 0
      else ins0
    in
    let st0 = upd_jitted_list to0 ins in
    copy_arm32_list_from_to_aux n0 (Stdlib.succ pc) from st0 ls)
    fuel0

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

let rec jit_alu32_aux fuel0 pc epl st =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> ((Some st), "OK"))
    (fun n0 ->
    let entry_point0 = ListNat.index epl pc in
    let (o, str) =
      jit_alu32_aux_arm32 (length st.ibpf) entry_point0 st.ibpf
        init_jittedarm32
    in
    (match o with
     | Some pre_jarm32 ->
       let ls_reg = pre_jarm32.load_store_regs in
       let ibpf_ins = construct_iBPF_ins st entry_point0 pre_jarm32.offset in
       let arm32_pre = jit_alu32_pre st in
       let arm32_save =
         jit_alu32_arm32_save arm32_pre ls_reg pre_jarm32.is_IR11
       in
       let arm32_load = jit_alu32_arm32_load arm32_save ls_reg in
       let arm32_core = copy_arm32_list_from_to pre_jarm32 arm32_load ls_reg
       in
       let arm32_store = jit_alu32_arm32_store arm32_core ls_reg in
       let arm32_reset =
         jit_alu32_arm32_reset arm32_store ls_reg pre_jarm32.is_IR11
       in
       let arm32_post = jit_alu32_post arm32_reset in
       let next_st = upd_ibpf arm32_post ibpf_ins entry_point0 in
       jit_alu32_aux n0 (Stdlib.succ pc) epl next_st
     | None -> (None, str)))
    fuel0

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

(** val test_add_reg_int64 : int list **)

let test_add_reg_int64 =
  (Int64.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    1)))))))))))))) :: ((Int64.repr ((fun p->2*p) ((fun p->2*p)
                          ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
                          ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                          ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                          ((fun p->2*p) 1))))))))))))) :: ((Int64.repr
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
                                                             1)))))))))))))) :: (
    (Int64.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
      ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) 1)))))))) :: [])))

(** val add_reg_init_jitted_list : List32.t **)

let add_reg_init_jitted_list =
  List32.create_int_list jITTED_LIST_MAX_LENGTH

(** val add_reg_init_jit_state : jit_state **)

let add_reg_init_jit_state =
  { pc_loc = Int.zero; flag = Vundef; regs_st = Vundef; mrs_num = 0;
    bpf_mrs = []; ins_len = (length test_add_reg_int64); ibpf =
    test_add_reg_int64; stack_len = 0; stack_ofs = 0; jitted_len = 0;
    jitted_list = add_reg_init_jitted_list; jit_mem = Mem.empty }

(** val final_jitted_state : jit_state **)

let final_jitted_state =
  let (o, _) = jit_alu32 add_reg_init_jit_state in
  (match o with
   | Some st -> st
   | None -> empty_jit_state)

(** val init_mem : Mem.mem' * block **)

let init_mem =
  Mem.alloc Mem.empty 0 ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) 1))))))

(** val jit_state_memval_list : memval list **)

let jit_state_memval_list =
  (Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.one) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    (Byte.repr ((fun p->2*p) 1))) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    (Byte.repr ((fun p->1+2*p) 1))) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: ((Byte Byte.zero) :: ((Byte
    Byte.zero) :: [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val jit_state_blk : block **)

let jit_state_blk =
  snd init_mem

(** val mem1 : Mem.mem **)

let mem1 =
  match Mem.storebytes (fst init_mem) jit_state_blk 0 jit_state_memval_list with
  | Some m -> m
  | None -> Mem.empty

(** val calc_jitted_code_aux : int -> int -> int list -> int list **)

let rec calc_jitted_code_aux fuel0 pc l =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n0 ->
    let ins = List32.index l (Int.repr (Z.of_nat pc)) in
    ins :: (calc_jitted_code_aux n0 (Stdlib.succ pc) l))
    fuel0

(** val calc_jitted_code : int -> int list -> int list **)

let calc_jitted_code len l =
  calc_jitted_code_aux len 0 l

(** val int2listmemval : int -> memval list **)

let int2listmemval i =
  encode_val Mint32 (Vint i)

(** val jitted_arm_memval_list : memval list **)

let jitted_arm_memval_list =
  let l =
    map int2listmemval
      (calc_jitted_code final_jitted_state.jitted_len
        final_jitted_state.jitted_list)
  in
  flat_map (fun x -> x) l

(** val init_mem2 : Mem.mem' * block **)

let init_mem2 =
  Mem.alloc mem1 0 (Z.of_nat (length jitted_arm_memval_list))

(** val jitted_arm_blk : block **)

let jitted_arm_blk =
  snd init_mem2

(** val mem2 : Mem.mem **)

let mem2 =
  match Mem.storebytes (fst init_mem2) jitted_arm_blk 0 jitted_arm_memval_list with
  | Some m -> m
  | None -> Mem.empty

(** val test_signature : signature **)

let test_signature =
  { sig_args = (Tint :: (Tint :: [])); sig_res = (Tret Tint); sig_cc =
    cc_default }

(** val fuel : int **)

let fuel =
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
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    (Stdlib.succ (Stdlib.succ (Stdlib.succ (Stdlib.succ
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val test_stack_size : int **)

let test_stack_size =
  Z.of_nat final_jitted_state.stack_len

(** val test_list_val : val0 list **)

let test_list_val =
  (Vptr (jitted_arm_blk, Ptrofs.zero)) :: ((Vptr (jit_state_blk,
    Ptrofs.zero)) :: [])

(** val print_registers : Mem.mem -> block -> int -> memval list **)

let print_registers m b len =
  match Mem.loadbytes m b 0 (Z.of_nat len) with
  | Some l -> l
  | None -> []

(** val test_compcertbin : memval list **)

let test_compcertbin =
  match bin_exec fuel test_signature test_stack_size Ptrofs.zero
          test_list_val mem2 with
  | Some res -> let _ = print_val0 (fst res) in
    print_registers (snd res) jit_state_blk (length jit_state_memval_list)
  | None -> let _ = print_endline "error test_compcertbin" in []
