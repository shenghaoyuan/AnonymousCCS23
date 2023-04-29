
val xorb : bool -> bool -> bool

val negb : bool -> bool

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val length : 'a1 list -> int

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

val compOpp : comparison -> comparison

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



val add : int -> int -> int

val mul : int -> int -> int

val bool_dec : bool -> bool -> bool

val eqb : bool -> bool -> bool

module Nat :
 sig
  val add : int -> int -> int

  val mul : int -> int -> int

  val sub : int -> int -> int

  val ltb : int -> int -> bool

  val even : int -> bool

  val odd : int -> bool

  val div2 : int -> int

  val bitwise : (bool -> bool -> bool) -> int -> int -> int -> int

  val coq_land : int -> int -> int
 end

module Pos :
 sig
  type mask =
  | IsNul
  | IsPos of int
  | IsNeg
 end

module Coq_Pos :
 sig
  val succ : int -> int

  val add : int -> int -> int

  val add_carry : int -> int -> int

  val pred_double : int -> int

  val pred_N : int -> int

  type mask = Pos.mask =
  | IsNul
  | IsPos of int
  | IsNeg

  val succ_double_mask : mask -> mask

  val double_mask : mask -> mask

  val double_pred_mask : int -> mask

  val sub_mask : int -> int -> mask

  val sub_mask_carry : int -> int -> mask

  val mul : int -> int -> int

  val iter : ('a1 -> 'a1) -> 'a1 -> int -> 'a1

  val div2 : int -> int

  val div2_up : int -> int

  val size : int -> int

  val compare_cont : comparison -> int -> int -> comparison

  val compare : int -> int -> comparison

  val eqb : int -> int -> bool

  val coq_Nsucc_double : int -> int

  val coq_Ndouble : int -> int

  val coq_lor : int -> int -> int

  val coq_land : int -> int -> int

  val ldiff : int -> int -> int

  val coq_lxor : int -> int -> int

  val shiftl_nat : int -> int -> int

  val shiftr_nat : int -> int -> int

  val testbit : int -> int -> bool

  val iter_op : ('a1 -> 'a1 -> 'a1) -> int -> 'a1 -> 'a1

  val to_nat : int -> int

  val of_succ_nat : int -> int

  val eq_dec : int -> int -> bool
 end

module N :
 sig
  val of_nat : int -> int
 end

module Coq_N :
 sig
  val succ_double : int -> int

  val double : int -> int

  val succ_pos : int -> int

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val compare : int -> int -> comparison

  val leb : int -> int -> bool

  val pos_div_eucl : int -> int -> int * int

  val coq_lor : int -> int -> int

  val coq_land : int -> int -> int

  val ldiff : int -> int -> int

  val coq_lxor : int -> int -> int

  val testbit : int -> int -> bool
 end

module Z :
 sig
  val double : int -> int

  val succ_double : int -> int

  val pred_double : int -> int

  val pos_sub : int -> int -> int

  val add : int -> int -> int

  val opp : int -> int

  val pred : int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val pow_pos : int -> int -> int

  val pow : int -> int -> int

  val compare : int -> int -> comparison

  val leb : int -> int -> bool

  val ltb : int -> int -> bool

  val eqb : int -> int -> bool

  val max : int -> int -> int

  val min : int -> int -> int

  val to_nat : int -> int

  val of_nat : int -> int

  val of_N : int -> int

  val to_pos : int -> int

  val iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1

  val pos_div_eucl : int -> int -> int * int

  val div_eucl : int -> int -> int * int

  val div : int -> int -> int

  val modulo : int -> int -> int

  val quotrem : int -> int -> int * int

  val quot : int -> int -> int

  val rem : int -> int -> int

  val even : int -> bool

  val odd : int -> bool

  val div2 : int -> int

  val log2 : int -> int

  val testbit : int -> int -> bool

  val shiftl : int -> int -> int

  val shiftr : int -> int -> int

  val coq_lor : int -> int -> int

  val coq_land : int -> int -> int

  val coq_lxor : int -> int -> int

  val eq_dec : int -> int -> bool
 end

val z_lt_dec : int -> int -> bool

val z_le_dec : int -> int -> bool

val z_le_gt_dec : int -> int -> bool

val zeq_bool : int -> int -> bool

val nth_error : 'a1 list -> int -> 'a1 option

val rev : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val forallb : ('a1 -> bool) -> 'a1 list -> bool

val repeat : 'a1 -> int -> 'a1 list

val n_of_digits : bool list -> int

val n_of_ascii : char -> int

val zdivide_dec : int -> int -> bool

val shift_nat : int -> int -> int

val shift_pos : int -> int -> int

val two_power_nat : int -> int

val two_power_pos : int -> int

val two_p : int -> int

val peq : int -> int -> bool

val zeq : int -> int -> bool

val zlt : int -> int -> bool

val zle : int -> int -> bool

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

val proj_sumbool : bool -> bool

val p_mod_two_p : int -> int -> int

val zshiftin : bool -> int -> int

val zzero_ext : int -> int -> int

val zsign_ext : int -> int -> int

val z_one_bits : int -> int -> int -> int list

val p_is_power2 : int -> bool

val z_is_power2 : int -> int option

val zsize : int -> int

type spec_float =
| S754_zero of bool
| S754_infinity of bool
| S754_nan
| S754_finite of bool * int * int

val emin : int -> int -> int

val fexp : int -> int -> int -> int

val digits2_pos : int -> int

val zdigits2 : int -> int

val iter_pos : ('a1 -> 'a1) -> int -> 'a1 -> 'a1

type location =
| Loc_Exact
| Loc_Inexact of comparison

type shr_record = { shr_m : int; shr_r : bool; shr_s : bool }

val shr_1 : shr_record -> shr_record

val loc_of_shr_record : shr_record -> location

val shr_record_of_loc : int -> location -> shr_record

val shr : shr_record -> int -> int -> shr_record * int

val shr_fexp : int -> int -> int -> int -> location -> shr_record * int

val shl_align : int -> int -> int -> int * int

val sFcompare : spec_float -> spec_float -> comparison option

val cond_Zopp : bool -> int -> int

val new_location_even : int -> int -> location

val new_location_odd : int -> int -> location

val new_location : int -> int -> location

val sFdiv_core_binary :
  int -> int -> int -> int -> int -> int -> (int * int) * location

type radix = int
  (* singleton inductive, whose constructor was Build_radix *)

val radix_val : radix -> int

val radix2 : radix

val iter_nat : ('a1 -> 'a1) -> int -> 'a1 -> 'a1

val cond_incr : bool -> int -> int

val round_sign_DN : bool -> location -> bool

val round_sign_UP : bool -> location -> bool

val round_N : bool -> location -> bool

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan
| B754_finite of bool * int * int

val sF2B : int -> int -> spec_float -> binary_float

val b2SF : int -> int -> binary_float -> spec_float

val bcompare : int -> int -> binary_float -> binary_float -> comparison option

type mode =
| Mode_NE
| Mode_ZR
| Mode_DN
| Mode_UP
| Mode_NA

val choice_mode : mode -> bool -> int -> location -> int

val overflow_to_inf : mode -> bool -> bool

val binary_overflow : int -> int -> mode -> bool -> spec_float

val binary_fit_aux : int -> int -> mode -> bool -> int -> int -> spec_float

val binary_round_aux :
  int -> int -> mode -> bool -> int -> int -> location -> spec_float

val bmult : int -> int -> mode -> binary_float -> binary_float -> binary_float

val shl_align_fexp : int -> int -> int -> int -> int * int

val binary_round : int -> int -> mode -> bool -> int -> int -> spec_float

val binary_normalize :
  int -> int -> mode -> int -> int -> bool -> binary_float

val fplus_naive : bool -> int -> int -> bool -> int -> int -> int -> int

val bplus : int -> int -> mode -> binary_float -> binary_float -> binary_float

val bminus :
  int -> int -> mode -> binary_float -> binary_float -> binary_float

val bdiv : int -> int -> mode -> binary_float -> binary_float -> binary_float

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

val b2BSN : int -> int -> binary_float0 -> binary_float

val fF2B : int -> int -> full_float -> binary_float0

val bsign : int -> int -> binary_float0 -> bool

val get_nan_pl : int -> int -> binary_float0 -> int

val build_nan : int -> int -> binary_float0 -> binary_float0

val bSN2B : int -> int -> binary_float0 -> binary_float -> binary_float0

val bSN2B' : int -> int -> binary_float -> binary_float0

val bopp :
  int -> int -> (binary_float0 -> binary_float0) -> binary_float0 ->
  binary_float0

val babs :
  int -> int -> (binary_float0 -> binary_float0) -> binary_float0 ->
  binary_float0

val bcompare0 :
  int -> int -> binary_float0 -> binary_float0 -> comparison option

val bmult0 :
  int -> int -> (binary_float0 -> binary_float0 -> binary_float0) -> mode ->
  binary_float0 -> binary_float0 -> binary_float0

val binary_normalize0 :
  int -> int -> mode -> int -> int -> bool -> binary_float0

val bplus0 :
  int -> int -> (binary_float0 -> binary_float0 -> binary_float0) -> mode ->
  binary_float0 -> binary_float0 -> binary_float0

val bminus0 :
  int -> int -> (binary_float0 -> binary_float0 -> binary_float0) -> mode ->
  binary_float0 -> binary_float0 -> binary_float0

val bdiv0 :
  int -> int -> (binary_float0 -> binary_float0 -> binary_float0) -> mode ->
  binary_float0 -> binary_float0 -> binary_float0

val join_bits : int -> int -> bool -> int -> int -> int

val split_bits : int -> int -> int -> (bool * int) * int

val bits_of_binary_float : int -> int -> binary_float0 -> int

val binary_float_of_bits_aux : int -> int -> int -> full_float

val binary_float_of_bits : int -> int -> int -> binary_float0

type binary32 = binary_float0

val b32_of_bits : int -> binary32

val bits_of_b32 : binary32 -> int

type binary64 = binary_float0

val b64_of_bits : int -> binary64

val bits_of_b64 : binary64 -> int

val ptr64 : bool

val big_endian : bool

val default_nan_64 : bool * int

val default_nan_32 : bool * int

val choose_nan :
  (int -> bool) -> (bool * int) -> (bool * int) list -> bool * int

val choose_nan_64 : (bool * int) list -> bool * int

val choose_nan_32 : (bool * int) list -> bool * int

val float_of_single_preserves_sNaN : bool

val float_conversion_default_nan : bool

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

module Make :
 functor (WS:WORDSIZE) ->
 sig
  val wordsize : int

  val zwordsize : int

  val modulus : int

  val half_modulus : int

  val max_unsigned : int

  val max_signed : int

  val min_signed : int

  type int = int
    (* singleton inductive, whose constructor was mkint *)

  val intval : int -> int

  val coq_Z_mod_modulus : int -> int

  val unsigned : int -> int

  val signed : int -> int

  val repr : int -> int

  val zero : int

  val one : int

  val mone : int

  val iwordsize : int

  val eq_dec : int -> int -> bool

  val eq : int -> int -> bool

  val lt : int -> int -> bool

  val ltu : int -> int -> bool

  val neg : int -> int

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val divs : int -> int -> int

  val mods : int -> int -> int

  val divu : int -> int -> int

  val modu : int -> int -> int

  val coq_and : int -> int -> int

  val coq_or : int -> int -> int

  val xor : int -> int -> int

  val not : int -> int

  val shl : int -> int -> int

  val shru : int -> int -> int

  val shr : int -> int -> int

  val rol : int -> int -> int

  val ror : int -> int -> int

  val rolm : int -> int -> int -> int

  val shrx : int -> int -> int

  val mulhu : int -> int -> int

  val mulhs : int -> int -> int

  val negative : int -> int

  val add_carry : int -> int -> int -> int

  val add_overflow : int -> int -> int -> int

  val sub_borrow : int -> int -> int -> int

  val sub_overflow : int -> int -> int -> int

  val shr_carry : int -> int -> int

  val zero_ext : int -> int -> int

  val sign_ext : int -> int -> int

  val one_bits : int -> int list

  val is_power2 : int -> int option

  val cmp : comparison0 -> int -> int -> bool

  val cmpu : comparison0 -> int -> int -> bool

  val notbool : int -> int

  val divmodu2 : int -> int -> int -> (int * int) option

  val divmods2 : int -> int -> int -> (int * int) option

  val testbit : int -> int -> bool

  val int_of_one_bits : int list -> int

  val no_overlap : int -> int -> int -> int -> bool

  val size : int -> int

  val unsigned_bitfield_extract : int -> int -> int -> int

  val signed_bitfield_extract : int -> int -> int -> int

  val bitfield_insert : int -> int -> int -> int -> int
 end

module Wordsize_32 :
 sig
  val wordsize : int
 end

module Int :
 sig
  val wordsize : int

  val zwordsize : int

  val modulus : int

  val half_modulus : int

  val max_unsigned : int

  val max_signed : int

  val min_signed : int

  val intval : int -> int

  val coq_Z_mod_modulus : int -> int

  val unsigned : int -> int

  val signed : int -> int

  val repr : int -> int

  val zero : int

  val one : int

  val mone : int

  val iwordsize : int

  val eq_dec : int -> int -> bool

  val eq : int -> int -> bool

  val lt : int -> int -> bool

  val ltu : int -> int -> bool

  val neg : int -> int

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val divs : int -> int -> int

  val mods : int -> int -> int

  val divu : int -> int -> int

  val modu : int -> int -> int

  val coq_and : int -> int -> int

  val coq_or : int -> int -> int

  val xor : int -> int -> int

  val not : int -> int

  val shl : int -> int -> int

  val shru : int -> int -> int

  val shr : int -> int -> int

  val rol : int -> int -> int

  val ror : int -> int -> int

  val rolm : int -> int -> int -> int

  val shrx : int -> int -> int

  val mulhu : int -> int -> int

  val mulhs : int -> int -> int

  val negative : int -> int

  val add_carry : int -> int -> int -> int

  val add_overflow : int -> int -> int -> int

  val sub_borrow : int -> int -> int -> int

  val sub_overflow : int -> int -> int -> int

  val shr_carry : int -> int -> int

  val zero_ext : int -> int -> int

  val sign_ext : int -> int -> int

  val one_bits : int -> int list

  val is_power2 : int -> int option

  val cmp : comparison0 -> int -> int -> bool

  val cmpu : comparison0 -> int -> int -> bool

  val notbool : int -> int

  val divmodu2 : int -> int -> int -> (int * int) option

  val divmods2 : int -> int -> int -> (int * int) option

  val testbit : int -> int -> bool

  val int_of_one_bits : int list -> int

  val no_overlap : int -> int -> int -> int -> bool

  val size : int -> int

  val unsigned_bitfield_extract : int -> int -> int -> int

  val signed_bitfield_extract : int -> int -> int -> int

  val bitfield_insert : int -> int -> int -> int -> int
 end

module Wordsize_8 :
 sig
  val wordsize : int
 end

module Byte :
 sig
  val wordsize : int

  val zwordsize : int

  val modulus : int

  val half_modulus : int

  val max_unsigned : int

  val max_signed : int

  val min_signed : int

  val intval : int -> int

  val coq_Z_mod_modulus : int -> int

  val unsigned : int -> int

  val signed : int -> int

  val repr : int -> int

  val zero : int

  val one : int

  val mone : int

  val iwordsize : int

  val eq_dec : int -> int -> bool

  val eq : int -> int -> bool

  val lt : int -> int -> bool

  val ltu : int -> int -> bool

  val neg : int -> int

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val divs : int -> int -> int

  val mods : int -> int -> int

  val divu : int -> int -> int

  val modu : int -> int -> int

  val coq_and : int -> int -> int

  val coq_or : int -> int -> int

  val xor : int -> int -> int

  val not : int -> int

  val shl : int -> int -> int

  val shru : int -> int -> int

  val shr : int -> int -> int

  val rol : int -> int -> int

  val ror : int -> int -> int

  val rolm : int -> int -> int -> int

  val shrx : int -> int -> int

  val mulhu : int -> int -> int

  val mulhs : int -> int -> int

  val negative : int -> int

  val add_carry : int -> int -> int -> int

  val add_overflow : int -> int -> int -> int

  val sub_borrow : int -> int -> int -> int

  val sub_overflow : int -> int -> int -> int

  val shr_carry : int -> int -> int

  val zero_ext : int -> int -> int

  val sign_ext : int -> int -> int

  val one_bits : int -> int list

  val is_power2 : int -> int option

  val cmp : comparison0 -> int -> int -> bool

  val cmpu : comparison0 -> int -> int -> bool

  val notbool : int -> int

  val divmodu2 : int -> int -> int -> (int * int) option

  val divmods2 : int -> int -> int -> (int * int) option

  val testbit : int -> int -> bool

  val int_of_one_bits : int list -> int

  val no_overlap : int -> int -> int -> int -> bool

  val size : int -> int

  val unsigned_bitfield_extract : int -> int -> int -> int

  val signed_bitfield_extract : int -> int -> int -> int

  val bitfield_insert : int -> int -> int -> int -> int
 end

module Wordsize_64 :
 sig
  val wordsize : int
 end

module Int64 :
 sig
  val wordsize : int

  val zwordsize : int

  val modulus : int

  val half_modulus : int

  val intval : int -> int

  val coq_Z_mod_modulus : int -> int

  val unsigned : int -> int

  val signed : int -> int

  val repr : int -> int

  val zero : int

  val eq_dec : int -> int -> bool

  val eq : int -> int -> bool

  val lt : int -> int -> bool

  val ltu : int -> int -> bool

  val neg : int -> int

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val divu : int -> int -> int

  val modu : int -> int -> int

  val coq_and : int -> int -> int

  val coq_or : int -> int -> int

  val xor : int -> int -> int

  val shl : int -> int -> int

  val shru : int -> int -> int

  val iwordsize' : int

  val shl' : int -> int -> int

  val shru' : int -> int -> int

  val shr' : int -> int -> int
 end

module Wordsize_Ptrofs :
 sig
  val wordsize : int
 end

module Ptrofs :
 sig
  val wordsize : int

  val modulus : int

  val half_modulus : int

  val intval : int -> int

  val coq_Z_mod_modulus : int -> int

  val unsigned : int -> int

  val signed : int -> int

  val repr : int -> int

  val zero : int

  val one : int

  val eq_dec : int -> int -> bool

  val eq : int -> int -> bool

  val ltu : int -> int -> bool

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val cmpu : comparison0 -> int -> int -> bool

  val to_int : int -> int

  val to_int64 : int -> int

  val of_int : int -> int

  val of_ints : int -> int

  val of_int64 : int -> int
 end

module PTree :
 sig
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

  val empty : 'a1 t

  val get' : int -> 'a1 tree' -> 'a1 option

  val get : int -> 'a1 tree -> 'a1 option

  val set0 : int -> 'a1 -> 'a1 tree'

  val set' : int -> 'a1 -> 'a1 tree' -> 'a1 tree'

  val set : int -> 'a1 -> 'a1 tree -> 'a1 tree

  val map1' : ('a1 -> 'a2) -> 'a1 tree' -> 'a2 tree'

  val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module PMap :
 sig
  type 'a t = 'a * 'a PTree.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : int -> 'a1 t -> 'a1

  val set : int -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> int

  val eq : t -> t -> bool
 end

module IMap :
 functor (X:INDEXED_TYPE) ->
 sig
  type elt = X.t

  val elt_eq : X.t -> X.t -> bool

  type 'x t = 'x PMap.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : X.t -> 'a1 t -> 'a1

  val set : X.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module ZIndexed :
 sig
  type t = int

  val index : int -> int

  val eq : int -> int -> bool
 end

module ZMap :
 sig
  type elt = ZIndexed.t

  val elt_eq : ZIndexed.t -> ZIndexed.t -> bool

  type 'x t = 'x PMap.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : ZIndexed.t -> 'a1 t -> 'a1

  val set : ZIndexed.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module type EQUALITY_TYPE =
 sig
  type t

  val eq : t -> t -> bool
 end

module EMap :
 functor (X:EQUALITY_TYPE) ->
 sig
  type elt = X.t

  val elt_eq : X.t -> X.t -> bool

  type 'a t = X.t -> 'a

  val init : 'a1 -> X.t -> 'a1

  val get : X.t -> 'a1 t -> 'a1

  val set : X.t -> 'a1 -> 'a1 t -> X.t -> 'a1

  val map : ('a1 -> 'a2) -> 'a1 t -> X.t -> 'a2
 end

val beq_dec : int -> int -> binary_float0 -> binary_float0 -> bool

val bofZ : int -> int -> int -> binary_float0

val zofB : int -> int -> binary_float0 -> int option

val zofB_range : int -> int -> binary_float0 -> int -> int -> int option

val bconv :
  int -> int -> int -> int -> (binary_float0 -> binary_float0) -> mode ->
  binary_float0 -> binary_float0

type float = binary64

type float32 = binary32

val cmp_of_comparison : comparison0 -> comparison option -> bool

val quiet_nan_64_payload : int -> int

val quiet_nan_64 : (bool * int) -> float

val default_nan_0 : float

val quiet_nan_32_payload : int -> int

val quiet_nan_32 : (bool * int) -> float32

val default_nan_1 : float32

module Float :
 sig
  val expand_nan_payload : int -> int

  val expand_nan : bool -> int -> binary_float0

  val of_single_nan : float32 -> float

  val reduce_nan_payload : int -> int

  val to_single_nan : float -> float32

  val neg_nan : float -> float

  val abs_nan : float -> float

  val cons_pl : float -> (bool * int) list -> (bool * int) list

  val binop_nan : float -> float -> float

  val zero : float

  val eq_dec : float -> float -> bool

  val neg : float -> float

  val abs : float -> float

  val add : float -> float -> float

  val sub : float -> float -> float

  val mul : float -> float -> float

  val div : float -> float -> float

  val compare : float -> float -> comparison option

  val cmp : comparison0 -> float -> float -> bool

  val of_single : float32 -> float

  val to_single : float -> float32

  val to_int : float -> int option

  val to_intu : float -> int option

  val of_int : int -> float

  val of_intu : int -> float

  val to_bits : float -> int

  val of_bits : int -> float
 end

module Float32 :
 sig
  val neg_nan : float32 -> float32

  val abs_nan : float32 -> float32

  val cons_pl : float32 -> (bool * int) list -> (bool * int) list

  val binop_nan : float32 -> float32 -> float32

  val zero : float32

  val eq_dec : float32 -> float32 -> bool

  val neg : float32 -> float32

  val abs : float32 -> float32

  val add : float32 -> float32 -> float32

  val sub : float32 -> float32 -> float32

  val mul : float32 -> float32 -> float32

  val div : float32 -> float32 -> float32

  val compare : float32 -> float32 -> comparison option

  val cmp : comparison0 -> float32 -> float32 -> bool

  val to_int : float32 -> int option

  val to_intu : float32 -> int option

  val of_int : int -> float32

  val of_intu : int -> float32

  val to_bits : float32 -> int

  val of_bits : int -> float32
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

val cc_default : calling_convention

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

val eq_block : int -> int -> bool

type val0 =
| Vundef
| Vint of int
| Vlong of int
| Vfloat of float
| Vsingle of float32
| Vptr of block * int

val vzero : val0

val vtrue : val0

val vfalse : val0

val vnullptr : val0

module Val :
 sig
  val eq : val0 -> val0 -> bool

  val has_type_dec : val0 -> typ -> bool

  val neg : val0 -> val0

  val negf : val0 -> val0

  val absf : val0 -> val0

  val negfs : val0 -> val0

  val absfs : val0 -> val0

  val intoffloat : val0 -> val0 option

  val intuoffloat : val0 -> val0 option

  val floatofint : val0 -> val0 option

  val floatofintu : val0 -> val0 option

  val intofsingle : val0 -> val0 option

  val intuofsingle : val0 -> val0 option

  val singleofint : val0 -> val0 option

  val singleofintu : val0 -> val0 option

  val notint : val0 -> val0

  val of_bool : bool -> val0

  val sign_ext : int -> val0 -> val0

  val singleoffloat : val0 -> val0

  val floatofsingle : val0 -> val0

  val add : val0 -> val0 -> val0

  val sub : val0 -> val0 -> val0

  val mul : val0 -> val0 -> val0

  val mulhs : val0 -> val0 -> val0

  val mulhu : val0 -> val0 -> val0

  val divs : val0 -> val0 -> val0 option

  val divu : val0 -> val0 -> val0 option

  val modu : val0 -> val0 -> val0 option

  val sub_overflow : val0 -> val0 -> val0

  val negative : val0 -> val0

  val coq_and : val0 -> val0 -> val0

  val coq_or : val0 -> val0 -> val0

  val xor : val0 -> val0 -> val0

  val shl : val0 -> val0 -> val0

  val shr : val0 -> val0 -> val0

  val shru : val0 -> val0 -> val0

  val ror : val0 -> val0 -> val0

  val addf : val0 -> val0 -> val0

  val subf : val0 -> val0 -> val0

  val mulf : val0 -> val0 -> val0

  val divf : val0 -> val0 -> val0

  val addfs : val0 -> val0 -> val0

  val subfs : val0 -> val0 -> val0

  val mulfs : val0 -> val0 -> val0

  val divfs : val0 -> val0 -> val0

  val negl : val0 -> val0

  val longofint : val0 -> val0

  val longofintu : val0 -> val0

  val addl : val0 -> val0 -> val0

  val subl : val0 -> val0 -> val0

  val mull : val0 -> val0 -> val0

  val divlu : val0 -> val0 -> val0 option

  val modlu : val0 -> val0 -> val0 option

  val andl : val0 -> val0 -> val0

  val orl : val0 -> val0 -> val0

  val xorl : val0 -> val0 -> val0

  val shll : val0 -> val0 -> val0

  val shrl : val0 -> val0 -> val0

  val shrlu : val0 -> val0 -> val0

  val cmp_different_blocks : comparison0 -> bool option

  val cmpu_bool :
    (block -> int -> bool) -> comparison0 -> val0 -> val0 -> bool option

  val of_optbool : bool option -> val0

  val cmpu : (block -> int -> bool) -> comparison0 -> val0 -> val0 -> val0

  val offset_ptr : val0 -> int -> val0

  val load_result : memory_chunk -> val0 -> val0
 end

val size_chunk : memory_chunk -> int

val size_chunk_nat : memory_chunk -> int

val align_chunk : memory_chunk -> int

type quantity =
| Q32
| Q64

val quantity_eq : quantity -> quantity -> bool

val size_quantity_nat : quantity -> int

type memval =
| Undef
| Byte of int
| Fragment of val0 * quantity * int

val bytes_of_int : int -> int -> int list

val int_of_bytes : int list -> int

val rev_if_be : int list -> int list

val encode_int : int -> int -> int list

val decode_int : int list -> int

val inj_bytes : int list -> memval list

val proj_bytes : memval list -> int list option

val inj_value_rec : int -> val0 -> quantity -> memval list

val inj_value : quantity -> val0 -> memval list

val check_value : int -> val0 -> quantity -> memval list -> bool

val proj_value : quantity -> memval list -> val0

val encode_val : memory_chunk -> val0 -> memval list

val decode_val : memory_chunk -> memval list -> val0

type permission =
| Freeable
| Writable
| Readable
| Nonempty

type perm_kind =
| Max
| Cur

module Mem :
 sig
  type mem' = { mem_contents : memval ZMap.t PMap.t;
                mem_access : (int -> perm_kind -> permission option) PMap.t;
                nextblock : block }

  val mem_contents : mem' -> memval ZMap.t PMap.t

  val mem_access : mem' -> (int -> perm_kind -> permission option) PMap.t

  val nextblock : mem' -> block

  type mem = mem'

  val perm_order_dec : permission -> permission -> bool

  val perm_order'_dec : permission option -> permission -> bool

  val perm_dec : mem -> block -> int -> perm_kind -> permission -> bool

  val range_perm_dec :
    mem -> block -> int -> int -> perm_kind -> permission -> bool

  val valid_access_dec :
    mem -> memory_chunk -> block -> int -> permission -> bool

  val valid_pointer : mem -> block -> int -> bool

  val empty : mem

  val alloc : mem -> int -> int -> mem' * block

  val getN : int -> int -> memval ZMap.t -> memval list

  val load : memory_chunk -> mem -> block -> int -> val0 option

  val loadv : memory_chunk -> mem -> val0 -> val0 option

  val setN : memval list -> int -> memval ZMap.t -> memval ZMap.t

  val store : memory_chunk -> mem -> block -> int -> val0 -> mem option

  val storev : memory_chunk -> mem -> val0 -> val0 -> mem option

  val storebytes : mem -> block -> int -> memval list -> mem option
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

val ireg_eq : ireg -> ireg -> bool

val freg_eq : freg -> freg -> bool

type crbit =
| CN
| CZ
| CC
| CV

val crbit_eq : crbit -> crbit -> bool

type preg =
| IR of ireg
| FR of freg
| CR of crbit
| PC

val preg_eq : preg -> preg -> bool

module PregEq :
 sig
  type t = preg

  val eq : preg -> preg -> bool
 end

module Pregmap :
 sig
  type elt = PregEq.t

  val elt_eq : PregEq.t -> PregEq.t -> bool

  type 'a t = PregEq.t -> 'a

  val init : 'a1 -> PregEq.t -> 'a1

  val get : PregEq.t -> 'a1 t -> 'a1

  val set : PregEq.t -> 'a1 -> 'a1 t -> PregEq.t -> 'a1

  val map : ('a1 -> 'a2) -> 'a1 t -> PregEq.t -> 'a2
 end

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

val preg_of : mreg -> preg

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

val int_callee_save_regs : mreg list

val float_callee_save_regs : mreg list

val int_param_regs : mreg list

val ireg_eqb : ireg -> ireg -> bool

val encode_arm32 : int -> int -> int -> int -> int

val decode_arm32 : int -> int -> int -> int

val int2ireg : int -> ireg option

val is_thumb2 : int -> bool

val decode_thumb : int -> instruction option

val thumb_constant_range_imm12 : int -> int -> int -> bool

val decode_thumb2 : int -> int -> instruction option

val find_instr : val0 -> Mem.mem -> (instruction * bool) option

val isize : int

val wsize : int

type sreg =
| Sreg of preg
| Ssp

type sval =
| Sval of sreg * int
| Rval of sreg

type aval =
| Cval of val0
| Aval of sval

val typ_of_preg : preg -> typ

val typ_of_sreg : sreg -> typ

val typ_of_sval : sval -> typ

type aregset = aval Pregmap.t

type rchunk =
| Cany32
| Cint32

type amemval =
| Cmemval of memval
| Amemval of sval * rchunk * int

val sreg_eq : sreg -> sreg -> bool

val sval_eq : sval -> sval -> bool

val aval_eq : aval -> aval -> bool

val find_instr0 : aval -> Mem.mem -> (instruction * bool) option

val encode_sval_fragments : rchunk -> sval -> amemval list

val rchunk_of_chunk : memory_chunk -> rchunk option

val encode_sval : memory_chunk -> sval -> amemval list option

val encode_aval : memory_chunk -> aval -> amemval list option

val setN0 : amemval list -> int -> amemval ZMap.t -> amemval ZMap.t

val getN0 : int -> int -> amemval ZMap.t -> amemval list

val astack_store :
  memory_chunk -> Mem.mem -> amemval ZMap.t -> block -> int -> aval ->
  amemval ZMap.t option

type amemSplit =
| SplitErr
| SplitNIL
| SplitC of memval list
| SplitA of ((sval * rchunk) * int) list

val cons_amemval : amemval -> amemSplit -> amemSplit

val rchunk_eq : rchunk -> rchunk -> bool

val split_amemval : amemval list -> amemSplit

val check_value0 :
  int -> sval -> rchunk -> ((sval * rchunk) * int) list -> bool

val proj_value0 : memory_chunk -> ((sval * rchunk) * int) list -> aval option

val decode_val0 : memory_chunk -> amemval list -> aval option

val astack_load :
  memory_chunk -> Mem.mem -> amemval ZMap.t -> block -> int -> aval option

type astack = amemval ZMap.t

type aoutcome =
| ANext of aregset * astack * block * Mem.mem
| AStuck

val alift1 : (val0 -> val0) -> aval -> aval

val ashl : aval -> int -> aval

val ashru : aval -> int -> aval

val ashr : aval -> int -> aval

val aror : aval -> int -> aval

val aeval_shift_op : shift_op -> aregset -> aval

val aadd : aval -> aval -> aval option

val alift2 : (val0 -> val0 -> val0) -> aval -> aval -> aval

val aoffset_ptr : aval -> int -> aval

val agoto_label : int -> aregset -> block -> astack -> Mem.mem -> aoutcome

val size_of_bool : bool -> int

val anextinstr : bool -> aregset -> PregEq.t -> aval

val incrinstr : aregset -> PregEq.t -> aval

val aundef_flags : aregset -> aregset

val anextinstr_nf : bool -> aregset -> PregEq.t -> aval

val acompare_int : aregset -> aval -> aval -> Mem.mem -> PregEq.t -> aval

val get_offset : sval -> int

val get_register : sval -> sreg

val get_stack_offset : sval -> int option

val aexec_load :
  bool -> memory_chunk -> aval option -> preg -> aregset -> astack -> block
  -> Mem.mem -> aoutcome

val is_stack_pointer : block -> aval -> int option

val aexec_store' :
  memory_chunk -> aval option -> preg -> aregset -> astack -> block ->
  Mem.mem -> aoutcome

val aexec_store :
  bool -> memory_chunk -> aval option -> preg -> aregset -> astack -> block
  -> Mem.mem -> aoutcome

val adivs : aval -> aval -> aval option

val adivu : aval -> aval -> aval option

val update_regset : (preg * aval) list -> aregset -> aregset

val init_regset : (preg * val0) list -> aregset -> aregset

val acompare_float_update : aval -> aval -> (preg * aval) list

val acompare_float : aregset -> aval -> aval -> aregset

val acompare_float32_update : aval -> aval -> (preg * aval) list

val acompare_float32 : aregset -> aval -> aval -> aregset

val amaketotal : aval option -> aval

val alifto1 : (val0 -> val0 option) -> aval -> aval option

val eval_unary_cond : (int -> bool) -> aregset -> preg -> bool option

val eval_binary_cond :
  (int -> int -> bool) -> aregset -> preg -> preg -> bool option

val eval_ternary_cond :
  (int -> int -> int -> bool) -> aregset -> preg -> preg -> preg -> bool
  option

val aeval_testcond : testcond -> aregset -> bool option

val aexec_instr :
  bool -> instruction -> aregset -> astack -> block -> Mem.mem -> aoutcome

val alloc_arguments :
  val0 list -> typ list -> mreg list -> (preg * val0) list option

val regset0 : aregset

val allocframe : int -> int -> Mem.mem -> aregset -> aoutcome

val init_state : signature -> int -> int -> val0 list -> Mem.mem -> aoutcome

val is_final_state : aregset -> bool

val has_rettypeb : val0 -> rettype -> bool

val is_definedb : val0 -> bool

val get_result : rettype -> aval -> Mem.mem -> (val0 * Mem.mem) option

val bin_interp :
  rettype -> int -> aregset -> astack -> block -> Mem.mem -> (val0 * Mem.mem)
  option

val bin_exec :
  int -> signature -> int -> int -> val0 list -> Mem.mem -> (val0 * Mem.mem)
  option

module List64AsArray :
 sig
  type t = int list

  val index : t -> int -> int option
 end

module List16 :
 sig
  type t = int list

  val index : t -> int -> int option

  val assign' : t -> int -> int -> t option

  val assign : t -> int -> int -> t

  val create_int_list : int -> t
 end

type signedness =
| Signed
| Unsigned

val cmp_ptr32_null : Mem.mem -> val0 -> bool option

val val32_modu : val0 -> val0 -> val0

val comp_eq_32 : val0 -> val0 -> bool

val compu_lt_32 : val0 -> val0 -> bool

val compu_le_32 : val0 -> val0 -> bool

val val64_divlu : val0 -> val0 -> val0

val val64_modlu : val0 -> val0 -> val0

val compl_eq : val0 -> val0 -> bool

val compl_ne : val0 -> val0 -> bool

val compl_lt : val0 -> val0 -> bool

val compl_le : val0 -> val0 -> bool

val compl_gt : val0 -> val0 -> bool

val compl_ge : val0 -> val0 -> bool

val complu_lt : val0 -> val0 -> bool

val complu_le : val0 -> val0 -> bool

val complu_gt : val0 -> val0 -> bool

val complu_ge : val0 -> val0 -> bool

val complu_set : val0 -> val0 -> bool

val val_intuoflongu : val0 -> val0

val val_intsoflongu : val0 -> val0

val sint32_to_vint : int -> val0

val int64_to_sint32 : int -> int

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

val reg_eqb : reg -> reg -> bool

val id_of_reg : reg -> int

val int_of_reg : reg -> int

val perm_ge : permission -> permission -> bool

type memory_region = { start_addr : val0; block_size : val0;
                       block_perm : permission; block_ptr : val0 }

module Memory_regions :
 sig
  type t = memory_region list
 end

type myMemRegionsType = Memory_regions.t

val z_to_reg : int -> reg option

val get_dst : int -> int

val get_src : int -> int

val int64_to_dst_reg' : int -> reg option

val int64_to_src_reg' : int -> reg option

val get_opcode : int -> int

val get_offset0 : int -> int

val get_immediate : int -> int

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
| BPF_ILLEGAL_JIT
| BPF_ILLEGAL_ARM_LEN
| BPF_ILLEGAL_EP_LEN

val z_of_flag : bpf_flag -> int

val int_of_flag : bpf_flag -> int

val well_chunk_Z : memory_chunk -> int

val memory_chunk_to_valu32 : memory_chunk -> val0

val memory_chunk_to_valu32_upbound : memory_chunk -> val0

val _to_vlong : val0 -> val0 option

val vlong_to_vint_or_vlong : memory_chunk -> val0 -> val0

val vint_to_vint_or_vlong : memory_chunk -> val0 -> val0

type ('st, 'a) m = 'st -> ('a * 'st) option

val returnM : 'a1 -> ('a2, 'a1) m

val bindM : ('a3, 'a1) m -> ('a1 -> ('a3, 'a2) m) -> ('a3, 'a2) m

type arch =
| A32
| A64

type cond =
| Eq0
| Gt0 of signedness
| Ge of signedness
| Lt0 of signedness
| Le of signedness
| SEt
| Ne

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

type bpf_instruction =
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

val get_instruction_alu64_imm : int -> reg -> int -> int -> bpf_instruction

val get_instruction_alu64_reg : int -> reg -> reg -> int -> bpf_instruction

val get_instruction_alu32_imm : int -> reg -> int -> int -> bpf_instruction

val get_instruction_alu32_reg : int -> reg -> reg -> int -> bpf_instruction

val get_instruction_ld : int -> reg -> int -> int -> bpf_instruction

val get_instruction_ldx : int -> reg -> reg -> int -> int -> bpf_instruction

val get_instruction_st : int -> reg -> int -> int -> int -> bpf_instruction

val get_instruction_stx : int -> reg -> reg -> int -> int -> bpf_instruction

val get_instruction_branch_imm :
  int -> reg -> int -> int -> int -> bpf_instruction

val get_instruction_branch_reg :
  int -> reg -> reg -> int -> int -> bpf_instruction

val decode : int -> bpf_instruction option

val decode_prog_aux : int -> int -> List64AsArray.t -> bpf_instruction list

val decode_prog : List64AsArray.t -> int -> bpf_instruction list

module Raw :
 sig
  val of_pos : int -> string -> string
 end

val of_pos0 : int -> string

val of_N0 : int -> string

val of_Z : int -> string

val of_nat0 : int -> string

val string_of_reg : reg -> string

val string_of_int_unsigned : int -> string

val string_of_int_signed : int -> string

val print_reg_imm : (reg, imm) sum -> string

val string_of_signedness : signedness -> string

val string_of_memory_chunk : memory_chunk -> string

val string_of_arch : arch -> string

val print_rBPF_instruction : bpf_instruction -> string

val print_rBPF_prog_aux : int -> bpf_instruction list -> string list

val print_rBPF_prog : bpf_instruction list -> string list

type loadStorePerm =
| NonPerm
| LoadPerm
| StorePerm
| LoadAndStore

val loadStorePerm_eq : loadStorePerm -> loadStorePerm -> bool

val loadStorePerm_eqb : loadStorePerm -> loadStorePerm -> bool

val upd_LoadStorePerm : loadStorePerm -> loadStorePerm -> loadStorePerm option

type loadStoreRegs = { is_R0 : loadStorePerm; is_R1 : loadStorePerm;
                       is_R2 : loadStorePerm; is_R3 : loadStorePerm;
                       is_R4 : loadStorePerm; is_R5 : loadStorePerm;
                       is_R6 : loadStorePerm; is_R7 : loadStorePerm;
                       is_R8 : loadStorePerm; is_R9 : loadStorePerm;
                       is_R10 : loadStorePerm }

val init_LoadStoreRegs : loadStoreRegs

val eval_LoadStoreRegs : loadStoreRegs -> reg -> loadStorePerm

val upd_LoadStoreRegs :
  loadStoreRegs -> reg -> loadStorePerm -> loadStoreRegs option

val is_load_reg : reg -> loadStoreRegs -> bool

val is_store_reg : reg -> loadStoreRegs -> bool

val is_non_reg : reg -> loadStoreRegs -> bool

type key_value2 = { arm_ofs : int; alu32_ofs : int }

val empty_kv : key_value2

module ListKeyV :
 sig
  type t = key_value2 list

  val index : t -> int -> key_value2 option

  val assign' : t -> int -> key_value2 -> t option

  val assign : t -> int -> key_value2 -> t

  val create_int_list : int -> t
 end

val jITTED_LIST_MAX_LENGTH : int

type jit_state = { jit_pc : int; jit_flag : val0; jit_regs : val0;
                   jit_mrs_num : int; jit_mrs : myMemRegionsType;
                   jit_ins_len : int; jit_ins : List64AsArray.t;
                   kv2 : ListKeyV.t; use_IR11 : bool;
                   load_store_regs : loadStoreRegs; offset : int;
                   thumb_len : int; thumb : List16.t; jitted_len : int;
                   jitted_list : val0; jit_mem : Mem.mem }

val empty_jit_state : jit_state

val eval_jit_pc : jit_state -> int

val upd_jit_pc : int -> jit_state -> jit_state

val upd_jit_pc_incr : jit_state -> jit_state

val eval_jit_flag : jit_state -> val0 option

val upd_jit_flag' : val0 -> jit_state -> Mem.mem option

val upd_jit_flag : bpf_flag -> jit_state -> jit_state option

val eval_jit_reg : reg -> jit_state -> val0 option

val upd_jit_reg' : reg -> val0 -> jit_state -> Mem.mem option

val upd_jit_reg : reg -> val0 -> jit_state -> jit_state option

val eval_jit_mem_num : jit_state -> int

val eval_jit_mem_regions : jit_state -> myMemRegionsType

val eval_jit_mem : jit_state -> Mem.mem

val upd_jit_mem : Mem.mem -> jit_state -> jit_state

val load_jit_mem : memory_chunk -> val0 -> jit_state -> val0 option

val store_jit_mem_imm :
  val0 -> memory_chunk -> val0 -> jit_state -> jit_state option

val store_jit_mem_reg :
  val0 -> memory_chunk -> val0 -> jit_state -> jit_state option

val eval_jit_ins_len : jit_state -> int

val eval_jit_ins : int -> jit_state -> int option

val add_key_value2 : int -> int -> int -> jit_state -> jit_state

val upd_IR11_jittedthumb : bool -> jit_state -> jit_state

val add_ins_jittedthumb : int -> jit_state -> jit_state

val upd_bpf_offset_jittedthumb : jit_state -> jit_state

val upd_load_store_regs_jittedthumb :
  reg -> loadStorePerm -> jit_state -> jit_state option

val upd_jitted_list' : int -> jit_state -> Mem.mem option

val upd_jitted_list : int -> jit_state -> jit_state option

val compcertbin_signature : signature

val jit_state_start_address : val0

val magic_function : int -> int -> int -> jit_state -> jit_state option

val reset_init_jittedthumb : jit_state -> jit_state

val eval_thumb_ins : int -> jit_state -> int option

val eval_key_value2_arm_ofs : int -> jit_state -> int option

val eval_key_value2_alu32_ofs : int -> jit_state -> int option

val ireg_eqb0 : ireg -> ireg -> bool

val reg_ireg_eqb : reg -> ireg -> bool

val reg_of_ireg : ireg -> reg option

val ireg_of_reg : reg -> ireg

val ireg2nat : ireg -> int

val int_of_ireg : ireg -> int

val encode_arm0 : int -> int -> int -> int -> int

val arm32_decode_prog_aux : int -> int -> val0 -> Mem.mem -> instruction list

val arm32_decode_prog : val0 -> int -> Mem.mem -> instruction list

type opcode_alu32_reg =
| BPF_ADD32_REG
| BPF_SUB32_REG
| BPF_MUL32_REG
| BPF_DIV32_REG
| BPF_OR32_REG
| BPF_AND32_REG
| BPF_LSH32_REG
| BPF_RSH32_REG
| BPF_XOR32_REG
| BPF_MOV32_REG
| BPF_ARSH32_REG
| BPF_ALU32_REG_ILLEGAL_INS

type opcode_alu32_imm =
| BPF_ADD32_IMM
| BPF_SUB32_IMM
| BPF_MUL32_IMM
| BPF_OR32_IMM
| BPF_AND32_IMM
| BPF_NEG32_IMM
| BPF_XOR32_IMM
| BPF_MOV32_IMM
| BPF_ALU32_IMM_ILLEGAL_INS

val opcode_alu32_imm_eqb : opcode_alu32_imm -> opcode_alu32_imm -> bool

val nat_to_opcode_alu32_reg : int -> opcode_alu32_reg

val nat_to_opcode_alu32_imm : int -> opcode_alu32_imm

type opcode_alu32 =
| ALU32_REG
| ALU32_IMM
| ALU32_ILLEGAL_INS

val nat_to_opcode_alu32 : int -> opcode_alu32

val ins_is_bpf_alu32 : int -> bool

val ins_is_bpf_jump : int -> bool

val opcode_reg_of_imm : opcode_alu32_imm -> opcode_alu32_reg

val aDD_R_OP : int

val aDD_I_OP : int

val aND_R_OP : int

val aND_I_OP : int

val aSR_R_OP : int

val eOR_R_OP : int

val eOR_I_OP : int

val lSL_R_OP : int

val lSR_R_OP : int

val mOVW_OP : int

val mOVT_OP : int

val mOV_R_OP : int

val mUL_OP : int

val oRR_R_OP : int

val oRR_I_OP : int

val sUB_R_OP : int

val sUB_I_OP : int

val rSB_I_OP : int

val uDIV_OP : int

val bX_OP : int

val lDR_I_OP : int

val sTR_I_OP : int

val construct_thumb2_shift_rd_rm : int -> int -> int

val jit_alu32_thumb_store_template_jit :
  int -> int -> int -> jit_state -> jit_state option

val jit_alu32_thumb_load_template_jit :
  int -> int -> int -> jit_state -> jit_state option

val jit_alu32_pre : jit_state -> jit_state option

val jit_alu32_thumb_upd_save : reg -> jit_state -> jit_state option

val jit_alu32_thumb_save : jit_state -> jit_state option

val jit_alu32_thumb_upd_load : reg -> jit_state -> jit_state option

val no_reg_load : jit_state -> bool

val jit_alu32_thumb_load : jit_state -> jit_state option

val bpf_alu32_to_thumb_reg :
  opcode_alu32_reg -> reg -> ireg -> jit_state -> jit_state option

val bpf_alu32_to_thumb_imm :
  opcode_alu32_imm -> reg -> int -> jit_state -> jit_state option

val mov_int_to_movw : int -> ireg -> jit_state -> jit_state

val mov_int_to_movt : int -> ireg -> jit_state -> jit_state

val bpf_alu32_to_thumb : int -> jit_state -> jit_state option

val jit_alu32_to_thumb_pass : int -> int -> jit_state -> jit_state option

val jit_alu32_thumb_upd_store : reg -> jit_state -> jit_state option

val jit_alu32_thumb_store : jit_state -> jit_state option

val jit_alu32_thumb_upd_reset : reg -> jit_state -> jit_state option

val jit_alu32_thumb_reset : jit_state -> jit_state option

val jit_alu32_post : jit_state -> jit_state option

val copy_thumb_list_from_to_aux : int -> int -> jit_state -> jit_state option

val copy_thumb_list_from_to : jit_state -> jit_state option

val jit_alu32_to_thumb : int -> jit_state -> jit_state option

val jit_alu32_aux : int -> int -> bool -> jit_state -> jit_state option

val jit_alu32 : jit_state -> jit_state option

val string_of_ireg : ireg -> string

val string_of_ptrofs_signed : int -> string

val string_of_shift_op : shift_op -> string

val print_arm32_instruction : instruction -> int -> string

val is_thumb : instruction -> bool

val print_arm32_prog_aux : int -> int -> instruction list -> string list

val print_arm32_prog : instruction list -> int -> string list

type opcode_alu64 =
| Op_BPF_ADD64
| Op_BPF_SUB64
| Op_BPF_MUL64
| Op_BPF_DIV64
| Op_BPF_OR64
| Op_BPF_AND64
| Op_BPF_LSH64
| Op_BPF_RSH64
| Op_BPF_NEG64
| Op_BPF_MOD64
| Op_BPF_XOR64
| Op_BPF_MOV64
| Op_BPF_ARSH64
| Op_BPF_ALU64_ILLEGAL_INS

val byte_to_opcode_alu64 : int -> opcode_alu64

type opcode_alu0 =
| Op_BPF_ADD32
| Op_BPF_SUB32
| Op_BPF_MUL32
| Op_BPF_DIV32
| Op_BPF_OR32
| Op_BPF_AND32
| Op_BPF_LSH32
| Op_BPF_RSH32
| Op_BPF_NEG32
| Op_BPF_MOD32
| Op_BPF_XOR32
| Op_BPF_MOV32
| Op_BPF_ARSH32
| Op_BPF_ALU32_ILLEGAL_INS

val byte_to_opcode_alu32 : int -> opcode_alu0

type opcode_branch =
| Op_BPF_JA
| Op_BPF_JEQ
| Op_BPF_JGT
| Op_BPF_JGE
| Op_BPF_JLT
| Op_BPF_JLE
| Op_BPF_JSET
| Op_BPF_JNE
| Op_BPF_JSGT
| Op_BPF_JSGE
| Op_BPF_JSLT
| Op_BPF_JSLE
| Op_BPF_CALL
| Op_BPF_RET
| Op_BPF_JMP_ILLEGAL_INS

val byte_to_opcode_branch : int -> opcode_branch

type opcode_mem_ld_imm =
| Op_BPF_LDDW_low
| Op_BPF_LDDW_high
| Op_BPF_LDX_IMM_ILLEGAL_INS

val byte_to_opcode_mem_ld_imm : int -> opcode_mem_ld_imm

type opcode_mem_ld_reg =
| Op_BPF_LDXW
| Op_BPF_LDXH
| Op_BPF_LDXB
| Op_BPF_LDXDW
| Op_BPF_LDX_REG_ILLEGAL_INS

val byte_to_opcode_mem_ld_reg : int -> opcode_mem_ld_reg

type opcode_mem_st_imm =
| Op_BPF_STW
| Op_BPF_STH
| Op_BPF_STB
| Op_BPF_STDW
| Op_BPF_ST_ILLEGAL_INS

val byte_to_opcode_mem_st_imm : int -> opcode_mem_st_imm

type opcode_mem_st_reg =
| Op_BPF_STXW
| Op_BPF_STXH
| Op_BPF_STXB
| Op_BPF_STXDW
| Op_BPF_STX_ILLEGAL_INS

val byte_to_opcode_mem_st_reg : int -> opcode_mem_st_reg

type opcode =
| Op_BPF_ALU64
| Op_BPF_ALU32
| Op_BPF_Branch
| Op_BPF_Mem_ld_imm
| Op_BPF_Mem_ld_reg
| Op_BPF_Mem_st_imm
| Op_BPF_Mem_st_reg
| Op_BPF_ILLEGAL_INS

val byte_to_opcode : int -> opcode

val magic_function0 : int -> (jit_state, unit) m

val eval_jit_pc0 : (jit_state, int) m

val upd_jit_pc0 : int -> (jit_state, unit) m

val upd_jit_pc_incr0 : (jit_state, unit) m

val eval_jit_flag0 : (jit_state, val0) m

val upd_jit_flag0 : bpf_flag -> (jit_state, unit) m

val eval_jit_mrs_num : (jit_state, int) m

val eval_jit_reg0 : reg -> (jit_state, val0) m

val upd_jit_reg0 : reg -> val0 -> (jit_state, unit) m

val eval_jit_mrs_regions : (jit_state, myMemRegionsType) m

val load_jit_mem0 : memory_chunk -> val0 -> (jit_state, val0) m

val store_jit_mem_imm0 : val0 -> memory_chunk -> val0 -> (jit_state, unit) m

val store_jit_mem_reg0 : val0 -> memory_chunk -> val0 -> (jit_state, unit) m

val eval_jit_ins_len0 : (jit_state, int) m

val eval_jit_ins0 : int -> (jit_state, int) m

val cmp_ptr32_nullM : val0 -> (jit_state, bool) m

val int64_to_dst_reg : int -> (jit_state, reg) m

val int64_to_src_reg : int -> (jit_state, reg) m

val get_jit_mem_region :
  int -> myMemRegionsType -> (jit_state, memory_region) m

val eval_key_value2_arm_ofs0 : int -> (jit_state, int) m

val eval_key_value2_alu32_ofs0 : int -> (jit_state, int) m

val _jit_bpf_get_call : val0 -> (jit_state, val0) m

val jit_exec_function : val0 -> (jit_state, val0) m

val get_dst0 : int -> (jit_state, reg) m

val reg64_to_reg32 : val0 -> (jit_state, val0) m

val get_src0 : int -> (jit_state, reg) m

val get_offset1 : int -> (jit_state, int) m

val get_immediate0 : int -> (jit_state, int) m

val eval_immediate : int -> (jit_state, val0) m

val get_src64 : int -> int -> (jit_state, val0) m

val get_opcode_ins : int -> (jit_state, int) m

val get_opcode_alu64 : int -> (jit_state, opcode_alu64) m

val get_opcode_alu32 : int -> (jit_state, opcode_alu0) m

val get_opcode_branch : int -> (jit_state, opcode_branch) m

val get_opcode_mem_ld_imm : int -> (jit_state, opcode_mem_ld_imm) m

val get_opcode_mem_ld_reg : int -> (jit_state, opcode_mem_ld_reg) m

val get_opcode_mem_st_imm : int -> (jit_state, opcode_mem_st_imm) m

val get_opcode_mem_st_reg : int -> (jit_state, opcode_mem_st_reg) m

val get_opcode0 : int -> (jit_state, opcode) m

val get_add : val0 -> val0 -> (jit_state, val0) m

val get_sub : val0 -> val0 -> (jit_state, val0) m

val get_addr_ofs : val0 -> int -> (jit_state, val0) m

val get_start_addr : memory_region -> (jit_state, val0) m

val get_block_size : memory_region -> (jit_state, val0) m

val get_block_perm : memory_region -> (jit_state, permission) m

val is_well_chunk_bool : memory_chunk -> (jit_state, bool) m

val check_mem_aux2 :
  memory_region -> permission -> val0 -> memory_chunk -> (jit_state, val0) m

val check_mem_aux :
  int -> permission -> memory_chunk -> val0 -> myMemRegionsType ->
  (jit_state, val0) m

val check_mem : permission -> memory_chunk -> val0 -> (jit_state, val0) m

val ibpf_step_opcode_alu64 : val0 -> val0 -> reg -> int -> (jit_state, unit) m

val ibpf_step_opcode_alu32 : int -> int -> (jit_state, unit) m

val ibpf_step_opcode_branch :
  val0 -> val0 -> int -> int -> int -> (jit_state, unit) m

val ibpf_step_opcode_mem_ld_imm :
  int -> val0 -> reg -> int -> (jit_state, unit) m

val ibpf_step_opcode_mem_ld_reg : val0 -> reg -> int -> (jit_state, unit) m

val ibpf_step_opcode_mem_st_imm : val0 -> val0 -> int -> (jit_state, unit) m

val ibpf_step_opcode_mem_st_reg : val0 -> val0 -> int -> (jit_state, unit) m

val ibpf_step : (jit_state, unit) m

val ibpf_interpreter_aux : int -> (jit_state, unit) m

val ibpf_interpreter : int -> (jit_state, val0) m

val wrap_around_data_ascii : string

val wrap_around_data_byte : memval list

val test_fletcher32_int64 : int list

val init_mem : Mem.mem' * block

val input_blk : block

val mem1 : Mem.mem

val init_mem2 : Mem.mem' * block

val stk_blk : block

val mem2 : Mem.mem'

val bpf_regs_memval_list : memval list

val jit_state_memval_list : memval list

val init_mem3 : Mem.mem' * block

val jit_state_blk : block

val mem3 : Mem.mem

val fletcher32_init_memory_regions : memory_region list

val init_mem4 : Mem.mem' * block

val jitted_arm_blk : block

val fletcher32_init_jit_state : jit_state

val final_jitted_state : jit_state

val extract_final_jitted_state : string list

val fletcher32_ibpf_main : string list
