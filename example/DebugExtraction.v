From Coq Require Extraction.
From Coq Require Import ExtrOcamlBasic ExtrOcamlChar ExtrOcamlIntConv ExtrOcamlNatInt ExtrOcamlString ExtrOcamlNativeString ExtrOcamlZInt.

From Coq Require Import Ascii String List ZArith PArith.
Import ListNotations.

From compcert.lib Require Import Integers.
From compcert.arm Require Import Archi.

Extract Constant big_endian => "false".

(**r replace some extraction from ExtrOcamlZInt *)
Extract Constant Pos.succ => "Stdlib.succ".
Extract Constant Pos.pred => "fun n -> Stdlib.max 1 (n-1)".
Extract Constant Pos.sub => "fun n m -> Stdlib.max 1 (n-m)".
Extract Constant Pos.min => "Stdlib.min".
Extract Constant Pos.max => "Stdlib.max". (*
Extract Constant Pos.compare =>
 "fun x y -> if x=y then 0 else if x<y then (-1) else 1".
Extract Constant Pos.compare_cont =>
 "fun c x y -> if x=y then c else if x<y then (-1) else 1". *)

Extract Constant N.succ => "Stdlib.succ".
Extract Constant N.pred => "fun n -> Stdlib.max 0 (n-1)".
Extract Constant N.sub => "fun n m -> Stdlib.max 0 (n-m)".
Extract Constant N.min => "Stdlib.min".
Extract Constant N.max => "Stdlib.max". (*
Extract Constant N.compare =>
 "fun x y -> if x=y then 0 else if x<y then (-1) else 1". *)

Extract Constant Z.succ => "Stdlib.succ".
Extract Constant Z.pred => "Stdlib.pred".
Extract Constant Z.abs => "Stdlib.abs".
Extract Constant Z.min => "Stdlib.min".
Extract Constant Z.max => "Stdlib.max". (*
Extract Constant Z.compare =>
 "fun x y -> if x=y then 0 else if x<y then (-1) else 1". *)
Extract Constant Z.abs_N => "Stdlib.abs".


Extract Inductive Int.int => "int" [""].
Extract Inductive Int64.int => "int" [""].
Extract Inductive Ptrofs.int => "int" [""].
Extract Inductive Byte.int => "int" [""].