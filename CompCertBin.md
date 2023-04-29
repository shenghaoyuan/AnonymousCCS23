
# CompCertBin

## Install CompCertBin
```shell
# download source code to `YOUR_COMPCERT_DIR` and rename the unzip folder to `compcert` (it is very important!!!)
cd YOUR_COMPCERT_DIR/
git clone --branch binary https://gitlab.inria.fr/fbesson/CompCert

# rename the directory
mv CompCert compcert
cd compcert

# install coq-flocq.4.1.0
opam install coq-flocq.4.1.0
# install 32-bit compcert
./configure arm-linux -use-external-Flocq -clightgen
make all
make clightgen

# set COQPATH
# Important: if you recompile CompCert again, remember to comment COQPATH firstly!!!
vim /home/YOUR/.bashrc
# adding the line `export COQPATH="YOUR_COMPCERT_DIR"`
source /home/YOUR/.bashrc
```

## 2022-09-14

1. Why does `allocframe` use `IR12` instead of `IR11`? (save `FP`)
```Coq
  | Pallocframe sz pos =>
      let (m1, stk) := Mem.alloc m 0 sz in
      let sp := (Vptr stk Ptrofs.zero) in
      match Mem.storev Mint32 m1 (Val.offset_ptr sp pos) rs#IR13 with
      | None => Stuck
      | Some m2 => Next (nextinstr (rs #IR12 <- (rs#IR13) #IR13 <- sp)) m2
```
A: CompCert uses `IR12` as the frame pointer

2. Why does `ABinSem.v` have `ofs`?
```Coq
Inductive sval :=
| Sval (base: sreg) (ofs: int).
```

A: when do stack load/store, `ofs` will be useful

## 2022-09-02

- stacklayout (from bottom to top), CompCert ARM is full ascending or emtry ascending?
- arm instructions
  - **Pasr, Plsl, Plsr** should be `ireg -> ireg -> shift_op -> instruction` instead of  `ireg -> ireg -> ireg -> instruction`
  - **Pudiv** should be `ireg -> ireg -> ireg -> instruction` instead of `instruction`
  - **Pmls** should be added


## 2022-08-19

To clarify what `ABinSem.bin_exec` does:
The initialisation is performed by `init_state`.
- It maps the arguments in registers (function `alloc_arguments`)
  This simulates the calling conventions for passing arguments.
  arg0 -> R0
  arg1 -> R1
  arg2 -> R2
  arg3 -> R3
- Then it allocates the stack_frame (function `allocframe`)
  This is mimicking the semantics pseudo-instruction Pallocframe of
Asm.v
  The pseudo-code is as follows:
  R12 := R13 (* Save the caller stack pointer in the frame-pointer *)
  R13 := <base_address of new allocated (stack frame) memory block>
  *(R13 + ofs) := R12 (* Save the frame-pointer on the local stack-
frame. This may be unneeded but is done by CompCert *)
- Jumps to R0
  PC := R0

Then it executes the binary instructions from PC.
It executes until reaching a final state.
A state is final if:
- All the callee_save are restored to their initial value
- The stack pointer SP (R13) is restored
- The value of PC is the return address (initial value of RA i.e R14)

To do this verification, the semantics is instrumented to compute with
abstract values. This is a bit tricky and untested.

The functions `bin_interp` also takes some fuel.
I think this is the only simple way to get something executable.
Currently, it is easy to compute the value of fuel, that would get
complicated (not feasible) if we would have loops.

## 2022-07-11: add printer

```shell
(* see export/ExportBase.ml#L193 *)
let external_function p = function
  | EF_exec_binary(n, sg, sz, pos) -> (* new printer *)
      fprintf p "(EF_exec_binary %ld %a %ld %a)" (Camlcoq.Nat.to_int32 n) signatur sg (Z.to_int32 sz) coqptrofs pos
```


## 2022-06-30: first version

The relevant part is in
arm/ABinSem.v
The `A` stands for abstract because it computes with symbolic names to
track calling conventions.

The entry point is the `bin_exec` function.
The first 4 arguments need to be constants.
The last 2 arguments are (args:list val) and (m:mem)
args is a list of length at most 4, the elements have type Tint
and are stored in IR0, IR1, IR2 and IR3 (ARM conventions)
IRO is a pointer to the JITTED code

The return is of type option (val * mem)
- None means an undefined behaviour
- val is the return value (extracted from IR0)
  For a correct execution, the callee-save registers 
  need to have their initial value.
  The SP register need to be restored
  And PC has the value of the return address stored in RA
  So, the end of a jitted code should be like:
  SP := *SP (* restore the old SP saved in the stack *)
  jump RA   (* jump to the value of the register RA *)


The semantics is parameterised by a decoding function
arm/BinDecode.v this is currently an axiom to be implemented by your
ARM decoding .

The semantics has almost all the CompCert ARM instructions 
but the ALU + load Mint32, store Mint32 are probably the only things
needed.

./configure arm-linux

On my machine, I do not get a working compcert because I do not have a
workin ARM toolchain. However, all the .v files compile...

