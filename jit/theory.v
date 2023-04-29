From Coq Require Import List.
Section JIT.
  Variable int64: Type.
  Variable int32: Type.
  Variable bpf_ins: Type.
  Variable arm_ins: Type.
  Variable bpf_state: Type.
  Variable arm_state: Type.

  (**r ebpf/rbpf part *)
  Variable bpf_decode: list int64 -> list bpf_ins.

  Variable bpf_interpreter: list bpf_ins -> bpf_state -> bpf_state.

  (**r arm part *)
  Variable arm_decode: list int32 -> list arm_ins.

  Variable arm_interpreter: list arm_ins -> arm_state -> arm_state.
  (** we could reuse compcert arm semantics to implement an interpreter for our arm ISA subset *)

  (**r jit part: only alu32 now *)
  Variable jit: list int64 -> list int32.
  (** we could implement this function according to: https://elixir.bootlin.com/linux/v5.18-rc6/source/arch/arm/net/bpf_jit_32.c *)

  Variable magic_fun_from: bpf_state -> arm_state.

  Variable magic_fun_back: arm_state -> bpf_state.

  Theorem jit_alu_correct:
    forall b_st b_l,
      bpf_interpreter (bpf_decode b_l) b_st = 
      magic_fun_back (arm_interpreter (arm_decode (jit b_l)) (magic_fun_from b_st)).
  Proof.
  Admitted.
End JIT.