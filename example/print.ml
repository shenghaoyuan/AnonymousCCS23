(** * Common Printing Functions *)

let string_of_val0 v =
  match v with
  | Vundef -> "undefied value"
  | Vint x0 -> "Int32: " ^ (string_of_int x0)
  | Vlong x0 -> "Int64: " ^ (string_of_int x0)
  | Vptr (b, ofs) -> (string_of_int b) ^ "," ^ (string_of_int ofs)
  | _ -> "unexpected cases"

let print_val0 v = print_endline (string_of_val0 v)

let string_of_val0_hex v =
  match v with
  | Vundef -> "undefied value"
  | Vint x0 -> "Int32: " ^ (Printf.sprintf "0x%02x" x0)
  | Vlong x0 -> "Int64: " ^ (Printf.sprintf "0x%02x" x0)
  | Vptr (b, ofs) -> (Printf.sprintf "0x%02x" b) ^ "," ^ (Printf.sprintf "0x%02x" ofs)
  | _ -> "unexpected cases"

let print_val0_hex v = print_endline (string_of_val0_hex v)
  
(** * rBPF related Printing Functions *)  

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

let string_of_arch a =
  match a with
  | A32 -> "32"
  | A64 -> "64"

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

let string_of_signedness s =
  match s with
  | Signed -> "s"
  | Unsigned -> ""


let strng_of_cond cd =
  match cd with
  | Eq0 -> "eq"
  | Gt0 s -> (string_of_signedness s) ^ "gt"
  | Ge s -> (string_of_signedness s) ^ "ge"
  | Lt0 s -> (string_of_signedness s) ^ "lt"
  | Le s -> (string_of_signedness s) ^ "le"
  | SEt -> "set"
  | Ne ->"ne"

let string_of_memory_chunk mc =
  match mc with
  | Mint32          -> "32"
  | Mint16unsigned  -> "16"
  | Mint8unsigned   -> "8"
  | Mint64          -> "64"
  | _               -> "error"


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
  
  
let print_state st =
  let _ = print_endline "****************bpf state***************" in
  let _ = print_endline ("pc:= " ^ string_of_int st.pc_loc) in
  let _ = print_endline ("flag:= " ^ string_of_flag st.flag) in
  let _ = print_regmap st.regs_st in
    print_endline "****************bpf state***************\n\n"
  

(** * arm32 related Printing Functions *)

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

let string_of_shift_op sop =
  match sop with
  | SOimm i  -> string_of_int i
  | SOreg ir -> string_of_ireg ir
  | _ -> "shift_op not yet"

let string_of_arm_instruction ins =
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
    
let print_aexec_load chunk addr stk b m0 =
  match addr with
  | Some a ->
    (match a with
     | Cval addr' ->
       (match Mem.loadv chunk m0 addr' with
        | Some v0 -> "Cval " ^ (string_of_val0 v0)
        | None -> "Cval None")
     | Aval sv ->
       (match get_stack_offset sv with
        | Some o ->
          (match astack_load chunk m0 stk b (Ptrofs.unsigned o) with
           | Some v0 -> "Aval " ^ (string_of_reg_val v0)
           | None -> "Aval None")
        | None -> "Aval get_stack_offset None"))
  | None -> "addr None"     
    

let print_aexec_load chunk addr stk b m0 =
  match addr with
  | Some a ->
    (match a with
     | Cval addr' ->
       (match Mem.loadv chunk m0 addr' with
        | Some v0 -> "Cval " ^ (string_of_val0 v0)
        | None -> "Cval None" )
     | Aval v ->
       let Sval (s, o) = v in
       if sreg_eq Ssp s
       then (match astack_load chunk m0 stk b (Int.unsigned o) with
             | Some v0 -> "Aval " ^ (string_of_reg_val v0)
             | None -> "Aval None")
       else "Aval get_stack_offset None")
  | None -> "addr None"

let print_arm_stack rs stk b m =
  let _ = print_endline "****************arm32 stack***************" in
  let _ = print_endline ("Stack13=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 52)))) stk b m) ) in
  let _ = print_endline ("Stack12=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 48)))) stk b m) ) in
  let _ = print_endline ("Stack11=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 44)))) stk b m) ) in
  let _ = print_endline ("Stack10=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 40)))) stk b m) ) in
  let _ = print_endline ("Stack9=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 36)))) stk b m) ) in
  let _ = print_endline ("Stack8=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 32)))) stk b m) ) in
  let _ = print_endline ("Stack7=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 28)))) stk b m) ) in
  let _ = print_endline ("Stack6=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 24)))) stk b m) ) in
  let _ = print_endline ("Stack5=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 20)))) stk b m) ) in
  let _ = print_endline ("Stack4=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 16)))) stk b m) ) in
  let _ = print_endline ("Stack3=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 12)))) stk b m) ) in
  let _ = print_endline ("Stack2=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 8)))) stk b m) ) in
  let _ = print_endline ("Stack1=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 4)))) stk b m) ) in
  let _ = print_endline ("Stack0=  " ^ (print_aexec_load Mint32 (aadd (rs (IR IR13)) (Cval (Vint (Int.repr 0)))) stk b m) ) in
    print_endline "****************arm32 stack***************\n\n"

(** * JIT related Printing Functions *)

let string_of_loadStorePerm ls =
  match ls with
  | NonPerm -> "NonPerm"
  | LoadPerm -> "LoadPerm"
  | StorePerm -> "StorePerm"
  | LoadAndStore -> "LoadAndStore"

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

let string_of_opt_val0 o =
  match o with
  | Some v -> string_of_val0 v
  | None -> "None"
  
let print_regmap_ptr v m =
  let _ = print_endline "*******jitted ibpf regs*********" in
  let _ = print_endline ("R0=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m v))) in
  let _ = print_endline ("R1=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 8))))) in
  let _ = print_endline ("R2=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 16))))) in
  let _ = print_endline ("R3=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 24))))) in
  let _ = print_endline ("R4=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 32))))) in
  let _ = print_endline ("R5=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 40))))) in
  let _ = print_endline ("R6=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 48))))) in
  let _ = print_endline ("R7=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 56))))) in
  let _ = print_endline ("R8=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 64))))) in
  let _ = print_endline ("R9=  " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 72))))) in
  let _ = print_endline ("R10= " ^ (string_of_opt_val0 (Mem.loadv Mint64 m (Val.add v (Vint 80))))) in
    print_endline "*******jitted ibpf regs*********"
  
let print_jit_state st =
  let _ = print_endline "****************JIT state***************" in
  let _ = print_string "cur_ins: " in
  let _ = print_endline (print_iBPF_instruction (match ibpf_decode (List64AsArray.index st.ibpf st.pc_loc) with | Some i -> i | None -> BPF BPF_ERR)) in
  let _ = print_endline ("pc:= " ^ string_of_int st.pc_loc) in
  let _ = print_endline ("flag:= " ^ string_of_opt_val0 (Mem.loadv Mint32 st.jit_mem st.flag)) in
  let _ = print_regmap_ptr st.regs_st st.jit_mem in
    print_endline "****************JIT state***************\n\n"
  

    
