open AST
open BinNums
open Cop
open Csyntax
open Ctypes
open DXModule
open Datatypes
open DumpAsC
open Maps
open ResultMonad
open Values

(** val main : unit **)

let main =
  print_dx_modules
    ((('g'::('e'::('n'::('e'::('r'::('a'::('t'::('e'::('d'::('.'::('c'::[]))))))))))),
    (Ok { dxModuleContent = { prog_defs = (((Coq_xO Coq_xH), (Gfun (External
    ((EF_external
    (('e'::('v'::('a'::('l'::('_'::('i'::('n'::('s'::('_'::('l'::('e'::('n'::[])))))))))))),
    { sig_args = []; sig_res = (Tret AST.Tint); sig_cc = { cc_vararg = None;
    cc_unproto = false; cc_structret = false } })), Tnil, (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))) :: (((Coq_xO (Coq_xO
    Coq_xH)), (Gfun (External ((EF_external
    (('e'::('v'::('a'::('l'::('_'::('i'::('n'::('s'::[])))))))), { sig_args =
    (AST.Tint :: []); sig_res = (Tret AST.Tlong); sig_cc = { cc_vararg =
    None; cc_unproto = false; cc_structret = false } })), (Tcons ((Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))) :: (((Coq_xO (Coq_xI Coq_xH)), (Gfun (Internal { fn_return =
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None }));
    fn_callconv = { cc_vararg = None; cc_unproto = false; cc_structret =
    false }; fn_params = (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: []); fn_vars = []; fn_body = (Sreturn (Some (Ebinop (Oeq,
    (Ebinop (Oeq, (Ecast ((Ebinop (Oshr, (Ebinop (Oand, (Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Eval ((Vlong (Zpos
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI Coq_xH))))))))))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Eval ((Vlong (Zpos (Coq_xO (Coq_xO
    (Coq_xO Coq_xH))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vint Z0), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Ebinop (Oeq, (Ecast
    ((Ebinop (Oshr, (Ebinop (Oand, (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI Coq_xH))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vlong (Zpos (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))))))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Eval ((Vlong (Zpos
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Eval ((Vint (Zpos Coq_xH)), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })))))) }))) :: (((Coq_xO (Coq_xO (Coq_xO Coq_xH))), (Gfun (Internal
    { fn_return = (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })); fn_callconv = { cc_vararg = None; cc_unproto =
    false; cc_structret = false }; fn_params = (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: []); fn_vars = []; fn_body = (Sreturn (Some
    (Ebinop (Oeq, (Ecast ((Ebinop (Oshr, (Ebinop (Oand, (Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Eval ((Vlong (Zpos
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI Coq_xH))))))))))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Eval ((Vlong (Zpos (Coq_xO (Coq_xO
    (Coq_xO Coq_xH))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vint Z0), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))) }))) :: (((Coq_xO
    (Coq_xI (Coq_xO Coq_xH))), (Gfun (Internal { fn_return = (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None }))) :: []);
    fn_vars = []; fn_body = (Sreturn (Some (Ebinop (Ole, (Ecast ((Ebinop
    (Oshr, (Ebinop (Oand, (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Eval ((Vlong (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Eval ((Vlong (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Eval
    ((Vint (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))) }))) :: (((Coq_xO
    (Coq_xO (Coq_xI Coq_xH))), (Gfun (Internal { fn_return = (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None }))) :: []);
    fn_vars = []; fn_body = (Sreturn (Some (Ebinop (Ole, (Ecast ((Ebinop
    (Oshr, (Ebinop (Oand, (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Eval ((Vlong (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI Coq_xH))))))))))))))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Eval ((Vlong (Zpos (Coq_xO (Coq_xO
    (Coq_xI Coq_xH))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vint (Zpos (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))))) }))) :: (((Coq_xO (Coq_xI (Coq_xI Coq_xH))),
    (Gfun (Internal { fn_return = (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })); fn_callconv = { cc_vararg = None;
    cc_unproto = false; cc_structret = false }; fn_params = (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI Coq_xH))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: []))); fn_vars = [];
    fn_body = (Sreturn (Some (Ebinop (Ole, (Ebinop (Oadd, (Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Ebinop (Osub, (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Eval
    ((Vint (Zpos (Coq_xO Coq_xH))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))))) }))) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: []); fn_vars = []; fn_body = (Sreturn (Some (Ebinop (One,
    (Ecast ((Ecast ((Ebinop (Oshr, (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vlong (Zpos (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tlong (Signed, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vlong Z0), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tlong (Signed,
    { attr_volatile = false; attr_alignas = None })))))) }))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))), (Gfun (Internal { fn_return = (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }));
    fn_callconv = { cc_vararg = None; cc_unproto = false; cc_structret =
    false }; fn_params = (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: [])); fn_vars = []; fn_body = (Sreturn (Some
    (Ebinop (Olt, (Ecast ((Ecast ((Ecast ((Ebinop (Oshr, (Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Eval ((Vlong (Zpos
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas =
    None })))))) }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))), (Gfun
    (Internal { fn_return = (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })); fn_callconv = { cc_vararg = None; cc_unproto =
    false; cc_structret = false }; fn_params = (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None }))) :: []); fn_vars = []; fn_body = (Sreturn
    (Some (Ecast ((Ebinop (Oand, (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vlong (Zpos (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))))) }))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Gfun (Internal { fn_return = (Tint
    (I32, Signed, { attr_volatile = false; attr_alignas = None }));
    fn_callconv = { cc_vararg = None; cc_unproto = false; cc_structret =
    false }; fn_params = (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: []); fn_vars = []; fn_body = (Sreturn (Some (Ecast ((Ecast
    ((Ebinop (Oshr, (Ebinop (Oshl, (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vlong (Zpos (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vlong (Zpos (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I16, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))))) }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: [])); fn_vars = []; fn_body = (Sswitch ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (LScons
    ((Some (Zpos (Coq_xO (Coq_xO Coq_xH)))), (Sreturn (Some (Eval ((Vint
    (Zpos Coq_xH)), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))), (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))), (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))),
    (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))), (LScons
    ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))), (LScons ((Some (Zpos (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Sreturn (Some (Eval
    ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))), (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Sreturn (Some (Eval ((Vint (Zpos
    Coq_xH)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))), (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI Coq_xH)))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))),
    (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (LScons ((Some
    (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))))))), (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))),
    (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH))))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))), (LScons
    ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))))), (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))),
    (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH))))))))), (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))), (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))), (Sreturn (Some (Eval ((Vint Z0),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))), (LScons (None, (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))),
    LSnil)))))))))))))))))))))))))))))) }))) :: (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI Coq_xH)))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: [])); fn_vars = (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: []))))))))));
    fn_body = (Sswitch ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Sreturn (Some (Ecall
    ((Evar ((Coq_xO (Coq_xI Coq_xH)), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))), (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH))))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))), (LScons
    ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (LScons (None, (Sreturn (Some (Eval
    ((Vint Z0), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))), LSnil)))))))))))))))))))))))))))) }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI Coq_xH)))), (Gfun (Internal { fn_return = (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: [])); fn_vars = (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: []))))); fn_body =
    (Sswitch ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    Coq_xH)))), (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))), (LScons
    ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), (Sreturn (Some
    (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Sreturn (Some (Eval ((Vint (Zpos
    Coq_xH)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI Coq_xH))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econs
    ((Eval ((Vint (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Eval ((Vint (Zpos (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))), (Sreturn (Some
    (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sreturn
    (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))), (Sreturn (Some
    (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))), (Sreturn (Some
    (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econs
    ((Eval ((Vint (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons (None, (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))),
    LSnil)))))))))))))))))))))))))))))) }))) :: (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI Coq_xH)))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: [])); fn_vars = (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: [])))))))))))); fn_body = (Sswitch ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (LScons
    ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI Coq_xH))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI Coq_xH)))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Ssequence ((Sdo (Eassign
    ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))), (LScons (None, (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))),
    LSnil)))))))))))))))))))))))))))) }))) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))), (Gfun (Internal { fn_return = (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: [])))); fn_vars =
    (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint
    (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: [])))))))))))))))))))))))))); fn_body = (Sswitch ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (LScons ((Some (Zpos (Coq_xI (Coq_xO Coq_xH)))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Ssequence
    ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Ssequence
    ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), Tnil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), Enil)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    Enil)), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons
    ((Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })),
    Tnil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    Enil)), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons
    ((Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })),
    Tnil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    Enil)), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons
    ((Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })),
    Tnil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    Enil)), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons
    ((Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })),
    Tnil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), Tnil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), Enil)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xO Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    Enil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))), (LScons ((Some
    (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xO Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    Enil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))), (LScons (None,
    (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))),
    LSnil)))))))))))))))))))))))))))))))) }))) :: (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))), (Gfun (Internal { fn_return = (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: [])))); fn_vars =
    (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (I32, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: []))))))))))))))))))))))))))))))))); fn_body = (Sswitch
    ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))), (Sreturn (Some
    (Eval ((Vint Z0), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction
    ((Tcons ((Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })), Tnil)), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Ssequence
    ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse
    ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), Tnil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), Enil)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI Coq_xH))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction ((Tcons ((Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))), (Sreturn (Some
    (Eval ((Vint Z0), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse
    ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), Tnil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), Enil)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction
    ((Tcons ((Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })), Tnil)), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Ssequence
    ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse
    ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), Tnil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), Enil)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall
    ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None }))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    Enil)))))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))), (Sreturn (Some
    (Eval ((Vint Z0), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))), (Ssequence ((Sdo (Eassign
    ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse
    ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), Tnil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), Enil)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI Coq_xH))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint
    (I32, Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall
    ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction ((Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tint (I32, Signed, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Tint (I32, Signed, { attr_volatile =
    false; attr_alignas = None }))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse
    ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), Tnil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), Enil)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))), (Tfunction
    ((Tcons ((Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })), Tnil)), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Ssequence
    ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse
    ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI Coq_xH))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })), Tnil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })))), Enil)))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    Enil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sifthenelse ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), Enil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)), (Tint (I32, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))), (Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })))), Enil)),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    Enil)), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))))), (Sifthenelse ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Signed, { attr_volatile = false; attr_alignas = None })), Tnil)))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (I32, Signed, { attr_volatile = false;
    attr_alignas = None })))), Enil)))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))), (LScons (None, (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))),
    LSnil)))))))))))))))))))))))))) }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH))))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: [])); fn_vars = [];
    fn_body = (Sswitch ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (LScons ((Some (Zpos
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), (Sreturn (Some (Eval ((Vint
    (Zpos Coq_xH)), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))), (LScons ((Some (Zpos (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))), (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))))),
    (LScons (None, (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))),
    LSnil)))))))) }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: [])); fn_vars =
    (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: [])))); fn_body = (Sswitch ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (LScons ((Some (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))))), (LScons
    ((Some (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))))), (LScons
    ((Some (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))))), (LScons
    ((Some (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), Tnil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })), { cc_vararg =
    None; cc_unproto = false; cc_structret = false })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))))), (LScons
    (None, (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))),
    LSnil)))))))))))) }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: [])); fn_vars = [];
    fn_body = (Sswitch ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (LScons ((Some (Zpos
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))), (Sreturn
    (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (LScons ((Some
    (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))))),
    (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (LScons ((Some
    (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))))),
    (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (LScons ((Some
    (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))),
    (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (LScons (None,
    (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))))), LSnil)))))))))))) }))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), (Gfun (Internal
    { fn_return = (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })); fn_callconv = { cc_vararg = None; cc_unproto =
    false; cc_structret = false }; fn_params = (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: [])); fn_vars = (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: [])))); fn_body =
    (Sswitch ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (LScons ((Some (Zpos (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sreturn
    (Some (Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo (Eassign
    ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sreturn
    (Some (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo (Eassign
    ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sreturn
    (Some (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))), (Ssequence ((Sdo (Eassign
    ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sreturn
    (Some (Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (LScons (None, (Sreturn (Some (Eval ((Vint
    Z0), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))), LSnil)))))))))))) }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH))))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint
    (I8, Unsigned, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: [])))); fn_vars = (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: []))))))))));
    fn_body = (Sswitch ((Ecast ((Ebinop (Oand, (Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Eval
    ((Vint (Zpos (Coq_xI (Coq_xI Coq_xH)))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (LScons ((Some (Zpos
    (Coq_xI (Coq_xI Coq_xH)))), (Sifthenelse ((Ebinop (Oeq, (Eval ((Vint Z0),
    (Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Ebinop (Oand, (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Eval ((Vint (Zpos (Coq_xO (Coq_xO
    (Coq_xO Coq_xH))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))), (Tfunction
    ((Tcons ((Tint (I8, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))), (Tfunction
    ((Tcons ((Tint (I8, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))), (LScons ((Some (Zpos (Coq_xO (Coq_xO
    Coq_xH)))), (Sifthenelse ((Ebinop (Oeq, (Eval ((Vint Z0), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Ebinop
    (Oand, (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vint (Zpos (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))), (Tfunction
    ((Tcons ((Tint (I8, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))), (Tfunction
    ((Tcons ((Tint (I8, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))), (LScons ((Some (Zpos (Coq_xI (Coq_xO
    Coq_xH)))), (Sifthenelse ((Ebinop (Oeq, (Eval ((Vint Z0), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Ebinop
    (Oand, (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Eval ((Vint (Zpos (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
    (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), (Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), (Tcons ((Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tlong
    (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econs
    ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), (Tcons ((Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })), (Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))))))), (LScons
    ((Some Z0), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))), (Tfunction ((Tcons
    ((Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })), Tnil)))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econs
    ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))))), (LScons
    ((Some (Zpos Coq_xH)), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))),
    (Tfunction ((Tcons ((Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })), (Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (LScons ((Some (Zpos (Coq_xO Coq_xH))),
    (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Ecall ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))), (Tfunction ((Tcons
    ((Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })),
    (Tcons ((Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })), Tnil)))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econs
    ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sreturn (Some (Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))))))), (LScons
    ((Some (Zpos (Coq_xI Coq_xH))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    (Tfunction ((Tcons ((Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })), (Tcons ((Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))))), (Sreturn (Some (Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))), (LScons (None, (Sreturn (Some (Eval ((Vint
    Z0), (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))), LSnil)))))))))))))))))) }))) :: (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), (Gfun (Internal { fn_return = (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: [])); fn_vars =
    (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None }))) :: []))))); fn_body =
    (Sifthenelse ((Ebinop (Oeq, (Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Eval ((Vint Z0),
    (Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Sreturn (Some (Eval ((Vint (Zpos Coq_xH)), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Ebinop (Osub, (Evar ((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Eval
    ((Vint (Zpos Coq_xH)), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO Coq_xH)), (Tfunction ((Tcons ((Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)),
    (Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None }))))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xO
    Coq_xH))), (Tfunction ((Tcons ((Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None })), Tnil)), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None }))))), (Sifthenelse
    ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar ((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tint (I8, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))), (Tfunction
    ((Tcons ((Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })), Tnil)), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None }))))), (Ssequence
    ((Sdo (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))), (Tfunction ((Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint
    (I8, Unsigned, { attr_volatile = false; attr_alignas = None })), (Tcons
    ((Tlong (Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })), { cc_vararg = None; cc_unproto = false;
    cc_structret = false })))), (Econs ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Econs
    ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Econs ((Evar
    ((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), Enil)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None }))))), (Sifthenelse ((Evar ((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool,
    Signed, { attr_volatile = false; attr_alignas = None })))), (Sreturn
    (Some (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    (Tfunction ((Tcons ((Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })), (Tcons ((Tint (I32, Unsigned, { attr_volatile =
    false; attr_alignas = None })), Tnil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })), { cc_vararg = None;
    cc_unproto = false; cc_structret = false })))), (Econs ((Evar ((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    (Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)))), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))), (Sreturn (Some
    (Eval ((Vint Z0), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))))))))) }))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))), (Gfun (Internal { fn_return = (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })); fn_callconv =
    { cc_vararg = None; cc_unproto = false; cc_structret = false };
    fn_params = []; fn_vars = (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None }))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas =
    None }))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false;
    attr_alignas = None }))) :: []))); fn_body = (Ssequence ((Sdo (Eassign
    ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO Coq_xH), (Tfunction
    (Tnil, (Tint (I32, Unsigned, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), Enil, (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None }))))), (Sifthenelse ((Ebinop (Ole, (Eval ((Vint
    (Zpos Coq_xH)), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Sifthenelse ((Ebinop
    (Ole, (Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Ebinop (Odiv, (Eval ((Vint (Zpos (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))))))))))))))))))))))))))), (Tint
    (I32, Unsigned, { attr_volatile = false; attr_alignas = None })))), (Eval
    ((Vint (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), (Ssequence ((Sdo
    (Eassign ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))), (Ecall ((Evar ((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO Coq_xH))))), (Tfunction ((Tcons ((Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })), (Tcons ((Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas = None })), Tnil)))),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas = None })),
    { cc_vararg = None; cc_unproto = false; cc_structret = false })))),
    (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Econs ((Evar ((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))), Enil)))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None })))), (Tint
    (IBool, Signed, { attr_volatile = false; attr_alignas = None }))))),
    (Sifthenelse ((Evar ((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (IBool, Signed, { attr_volatile =
    false; attr_alignas = None })))), (Ssequence ((Sdo (Eassign ((Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Ecall ((Evar ((Coq_xO (Coq_xO Coq_xH)), (Tfunction ((Tcons
    ((Tint (I32, Unsigned, { attr_volatile = false; attr_alignas = None })),
    Tnil)), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })), { cc_vararg = None; cc_unproto = false; cc_structret =
    false })))), (Econs ((Ebinop (Osub, (Evar ((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))), (Tint (I8,
    Unsigned, { attr_volatile = false; attr_alignas = None })))), (Eval
    ((Vint (Zpos Coq_xH)), (Tint (I32, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), (Tint (I8, Unsigned, { attr_volatile = false;
    attr_alignas = None })))), Enil)), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None }))))), (Sreturn (Some (Ebinop (Oeq, (Evar
    ((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))))))), (Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })))), (Eval ((Vlong (Zpos (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))), (Tlong (Unsigned, { attr_volatile =
    false; attr_alignas = None })))))))), (Sreturn (Some (Eval ((Vint Z0),
    (Tint (IBool, Signed, { attr_volatile = false; attr_alignas =
    None })))))))))), (Sreturn (Some (Eval ((Vint Z0), (Tint (IBool, Signed,
    { attr_volatile = false; attr_alignas = None })))))))), (Sreturn (Some
    (Eval ((Vint Z0), (Tint (IBool, Signed, { attr_volatile = false;
    attr_alignas = None })))))))))) }))) :: []))))))))))))))))))))))));
    prog_public = (Coq_xH :: ((Coq_xO Coq_xH) :: ((Coq_xI Coq_xH) :: ((Coq_xO
    (Coq_xO Coq_xH)) :: ((Coq_xI (Coq_xO Coq_xH)) :: ((Coq_xO (Coq_xI
    Coq_xH)) :: ((Coq_xI (Coq_xI Coq_xH)) :: ((Coq_xO (Coq_xO (Coq_xO
    Coq_xH))) :: ((Coq_xI (Coq_xO (Coq_xO Coq_xH))) :: ((Coq_xO (Coq_xI
    (Coq_xO Coq_xH))) :: ((Coq_xI (Coq_xI (Coq_xO Coq_xH))) :: ((Coq_xO
    (Coq_xO (Coq_xI Coq_xH))) :: ((Coq_xI (Coq_xO (Coq_xI
    Coq_xH))) :: ((Coq_xO (Coq_xI (Coq_xI Coq_xH))) :: ((Coq_xI (Coq_xI
    (Coq_xI Coq_xH))) :: ((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))) :: ((Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) :: ((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH)))) :: ((Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))) :: ((Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))) :: ((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI Coq_xH)))) :: []))))))))))))))))))))))));
    prog_main = (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))))))))))))))))))))); prog_types = ((Composite ((Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
    Struct, ((Member_plain ((Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO Coq_xH))))))))))))))))))))))))))))))))))))))))))), (Tint (I32,
    Unsigned, { attr_volatile = false; attr_alignas =
    None })))) :: ((Member_plain ((Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))))))))))))))),
    (Tpointer ((Tlong (Unsigned, { attr_volatile = false; attr_alignas =
    None })), { attr_volatile = false; attr_alignas = None })))) :: [])),
    { attr_volatile = false; attr_alignas = None })) :: []); prog_comp_env =
    (PTree.Nodes (PTree.Node001 (PTree.Node001 (PTree.Node001 (PTree.Node001
    (PTree.Node001 (PTree.Node001 (PTree.Node100 (PTree.Node100
    (PTree.Node001 (PTree.Node001 (PTree.Node001 (PTree.Node100
    (PTree.Node100 (PTree.Node001 (PTree.Node001 (PTree.Node100
    (PTree.Node001 (PTree.Node001 (PTree.Node100 (PTree.Node100
    (PTree.Node001 (PTree.Node100 (PTree.Node100 (PTree.Node001
    (PTree.Node100 (PTree.Node001 (PTree.Node001 (PTree.Node001
    (PTree.Node001 (PTree.Node100 (PTree.Node100 (PTree.Node100
    (PTree.Node001 (PTree.Node100 (PTree.Node100 (PTree.Node001
    (PTree.Node100 (PTree.Node100 (PTree.Node001 (PTree.Node001
    (PTree.Node001 (PTree.Node100 (PTree.Node100 (PTree.Node001
    (PTree.Node001 (PTree.Node100 (PTree.Node001 (PTree.Node001
    (PTree.Node100 (PTree.Node100 (PTree.Node001 (PTree.Node001
    (PTree.Node001 (PTree.Node001 (PTree.Node001 (PTree.Node100
    (PTree.Node100 (PTree.Node001 (PTree.Node001 (PTree.Node001
    (PTree.Node100 (PTree.Node001 (PTree.Node100 (PTree.Node001
    (PTree.Node001 (PTree.Node001 (PTree.Node100 (PTree.Node100
    (PTree.Node001 (PTree.Node100 (PTree.Node001 (PTree.Node100
    (PTree.Node100 (PTree.Node001 (PTree.Node100 (PTree.Node001
    (PTree.Node001 (PTree.Node001 (PTree.Node100 (PTree.Node100
    (PTree.Node001 (PTree.Node001 (PTree.Node001 (PTree.Node100
    (PTree.Node100 (PTree.Node010 { co_su = Struct; co_members =
    ((Member_plain ((Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))))))))))))))))))))))))))))))))))))))), (Tint (I32, Unsigned,
    { attr_volatile = false; attr_alignas = None })))) :: ((Member_plain
    ((Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO Coq_xH))))))))))))))))))), (Tpointer ((Tlong (Unsigned,
    { attr_volatile = false; attr_alignas = None })), { attr_volatile =
    false; attr_alignas = None })))) :: [])); co_attr = { attr_volatile =
    false; attr_alignas = None }; co_sizeof = (Zpos (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))); co_alignof = (Zpos (Coq_xO (Coq_xO Coq_xH))); co_rank =
    O }))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) };
    dxModuleNames = (((Coq_xO Coq_xH),
    ('e'::('v'::('a'::('l'::('_'::('i'::('n'::('s'::('_'::('l'::('e'::('n'::[]))))))))))))) :: (((Coq_xO
    (Coq_xO Coq_xH)),
    ('e'::('v'::('a'::('l'::('_'::('i'::('n'::('s'::[]))))))))) :: (((Coq_xO
    (Coq_xI Coq_xH)),
    ('i'::('s'::('_'::('c'::('o'::('m'::('p'::('c'::('e'::('r'::('t'::('_'::('u'::('d'::('i'::('v'::[]))))))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xO Coq_xH))),
    ('i'::('s'::('_'::('d'::('s'::('t'::('_'::('R'::('0'::[])))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xO Coq_xH))),
    ('i'::('s'::('_'::('w'::('e'::('l'::('l'::('_'::('d'::('s'::('t'::[])))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xI Coq_xH))),
    ('i'::('s'::('_'::('w'::('e'::('l'::('l'::('_'::('s'::('r'::('c'::[])))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xI Coq_xH))),
    ('i'::('s'::('_'::('w'::('e'::('l'::('l'::('_'::('j'::('u'::('m'::('p'::[]))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))),
    ('i'::('s'::('_'::('n'::('o'::('t'::('_'::('d'::('i'::('v'::('_'::('b'::('y'::('_'::('z'::('e'::('r'::('o'::('6'::('4'::[]))))))))))))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))),
    ('i'::('s'::('_'::('s'::('h'::('i'::('f'::('t'::('_'::('r'::('a'::('n'::('g'::('e'::('6'::('4'::[]))))))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))),
    ('g'::('e'::('t'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::[]))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))),
    ('g'::('e'::('t'::('_'::('o'::('f'::('f'::('s'::('e'::('t'::[]))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI Coq_xH)))),
    ('j'::('i'::('t'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('a'::('l'::('u'::('3'::('2'::('_'::('i'::('m'::('m'::[])))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))),
    ('j'::('i'::('t'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('a'::('l'::('u'::('3'::('2'::('_'::('r'::('e'::('g'::[])))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI Coq_xH)))),
    ('b'::('p'::('f'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('a'::('l'::('u'::('6'::('4'::('_'::('i'::('m'::('m'::[])))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI Coq_xH)))),
    ('b'::('p'::('f'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('a'::('l'::('u'::('6'::('4'::('_'::('r'::('e'::('g'::[])))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
    ('b'::('p'::('f'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('b'::('r'::('a'::('n'::('c'::('h'::('_'::('i'::('m'::('m'::[]))))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
    ('b'::('p'::('f'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('b'::('r'::('a'::('n'::('c'::('h'::('_'::('r'::('e'::('g'::[]))))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))),
    ('b'::('p'::('f'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('l'::('o'::('a'::('d'::('_'::('i'::('m'::('m'::[]))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))),
    ('b'::('p'::('f'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('l'::('o'::('a'::('d'::('_'::('r'::('e'::('g'::[]))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    ('b'::('p'::('f'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('s'::('t'::('o'::('r'::('e'::('_'::('i'::('m'::('m'::[])))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))),
    ('b'::('p'::('f'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('o'::('p'::('c'::('o'::('d'::('e'::('_'::('s'::('t'::('o'::('r'::('e'::('_'::('r'::('e'::('g'::[])))))))))))))))))))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    ('j'::('i'::('t'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('a'::('u'::('x'::('2'::[])))))))))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))),
    ('j'::('i'::('t'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('a'::('u'::('x'::[]))))))))))))))))) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    ('j'::('i'::('t'::('_'::('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::[]))))))))))))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))), ('i'::[])) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    Coq_xH))))), ('i'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))), ('i'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))), ('p'::('c'::[]))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI Coq_xH))))), ('l'::('e'::('n'::[])))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))), ('i'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))), ('i'::[])) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))),
    ('u'::('p'::('p'::('e'::('r'::[])))))) :: (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))), ('i'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO Coq_xH)))))), ('o'::('p'::[]))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))), ('o'::('p'::[]))) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))), ('b'::[])) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))),
    ('o'::('p'::[]))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    Coq_xH)))))), ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI Coq_xH)))))), ('o'::('p'::[]))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))), ('b'::[])) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    ('p'::('c'::[]))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH))))))), ('l'::('e'::('n'::[])))) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    ('o'::('p'::[]))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH))))))), ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))), ('b'::[])) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    Coq_xH))))))), ('p'::('c'::[]))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))),
    ('l'::('e'::('n'::[])))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), ('o'::('p'::[]))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), ('o'::('f'::('s'::[])))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    ('b'::('0'::[]))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI Coq_xH))))))), ('b'::('0'::[]))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))))), ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))),
    ('b'::('0'::[]))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI Coq_xH))))))), ('b'::('0'::[]))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    Coq_xH))))))), ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))),
    ('b'::('0'::[]))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI Coq_xH))))))), ('b'::('0'::[]))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    Coq_xH))))))), ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))),
    ('b'::('0'::[]))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI Coq_xH))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), ('b'::('0'::[]))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    ('b'::('0'::[]))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))), ('b'::('0'::[]))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))), ('o'::('f'::('s'::[])))) :: (((Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    ('b'::('0'::[]))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    ('o'::('p'::[]))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))))), ('i'::('n'::('s'::[])))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    ('o'::('p'::[]))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO Coq_xH)))))))), ('i'::('n'::('s'::[])))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), ('o'::('p'::[]))) :: (((Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), ('o'::('p'::[]))) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    ('p'::('c'::[]))) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH)))))))), ('l'::('e'::('n'::[])))) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    ('o'::('p'::[]))) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO Coq_xH)))))))), ('i'::('n'::('s'::[])))) :: (((Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    ('b'::[])) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO Coq_xH)))))))), ('p'::('c'::[]))) :: (((Coq_xO (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    ('l'::('e'::('n'::[])))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), ('n'::[])) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    ('i'::('n'::('s'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))))),
    ('o'::('p'::[]))) :: (((Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO Coq_xH)))))))), ('b'::('0'::[]))) :: (((Coq_xO (Coq_xO
    (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))),
    ('l'::('e'::('n'::[])))) :: (((Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))), ('b'::[])) :: (((Coq_xO (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))),
    ('i'::('n'::('s'::('6'::('4'::[])))))) :: (((Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))))))))))))))))),
    ('m'::('a'::('i'::('n'::[]))))) :: (((Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI
    (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    Coq_xH))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
    ('v'::('e'::('r'::('i'::('f'::('i'::('e'::('r'::('_'::('s'::('t'::('a'::('t'::('e'::[]))))))))))))))) :: (((Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI
    (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH))))))))))))))))))))))))))))))))))))))))))),
    ('i'::('n'::('s'::('_'::('l'::('e'::('n'::[])))))))) :: (((Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO
    Coq_xH))))))))))))))))))),
    ('i'::('n'::('s'::[])))) :: [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) })) :: [])
