(* exp.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Utility code for dealing with expressions in the AST IR.
 *)

structure Exp : sig

    val typeOf : AST.exp -> Types.ty

    val repOf : AST.exp -> AST.exp_rep

    (* "smart" constructors that figure out the type of the expression *)
    val mkLET : AST.bind * AST.exp -> AST.exp
    val mkVAL : AST.pat * AST.exp * AST.exp -> AST.exp
    val mkFUN : Var.t * AST.pat list * AST.exp * AST.exp -> AST.exp
    val mkCOND : AST.exp * AST.exp * AST.exp -> AST.exp
    val mkCASE : AST.exp * (AST.pat * AST.exp) list -> AST.exp
    val mkTUPLE : AST.exp list -> AST.exp
    val mkINT : IntInf.int -> AST.exp
    val mkSTR : string -> AST.exp

    val mkBoolConst : bool -> AST.exp

  end = struct

    datatype exp = datatype AST.exp
    datatype exp_rep = datatype AST.exp_rep

    fun typeOf (E(_, ty)) = ty
    fun repOf (E(rep, _)) = rep

    fun mkLET (bind, e) = E(E_LET(bind, e), typeOf e)
    fun mkVAL (x, e1, e2) = mkLET(AST.B_VAL(x, e1), e2)
    fun mkFUN (f, params, body, e) = mkLET(AST.B_FUN(f, params, body), e)

    fun mkCOND (e1, e2, e3) = E(E_COND(e1, e2, e3), typeOf e2)

    fun mkCASE (_, []) = raise Fail "empty case"
      | mkCASE (arg, rules as (_, e)::_) = E(E_CASE(arg, rules), typeOf e)

    fun mkTUPLE [] = E(E_TUPLE[], Basis.tyUnit)
      | mkTUPLE [e] = e
      | mkTUPLE es = E(E_TUPLE es, Types.TyTuple(List.map typeOf es))

    fun mkINT n = E(E_CONST(AST.C_INT n), Basis.tyInt)
    fun mkSTR s = E(E_CONST(AST.C_STR s), Basis.tyString)

    fun mkBoolConst b = E(
          E_CONST(AST.C_DCON(if b then Basis.conTrue else Basis.conFalse)),
          Basis.tyBool)

  end
