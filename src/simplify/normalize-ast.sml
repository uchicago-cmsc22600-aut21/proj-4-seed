(* normalize-ast.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * This structure implements a pass that cleans up the AST IR prior to simplification.
 * The resulting AST will have the following invariants:
 *      - data-constructor functions will always be applied to arguments
 *      - built-in functions will always be applied to arguments
 *      - function bindings will involve only one level of parameters (i.e., curried
 *        bindings will have been converted to nested functions) and the function
 *        parameter will be a single variable
 *      - P_CON and P_TUPLE will have variables (P_VAR) as arguments
 *)

structure NormalizeAST : sig

    val transform : AST.program -> AST.program

  end = struct

    structure Ty = Types
    structure DC = DataCon

    (* given a data constructor that appears in an argument position, convert it to
     * a function abstraction (if necessary).
     *)
    fun cvtCon (dc, ty) = (case TypeUtil.prune ty
           of ty as Ty.TyFun(argTy, resTy) => let
                val f = Var.newTmp(DC.nameOf dc, ty)
                val x = Var.newTmp("x", argTy)
                in
                  Exp.mkLET(
                    AST.B_FUN(f, [AST.P_VAR x],
                      AST.E(AST.E_APPLY(
                        AST.E(AST.E_CONST(AST.C_DCON dc), ty),
                        AST.E(AST.E_VAR x, argTy)), resTy)),
                    AST.E(AST.E_VAR f, ty))
                end
            | Ty.TyCon _ => AST.E(AST.E_CONST(AST.C_DCON dc), ty)
            | _ => raise Fail "bogus type for data constructor"
          (* end case *))

    (* given a built-in function that is being used in an argument position,
     * convert it to a function abstraction.
     *)
    fun cvtPrim (prim, ty) = (case TypeUtil.prune ty
           of ty as Ty.TyFun(argTy, resTy) => let
                val f = Var.newTmp(Var.nameOf prim, ty)
                val x = Var.newTmp("x", argTy)
                in
                  Exp.mkLET(
                    AST.B_FUN(f, [AST.P_VAR x],
                      AST.E(AST.E_APPLY(
                        AST.E(AST.E_VAR prim, ty),
                        AST.E(AST.E_VAR x, argTy)), resTy)),
                    AST.E(AST.E_VAR f, ty))
                end
            | _ => AST.E(AST.E_VAR prim, ty)
          (* end case *))

    (* transform the program *)
    fun transform (AST.PROG{datatycs, body}) = let
          fun xformExp (exp as AST.E(rep, ty)) = let
                fun mk rep' = AST.E(rep', ty)
                in
                  case rep
                   of AST.E_LET(bnd, e) =>
                        mk (AST.E_LET(xformBind bnd, xformExp e))
                    | AST.E_COND(e1, e2, e3) =>
                        mk (AST.E_COND(xformExp e1, xformExp e2, xformExp e3))
                    | AST.E_CASE(e, rules) =>
                        mk (AST.E_CASE(xformExp e, List.map xformRule rules))
                    | AST.E_APPLY(e1 as AST.E(AST.E_CONST _, _), e2) =>
                        (* application of a constructor, so do not transform lhs *)
                        mk (AST.E_APPLY(e1, xformExp e2))
                    | AST.E_APPLY(e1 as AST.E(AST.E_VAR f, _), e2) =>
                        (* application of a variable, so do not transform lhs *)
                        mk (AST.E_APPLY(e1, xformExp e2))
                    | AST.E_APPLY(e1, e2) =>
                        mk (AST.E_APPLY(xformExp e1, xformExp e2))
                    | AST.E_TUPLE es => mk (AST.E_TUPLE(List.map xformExp es))
                    | AST.E_CONST(AST.C_DCON dc) => cvtCon (dc, ty)
                    | AST.E_CONST _ => exp
                    | AST.E_VAR x => if Var.isPrim x
                        then cvtPrim (x, ty)
                        else exp
                  (* end case *)
                end
          and xformBind (AST.B_VAL(p, e)) = AST.B_VAL(p, xformExp e)
            | xformBind (AST.B_FUN(f, p1::ps, body)) = let
                val name = Var.nameOf f
                val (_, Ty.TyFun(_, resTy)) = Var.typeOf f
                (* make a function binding of a single variable pattern
                 * for the parameters
                 *)
                fun mkFunBind (f, p, e) = let
                      val (p', e') = (case p
                             of AST.P_TUPLE ps => let
                                  val (x, e') = xformTuplePat (ps, e)
                                  in
                                    (AST.P_VAR x, e')
                                  end
                              | AST.P_VAR _ => (p, e)
                              | _ => raise Fail "ill formed parameter pattern"
                            (* end case *))
                      in
                        AST.B_FUN(f, [p'], e')
                      end
                (* convert curried bindings to nested function definitions *)
                fun nestCurriedFn (_, [], _) = xformExp body
                  | nestCurriedFn (i, p::ps, ty as Ty.TyFun(_, resTy)) = let
                      val f' = Var.newTmp(name ^ Int.toString i, ty)
                      in
                        Exp.mkLET(
                          mkFunBind(f', p, nestCurriedFn (i+1, ps, resTy)),
                          AST.E(AST.E_VAR f', ty))
                      end
                  | nestCurriedFn _ = raise Fail "badly formed function binding"
                in
                  mkFunBind(f, p1, nestCurriedFn (1, ps, resTy))
                end
            | xformBind _ = raise Fail "ill-formed binding"
          (* the '::' syntax leads to nesting of patterns, which we flatten here *)
          and xformRule (AST.P_CON(dc, AST.P_TUPLE ps), e) = let
                val (tpl, e') = xformTuplePat (ps, xformExp e)
                in
                  (AST.P_CON(dc, AST.P_VAR tpl), e')
                end
            | xformRule (p, e) = (p, xformExp e)
          and xformTuplePat (ps, e) = let
                (* arguments to tuple patterns are always variables *)
                val ty = Ty.TyTuple(List.map (fn AST.P_VAR x => Var.monoTypeOf x) ps)
                val tpl = Var.newTmp("tpl", ty)
                val e' = Exp.mkCASE(AST.E(AST.E_VAR tpl, ty), [
                        (AST.P_TUPLE ps, e)
                      ])
                in
                  (tpl, e')
                end
          in
            AST.PROG{datatycs=datatycs, body=xformExp body}
          end

  end
