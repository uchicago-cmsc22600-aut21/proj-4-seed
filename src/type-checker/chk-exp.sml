(* chk-exp.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure ChkExp : sig

    (* convert a binding-tree expression to an AST expression, while checkinga
     * that it is well typed.s
     *)
    val check : Context.t * BindTree.exp -> AST.exp

    (* type check a value binding while converting it to AST *)
    val chkValBind : Context.t * BindTree.bind -> AST.bind

  end = struct

    structure BT = BindTree
    structure C = Context
    structure Cov = Coverage
    structure Ty = Types
    structure TU = TypeUtil
    structure U = Unify

    (* an expression/type pair for when there is an error *)
    val bogusExp = AST.E(AST.E_TUPLE[], Ty.TyError)

    fun check (cxt, exp) = (case exp
           of BT.ExpMark m => check (C.withMark (cxt, m))
            | BT.ExpIf(e1, e2, e3) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
		val e3' = check (cxt, e3)
		in
		  if not(U.unify(Exp.typeOf e1', Basis.tyBool))
		    then C.error (cxt, ["type of conditional not bool"])
		    else ();
		  if not(U.unify(Exp.typeOf e2', Exp.typeOf e3'))
		    then (
		      C.error (cxt, ["types of then and else clauses must match"]);
		      bogusExp)
		    else Exp.mkCOND(e1', e2', e3')
		end
            | BT.ExpOrElse(e1, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
		in
		  if not(U.unify(Exp.typeOf e1', Basis.tyBool) andalso
                         U.unify(Exp.typeOf e2', Basis.tyBool))
		    then C.error (cxt, ["arguments of `||` must have type bool"])
		    else ();
		  Exp.mkCOND(e1', Exp.mkBoolConst true, e2')
		end
            | BT.ExpAndAlso(e1, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
		in
		  if not(U.unify(Exp.typeOf e1', Basis.tyBool) andalso
                         U.unify(Exp.typeOf e2', Basis.tyBool))
		    then C.error (cxt, ["arguments of `&&` must have type bool"])
		    else ();
		  Exp.mkCOND(e1', e2', Exp.mkBoolConst false)
		end
            | BT.ExpBin(e1, oper, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
                val oper' = IdProps.var oper
                (* the operator's type *)
                val ty as Ty.TyFun(argTy, resTy) =
                      TU.instantiate (Var.typeOf oper', C.depthOf cxt)
                in
                  if not(U.unify(argTy, Ty.TyTuple[Exp.typeOf e1', Exp.typeOf e2']))
                    then C.error (cxt, [
                        "type mismatch for operator '",
                        BindTree.VarId.nameOf oper, "'"
                      ])
                    else ();
                  AST.E(AST.E_APPLY(
                    AST.E(AST.E_VAR oper', ty), Exp.mkTUPLE[e1', e2']),
                    resTy)
                end
            | BT.ExpListCons(e1, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
                val tplTy = Ty.TyTuple[Exp.typeOf e1', Exp.typeOf e2']
                val oper' = Basis.conCons
                val ty as Ty.TyFun(argTy, resTy) =
                      TU.instantiate (DataCon.typeOf oper', C.depthOf cxt)
                in
                  if not(U.unify(argTy, Ty.TyTuple[Exp.typeOf e1', Exp.typeOf e2']))
                    then C.error (cxt, ["type mismatch for operator '::'"])
                    else ();
                  AST.E(AST.E_APPLY(
                      AST.E(AST.E_CONST(AST.C_DCON oper'), ty),
                      Exp.mkTUPLE[e1', e2']),
                    resTy)
                end
            | BT.ExpUn(oper, e) => let
		val e' = check (cxt, e)
                val oper' = IdProps.var oper
                (* the operator's type *)
                val ty as Ty.TyFun(argTy, resTy) =
                      TU.instantiate (Var.typeOf oper', C.depthOf cxt)
                in
                  if not(U.unify(argTy, Exp.typeOf e'))
                    then C.error (cxt, [
                        "type mismatch for operator '",
                        BindTree.VarId.nameOf oper, "'"
                      ])
                    else ();
                  AST.E(AST.E_APPLY(AST.E(AST.E_VAR oper', ty), e'), resTy)
                end
            | BT.ExpApp(e1, e2) => let
		val e1' = check (cxt, e1)
		val e2' = check (cxt, e2)
		val resTy = TU.freshMV (C.depthOf cxt)
		in
		  if not(U.unify(Exp.typeOf e1', Ty.TyFun(Exp.typeOf e2', resTy)))
		    then C.error (cxt, ["type mismatch in application"])
		    else ();
		  AST.E(AST.E_APPLY(e1', e2'), resTy)
		end
            | BT.ExpVar x => let
                val x' = IdProps.var x
                val ty = TU.instantiate (Var.typeOf x', C.depthOf cxt)
                in
                  AST.E(AST.E_VAR x', ty)
                end
            | BT.ExpCon dc => let
                val dc' = IdProps.dcon dc
                val ty = TU.instantiate (DataCon.typeOf dc', C.depthOf cxt)
                in
                  AST.E(AST.E_CONST(AST.C_DCON dc'), ty)
                end
            | BT.ExpInt n =>
              (* integer literals are always positive, but they have to be less
               * than 2^62.
               *)
                if (n < 0x4000000000000000)
                  then Exp.mkINT n
                  else (
                    C.error (cxt, [
                        "integer literal ", IntInf.toString n, " too large"
                      ]);
                    bogusExp)
            | BT.ExpStr s => Exp.mkSTR s
            | BT.ExpTuple es =>	Exp.mkTUPLE (List.map (fn e => check(cxt, e)) es)
            | BT.ExpCase(e, rules) => let
                val e' = check (cxt, e)
                val rules' = chkRules (cxt, Exp.typeOf e', rules)
                in
                  Exp.mkCASE(e', rules')
                end
            | BT.ExpScope scope => chkScope (cxt, scope)
          (* end case *))

    (* typecheck a list of case-expression rules (including coverage checking) *)
    and chkRules (cxt, argTy, rules) = let
          val resTy = TU.freshMV (C.depthOf cxt)
          fun chkRule (cxt, BT.RuleMark m) = chkRule (C.withMark (cxt, m))
            | chkRule (cxt, BT.RuleCase(p, scope)) = let
                val p' = ChkPat.check (cxt, argTy, p)
                val e' = chkScope (cxt, scope)
                in
                  (* check that the result type of the rule is compatible with
                   * the previous rules.
                   *)
                  if not (U.unify (Exp.typeOf e', resTy))
                    then C.error (cxt, ["inconsistent result type for case rule"])
                    else ();
                  (C.spanOf cxt, (p', chkScope (cxt, scope)))
                end
          val rules' = List.map (fn r => chkRule (cxt, r)) rules
          (* once we have done the type checking, we can check for coverage *)
          fun chkCover ([], rules', cover) = (
                if not(Cov.exhaustive cover)
                  then C.error(cxt, ["non-exhaustive case expression"])
                  else ();
                List.rev rules')
            | chkCover ((span, (p, e)) :: rules, rules', cover) = let
                val (cover, isRedundant) = Cov.update(cover, p)
                in
                  if isRedundant
                    then C.error(C.setSpan(cxt, span), ["redundant pattern in case"])
                    else ();
                  chkCover (rules, (p, e)::rules', cover)
                end
          in
            chkCover (rules', [], Cov.init (TU.prune argTy))
          end

    (* typecheck a scope *)
    and chkScope (cxt, (bnds, e)) = let
          fun chk [] = check (cxt, e)
            | chk (bnd::bndr) = let
                val bnd' = chkValBind (cxt, bnd)
                in
                  Exp.mkLET(bnd', chk bndr)
                end
          in
            chk bnds
          end

    and chkValBind (cxt, BT.BindMark m) = chkValBind (C.withMark (cxt, m))
      | chkValBind (cxt, BT.BindFun(f, params, body)) = let
          val cxt' = C.incDepth cxt
          val resTy = TU.freshMV (C.depthOf cxt')
          (* check a parameter pattern; this function is designed to be applied
           * the the function parameters in right-to-left order.
           *)
          fun chkParam (p, (ps', resTy)) = let
                val paramTy = TU.freshMV (C.depthOf cxt')
                val p' = ChkPat.check (cxt', paramTy, p)
                in
                  (p'::ps', Ty.TyFun(TU.prune paramTy, resTy))
                end
          val (params', fnTy) = List.foldr chkParam ([], resTy) params
          val f' = Var.new (f, fnTy)
          val _ = IdProps.varSet (f, f')
          val body' = check (cxt', body)
          in
            if not (U.unify(resTy, Exp.typeOf body'))
              then C.error (cxt, ["type mismatch in function body"])
              else Var.updateTy (f', TU.closeTy (fnTy, C.depthOf cxt));
            AST.B_FUN(f', params', body')
          end
      | chkValBind (cxt, BT.BindVal(lhs, rhs)) = let
          val lhsTy = TU.freshMV (C.depthOf cxt)
          val lhs' = ChkPat.check (cxt, lhsTy, lhs)
          val rhs' = check (cxt, rhs)
          in
            AST.B_VAL(lhs', rhs')
          end
      | chkValBind (cxt, BT.BindExp e) = let
          val e' = check (cxt, e)
          in
            if not (U.unify (Exp.typeOf e', Basis.tyUnit))
              then C.error (cxt, ["expected 'Unit' type for expression"])
              else ();
            AST.B_VAL(AST.P_TUPLE[], e')
          end

  end
