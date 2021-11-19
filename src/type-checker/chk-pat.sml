(* chk-pat.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure ChkPat : sig

    (* type check a pattern with respect to the given type.  Return the AST
     * equivalent pattern.
     * Note: the specified type of the pattern might just be a meta variable
     * (e.g., when the pattern is a function parameter), but when it has more
     * information we can check that against the structure of the pattern.
     *)
    val check : Context.t * Types.ty * BindTree.pat -> AST.pat

  end = struct

    structure BT = BindTree
    structure C = Context
    structure Ty = Types
    structure TU = TypeUtil
    structure U = Unify

    (* placeholder pattern for when there is an error *)
    val bogusPat = AST.P_TUPLE[]

    fun check (cxt, ty, BT.PatMark m) = let
          val (cxt, pat) = C.withMark (cxt, m)
          in
            check (cxt, ty, pat)
          end
      | check (cxt, ty, BT.PatVar x) = let
          val x' = Var.new(x, ty)
          in
            IdProps.varSet (x, x');
            AST.P_VAR x'
          end
      | check (cxt, ty, BT.PatCon(dc, NONE)) = let
          val dc' = IdProps.dcon dc
          val resTy = TU.instantiate (DataCon.typeOf dc', C.depthOf cxt)
          in
            if not (U.unify (ty, resTy))
              then (
                C.error (cxt, ["type mismatch"]);
                bogusPat)
              else AST.P_CONST dc'
          end
      | check (cxt, ty, BT.PatCon(dc, SOME p)) = let
          val dc' = IdProps.dcon dc
          val Ty.TyFun(argTy, resTy) =
                TU.instantiate (DataCon.typeOf dc', C.depthOf cxt)
          in
            if not (U.unify (ty, resTy))
              then (
                C.error (cxt, ["type mismatch"]);
                bogusPat)
              else let
                val p' = check (cxt, argTy, p)
                in
                  AST.P_CON(dc', p')
                end
          end
      | check (cxt, ty, BT.PatListCons(p1, p2)) = let
          val dc' = Basis.conCons
          val Ty.TyFun(Ty.TyTuple[elemTy, _], listTy) =
                TU.instantiate (DataCon.typeOf dc', C.depthOf cxt)
          in
            if not (U.unify (ty, listTy))
              then (
                C.error (cxt, ["type mismatch in '::' pattern"]);
                bogusPat)
              else let
                val p1' = check (cxt, elemTy, p1)
                val p2' = check (cxt, listTy, p2)
                in
                  AST.P_CON(dc', AST.P_TUPLE[p1', p2'])
                end
          end
      | check (cxt, ty, BT.PatTuple[]) = (
          if not(U.unify(ty, Basis.tyUnit))
            then C.error(cxt, ["type mismatch"])
            else ();
          AST.P_TUPLE[])
      | check (cxt, ty, BT.PatTuple ps) = let
          val tys = List.map (fn _ => TU.freshMV (C.depthOf cxt)) ps
          in
            if not(U.unify(ty, Ty.TyTuple tys))
              then (
                C.error(cxt, ["type mismatch"]);
                bogusPat)
              else let
                val ps' = ListPair.map (fn (ty, p) => check (cxt, ty, p)) (tys, ps)
                in
                  AST.P_TUPLE ps'
                end
          end
      | check (cxt, ty, BT.PatWild) = AST.P_VAR(Var.wild ty)

  end
