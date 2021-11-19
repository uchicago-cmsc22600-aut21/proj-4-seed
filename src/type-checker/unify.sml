(* unify.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Destructive unification of type terms.
 *)

structure Unify : sig

    (* destructively unify two types; returns `true` if successful and `false` otherwise. *)
    val unify : Types.ty * Types.ty -> bool

  end = struct

    structure Ty = Types
    structure MV = MetaVar
    structure TU = TypeUtil

    (* does a meta-variable occur in a type? *)
    fun occursIn (mv, ty) = let
	  fun occurs ty = (case TU.prune ty
		 of (Ty.TyMeta mv') => MV.same(mv, mv')
		  | (Ty.TyVar _) => raise Fail "unexpected bound type variable"
		  | (Ty.TyCon(_, args)) => List.exists occurs args
		  | (Ty.TyFun(ty1, ty2)) => occurs ty1 orelse occurs ty2
		  | (Ty.TyTuple tys) => List.exists occurs tys
                  | Ty.TyError => false
		(* end case *))
	  in
	    occurs ty
	  end

    (* adjust the depth of any non-instantiated meta-variable that is bound
     * deeper than the given depth.
     *)
    fun adjustDepth (ty, depth) = let
	  fun adjust (Ty.TyMeta(Ty.MV{inst as ref(Ty.UNIV d), ...})) =
		if (depth < d) then inst := Ty.UNIV depth else ()
	    | adjust (Ty.TyMeta(Ty.MV{inst=ref(Ty.INST ty), ...})) = adjust ty
	    | adjust (Ty.TyVar _) = raise Fail "unexpected bound type variable"
	    | adjust (Ty.TyCon(_, args)) = List.app adjust args
	    | adjust (Ty.TyFun(ty1, ty2)) = (adjust ty1; adjust ty2)
	    | adjust (Ty.TyTuple tys) = List.app adjust tys
            | adjust Ty.TyError = ()
	  in
	    adjust ty
	  end

    (* destructively unify two types *)
    fun unify (ty1, ty2) = (case (TU.prune ty1, TU.prune ty2)
	   of (Ty.TyMeta mv1, Ty.TyMeta mv2) => (
		if MV.same(mv1, mv2) then ()
		else if MV.isDeeper(mv1, mv2)
		  then MV.instantiate(mv1, ty2)
		  else MV.instantiate(mv2, ty1);
		true)
	    | (Ty.TyMeta mv1, ty2) => unifyWithMV (ty2, mv1)
	    | (ty1, Ty.TyMeta mv2) => unifyWithMV (ty1, mv2)
	    | (Ty.TyCon(tyc1, tys1), Ty.TyCon(tyc2, tys2)) =>
		(TyCon.same(tyc1, tyc2)) andalso ListPair.allEq unify (tys1, tys2)
	    | (Ty.TyFun(ty11, ty12), Ty.TyFun(ty21, ty22)) =>
		unify(ty11, ty21) andalso unify(ty12, ty22)
	    | (Ty.TyTuple tys1, Ty.TyTuple tys2) =>
		ListPair.allEq unify (tys1, tys2)
	    | _ => false
	  (* end case *))

    (* unify a type with an uninstantiated meta-variable *)
    and unifyWithMV (ty, mv as Ty.MV{inst=ref(Ty.UNIV d), ...}) =
	  if (occursIn(mv, ty))
	    then false
	    else (adjustDepth(ty, d); MV.instantiate(mv, ty); true)
      | unifyWithMV _ = raise Fail "impossible"

  end
