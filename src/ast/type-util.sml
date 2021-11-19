(* type-util.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure TypeUtil : sig

    (* a fresh meta-variable at the given lambda-nesting depth *)
    val freshMV : int -> Types.ty

    (* return the "head-normal form" by pruning an instantiated meta
     * variables.
     *)
    val prune : Types.ty -> Types.ty

    (* apply a type variable to type substitution to a type.  The substitution
     * is represented as a list of type variable/type pairs.
     *)
    val substitute : (Types.ty * (Types.tyvar * Types.ty) list) -> Types.ty

    (* instantiate a type scheme with fresh meta variables at the specified depth *)
    val instantiate : Types.ty_scheme * int -> Types.ty

    (* close a type w.r.t. to a set of non-generic variables (i.e., those
     * variables whose depth is less than or equal to the given depth).
     *)
    val closeTy : (Types.ty * int) -> Types.ty_scheme

    (* return true if two types are equal (or one is an TyError).  This function
     * does not do unification or alpha renaming of meta variables, but it does
     * chase instantiated meta-variable types.
     *)
    val same : (Types.ty * Types.ty) -> bool

    (* return the string representation of a type (for debugging) *)
    val toString : Types.ty -> string

  end = struct

    structure MV = MetaVar
    structure TVMap = TyVar.Map
    structure MVMap = MetaVar.Map
    structure Ty = Types

    fun freshMV depth = Ty.TyMeta(MV.fresh depth)

    fun prune (Ty.TyMeta(Ty.MV{inst as ref(Ty.INST ty), ...})) = let
	  val ty = prune ty
	  in
	    inst := Ty.INST ty;	(* path compression *)
	    ty
	  end
      | prune ty = ty

    (* helper function that applies a variable to type map to a type *)
    fun applySubst (subst, ty) = let
	  fun inst ty = (case prune ty
		 of Ty.TyMeta _ => ty
		  | Ty.TyVar tv => TVMap.lookup(subst, tv)
		  | Ty.TyCon(tyc, args) => Ty.TyCon(tyc, List.map inst args)
		  | Ty.TyFun(ty1, ty2) => Ty.TyFun(inst ty1, inst ty2)
		  | Ty.TyTuple tys => Ty.TyTuple(List.map inst tys)
                  | Ty.TyError => Ty.TyError
		(* end case *))
	  in
	    inst ty
	  end

    fun substitute (ty, []) = ty
      | substitute (ty, s) = applySubst (List.foldl TVMap.insert' TVMap.empty s, ty)

    fun instantiate (([], ty), depth) = ty
      | instantiate ((tvs, ty), depth) = let
	  (* create a substitution from type variables to fresh meta variables *)
	  val subst = List.foldl
		(fn (tv, s) => TVMap.insert(s, tv, freshMV depth))
		  TVMap.empty tvs
	  in
	    applySubst (subst, ty)
	  end

    fun closeTy (ty, depth) = let
          val tvs = ref []
	  (* generate a fresh type variable *)
	  fun newVar () = let
                val tv = TyVar.fresh()
		in
		  tvs := tv :: !tvs;
                  tv
		end
          (* determine the generic variables in the type *)
	  fun genVars (ty, env) = (case prune ty
		 of ty as Ty.TyMeta(mv as Ty.MV{inst=ref(Ty.UNIV d), ...}) =>
		      if (d > depth)
			then (case MVMap.find(env, mv) (* generic variable *)
			   of SOME tv => (env, Ty.TyVar tv)
			    | NONE => let
				val tv = newVar()
				in
				  (MVMap.insert(env, mv, tv), Ty.TyVar tv)
				end
			  (* end case *))
			else (env, ty) (* non-generic variable *)
		  | Ty.TyMeta _ => raise Fail "impossible"
		  | Ty.TyVar _ => raise Fail "unexpected type variable"
		  | Ty.TyCon(tyc, args) => let
		      val (env, tys) = genVarsForTys (args, env)
		      in
			(env, Ty.TyCon(tyc, tys))
		      end
		  | Ty.TyFun(ty1, ty2) => let
		      val (env, ty1) = genVars (ty1, env)
		      val (env, ty2) = genVars (ty2, env)
		      in
			(env, Ty.TyFun(ty1, ty2))
		      end
		  | Ty.TyTuple tys => let
		      val (env, tys) = genVarsForTys (tys, env)
		      in
			(env, Ty.TyTuple tys)
		      end
                  | Ty.TyError => (env, Ty.TyError)
		(* end case *))
	  and genVarsForTys (tys, env) = let
		fun f (ty, (env, tys)) = let
			val (env', ty') = genVars(ty, env)
			in
			  (env', ty'::tys)
			end
		in
		  List.foldr f (env, []) tys
		end
	  val (_, ty) = genVars (ty, MVMap.empty)
	  in
	    (List.rev(!tvs), ty)
	  end

    fun same (ty1, ty2) = (case (prune ty1, prune ty2)
	   of (Ty.TyMeta mv1, Ty.TyMeta mv2) => MV.same(mv1, mv2)
	    | (Ty.TyVar tv1, Ty.TyVar tv2) => TyVar.same(tv1, tv2)
	    | (Ty.TyCon(tyc1, args1), Ty.TyCon(tyc2, args2)) =>
		TyCon.same(tyc1, tyc2) andalso ListPair.allEq same (args1, args2)
	    | (Ty.TyFun(ty11, ty12), Ty.TyFun(ty21, ty22)) =>
		same(ty11, ty21) andalso same(ty21, ty22)
	    | (Ty.TyTuple tys1, Ty.TyTuple tys2) =>
		ListPair.allEq same (tys1, tys2)
            | (Ty.TyError, _) => true
            | (_, Ty.TyError) => true
	    | _ => false
	  (* end case *))

    fun toString (Ty.TyMeta(Ty.MV{id, inst})) = (case !inst
	   of Ty.UNIV d => concat["$", Stamp.toString id, "@", Int.toString d]
	    | Ty.INST ty => (
		inst := Ty.UNIV(~1);
		concat["$", Stamp.toString id, " == ", toString ty]
		  before inst := Ty.INST ty)
	  (* end case *))
      | toString (Ty.TyVar tv) = TyVar.nameOf tv
      | toString (Ty.TyCon(tyc, [])) = TyCon.nameOf tyc
      | toString (Ty.TyCon(tyc, tys)) = concat[
	    TyCon.nameOf tyc, "[", String.concatWithMap "," toString tys, "]"
	  ]
      | toString (Ty.TyFun(ty1, ty2)) = concat[toString ty1, " -> ", toString ty2]
      | toString (Ty.TyTuple tys) = let
          fun paren (ty as Ty.TyFun _) = concat["(", toString ty, ")"]
            | paren (ty as Ty.TyTuple _) = concat["(", toString ty, ")"]
            | paren ty = toString ty
          in
            String.concatWithMap " * " paren tys
          end
      | toString Ty.TyError = "<error>"

  end
