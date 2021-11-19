(* basis.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Type constructors, data constructors, and variables defined in the ML Lite Basis.
 *)

structure Basis : sig

  (* predefined type constructors *)
    val tycBool : TyCon.t
    val tycInt : TyCon.t
    val tycList : TyCon.t
    val tycRef : TyCon.t
    val tycString : TyCon.t
    val tycUnit : TyCon.t

  (* predefined types *)
    val tyBool : Types.ty
    val tyInt : Types.ty
    val tyList : Types.ty -> Types.ty
    val tyRef : Types.ty -> Types.ty
    val tyString : Types.ty
    val tyUnit : Types.ty

  (* pre-defined data constructors *)
    val conTrue : DataCon.t
    val conFalse : DataCon.t
    val conCons : DataCon.t
    val conNil : DataCon.t

  (* operators *)
    val opASSIGN : Var.t        (* assignment *)
    val opEQ : Var.t            (* integer equality relation *)
    val opNEQ : Var.t           (* integer inequality relation *)
    val opLTE : Var.t           (* integer less-than-or-equal relation *)
    val opLT : Var.t            (* integer less-than relation *)
    val opCONCAT : Var.t        (* string concatenation *)
    val opADD : Var.t           (* integer addition *)
    val opSUB : Var.t           (* integer subtraction *)
    val opMUL : Var.t           (* integer multiplication *)
    val opDIV : Var.t           (* integer division *)
    val opMOD : Var.t           (* integer remainder *)
    val opNEG : Var.t           (* unary integer negation *)
    val opDEREF : Var.t         (* dereference *)

  (* predefined variables *)
    val varArguments : Var.t
    val varChr : Var.t
    val varExit : Var.t
    val varFail : Var.t
    val varNewRef : Var.t
    val varPrint : Var.t
    val varSize : Var.t
    val varSub : Var.t

  end = struct

    structure BB = BindBasis
    structure BT = BindTree
    structure Ty = Types

    (* helper for creating polymorphic type schemes *)
    fun tyScheme mk = let
          val tv = TyVar.new(BT.TyVar.new(Atom.atom "t"))
          in
            ([tv], mk (Ty.TyVar tv))
          end
    (* monomorphic type scheme *)
    fun monoScheme ty = ([], ty)

    (* type variables used in the definition of the List and Ref types. *)
    val listTyParam  = TyVar.new(BT.TyVar.new(Atom.atom "t"))
    val listTyParam' = Ty.TyVar listTyParam
    val refTyParam = TyVar.new(BT.TyVar.new(Atom.atom "t"))

    (* predefined type constructors *)
    val tycBool = TyCon.new(BB.tycBool, [])
    val tycInt = TyCon.new(BB.tycInt, [])
    val tycList = TyCon.new(BB.tycList, [listTyParam])
    val tycRef = TyCon.new(BB.tycRef, [refTyParam])
    val tycString = TyCon.new(BB.tycString, [])
    val tycUnit = TyCon.new(BB.tycUnit, [])

    (* predefined types *)
    val tyBool = Ty.TyCon(tycBool, [])
    val tyInt = Ty.TyCon(tycInt, [])
    fun tyList ty = Ty.TyCon(tycList, [ty])
    fun tyRef ty = Ty.TyCon(tycRef, [ty])
    val tyString = Ty.TyCon(tycString, [])
    val tyUnit = Ty.TyCon(tycUnit, [])

  (* pre-defined data constructors *)
    val conTrue = DataCon.new (tycBool, BB.conTrue, NONE)
    val conFalse = DataCon.new (tycBool, BB.conFalse, NONE)
    val conCons = DataCon.new (
          tycList,
          BB.conCons,
          SOME(Ty.TyTuple[listTyParam', tyList listTyParam']))
    val conNil = DataCon.new (tycList, BB.conNil, NONE)

    (* finish predefined data-type constructors *)
    val _ = (TyCon.finish tycBool; TyCon.finish tycList; TyCon.finish tycUnit)

    (* operators *)
    local
      fun pairTy (ty1, ty2) = Ty.TyTuple[ty1, ty2]
      fun binOp (id, ty1, ty2, ty3) =
            Var.prim (id, monoScheme(Ty.TyFun(pairTy(ty1, ty2), ty3)))
    in
    val opASSIGN = Var.prim (
          BB.opASSIGN,
          tyScheme(fn t => Ty.TyFun(pairTy(tyRef t, t), tyUnit)))
    val opEQ = binOp (BB.opEQ, tyInt, tyInt, tyBool)
    val opNEQ = binOp (BB.opNEQ, tyInt, tyInt, tyBool)
    val opLTE = binOp (BB.opLTE, tyInt, tyInt, tyBool)
    val opLT = binOp (BB.opLT, tyInt, tyInt, tyBool)
    val opCONCAT = binOp (BB.opCONCAT, tyString, tyString, tyString)
    val opADD = binOp (BB.opADD, tyInt, tyInt, tyInt)
    val opSUB = binOp (BB.opSUB, tyInt, tyInt, tyInt)
    val opMUL = binOp (BB.opMUL, tyInt, tyInt, tyInt)
    val opDIV = binOp (BB.opDIV, tyInt, tyInt, tyInt)
    val opMOD = binOp (BB.opMOD, tyInt, tyInt, tyInt)
    val opNEG = Var.prim (BB.opNEG, monoScheme(Ty.TyFun(tyInt, tyInt)))
    val opDEREF = Var.prim (BB.opDEREF, tyScheme(fn t => Ty.TyFun(tyRef t, t)))
    end (* local *)

  (* predefined variables *)
    local
      fun prim (id, ty) = Var.prim(id, monoScheme ty)
      fun polyPrim (id, mkTy) = Var.prim(id, tyScheme mkTy)
    in
    val varArguments = prim (BB.varArguments, tyList tyString)
    val varChr = prim (BB.varChr, Ty.TyFun(tyInt, tyString))
    val varExit = polyPrim (BB.varExit, fn t => Ty.TyFun(tyInt, t))
    val varFail = polyPrim (BB.varFail, fn t => Ty.TyFun(tyString, t))
    val varNewRef = polyPrim (BB.varNewRef, fn t => Ty.TyFun(t, tyRef t))
    val varPrint = prim (BB.varPrint, Ty.TyFun(tyString, tyUnit))
    val varSize = prim (BB.varSize, Ty.TyFun(tyString, tyInt))
    val varSub = prim (BB.varSub, Ty.TyFun(Ty.TyTuple[tyString, tyInt], tyInt))
    end (* local *)

  end
