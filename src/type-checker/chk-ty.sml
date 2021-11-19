(* chk-ty.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure ChkTy : sig

    (* convert a binding-tree type to an AST type, while checking the
     * well-formedness of the type expression.
     *)
    val check : Context.t * BindTree.ty -> Types.ty

  end = struct

    structure BT = BindTree
    structure Ty = Types
    structure C = Context

    fun check (cxt, ty) = (case ty
           of BT.TyMark mark => check (Context.withMark (cxt, mark))
            | BT.TyVar tv => Ty.TyVar(IdProps.tyvar tv)
            | BT.TyCon(tyc, tys) => let
                val tys' = List.map (fn ty => check(cxt, ty)) tys
                val tyc' = IdProps.tycon tyc
                in
                  if (TyCon.arityOf tyc' <> List.length tys')
                    then (
                      C.error (cxt, [
                          "arity mismatch in application of '",
                          BT.TycId.nameOf tyc, "'"
                        ]);
                      Ty.TyError)
                    else Ty.TyCon(tyc', tys')
                end
            | BT.TyFun(ty1, ty2) => Ty.TyFun(check(cxt, ty1), check(cxt, ty2))
            | BT.TyTuple tys => Ty.TyTuple(List.map (fn ty => check(cxt, ty)) tys)
          (* end case *))

  end
