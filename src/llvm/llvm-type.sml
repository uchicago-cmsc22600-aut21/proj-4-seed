(* llvm-type.sml
 *
 * COPYRIGHT (c) 2021 Kavon Farvardin and John Reppy
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure LLVMType : sig

    datatype t
      = Void
      | Int1                    (* a 1 bit integer. for bools *)
      | Int8                    (* an 8 bit integer *)
      | Int32                   (* a 32 bit integer *)
      | Int64                   (* a 64 bit integer for ML Lite ints *)
      | Func of t * t list      (* return type paired with argument types  *)
      | Ptr of t                (* pointer *)
      | Array of int * t        (* int = num elements *)
      | Struct of t list
      | Token                   (* unique returned by GC wrapped function calls *)
      | VarArg                  (* varargs; used for the GC intrinsics *)

    val toString : t -> string

    (* dereference a pointer type *)
    val deref : t -> t option

    val gepTy : t * LLVMRep.var list -> t

    (* are two types equal? *)
    val same : t * t -> bool

  end = struct

    structure Rep = LLVMRep

    type addr_space = int

    datatype t = datatype Rep.ty

    val heap : addr_space = 1

    fun toString ty = (case ty
           of Void => "void"
            | Int1 => "i1"
            | Int8 => "i8"
            | Int32 => "i32"
            | Int64 => "i64"
            | Func(retTy, paramTys) => String.concat [
                  toString retTy, " (", String.concatWithMap "," toString paramTys, ")*"
                ]
            | Ptr ty => toString ty ^ " addrspace(1)*"
            | Array(sz, ty) => String.concat ["[", Int.toString sz, " x ", toString ty, "]"]
            | Struct tys => String.concat [
                  "{", String.concatWithMap "," toString tys, "}"
                ]
            | Token => "token"
            | VarArg => "..."
          (* end case *))

    fun deref ty = (case ty
           of Ptr ty => SOME ty
            | _ => NONE
          (* end case *))

  (* error checking/reporting is light here, we just care about
   * getting the right type for well-formed queries.
   *)
    fun gepTy (ptrTy, _::offsets) = let
          fun lp ([], ty) = ty
            | lp (v::vs, ty) = (case (ty, v)
                 of (Struct us, Rep.IConst(_, i)) => lp(vs, List.nth(us, IntInf.toInt i))
                  | (Array(_, u), _) => u
                  | _ => ty    (* optimistic. it's either ty or an error *)
                (* end case *))
          in
            case ptrTy
             of Ptr ty => Ptr(lp(offsets, ty)) (* step through the pointer *)
              | _ => raise Fail(concat[
                    "gepTy (", toString ptrTy, "); argument must be pointer type"
                  ])
            (* end case *)
          end

  (* are two types equal? *)
    fun same (ty1, ty2) = (case (ty1, ty2)
           of (Void, Void) => true
            | (Int1, Int1) => true
            | (Int8, Int8) => true
            | (Int32, Int32) => true
            | (Int64, Int64) => true
            | (Func(retTy1, tys1), Func(retTy2, tys2)) =>
                same(retTy1, retTy2) andalso ListPair.allEq same (tys1, tys2)
            | (Ptr ty1, Ptr ty2) => same (ty1, ty2)
            | (Array(n1, ty1), Array(n2, ty2)) => (n1 = n2) andalso same (ty1, ty2)
            | (Struct tys1, Struct tys2) => ListPair.allEq same (tys1, tys2)
            | (Token, Token) => true
            | (VarArg, VarArg) => true
            | _ => false
          (* end case *))

  end
