(* prim-type.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Primitive types are used in the SimpleAST and CFG IRs to characterize the runtime
 * representations of data.
 *)

structure PrimType : sig

    (* simplified types that track runtime representations *)
    datatype t
      = Any                   (* unknown type *)
      | Obj                   (* pointer to a heap object of unknown type *)
      | Int                   (* tagged integer value *)
      | String                (* string value *)
      | Ref                   (* reference value *)
      | Tuple of t list       (* heap tuple; the list should have at least one element *)
      | Fun of t list * t     (* function type *)

  (* the representation kind of a type, which is either an unboxed tagged integer, a boxed
   * value (i.e., pointer), or has a mixed representation, where some values are unboxed
   * and some are boxed.
   *)
    datatype kind = Unboxed | Boxed | Mixed

  (* return the representation kind of a type *)
    val kindOf : t -> kind

  (* return the range of a function type *)
    val rangeOf : t -> t

  (* return a string representation of a type *)
    val toString : t -> string

  end = struct

    datatype t
      = Any
      | Obj
      | Int
      | String
      | Ref
      | Tuple of t list
      | Fun of t list * t

    datatype kind = Unboxed | Boxed | Mixed

    fun kindOf Any = Mixed
      | kindOf Obj = Boxed
      | kindOf Int = Unboxed
      | kindOf _ = Boxed

    fun toString Any = "<any>"
      | toString Obj = "<obj>"
      | toString Int = "Int"
      | toString String = "String"
      | toString Ref = "Ref<>"
      | toString (Tuple tys) =
          String.concat["{", String.concatWithMap ", " toString tys, "}"]
      | toString (Fun([ty1], ty2)) = concat[toString ty1, " -> ", toString ty2]
      | toString (Fun(tys, ty)) = concat[
            "(", String.concatWithMap ", " toString tys, ") -> ", toString ty
          ]

    fun rangeOf (Fun(_, ty)) = ty
      | rangeOf Any = Any
      | rangeOf ty = raise Fail(concat["rangeOf(", toString ty, ")"])

  end
