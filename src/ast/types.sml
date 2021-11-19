(* types.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * The representation of types in the ML Lite abstract syntax.
 *)

structure Types =
  struct

    (* bound type variables *)
    type tyvar = TyVar.t

    datatype ty
     = TyVar of tyvar		(* bound type variable *)
     | TyMeta of meta_var	(* meta-variable *)
     | TyFun of ty * ty		(* function type *)
     | TyTuple of ty list       (* list has at least two elements *)
     | TyCon of tyc * ty list	(* type constructor application *)
     | TyError			(* unknown type used to avoid cascading errors *)

    (* unification meta variables *)
    and meta_var = MV of {
        id : Stamp.t,
        inst : instance ref
      }

    and instance = UNIV of int | INST of ty

    (* type constructors: either builtin abstract types or concrete
     * data types.
     *)
    and tyc
      = AbsTyc of {
          name : string,        (* the abstract type's name *)
          id : Stamp.t,         (* unique stamp *)
          arity : int           (* number of type parameters *)
        }
      | DataTyc of data_ty

    and dcon = DCon of {        (* data constructor *)
        name : string,          (* constructor name *)
        id : Stamp.t,           (* unique identifier *)
        argTy : ty option,      (* argument type; NONE for nullary constructors *)
        dty : data_ty           (* the type constructor that this dcon belongs to *)
      }

    withtype data_ty = {
          name : string,        (* the type's name *)
          id : Stamp.t,         (* unique stamp *)
          params : tyvar list,  (* bound type parameters *)
          cons : dcon list ref  (* list of data constructors *)
        }

    (* polymorphic type schemes *)
    type ty_scheme = tyvar list * ty

  end
