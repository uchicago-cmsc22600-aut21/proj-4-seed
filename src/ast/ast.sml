(* ast.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * The Typed Abstract Syntax Tree representation of ML Lite programs.
 *)

structure AST =
  struct

    type ty = Types.ty
    type var = Var.t
    type dcon = Types.dcon

    datatype exp = E of exp_rep * ty    (* expression paired with its type *)

    (* abstract syntax of expressions *)
    and exp_rep
      = E_LET of bind * exp
      | E_COND of exp * exp * exp
      | E_CASE of exp * (pat * exp) list
      | E_APPLY of exp * exp
      | E_TUPLE of exp list
      | E_CONST of const
      | E_VAR of var

    and bind
      = B_VAL of pat * exp
      | B_FUN of var * pat list * exp   (* potentially curried function definition *)

    (* patterns *)
    and pat
      = P_CON of dcon * pat
      | P_CONST of dcon
      | P_TUPLE of pat list
      | P_VAR of var

    (* constants: literals and constructors *)
    and const
      = C_DCON of dcon
      | C_INT of IntInf.int
      | C_STR of string

    datatype program = PROG of {
        datatycs : Types.data_ty list,
        body : exp
      }

  end
