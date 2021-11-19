(* parse-tree.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Parse-tree representation of ML-Lite programs
 *)

structure ParseTree =
  struct

    type 'a mark = 'a Error.mark (* {span : span, tree : 'a} *)

    type id = Atom.atom                         (* identifiers *)

    type number = IntInf.int                    (* integer literal *)

    datatype program
      = ProgMark of program mark                (* source-file mark *)
      | Prog of declaration list * exp          (* program *)

    and declaration
      = DclMark of declaration mark             (* source-file mark *)
      | DclData of id * id list * con list      (* data type declaration *)
      | DclVal of bind                          (* value declaration *)

    and con
      = ConMark of con mark                     (* source-file mark *)
      | Con of id * ty option                   (* data constructor declaration *)

    and ty
      = TyMark of ty mark                       (* source-file mark *)
      | TyVar of id                             (* type variable *)
      | TyCon of id * ty list                   (* type constructor application *)
      | TyFun of ty * ty                        (* function type *)
      | TyTuple of ty list                      (* tuple type; list has 2+ elements *)

    and bind
      = BindMark of bind mark                   (* source-file mark *)
      | BindFun of id * pat list * exp          (* function declaration *)
      | BindVal of pat * exp                    (* value identifier declaration *)
      | BindExp of exp                          (* expression *)

    and exp
      = ExpMark of exp mark                     (* source-file mark *)
      | ExpIf of exp * exp * exp                (* conditional *)
      | ExpOrElse of exp * exp                  (* `||` conditional operator *)
      | ExpAndAlso of exp * exp                 (* `&&` conditional operator *)
      | ExpBin of exp * id * exp                (* infix binary operators *)
      | ExpListCons of exp * exp                (* infix `::` operator *)
      | ExpUn of id * exp                       (* unary operator *)
      | ExpApp of exp * exp                     (* value application *)
      | ExpVar of id                            (* value identifier *)
      | ExpCon of id                            (* data constructor identifier *)
      | ExpInt of number                        (* integer literal *)
      | ExpStr of string                        (* string literal *)
      | ExpTuple of exp list                    (* tuple expression; list has 2+ elements *)
      | ExpCase of exp * case_rule list         (* case expression *)
      | ExpScope of scope                       (* local scope *)

    and case_rule
      = RuleMark of case_rule mark              (* source-file mark *)
      | RuleCase of pat * scope                 (* pattern matching rule in a case expression *)

    and pat
      = PatMark of pat mark                     (* source-file mark *)
      | PatVar of id                            (* variable binding pattern *)
      | PatCon of id * pat option               (* data constructor pattern *)
      | PatListCons of pat * pat                (* infix '::' pattern *)
      | PatTuple of pat list                    (* tuple pattern; list has 2+ elements *)
      | PatWild                                 (* wild-card pattern *)

    withtype scope = bind list * exp            (* local scope *)

  end
