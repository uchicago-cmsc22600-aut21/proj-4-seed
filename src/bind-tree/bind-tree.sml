(* bind-tree.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Representation of ML-Lite programs with binding information.
 *)

structure BindTree =
  struct

    (* a term marked with a source location *)
    type 'a mark = 'a Error.mark (* {span : span, tree : 'a} *)

    (* define the various classes of identifiers *)
    structure TyVar = IdentifierFn()    (* type variable identifiers *)
    structure TycId = IdentifierFn()    (* type constructor identifiers *)
    structure ConId = IdentifierFn()    (* data constructor identifiers *)
    structure VarId = IdentifierFn()    (* variable identifiers *)

    (* type aliases *)
    type tyvar = TyVar.t
    type tycon = TycId.t
    type conid = ConId.t
    type varid = VarId.t

    type number = IntInf.int                    (* integer literal *)

    datatype program
      = ProgMark of program mark                (* source-file mark *)
      | Prog of declaration list * exp          (* program *)

    and declaration
      = DclMark of declaration mark             (* source-file mark *)
      | DclData of tycon * tyvar list * con list
                                                (* data type declaration *)
      | DclVal of bind                          (* value declaration *)

    and con
      = ConMark of con mark                     (* source-file mark *)
      | Con of conid * ty option                (* data constructor declaration *)

    and ty
      = TyMark of ty mark                       (* source-file mark *)
      | TyVar of tyvar                          (* type variable *)
      | TyCon of tycon * ty list                (* type constructor application *)
      | TyFun of ty * ty                        (* function type *)
      | TyTuple of ty list                      (* tuple type; list has 2+ elements *)

    and bind
      = BindMark of bind mark                   (* source-file mark *)
      | BindFun of varid * pat list * exp       (* function declaration *)
      | BindVal of pat * exp                    (* value identifier declaration; pat will be *)
                                                (* either PatVar or PatWild *)
      | BindExp of exp                          (* expression *)

    and exp
      = ExpMark of exp mark                     (* source-file mark *)
      | ExpIf of exp * exp * exp                (* conditional *)
      | ExpOrElse of exp * exp                  (* `||` conditional operator *)
      | ExpAndAlso of exp * exp                 (* `&&` conditional operator *)
      | ExpBin of exp * varid * exp             (* infix binary operators *)
      | ExpListCons of exp * exp                (* infix `::` operator *)
      | ExpUn of varid * exp                    (* unary operator *)
      | ExpApp of exp * exp                     (* value application *)
      | ExpVar of varid                         (* value identifier *)
      | ExpCon of conid                         (* data constructor identifier *)
      | ExpInt of number                        (* integer literal *)
      | ExpStr of string                        (* string literal *)
      | ExpTuple of exp list                    (* tuple expression *)
      | ExpCase of exp * case_rule list         (* case expression *)
      | ExpScope of scope                       (* local scope *)

    and case_rule
      = RuleMark of case_rule mark              (* source-file mark *)
      | RuleCase of pat * scope                 (* pattern matching rule in a case expression *)

    and pat
      = PatMark of pat mark                     (* source-file mark *)
      | PatVar of varid                         (* variable binding pattern *)
      | PatCon of conid * pat option            (* data constructor pattern *)
      | PatListCons of pat * pat                (* infix '::' pattern *)
      | PatTuple of pat list                    (* tuple pattern *)
      | PatWild                                 (* wild-card pattern *)

    withtype scope = bind list * exp            (* local scope *)

  end
