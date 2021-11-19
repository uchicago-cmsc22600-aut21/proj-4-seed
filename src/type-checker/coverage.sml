(* coverage.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Support for tracking coverage of rules in a case expression.
 *)

structure Coverage : sig

  (* abstract representation of parts of the argument domain that are
   * not yet covered by the match-case rules
   *)
    type t

  (* given the argument type of a case expression, return the initial coverage
   * value.
   *)
    val init : Types.ty -> t

  (* update coverage based on a pattern; returns the updated missing coverage paired
   * with true if the pattern was redundant and false otherwise.
   *)
    val update : t * AST.pat -> t * bool

  (* is the coverage exhaustive? *)
    val exhaustive : t -> bool

  end = struct

    structure Ty = Types
    structure DC = DataCon

    datatype t
      = Complete                (* all values have been covered by previous rules. *)
      | NonDataCon              (* for non-data types (e.g., tuples, integers) *)
      | Missing of DC.t list    (* constructors that have not been covered yet *)
      | ErrorArg                (* ErrorTy for argument *)

    fun init ty = (case ty
           of Ty.TyCon(Ty.DataTyc{cons, ...}, _) => Missing(!cons)
            | Ty.TyError => ErrorArg
            | _ => NonDataCon
          (* end case *))

  (* when the constructors are all covered, then we have a complete match *)
    fun missing [] = Complete
      | missing dcs = Missing dcs

    fun update (Complete, _) =
        (* already complete, so any pattern is redundant *)
          (Complete, true)
      | update (_, AST.P_TUPLE _) = (Complete, false)
      | update (_, AST.P_VAR _) = (Complete, false)
      | update (cover, AST.P_CON(dc, _)) = updateConPat (cover, dc)
      | update (cover, AST.P_CONST dc) = updateConPat (cover, dc)

    and updateConPat (cover as Missing cons, dc) = let
        (* search the list of constructors that have not yet been covered
         * for `dc`.  If we don't find it, then it was already covered.
         * Otherwise, remove it from the missing list.
         *)
          fun find ([], _) = (cover, true)
            | find (dc'::dcs, prefix) = if DC.same(dc, dc')
                then (missing(List.revAppend(prefix, dcs)), false)
                else find (dcs, dc'::prefix)
          in
            find (cons, [])
          end
      | updateConPat (ErrorArg, _) = (ErrorArg, false)
      | updateConPat _ = raise Fail "impossible: bogus coverage state"

    fun exhaustive Complete = true
      | exhaustive _ = false

  end
