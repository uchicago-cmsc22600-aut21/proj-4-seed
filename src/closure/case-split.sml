(* case-split.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * The case splitting pass splits mixed case expressions into two sub-cases;
 * one for unboxed constructors and one for boxed constructors.  The census
 * counts are maintained and the new case expressions have correct case info.
 *)

structure CaseSplit : sig

    val transform : SimpleAST.program -> SimpleAST.program

  end = struct

    structure S = SimpleAST
    structure DC = SimpleDataCon
    structure PTy = PrimType

    fun xformExp (exp as S.E(ppt, e)) = let
          fun mk e = S.E(ppt, e)
          in
            case e
             of S.E_LET(xs, S.R_EXP e1, e2) =>
                  mk(S.E_LET(xs, S.R_EXP(xformExp e1), xformExp e2))
              | S.E_LET(xs, rhs, e2) =>
                  mk(S.E_LET(xs, rhs, xformExp e2))
              | S.E_FUN(f, xs, e1, e2) =>
                  mk(S.E_FUN(f, xs, xformExp e1, xformExp e2))
              | S.E_APPLY _ => exp
              | S.E_COND(tst, vs, e1, e2) =>
                  mk(S.E_COND(tst, vs, xformExp e1, xformExp e2))
              | S.E_CASE(v, rules) => let
                  val info = CaseInfo.get exp
                  (* convert the rule actions first *)
                  val rules = List.map (fn (p, e) => (p, xformExp e)) rules
                  in
                    case #argKind info
                     of PTy.Mixed => xformCase (info, v, rules)
                      | _ => mk(S.E_CASE(v, rules))
                    (* end case *)
                  end
              | S.E_RET _ => exp
            (* end case *)
          end

    and xformCase ({ty, argKind, nConsts, nFuns}, v, rules) = let
        (* partition the rules into constant and function rules; plus an optional
         * shared default rule.
         *)
          fun split ([], nc, constRules, nf, funRules) =
                (rev constRules, rev funRules, NONE)
            | split ((r as (p, e)) :: rules, nc, constRules, nf, funRules) = (
                case p
                 of S.P_DCON _ => split (rules, nc, constRules, nf+1, r::funRules)
                  | S.P_CONST _ => split (rules, nc+1, r::constRules, nf, funRules)
                  | S.P_VAR x => ((* must be the last rule, so we are done *)
                      case (nc < nConsts, nf < nFuns)
                       of (true, false) => (rev(r :: constRules), rev funRules, NONE)
                        | (false, true) => (rev constRules, rev(r :: funRules), NONE)
                        | _ => (* shared default *)
                            (rev constRules, rev funRules, SOME(x, e))
                      (* end case *))
                (* end case *))
          (* generate the "if boxed" test; the third argument is the optional
           * join function used to avoid code duplication.
           *)
          fun ifBoxed (ubCase, bCase) = S.mkCOND(PrimCond.Boxed, [v], bCase, ubCase)
          (* generate the cases with a shared default action *)
          fun withJoin (ubCase, bCase, x, e) = let
                val join = SimpleVar.new("join", PTy.Fun([SimpleVar.typeOf x], ty))
                val dflt = S.mkAPPLY(join, [v])
                in
                  S.mkFUN(join, [x], e, ifBoxed(ubCase dflt, bCase dflt))
                end
          (* build a new case expression from the rules *)
          fun mkCase (kind, rules) = let
                val exp = S.mkCASE(v, rules)
                in
                (* bookeeping *)
                  CaseInfo.set(exp, {
                      ty = ty, argKind = kind, nConsts = nConsts, nFuns = nFuns
                    });
                  exp
                end
          (* build a new case expression from the rules and default action *)
          fun mkCase' (kind, rules) dflt = let
                val dfltRule = (S.P_VAR(SimpleVar.new("t", PTy.Any)), dflt)
                in
                  mkCase (kind, rules @ [dfltRule])
                end
          in
            (* when adjusting the count of the argument value, we have +1 for each
             * use of the default rule, +1 for each case expression, +1 for the
             * boxity test, and -1 for the original case expression.
             *)
            case split (rules, 0, [], 0, [])
             of ([], [], SOME(x, e)) => (* trivial case that just rebinds `x` *)
                  S.mkLET(x, S.R_EXP(S.mkRET v), e)
              | ([], funRules, SOME(x, e)) => (
                (* trivial constant case is default rule *)
                  Census.adjustVal (v, 3);
                  withJoin (fn dflt => dflt, mkCase' (PTy.Boxed, funRules), x, e))
              | (constRules, [], SOME(x, e)) => (
                (* trivial function case is default rule *)
                  Census.adjustVal (v, 3);
                  withJoin (mkCase' (PTy.Unboxed, constRules), fn dflt => dflt, x, e))
              | (constRules, funRules, NONE) => (
                (* no shared default case *)
                  Census.adjustVal (v, 2);
                  ifBoxed (
                    mkCase (PTy.Unboxed, constRules),
                    mkCase (PTy.Boxed, funRules)))
              | (constRules, funRules, SOME(x, e)) => (
                (* shared default case *)
                  Census.adjustVal (v, 4);
                  withJoin (
                    mkCase' (PTy.Unboxed, constRules),
                    mkCase' (PTy.Boxed, funRules),
                    x, e))
            (* end case *)
          end

    fun transform (S.PROG(vArgs, body)) =
          S.PROG(vArgs, xformExp body)

  end
