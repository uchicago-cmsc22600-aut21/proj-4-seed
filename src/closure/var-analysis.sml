(* var-analysis.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * An analysis pass that computes the free variables of functions and the
 * live variables at program points.
 *)

structure VarAnalysis : sig

  (* Analyze a SimpleAST program to compute the free variables of functions
   * and the live variables at splits (if-then-else expressions) and joins
   * (let expressions with rhs expressions).
   *)
    val analyze : SimpleAST.program -> unit

  (* return the free variables of a function; the result is only valid after the
   * analysis has been done.
   *)
    val freeVarsOf : SimpleVar.t -> SimpleVar.t list

  (* return the live variables upon entry to an expression. *)
    val liveVarsAt : SimpleAST.exp -> SimpleVar.t list

  (* returns true for functions that have a tail-recursive call to themselves. *)
    val isSelfTailRecursive : SimpleVar.t -> bool

  end = struct

    structure S = SimpleAST
    structure V = SimpleVar
    structure Set = V.Set

  (* program-point property to track live variables of expressions *)
    local
      val {getFn, setFn, ...} = ProgPt.newProp (fn _ => Set.empty)
    in
    fun liveVarsAt (S.E(ppt, _)) = Set.toList (getFn ppt)
    fun getLiveVars (S.E(ppt, _)) = getFn ppt
    val setLiveVars = setFn
    end (* local *)

  (* variable property to track the free variables of functions *)
    local
      val {getFn, setFn, ...} = V.newProp (fn _ => Set.empty)
    in
    val freeVarsOf = Set.toList o getFn
    val getFreeVars = getFn
    val setFreeVars = setFn
    end (* local *)

  (* function property to mark self-tail-recursive functions. *)
    local
      val {getFn, setFn} = V.newFlag ()
    in
    val isSelfTailRecursive = getFn
    fun markSelfRecursive f = setFn(f, true)
    end (* local *)

  (* check to see if a function is self-tail-recursive *)
    fun checkForSelfTailRecursion (f, body) = let
          fun chk (S.E(_, e)) = (case e
                 of S.E_LET(_, _, e) => chk e
                  | S.E_FUN(_, _, _, e) => chk e
                  | S.E_APPLY(g, _) => V.same(f, g)
                  | S.E_COND(_, _, e1, e2) => chk e1 orelse chk e2
                  | S.E_CASE(_, rules) => List.exists (fn (_, e) => chk e) rules
                  | S.E_RET _ => false
                (* end case *))
          in
            if chk body then (markSelfRecursive f; true) else false
          end

  (* compute the free variables of the functions in the program; we also
   * mark self-tail-recursive functions in this pass.
   *)
    fun freeVars e = let
          fun fvOfVal (S.V_VAR x, fvs) = Set.add(fvs, x)
            | fvOfVal (_, fvs) = fvs
          fun fvsOfVals (vs, fvs) = List.foldl fvOfVal fvs vs
          fun analFB (f, xs, body) = let
                val fvsOfF = Set.subtractList(anal (body, Set.empty), f::xs)
                in
                  if checkForSelfTailRecursion (f, body)
                    then markSelfRecursive f
                    else ();
                (* record f's free variables *)
                  setFreeVars (f, fvsOfF);
(*
print(concat["freeVars(", V.toString f, ") = {",
String.concatWithMap "," V.toString (Set.toList fvsOfF), "}\n"]);
*)
                  fvsOfF
                end
        (* analyze an expression for its free variables *)
          and anal (S.E(ppt, e), fvs) = (case e
                 of S.E_LET(x, rhs, e) => let
                      val fvs = analRHS(rhs, anal (e, fvs))
                      in
                        Set.subtract (fvs, x)
                      end
                  | S.E_FUN(f, params, body, e) => let
                      val fvsOfBody = anal (body, Set.empty)
                      val fvsOfF = analFB (f, params, body)
                      in
                        Set.subtract(anal (e, Set.union(fvs, fvsOfF)), f)
                      end
                  | S.E_APPLY(f, vs) => Set.add(fvsOfVals (vs, fvs), f)
                  | S.E_COND(_, vs, e1, e2) => anal (e2, anal (e1, fvsOfVals (vs, fvs)))
                  | S.E_CASE(v, rules) => List.foldl analRule (fvOfVal (v, fvs)) rules
                  | S.E_RET v => fvOfVal (v, fvs)
                (* end case *))
        (* analyze a right-hand-side for its free variables *)
          and analRHS (rhs, fvs) = (case rhs
                 of S.R_EXP e => anal (e, fvs)
                  | S.R_PRIM(_, vs) => fvsOfVals (vs, fvs)
                  | S.R_CALL(_, vs) => fvsOfVals (vs, fvs)
                  | S.R_TUPLE vs => fvsOfVals (vs, fvs)
                  | S.R_SELECT(_, x) => Set.add(fvs, x)
                  | S.R_DCON(_, v) => fvOfVal (v, fvs)
                (* end case *))
        (* analyze a case rule for its free variables *)
          and analRule ((p, e), fvs) = let
                val fvs = anal (e, fvs)
                in
                  case p
                   of S.P_DCON(_, x) => Set.subtract(fvs, x)
                    | S.P_VAR x => Set.subtract(fvs, x)
                    | _ => fvs
                  (* end case *)
                end
        (* we can skip any code that is not in a function *)
          fun walk (S.E(_, e)) = (case e
                 of S.E_LET(_, S.R_EXP e1, e2) => (walk e1; walk e2)
                  | S.E_LET(_, _, e) => walk e
                  | S.E_FUN(f, xs, body, e) => (ignore (analFB(f, xs, body)); walk e)
                  | S.E_COND(_, _, e1, e2) => (walk e1; walk e2)
                  | S.E_CASE(_, rules) => List.app (fn (_, e) => walk e) rules
                  | _ => ()
                (* end case *))
          in
            walk e
          end

  (* compute live variables and record them at program points.  This analys can mostly
   * be done in one bottom-up pass over the body of a function.  The one point of
   * complication is when a function is self-tail-recursive.  Consider, for example
   * the function (with live variables as comments):
   *
   *    fun f (x) =
   *       let t1 = IntLt(x, 0)                         // { x, n }
   *          if t1                                     // { t1, x, n }
   *             then ret x                             // { x }
   *             else let t2 = IntSub(x, n) in          // { x, n, f }
   *                f (t2)                              // { f, t2 }
   *
   * The free variables of `f` are {`n`}.  In order that the tail application of `f`
   * can be turned into a jump back to the loop header, we need to make sure that `n`
   * is available at that site.
   *
   * Our approach, then is as follows:
   *
   *    - we add an extra set of implicit-tail-call parameter variables as an argument
   *      to the tree walk.  This argument is empty on the first pass.
   *
   *    - if a function is marked as self recursive, then we do a second pass, where
   *      the implicit-tail-call parameters are set the the intersection of the free
   *      variables of the function and the live variables at function entry (e.g.,
   *      { n } for the above example.
   *
   *    - the implicit-tail-call parameters are added to the live variables at any
   *      self-recursive tail call.
   *)
    fun liveVars e = let
          fun lvOfVal (S.V_VAR x, live) = Set.add(live, x)
            | lvOfVal (_, live) = live
          fun lvsOfVals (vs, live) = List.foldl lvOfVal live vs
        (* analyze an expression for variable liveness. *)
          fun anal (S.E(ppt, e), live) = let
                val live = (case e
                       of S.E_LET(x, rhs, e) =>
                            analRHS (rhs, Set.subtract (anal (e, live), x))
                        | S.E_FUN(f, xs, body, e) => (
                            ignore (anal (body, Set.empty));
                          (* if `f` is self-tail-recursive, then we do another pass *)
                            if isSelfTailRecursive f
                              then anal2 (f, body)
                              else ();
                          (* the live variables at this point include the free variables
                           * of `f`, since we need them to build the closure value.
                           *)
                            Set.union (
                              getFreeVars f,
                              Set.subtract (anal (e, live), f)))
                        | S.E_APPLY(f, vs) => Set.add(lvsOfVals(vs, live), f)
                        | S.E_COND(_, vs, e1, e2) =>
                            lvsOfVals (vs, anal(e2, anal(e1, live)))
                        | S.E_CASE(v, rules) =>
                            lvOfVal (v, List.foldl (analRule live) Set.empty rules)
                        | S.E_RET v => lvOfVal (v, live)
                      (* end case *))
                in
                  setLiveVars (ppt, live);
                  live
                end
        (* the second analysis pass that adds in variables from self-tail-recursive calls *)
          and anal2 (curFn, body) = let
              (* the additional live variables that we need at tail-call sites *)
                val extraLive = Set.intersection(getFreeVars curFn, getLiveVars body)
                fun anal (tail, S.E(ppt, e), live) = let
                      val live = (case e
                             of S.E_LET(x, rhs, e) =>
                                  analRHS (rhs, Set.subtract (anal (tail, e, live), x))
                              | S.E_FUN(f, xs, body, e) => (
                                (* the live variables at this point include the free variables
                                 * of `f`, since we need them to build the closure value.
                                 *)
                                  Set.union (
                                    getFreeVars f,
                                    Set.subtract (anal (tail, e, live), f)))
                              | S.E_APPLY(f, vs) => let
                                  val live = Set.add(lvsOfVals(vs, live), f)
                                  in
                                    if tail andalso V.same(f, curFn)
                                      then Set.union(live, extraLive)
                                      else live
                                  end
                              | S.E_COND(_, vs, e1, e2) =>
                                  lvsOfVals (vs, anal(tail, e2, anal(tail, e1, live)))
                              | S.E_CASE(v, rules) =>
                                  lvOfVal (v,
                                    List.foldl (analRule tail live) Set.empty rules)
                              | S.E_RET v => lvOfVal (v, live)
                            (* end case *))
                        in
                          setLiveVars (ppt, live);
                          live
                        end
                and analRHS (rhs, live) = (case rhs
                       of S.R_EXP e => anal (false, e, live)
                        | S.R_PRIM(_, vs) => lvsOfVals (vs, live)
                        | S.R_CALL(_, vs) => lvsOfVals (vs, live)
                        | S.R_TUPLE vs => lvsOfVals (vs, live)
                        | S.R_SELECT(_, x) => Set.add(live, x)
                        | S.R_DCON(_, v) => lvOfVal (v, live)
                      (* end case *))
              (* analyze a case rule for its free variables *)
                and analRule tail live ((p, e), acc) = (case p
                       of S.P_DCON(_, x) =>
                            Set.union(acc, Set.subtract(anal (tail, e, live), x))
                        | S.P_VAR x =>
                            Set.union(acc, Set.subtract(anal (tail, e, live), x))
                        | _ => Set.union(acc, anal (tail, e, live))
                      (* end case *))
                in
                  ignore (anal (true, body, Set.empty))
                end (* anal2 *)
          and analRHS (rhs, live) = (case rhs
                 of S.R_EXP e => anal (e, live)
                  | S.R_PRIM(_, vs) => lvsOfVals (vs, live)
                  | S.R_CALL(_, vs) => lvsOfVals (vs, live)
                  | S.R_TUPLE vs => lvsOfVals (vs, live)
                  | S.R_SELECT(_, x) => Set.add(live, x)
                  | S.R_DCON(_, v) => lvOfVal (v, live)
                (* end case *))
        (* analyze a case rule for its free variables *)
          and analRule live ((p, e), acc) = (case p
                 of S.P_DCON(_, x) =>
                      Set.union(acc, Set.subtract(anal (e, live), x))
                  | S.P_VAR x =>
                      Set.union(acc, Set.subtract(anal (e, live), x))
                  | _ => Set.union(acc, anal (e, live))
                (* end case *))
          in
            ignore (anal (e, Set.empty))
          end

  (* free vars + liveness analysis *)
    fun analyze (S.PROG(_, e)) = (
        (* first we compute free variables *)
          freeVars e;
        (* then compute live variables *)
          liveVars e)

  end
