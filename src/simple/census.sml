(* census.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Code to compute use and application census counts for SimpleAST.
 *)

structure Census : sig

  (* Initialize use and application counts for a program *)
    val init : SimpleAST.program -> unit

  (* adjust the count of a value; i.e., if the value is a variable, update
   * it's count by the supplied delta.
   *)
    val adjustVal : SimpleAST.value * int -> unit

  (* Support for function application counts.  These are used to drive the
   * non-expansive inlining (i.e., inlining functions that are only called once).
   * [EXTRA CREDIT]
   *)
    val clearApp : SimpleVar.t -> unit          (* set application count to zero *)
    val incApp : SimpleVar.t -> unit            (* add to application count *)
    val addApp : SimpleVar.t * int -> unit      (* increment count by one *)
    val decApp : SimpleVar.t -> unit            (* decrement count by one *)
    val appCntOf : SimpleVar.t -> int           (* return the variable's application count *)

  (* a variable to variable substitution for renaming variables before deleting them *)
    type subst = SimpleVar.t -> SimpleVar.t

  (* Decrement the counts of the variables that are used in the various terms *)
    val deleteExp : subst * SimpleAST.exp -> unit
    val deleteRHS : subst * SimpleAST.rhs -> unit
    val deleteRule : subst -> SimpleAST.pat * SimpleAST.exp -> unit

  end = struct

    structure S = SimpleAST
    structure V = SimpleVar

    fun adjustVal (S.V_VAR x, delta) = V.addUse (x, delta)
      | adjustVal _ = ()

  (* Support for application counts on variables.  These are used to drive the
   * non-expansive inlining (i.e., inlining functions that are only called once).
   * Since these values are only relevant to variables that are applied as functions,
   * we use a property to represent them.
   *)
    local
      val {clrFn, getFn, peekFn, setFn} = V.newProp (fn _ => 0)
    in
    val clearApp = clrFn
    fun addApp (f, n) = let
          val cnt = getFn f
          in
            setFn (f, cnt + n)
          end
    fun incApp f = addApp (f, 1)
    fun decApp f = addApp (f, ~1)
    fun appCntOf f = (case peekFn f of SOME cnt => cnt | NONE => 0)
    end

  (* increment the use count of a value *)
    fun incVal (S.V_VAR x) = V.incUse x
      | incVal _ = ()
    val incVals = List.app incVal

    fun init (S.PROG(vArgs, e)) = let
          fun clrVar x = (V.clearUse x; clearApp x)
          fun clrPat (S.P_DCON(_, x)) = clrVar x
            | clrPat (S.P_CONST _) = ()
            | clrPat (S.P_VAR x) = clrVar x
          fun doExp (S.E(_, e)) = (case e
                 of S.E_LET(x, rhs, e) => (
                      clrVar x;
                      doRHS rhs;
                      doExp e)
                  | S.E_FUN(f, xs, body, e) => (
                      clrVar f;
                      List.app clrVar xs;
                      doExp body;
                      doExp e)
                  | S.E_APPLY(f, vs) => (V.incUse f; incApp f; incVals vs)
                  | S.E_COND(_, vs, e1, e2) => (incVals vs; doExp e1; doExp e2)
                  | S.E_CASE(v, rules) => (
                      incVal v;
                      List.app (fn (p, e) => (clrPat p; doExp e)) rules)
                  | S.E_RET v => incVal v
                (* end case *))
          and doRHS rhs = (case rhs
                 of S.R_EXP e => doExp e
                  | S.R_PRIM(_, vs) => incVals vs
                  | S.R_CALL(_, vs) => incVals vs
                  | S.R_TUPLE vs => incVals vs
                  | S.R_SELECT(_, x) => V.incUse x
                  | S.R_DCON(_, v) => incVal v
                (* end case *))
          in
            doExp e
          end

    type subst = SimpleVar.t -> SimpleVar.t

  (* decrement the use count of a value *)
    fun decVar (subst, x) = V.decUse (subst x)
    fun decVal (subst, S.V_VAR x) = decVar (subst, x)
      | decVal _ = ()
    fun decVals (subst, vs) = List.app (fn v => decVal(subst, v)) vs

    fun deleteExp (subst, e) = let
          fun del (S.E(_, e)) = (case e
                 of S.E_LET(_, rhs, e) => (
                      delRHS rhs;
                      del e)
                  | S.E_FUN(f, xs, body, e) => (
                      del body;
                      del e)
                  | S.E_APPLY(f, vs) => let
                      val f = subst f
                      in
                        V.decUse f; decApp f; decVals (subst, vs)
                      end
                  | S.E_COND(_, vs, e1, e2) => (decVals (subst, vs); del e1; del e2)
                  | S.E_CASE(v, rules) => (
                      decVal (subst, v);
                      List.app delRule rules)
                  | S.E_RET v => decVal (subst, v)
                (* end case *))
          and delRHS rhs = (case rhs
                 of S.R_EXP e => del e
                  | S.R_PRIM(_, vs) => decVals (subst, vs)
                  | S.R_CALL(_, vs) => decVals (subst, vs)
                  | S.R_TUPLE vs => decVals (subst, vs)
                  | S.R_SELECT(_, x) => decVar (subst, x)
                  | S.R_DCON(_, v) => decVal (subst, v)
                (* end case *))
          and delRule (_, exp) = del exp
          in
            del e
          end

    fun deleteRHS (subst, rhs) = (case rhs
           of S.R_EXP e => deleteExp (subst, e)
            | S.R_PRIM(_, vs) => decVals (subst, vs)
            | S.R_CALL(_, vs) => decVals (subst, vs)
            | S.R_TUPLE vs => decVals (subst, vs)
            | S.R_SELECT(_, x) => decVar (subst, x)
            | S.R_DCON(_, v) => decVal (subst, v)
          (* end case *))

    fun deleteRule subst (_, exp) = deleteExp (subst, exp)

  end
