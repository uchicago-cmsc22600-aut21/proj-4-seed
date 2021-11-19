(* closure.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Convert the higher-order SimpleAST IR to the first-order CFG IR.
 *)

structure Closure : sig

    (* transform the higher-order SimpleAST program to a first-order
     * CFG representation by applying closure conversion.
     *)
    val transform : SimpleAST.program -> CFG.program

  end = struct

    structure S = SimpleAST
    structure SV = SimpleVar
    structure SDC = SimpleDataCon
    structure PTy = PrimType
    structure VMap = SimpleVar.Map
    structure VSet = SimpleVar.Set
    structure VA = VarAnalysis

  (* convert SimpleAST type to CFG types. *)
    fun cvtTy (PTy.Tuple tys) = PTy.Tuple(List.map cvtTy tys)
      | cvtTy (PTy.Fun(tys, ty)) = PTy.Obj
      | cvtTy pty = pty

  (********** Conversion environment **********)

    val lookupVar = Env.lookup
    val bindVar = Env.bind

  (* lookup a list of CFG variables *)
    fun lookupVars (env, xs) = List.map (fn x => lookupVar(env, x)) xs

  (* convert a SimpleAST value to a CFG value *)
    fun cvtVal env v = (case v
           of S.V_VAR x => lookupVar (env, x)
            | S.V_CON dc => let
                val SDC.Enum i = SDC.repOf dc
                in
                  CFG.INT(IntInf.fromInt i)
                end
            | S.V_INT n => CFG.INT n
            | S.V_STR s => CFG.STRING s
          (* end case *))

  (* convert a list of SimpleAST values to CFG values *)
    fun cvtVals (env, vs) = List.map (cvtVal env) vs

  (* convert an AST variable to a CFG variable; in this case, we expect that
   * the AST variable has been mapped to a CFG.VAR.
   *)
    fun cvtVar (env, x) = (case lookupVar (env, x)
           of CFG.VAR x' => x'
            | v => raise Fail(concat[
                  "cvtVar: '", SV.toString x, "' maps to '", CFGUtil.valueToString v, "'"
                ])
          (* end case *))

  (* create a new CFG variable for a SimpleAST variable *)
    fun newVar x = CFGVar.new(SV.nameOf x, cvtTy(SV.typeOf x))

  (* bind a variable to a new CFG variable *)
    fun bindNewVar (env, x) = let
          val x' = newVar x
          in
            (x', bindVar(env, x, CFG.VAR x'))
          end

  (* given a list of SimpleAST variables that are the parameters to a fragment,
   * create fresh CFG variables and initialize an environment that maps from the
   * SimpleAST variables to their CFG counterparts.
   *)
    fun mkParams (env, xs) = let
          val xs' = List.map newVar xs
          val env = ListPair.foldl
                (fn (x, x', env) => bindVar(env, x, CFG.VAR x'))
                  (Env.newFrag env) (xs, xs')
          in
            (xs', env)
          end

  (********** Conversion context **********)

  (* this datatype represents the continuation of an expression *)
    datatype continuation
    (* `TAIL` represents the tail position of the current function *)
      = TAIL
    (* `JOIN k` represents a join point for an `IfExp` or `CaseExp`.  The function `k`
     * takes an environment and the argument for the join (i.e., the result of a non-tail
     * application or `RetExp`) and returns a jump to the join fragment.
     *)
      | JOIN of Env.t * CFG.value -> CFG.jump

  (********** Function properties **********)

  (* a property to track the entry label of a function *)
    val {getFn=getLabel, ...} = SV.newProp (fn f => CFGLabel.new (SV.toString f))

  (********** Conversion **********)

  (* convert a SimpleAST function to a CFG function and add it to the list of functions *)
    fun cvtFunc (env, f, xs, body, fvs) = let
          val isSelfTail = VA.isSelfTailRecursive f
        (* prepare the environment for the function *)
          val env = Env.newFun (env, f, isSelfTail)
        (* get the return type of the function *)
          val retTy = PTy.rangeOf (SV.typeOf f)
        (* convert the function's parameters to CFG variables *)
          val (xs', env) = mkParams (env, xs)
        (* the function's closure parameter *)
          val closure = CFGVar.new (SV.nameOf f ^ "_clos", PTy.Obj)
        (* in the body of the function, we map the function to its own closure *)
          val env = bindVar (env, f, CFG.VAR closure)
        (* create the entry fragment by loading the free variables from the closure
         * and then translating the function body.
         *)
          fun loadFVs (_, [], env) = if isSelfTail
              (* this function is self-tail-recursive, so we need to split
               * the entry fragment into the entry fragment where the free
               * variables are loaded from the closure and the header fragment.
               *)
                then let
                  val lab = Env.getHeaderLable env
                (* for the header, the parameters will be in the order
                 * closure, function params, free variables.  These are
                 * guaranteed to be a superset of the live variables of
                 * the function body.
                 *)
                  val hdrParams = f :: xs @ fvs
                  val frag = expToFrag (env, lab, hdrParams, body, TAIL)
                  in
                    CFG.mkGOTO(lab, lookupVars (env, hdrParams))
                  end
                else cvtExp (env, body, TAIL)
            | loadFVs (ix, fv::fvs, env) = let
                val (fv', env') = bindNewVar (env, fv)
                in
                  CFG.mkLET(fv', CFG.SEL(ix, CFG.VAR closure),
                    loadFVs(ix+1, fvs, env'))
                end
        (* the entry fragment *)
          val entryFrag = CFGFrag.new (getLabel f, closure :: xs', loadFVs (1, fvs, env))
        (* create the CFG function *)
          val func = CFGFunct.new (retTy, entryFrag :: Env.getFrags env)
          in
            Env.addFun (env, func)
          end

  (* `expToFrag (env, lab, params, body, cont)` translates the expression `body`
   * to a CFG fragment, where `lab` is its label, `params` are the SimpleAST
   * parameters, and `cont` is the continuation for the fragment.  The resulting
   * fragment is added to the list of fragments for the current function.
   *)
    and expToFrag (env, lab, params, body, cont) = let
          val (params', env') = mkParams (env, params)
          val frag = CFGFrag.new(lab, params', cvtExp (env', body, cont))
          in
            Env.addFrag (env, frag)
          end

    and expToJump (_, env, S.E(_, S.E_RET v), JOIN k) =
          (* jump-chain elimination *)
            k (env, cvtVal env v)
      | expToJump (prefix, env, e, cont) = let
          val lab = CFGLabel.new prefix
          val live = VA.liveVarsAt e
          in
            expToFrag (env, lab, live, e, cont);
            (lab, lookupVars (env, live))
          end

  (* `cvtExp (env, exp, cont)` converts a SimpleAST expression to CFG. *)
    and cvtExp (env, exp as S.E(_, e), cont) = (case e
           of S.E_LET(x, S.R_EXP(rhsExp as S.E(_, rhs)), scope) => (case rhs
                 of S.E_LET _ => raise Fail "impossible: nested let"
                  | S.E_FUN _ => raise Fail "impossible: nested fun"
                  | S.E_APPLY(f, vs) => let
                      val (x', env') = bindNewVar(env, x)
                      in
                        cvtApply (env, f, vs, fn appl =>
                          CFG.mkLET(x', CFG.APPLY appl,
                            cvtExp(env', scope, cont)))
                      end
                  | S.E_COND(tst, vs, eThen, eElse) => raise Fail "YOUR CODE HERE"
                  | S.E_CASE(v, rules) => let
                    (* an CaseExp on the rhs of a let binding creates a join point *)
                      val joinLive = VA.liveVarsAt scope
                      val joinLab = CFGLabel.new "join"
                      val joinParams = x :: joinLive
                      val joinFrag = expToFrag (env, joinLab, joinParams, scope, cont)
                      val join = JOIN(fn (env, v) => (joinLab, v :: lookupVars (env, joinLive)))
                      in
                        cvtCaseExp (env, cvtVal env v, CaseInfo.getKind exp, rules, join)
                      end
                  | S.E_RET v => cvtExp (bindVar(env, x, cvtVal env v), scope, cont)
                (* end case *))
            | S.E_LET(x, S.R_PRIM(p, vs), scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_CALL(cf, vs), scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_TUPLE vs, scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_SELECT(i, y), scope) => raise Fail "YOUR CODE HERE"
            | S.E_LET(x, S.R_DCON(dc, v), scope) => raise Fail "YOUR CODE HERE"
            | S.E_FUN(f, xs, body, e) => raise Fail "YOUR CODE HERE"
            | S.E_APPLY(f, vs) => raise Fail "YOUR CODE HERE"
            | S.E_COND(tst, vs, eThen, eElse) => let
              (* make the jump for an arm of the if-then-else *)
                fun mkJump (prefix, e) = expToJump (prefix, env, e, cont)
                in
                  CFG.mkIF(tst, cvtVals (env, vs),
                    mkJump ("then", eThen),
                    mkJump ("else", eElse))
                end
            | S.E_CASE(v, rules) =>
                cvtCaseExp (env, cvtVal env v, CaseInfo.getKind exp, rules, cont)
            | S.E_RET v => (case cont
                 of TAIL => CFG.mkRETURN(cvtVal env v)
                  | JOIN k => CFG.mkGOTO(k (env, cvtVal env v))
                (* end case *))
          (* end case *))

  (* convert an application to CFG, which means extracting the code pointer
   * from the closure and including the closure as the first argument.  The
   * parameter `k` builds the actual expression, since the application might
   * be either a APPLY right-hand-side or a TAIL_APPLY expression.
   *)
    and cvtApply (env, f, vs, k : PTy.t * CFG.value * CFG.value list -> CFG.exp) = let
          val f' = cvtVar (env, f)
          val args = CFG.VAR f' :: cvtVals (env, vs)
          val retTy = cvtTy(PTy.rangeOf(SV.typeOf f))
          in
            if SV.same(f, Env.currentFun env)
              then k (retTy, CFG.CODE(getLabel f), args)
              else let
                val cp = CFGVar.new (SV.nameOf f ^ "_cp", PTy.Obj)
                in
                  CFG.mkLET(cp, CFG.SEL(0, CFG.VAR f'),
                    k (retTy, CFG.VAR cp, args))
                end
          end

  (* translate a case expression to CFG; because of case splitting, the `argKind`
   * must be either `Boxed` or `Unboxed`.
   *)
    and cvtCaseExp (env, v, argKind, rules, cont) =
          raise Fail "YOUR CODE HERE"

  (* the name of the program entry point *)
    val progEntryName = "_mll_entry"

  (* convert the SimpleAST program to CFG *)
    fun transform prog = let
        (* split mixed cases *)
          val prog as S.PROG(vArgs, body) = CaseSplit.transform prog
        (* analyze the programs *)
          val _ = VA.analyze prog
        (* define the program's entry label *)
          val entryLab = CFGLabel.newExport progEntryName
        (* the initial enviroment for processing the top-level expression *)
          val env = Env.newFun(
                Env.new(),
                SV.new(progEntryName, PTy.Fun([PTy.Any], PTy.Int)),
                false)
          val (vArgs', env) = bindNewVar (env, vArgs)
          val entryFrag = CFGFrag.new (entryLab, [vArgs'], cvtExp (env, body, TAIL))
          val func = CFGFunct.new (PTy.Int, entryFrag :: Env.getFrags env)
          in
            CFG.PROG(func :: Env.getFuns env)
          end

  end
