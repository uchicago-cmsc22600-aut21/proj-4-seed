(* simplify.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure Simplify : sig

  (* translate the AST IR to SimpleAST; in addition to converting the IR, we
   * also encapsulate the whole program in a function parameterized by the
   * `arguments` variable.
   *)
    val translate : AST.program -> SimpleAST.program

  end = struct

    structure S = SimpleAST
    structure SDCon = SimpleDataCon
    structure SVar = SimpleVar
    structure PTy = PrimType
    structure VMap = Var.Map
    structure B = SimpleBasis

  (* simplification environment; maps AST variables to SimpleAST variables. *)
    datatype env = Env of S.var VMap.map

    val emptyEnv = Env VMap.empty

  (* add a variable binding to the environment *)
    fun bindVar (Env vMap, x, x') = Env(VMap.insert(vMap, x, x'))

  (* look up a variable in the environment *)
    fun lookupVar (Env vMap, x) = (case VMap.find(vMap, x)
           of SOME x' => x'
            | NONE => raise Fail("unbound variable " ^ Var.toString x)
          (* end case *))

    fun repOfExp (AST.E(e, _)) = e
    fun typeOfExp (AST.E(_, ty)) = TypeUtil.prune ty

    val newTmpVar = SVar.fresh "_t"

    fun simplify (AST.PROG{datatycs, body}) = let
        (* create the initial type representation environment *)
          val repInfo = SimplifyType.initial()
        (* analyse the data types *)
          val _ = List.app (SimplifyType.analyze repInfo) datatycs
        (* convert from AST types to primitive types *)
          val cvtTy = SimplifyType.cvtTy repInfo
          val cvtScm = SimplifyType.cvtScheme repInfo
        (* convert from an AST data constructor to a SimpleAST constructor *)
          val cvtDCon = SimplifyType.conRepOf repInfo
        (* convert an AST variable to a SimpleAST variable and add the binding
         * to the environment
         *)
          fun cvtVar (env, x) = let
                val x' = SVar.new (Var.nameOf x, cvtScm(Var.typeOf x))
                in
                  (x', bindVar(env, x, x'))
                end
        (* convert expressions to values (the 𝒱⟦⟧ translation function) *)
          fun expToValue (env, exp, k : S.value -> S.exp) : S.exp = let
              (* fallback case for complicated expressions *)
                fun cvtExp () = let
                      val x' = newTmpVar (cvtTy (typeOfExp exp))
                      in
                        expToRHS (env, exp, fn rhs => S.mkLET(x', rhs, k (S.V_VAR x')))
                      end
                in
                  case repOfExp exp
                   of AST.E_VAR x => k (S.V_VAR(lookupVar(env, x)))
                    | AST.E_CONST(AST.C_DCON dc) => if DataCon.isNullary dc
                        then k (S.V_CON(cvtDCon dc))
                        else raise Fail "unexpected constructor function"
                    | AST.E_CONST(AST.C_INT n) => k (S.V_INT n)
                    | AST.E_CONST(AST.C_STR s) => k (S.V_STR s)
                    | _ => cvtExp ()
                  (* end case *)
                end
        (* convert expressions to variables *)
          and expToVar (env, e, k : SVar.t -> S.exp) =
                expToValue (env, e, fn (S.V_VAR x) => k x | _ => raise Match)
        (* convert expressions to right-hand-sides (the ℛ⟦⟧ translation function) *)
          and expToRHS (env, exp, k : S.rhs -> S.exp) : S.exp = (case repOfExp exp
                 of AST.E_APPLY(e1, e2) => let
                    (* fall-back function for non-primitive/non-constructor applications *)
                      fun expRHS () = k (S.R_EXP(expToExp(env, exp)))
                      fun argsToVals (arity, k) = argsToValues (env, arity, e2, k)
                      fun trFnApp f = (case B.lookup f
                             of B.PrimOp p =>
                                  argsToVals (Prim.arityOf p, fn vs => k (S.R_PRIM(p, vs)))
                              | B.CondOp tst => expRHS ()
                              | B.RTFun cf =>
                                  argsToVals (Runtime.arityOf cf, fn vs => k (S.R_CALL(cf, vs)))
                              | B.UserVar => expRHS ()
                            (* end case *))
                      fun trConApp dc = expToValue (env, e2, fn v =>
                            k (S.R_DCON(cvtDCon dc, v)))
                      in
                        case repOfExp e1
                         of AST.E_VAR f => trFnApp f
                          | AST.E_CONST(AST.C_DCON dc) => trConApp dc
                          | _ => expRHS ()
                        (* end case *)
                      end
                  | AST.E_TUPLE exps => let
                      fun e2v ([], vs) = k (S.R_TUPLE(List.rev vs))
                        | e2v (e::es, vs) = expToValue (env, e, fn v => e2v (es, v::vs))
                      in
                        e2v (exps, [])
                      end
                  | _ => k (S.R_EXP(expToExp (env, exp)))
                (* end case *))
        (* flatten and simplify the arguments to a basis primitive with the given arity *)
          and argsToValues (env, 1, e, k) = expToValue (env, e, fn v => k [v])
            | argsToValues (env, _, AST.E(AST.E_TUPLE exps, _), k) = let
                fun es2vs ([], vs) = k (List.rev vs)
                  | es2vs (e::es, vs) = expToValue (env, e, fn v => es2vs (es, v::vs))
                in
                  es2vs (exps, [])
                end
            | argsToValues _ = raise Fail "unflattened arguments to primitive"
        (* convert an expression to a conditional test *)
          and condToExp (env, exp, tExp, fExp) = let
                (* evaluate the expression to a Bool value and then test it for `True` *)
                fun mkBoolTest () =
                      expToValue (env, exp, fn v => let
                        val caseExp = S.mkCASE(v, [
                                (S.P_CONST B.conTrue, expToExp (env, tExp)),
                                (S.P_CONST B.conFalse, expToExp (env, fExp))
                              ])
                        in
                          (* need to attach case info for later phases *)
                          CaseInfo.set (caseExp, {
                              ty = PTy.Int, argKind = PTy.Unboxed,
                              nConsts = 2, nFuns = 0
                            });
                          caseExp
                        end)
                in
                  case repOfExp exp
                   of AST.E_APPLY(AST.E(AST.E_VAR p, _), AST.E(AST.E_TUPLE[e1, e2], _)) => (
                      case B.lookup p
                       of B.CondOp tst =>
                            expToValue (env, e1, fn v1 =>
                              expToValue (env, e2, fn v2 =>
                                S.mkCOND(tst, [v1, v2],
                                  expToExp (env, tExp),
                                  expToExp (env, fExp))))
                        | _ => mkBoolTest ()
                      (* end case *))
                    | _ => mkBoolTest ()
                  (* end case *)
                end
        (* convert expressions to expressions (the ℰ⟦⟧ translation function) *)
          and expToExp (env, exp) = (case repOfExp exp
                 of AST.E_COND(e1, e2, e3) => condToExp (env, e1, e2, e3)
                  | AST.E_APPLY(e1, e2) => let
                      fun doExp () = expToVar (env, e1, fn f =>
                            expToValue (env, e2, fn v => S.mkAPPLY(f, [v])))
                      fun mkLet rhs = let
                            val x = newTmpVar (cvtTy (typeOfExp exp))
                            in
                              S.mkLET(x, rhs, S.mkRET(S.V_VAR x))
                            end
                      fun argsToVals (arity, k) = argsToValues (env, arity, e2, k)
                      fun trFnApp f = (case B.lookup f
                             of B.PrimOp p =>
                                  argsToVals (Prim.arityOf p, fn vs =>
                                    mkLet (S.R_PRIM(p, vs)))
                              | B.CondOp tst =>
                                (* application of conditional operator outside
                                 * of a conditional context.
                                 *)
                                  argsToVals (PrimCond.arityOf tst, fn vs =>
                                    S.mkCOND(tst, vs,
                                      S.mkRET(S.V_CON B.conTrue),
                                      S.mkRET(S.V_CON B.conFalse)))
                              | B.RTFun cf =>
                                  argsToVals (Runtime.arityOf cf, fn vs =>
                                    mkLet (S.R_CALL(cf, vs)))
                              | B.UserVar => doExp ()
                            (* end case *))
                      in
                        case repOfExp e1
                         of AST.E_VAR f => trFnApp f
                          | AST.E_CONST(AST.C_DCON dc) =>
                              expToValue (env, e2, fn v =>
                                mkLet (S.R_DCON(cvtDCon dc, v)))
                          | _ => doExp ()
                        (* end case *)
                      end
                  | AST.E_LET(dcl, exp) =>
                      dclToExp (env, dcl, fn env' => expToExp (env', exp))
                  | AST.E_CASE(e, rules) => expToValue (env, e, fn (v : S.value) => (
                    (* the rule-patterns are either a single tuple pattern or
                     * else a list of data constructor patterns, possibly
                     * ending in a variable pattern.
                     *)
                      case rules
                       of [(AST.P_TUPLE xs, act)] => let
                            val S.V_VAR tpl = v
                          (* convert the tuple deconstruction to a sequence of Selects *)
                            fun cvt (_, [], env) = expToExp (env, act)
                              | cvt (i, (AST.P_VAR x)::xs, env) = let
                                  val (x', env') = cvtVar (env, x)
                                  in
                                    S.mkLET(x', S.R_SELECT(i, tpl),
                                      cvt (i+1, xs, env'))
                                  end
                              | cvt _ = raise Fail "ill-formed tuple pattern"
                            in
                              cvt (0, xs, env)
                            end
                        | [(AST.P_VAR x, act)] => ( (* trivial case *)
                            case v
                             of S.V_VAR x' => expToExp (bindVar(env, x, x'), act)
                              | _ => let
                                  val (x', env') = cvtVar (env, x)
                                  in
                                    S.mkLET(x', S.R_EXP(S.mkRET(v)),
                                      expToExp(env', act))
                                  end
                            (* end case *))
                        | _ => let
                          (* in this case, the argument must be an instance
                           * of a datatype
                           *)
                            val Types.TyCon(tyc, _) = typeOfExp e
                            val exp' = S.mkCASE(v, List.map (ruleToRule env) rules)
                          (* get the case information for `tyc` *)
                            val dcs = SimplifyType.consOf repInfo tyc
                            val (nn, nf) = List.foldl
                                  (fn (dc, (nn, nf)) =>
                                      if SDCon.isNullary dc then (nn+1, nf) else (nn, nf+1))
                                    (0, 0) (SimplifyType.consOf repInfo tyc)
                            val caseInfo = {
                                    ty = cvtTy(typeOfExp e),
                                    argKind = PTy.kindOf(SimplifyType.tycRepOf repInfo tyc),
                                    nConsts = nn,
                                    nFuns = nf
                                  }
                            in
                              (* record the domain of the case for future use *)
                                CaseInfo.set (exp', caseInfo);
                                exp'
                            end
                      (* end case *)))
                  | _ => expToValue (env, exp, fn v => S.mkRET(v))
                (* end case *))
        (* translate ValBind/FunBind declarations to expressions *)
          and dclToExp (env, dcl, k : env -> S.exp) = (case dcl
                 of AST.B_FUN(f, [AST.P_VAR x], body) => let
                      val (f', env') = cvtVar (env, f)
                    (* convert the parameter to SimpleAST and extend the environment *)
                      val (x', env'') = cvtVar (env', x)
                      val body' = expToExp (env'', body)
                      in
                        S.mkFUN(f', [x'], body', k env')
                      end
                  | AST.B_FUN _ => raise Fail "ill-formed function definition"
                  | AST.B_VAL(AST.P_TUPLE ps, e) => let
                    (* convert `let (x1, ..., xn) =  e; ...` to
                     * to `let tpl = e; let x1 = #1(tpl); ...
                     *)
                      val xs = List.map (fn (AST.P_VAR x) => x) ps
                      val tplTy = PTy.Tuple(List.map (fn x => cvtScm(Var.typeOf x)) xs)
                      val tpl' = SVar.new("tpl", tplTy)
                      fun mkSelect (_, [], env) = k env
                        | mkSelect (i, x::xs, env) = let
                            val (x', env') = cvtVar (env, x)
                            in
                              S.mkLET(x', S.R_SELECT(i, tpl'), mkSelect(i+1, xs, env'))
                            end
                      in
                      (* NOTE: the SimpleAST tuple selection is 0 based *)
                        expToRHS (env, e,
                          fn rhs => S.mkLET(tpl', rhs, mkSelect(0, xs, env)))
                      end
                  | AST.B_VAL(AST.P_VAR x, e) => let
                      val (x', env') = cvtVar (env, x)
                      in
                        expToRHS (env, e, fn rhs => S.mkLET(x', rhs, k env'))
                      end
                  | AST.B_VAL _ => raise Fail "ill-formed value binding"
                (* end case *))
        (* translate AST case rules to SimpleASt *)
          and ruleToRule env (pat, exp) = (case pat
                 of AST.P_CON(dc, AST.P_VAR x) => let
                      val (x', env') = cvtVar (env, x)
                      in
                        (S.P_DCON(cvtDCon dc, x'), expToExp (env', exp))
                      end
                  | AST.P_CONST dc => (S.P_CONST(cvtDCon dc), expToExp (env, exp))
                  | AST.P_TUPLE _ => raise Fail "impossible: TuplePat"
                  | AST.P_VAR x => let
                      val (x', env') = cvtVar (env, x)
                      in
                        (S.P_VAR x', expToExp(env', exp))
                      end
                  | _ => raise Fail "ill-formed pattern"
                (* end case *))
        (* define the `arguments` parameter to the main function *)
          val (arguments', env) = cvtVar (emptyEnv, Basis.varArguments)
        (* the program body has type Unit, but we want to avoid a tail call
         * at the end of the main function, since it is called by C.
         *)
          val body' = expToValue (env, body, fn _ => S.mkRET(S.V_INT 0))
          in
            S.PROG(arguments', body')
          end (* translate *)

    fun translate prog = let
          val prog = NormalizeAST.transform prog
          in
            simplify prog
          end

  end
