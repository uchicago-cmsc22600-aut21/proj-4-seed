(* binding.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Binding analysis for ML Lite.
 *)

structure Binding : sig

    (* check the bindings in a ML Lite parse-tree and return a binding-tree
     * representation that makes the connection between binding and use
     * occurrences of identifiers explicit.
     *)
    val analyze : Error.err_stream * ParseTree.program -> BindTree.program

  end = struct

    structure PT = ParseTree
    structure BT = BindTree
    structure AMap = AtomMap
    structure C = Context

    (* dummy binding-trees that we can use when an error prevents us from constructing
     * an actual tree.
     *)
    val bogusTy = BT.TyTuple[]
    val bogusExp = BT.ExpTuple[]
    val bogusPat = BT.PatWild

    (* The following two helper functions are used to process the mark nodes
     * in the parse tree.
     *
     * `chkWithMark wrap chk (cxt, {span, tree})` applies the `chk` function
     * to `tree` using a context that has been updated with the `span`.  The
     * resulting bind-tree form is then paired with span and wrapped by the
     * bind-tree constructor `wrap`.
     *)
    fun chkWithMark wrap chk (cxt, {span, tree}) =
          wrap {span = span, tree = chk (C.setSpan(cxt, span), tree)}

    (* `chkWithMark'` is similar to `chkWithMark`, except that it handles
     * `chk` functions that return an extended context.
     *)
    fun chkWithMark' wrap chk (cxt, {span, tree}) = let
          val (tree', cxt') = chk (C.setSpan(cxt, span), tree)
          in
            (wrap {span = span, tree = tree'}, cxt')
          end

    fun analyze (errS, prog) = let
          (* report an unbound-identifier error *)
          fun unbound (cxt, kind, id) =
                Error.errorAt(errS, C.spanOf cxt, [
                    "unbound ", kind, " `", Atom.toString id, "`"
                  ])
          (* report a duplicate identifier error; the second argument specifies
           * the kind of identifier as a string.
           *)
          fun duplicate (cxt, kind, x) = Error.errorAt (errS, C.spanOf cxt, [
                  "duplicate ", kind, " `", Atom.toString x, "` "
                ])
          (* check a list of bound type variables *)
          fun chkTyVars (cxt, tvs) = let
                fun chkTV (tv, (tvs', tvEnv)) = let
                      val tv' = BT.TyVar.new tv
                      in
                        if AMap.inDomain (tvEnv, tv)
                          then duplicate (cxt, "type variable", tv)
                          else ();
                        (tv'::tvs', AMap.insert(tvEnv, tv, tv'))
                      end
                val (tvs', tvEnv) = List.foldl chkTV ([], AMap.empty) tvs
                in
                  (List.rev tvs', tvEnv)
                end
          (* analyze a program *)
          fun chkProg (cxt, PT.ProgMark m) = chkWithMark BT.ProgMark chkProg (cxt, m)
            | chkProg (cxt, PT.Prog(dcls, exp)) = let
                (* process each of the top-level declarations while accumulating their
                 * bindings in the context.
                 *)
                fun chkDcls (cxt, [], dcls') = BT.Prog(List.rev dcls', chkExp(cxt, exp))
                  | chkDcls (cxt, dcl::dcls, dcls') = let
                      val (dcl', cxt) = chkDcl (cxt, dcl)
                      in
                        chkDcls (cxt, dcls, dcl'::dcls')
                      end
                in
                  chkDcls (cxt, dcls, [])
                end
          (* *)
          and chkDcl (cxt, PT.DclMark m) = chkWithMark' BT.DclMark chkDcl (cxt, m)
            | chkDcl (cxt, PT.DclData(tycId, tvs, cons)) = let
                val tycId' = BT.TycId.new tycId
                val cxt' = C.bindTyCon (cxt, tycId, tycId')
                val (tvs', tvEnv) = chkTyVars (cxt', tvs)
                val (cons', conEnv) = chkCons (C.setTVEnv(cxt', tvEnv), cons)
                in
                  (BT.DclData(tycId', tvs', cons'), C.mergeConEnv(cxt', conEnv))
                end
            | chkDcl (cxt, PT.DclVal vb) = let
                val (vb', cxt') = chkBind (cxt, vb)
                in
                  (BT.DclVal vb', cxt')
                end
          (* convert a list of parse-tree data constructors to binding trees *)
          and chkCons (cxt, cons) = let
                fun chk ([], cxt, cons', conEnv) = (List.rev cons', conEnv)
                  | chk (con::cons, cxt, cons', conEnv) = let
                      fun chkCon (cxt, PT.ConMark{span, tree}) = let
                            val (tree', conEnv') = chkCon (C.setSpan(cxt, span), tree)
                            in
                              (BT.ConMark{span=span, tree=tree'}, conEnv')
                            end
                        | chkCon (cxt, PT.Con(dc, optTy)) = let
                            val dc' = BT.ConId.new dc
                            val optTy' = Option.map (fn ty => chkTy (cxt, ty)) optTy
                            in
                              if AMap.inDomain (conEnv, dc)
                                then duplicate (cxt, "data constructor", dc)
                                else ();
                              (BT.Con(dc', optTy'), AMap.insert(conEnv, dc, dc'))
                            end
                      val (con', conEnv') = chkCon (cxt, con)
                      in
                        chk (cons, cxt, con'::cons', conEnv')
                      end
                val (cons', conEnv) = chk (cons, cxt, [], AMap.empty)
                in
                  (cons', conEnv)
                end
          (* convert types from parse trees to binding trees *)
          and chkTy (cxt, PT.TyMark m) = chkWithMark BT.TyMark chkTy (cxt, m)
            | chkTy (cxt, PT.TyVar tv) = (case C.findTyVar (cxt, tv)
                 of SOME tv' => BT.TyVar tv'
                  | NONE => (unbound (cxt, "type variable", tv); bogusTy)
                (* end case *))
            | chkTy (cxt, PT.TyCon(tyc, tys)) = let
              (* NOTE: we do *not* check the arity of the type constructor here;
               * that check is left for the typechecking pass.
               *)
                val tys' = List.map (fn ty => chkTy(cxt, ty)) tys
                in
                  case C.findTyCon (cxt, tyc)
                   of SOME tyc' => BT.TyCon(tyc', tys')
                    | NONE => (unbound (cxt, "type constructor", tyc); bogusTy)
                  (* end case *)
                end
            | chkTy (cxt, PT.TyFun(ty1, ty2)) =
                BT.TyFun(chkTy(cxt, ty1), chkTy(cxt, ty2))
            | chkTy (cxt, PT.TyTuple tys) =
                BT.TyTuple(List.map (fn ty => chkTy(cxt, ty)) tys)
          (* *)
          and chkBind (cxt, PT.BindMark m) = chkWithMark' BT.BindMark chkBind (cxt, m)
            | chkBind (cxt, PT.BindFun(f, params, e)) = let
                val f' = BT.VarId.new f
                val cxt' = C.bindVar (cxt, f, f')
                fun chkParam (p, (ps', varEnv)) = let
                      val (p', varEnv') = chkPatWithEnv (cxt, p, varEnv)
                      in
                        (p'::ps', varEnv')
                      end
                val (params', varEnv) = List.foldl chkParam ([], AMap.empty) params
                val cxt'' = C.mergeVarEnv (cxt', varEnv)
                in
                  (BT.BindFun(f', List.rev params', chkExp(cxt'', e)), cxt')
                end
            | chkBind (cxt, PT.BindVal(pat, e)) = let
                val (pat', varEnv) = chkPat (cxt, pat)
                in
                  (BT.BindVal(pat', chkExp(cxt, e)), C.mergeVarEnv(cxt, varEnv))
                end
            | chkBind (cxt, PT.BindExp e) = (BT.BindExp(chkExp(cxt, e)), cxt)
          (* *)
          and chkExp (cxt, PT.ExpMark m) =
                chkWithMark BT.ExpMark chkExp (cxt, m)
            | chkExp (cxt, PT.ExpIf(e1, e2, e3)) =
                BT.ExpIf(chkExp(cxt, e1), chkExp(cxt, e2), chkExp(cxt, e3))
            | chkExp (cxt, PT.ExpOrElse(e1, e2)) =
                BT.ExpOrElse(chkExp(cxt, e1), chkExp(cxt, e2))
            | chkExp (cxt, PT.ExpAndAlso(e1, e2)) =
                BT.ExpAndAlso(chkExp(cxt, e1), chkExp(cxt, e2))
            | chkExp (cxt, PT.ExpBin(e1, oper, e2)) =
                BT.ExpBin(chkExp(cxt, e1), C.lookupOp(cxt, oper), chkExp(cxt, e2))
            | chkExp (cxt, PT.ExpListCons(e1, e2)) =
                BT.ExpListCons(chkExp(cxt, e1), chkExp(cxt, e2))
            | chkExp (cxt, PT.ExpUn(oper, e)) =
                BT.ExpUn(C.lookupOp(cxt, oper), chkExp(cxt, e))
            | chkExp (cxt, PT.ExpApp(e1, e2)) =
                BT.ExpApp(chkExp(cxt, e1), chkExp(cxt, e2))
            | chkExp (cxt, PT.ExpVar x) = (case C.findVar (cxt, x)
                 of SOME x' => BT.ExpVar x'
                  | NONE => (unbound (cxt, "variable", x); bogusExp)
                (* end case *))
            | chkExp (cxt, PT.ExpCon dc) = (case C.findCon(cxt, dc)
                 of SOME dc' => BT.ExpCon dc'
                  | NONE => (unbound (cxt, "data constructor", dc); bogusExp)
                (* end case *))
            | chkExp (cxt, PT.ExpInt n) = BT.ExpInt n
            | chkExp (cxt, PT.ExpStr s) = BT.ExpStr s
            | chkExp (cxt, PT.ExpTuple es) =
                BT.ExpTuple(List.map (fn e => chkExp(cxt, e)) es)
            | chkExp (cxt, PT.ExpCase(e, rules)) = BT.ExpCase(
                chkExp(cxt, e),
                List.map (fn r => chkRule(cxt, r)) rules)
            | chkExp (cxt, PT.ExpScope sc) = BT.ExpScope(chkScope(cxt, sc))
          (* *)
          and chkRule (cxt, PT.RuleMark m) = chkWithMark BT.RuleMark chkRule (cxt, m)
            | chkRule (cxt, PT.RuleCase(p, sc)) = let
                val (p', varEnv) = chkPat (cxt, p)
                in
                  BT.RuleCase(p', chkScope(C.mergeVarEnv(cxt, varEnv), sc))
                end
          (* *)
          and chkPatWithEnv (cxt, pat, varEnv) = let
                fun chk (cxt, pat, varEnv) = (case pat
                       of PT.PatMark{span, tree} => let
                            val (tree', varEnv') =
                                  chk (C.setSpan(cxt, span), tree, varEnv)
                            in
                              (BT.PatMark{span=span, tree=tree'}, varEnv')
                            end
                        | PT.PatVar x => let
                            val x' = BT.VarId.new x
                            in
                              if AMap.inDomain (varEnv, x)
                                then duplicate (cxt, "variable", x)
                                else ();
                              (BT.PatVar x', AMap.insert(varEnv, x, x'))
                            end
                        | PT.PatCon(dc, NONE) => (case C.findCon(cxt, dc)
                             of SOME dc' => (BT.PatCon(dc', NONE), varEnv)
                              | NONE => (
                                  unbound (cxt, "data constructor", dc);
                                  (bogusPat, varEnv))
                            (* end case *))
                        | PT.PatCon(dc, SOME p) => (case C.findCon(cxt, dc)
                             of SOME dc' => let
                                  val (pat', varEnv') = chk (cxt, p, varEnv)
                                  in
                                    (BT.PatCon(dc', SOME pat'), varEnv')
                                  end
                              | NONE => (
                                  unbound (cxt, "data constructor", dc);
                                  (bogusPat, varEnv))
                            (* end case *))
                        | PT.PatListCons(p1, p2) => let
                            val (p1', varEnv') = chk (cxt, p1, varEnv)
                            val (p2', varEnv') = chk (cxt, p2, varEnv')
                            in
                              (BT.PatListCons(p1', p2'), varEnv')
                            end
                        | PT.PatTuple pats => let
                            fun chkOne (p, (ps', varEnv)) = let
                                  val (p', varEnv') = chk (cxt, p, varEnv)
                                  in
                                    (p' :: ps', varEnv')
                                  end
                            val (ps', varEnv') = List.foldl chkOne ([], varEnv) pats
                            in
                              (BT.PatTuple(rev ps'), varEnv')
                            end
                        | PT.PatWild => (BT.PatWild, varEnv)
                      (* end case *))
                in
                  chk (cxt, pat, varEnv)
                end
          and chkPat (cxt, pat) = chkPatWithEnv (cxt, pat, AMap.empty)
          (* binding analysis for a scope *)
          and chkScope (cxt, (bnds, exp)) = let
                fun chk (bnd, (bnds', cxt)) = let
                      val (bnd', cxt') = chkBind (cxt, bnd)
                      in
                        (bnd' :: bnds', cxt')
                      end
                val (bnds', cxt') = List.foldl chk ([], cxt) bnds
                in
                  (rev bnds', chkExp(cxt', exp))
                end
          in
            chkProg (C.new errS, prog)
          end (* analyze *)

  end (* Binding *)
