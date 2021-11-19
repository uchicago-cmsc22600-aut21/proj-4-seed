(* dump-parse-tree.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Dump the ParseTree data structure in S-Expression syntax.
 *)

structure DumpParseTree : sig

  (* `dumpToFile (stem, marks, pt)` dumps the parse tree `pt` to `stem.pt`.
   * If `marks` is true, then source-position marks are included in the
   * output.
   *)
    val dumpToFile : string * bool * ParseTree.program -> unit

  end = struct

    structure PT = ParseTree

    datatype sexp = datatype SExp.value

  (* helper functions to construct the S-Expression `(sym ...)` *)
    local
      fun mkNode name = let
            val sym = SYMBOL(Atom.atom name)
            in
              fn args => LIST(sym :: args)
            end
    in
    val mkSPAN = mkNode "SPAN"
    val mkProg = mkNode "Prog"
    val mkDclData = mkNode "DclData"
    val mkDclVal = mkNode "DclVal"
    val mkTyVar = mkNode "TyVar"
    val mkTyCon = mkNode "TyCon"
    val mkTyFun = mkNode "TyFun"
    val mkTyTuple = mkNode "TyTuple"
    val mkCon = mkNode "Con"
    val mkBindFun = mkNode "BindFun"
    val mkBindVal = mkNode "BindVal"
    val mkBindExp = mkNode "BindExp"
    val mkExpIf = mkNode "ExpIf"
    val mkExpOrElse = mkNode "OrElse"
    val mkExpAndAlso = mkNode "AndAlso"
    val mkExpBin = mkNode "ExpBin"
    val mkExpListCons = mkNode "ExpListCons"
    val mkExpUn = mkNode "ExpUn"
    val mkExpApp = mkNode "ExpApp"
    val mkExpVar = mkNode "ExpVar"
    val mkExpCon = mkNode "ExpCon"
    val mkExpInt = mkNode "ExpInt"
    val mkExpStr = mkNode "ExpStr"
    val mkExpTuple = mkNode "ExpTuple"
    val mkExpCase = mkNode "ExpCase"
    val mkExpScope = mkNode "ExpScope"
    val mkRuleCase = mkNode "RuleCase"
    val mkPatVar = mkNode "PatVar"
    val mkPatCon = mkNode "PatCon"
    val mkPatListCons = mkNode "PatListCons"
    val mkPatTuple = mkNode "PatTuple"
    val wildPat = SYMBOL(Atom.atom "PatWild")
    val mkScope = mkNode "SCOPE"
    end (* local *)

  (* convert file position to S-Expression *)
    fun pos2sexp pos = INT(AntlrStreamPos.FilePos.toLarge pos)

  (* conditionally wrap a SPAN around a subtree *)
    fun mark2sexp tree2sexp (false, {span, tree}) = tree2sexp (false, tree)
      | mark2sexp tree2sexp (true, {span=(l, r), tree}) =
          mkSPAN [pos2sexp l, pos2sexp r, tree2sexp(true, tree)]

    fun prog2sexp (marks, PT.ProgMark m) = mark2sexp prog2sexp (marks, m)
      | prog2sexp (marks, PT.Prog(dcls, exp)) = let
          val prog = List.foldr (fn (d, sexps) => dcl2sexp(marks, d) :: sexps)
                [exp2sexp(marks, exp)]
                  dcls
          in
            mkProg prog
          end

    and dcl2sexp (marks, dcl) = (case dcl
           of PT.DclMark m => mark2sexp dcl2sexp (marks, m)
            | PT.DclData(id, tyParams, cons) => mkDclData (
                SYMBOL id ::
                LIST(List.map SYMBOL tyParams) ::
                List.map (fn cd => con2sexp (marks, cd)) cons)
            | PT.DclVal bind => bind2sexp (marks, bind)
          (* end case *))

    and ty2sexp (marks, ty) = let
          fun toSExp ty = (case ty
                 of PT.TyMark m => mark2sexp ty2sexp (marks, m)
                  | PT.TyVar tv => mkTyVar [SYMBOL tv]
                  | PT.TyCon(tyc, tys) => mkTyCon (SYMBOL tyc :: List.map toSExp tys)
                  | PT.TyFun(ty1, ty2) => mkTyFun [toSExp ty1, toSExp ty2]
                  | PT.TyTuple tys => mkTyTuple (List.map toSExp tys)
                (* end case *))
          in
            toSExp ty
          end

    and con2sexp (marks, con) = (case con
           of PT.ConMark m => mark2sexp con2sexp (marks, m)
            | PT.Con(conid, NONE) => mkCon [SYMBOL conid]
            | PT.Con(conid, SOME ty) => mkCon [SYMBOL conid, ty2sexp (marks, ty)]
          (* end case *))

    and bind2sexp (marks, bind) = (case bind
           of PT.BindMark m => mark2sexp bind2sexp (marks, m)
            | PT.BindFun(id, params, exp) => mkBindFun [
                  SYMBOL id,
                  LIST(List.map (fn p => pat2sexp (marks, p)) params),
                  exp2sexp (marks, exp)
                ]
            | PT.BindVal(pat, exp) => mkBindVal [
                  pat2sexp (marks, pat), exp2sexp (marks, exp)
                ]
            | PT.BindExp exp => mkBindExp [exp2sexp (marks, exp)]
          (* end case *))

    and exp2sexp (marks, exp) = let
          fun toSExp exp = (case exp
                 of PT.ExpMark m => mark2sexp exp2sexp (marks, m)
                  | PT.ExpIf(e1, e2, e3) => mkExpIf [toSExp e1, toSExp e2, toSExp e3]
                  | PT.ExpOrElse(e1, e2) => mkExpOrElse [toSExp e1, toSExp e2]
                  | PT.ExpAndAlso(e1, e2) => mkExpAndAlso [toSExp e1, toSExp e2]
                  | PT.ExpBin(e1, id, e2) => mkExpBin [toSExp e1, SYMBOL id, toSExp e2]
                  | PT.ExpListCons(e1, e2) => mkExpListCons [toSExp e1, toSExp e2]
                  | PT.ExpUn(id, e) => mkExpUn [SYMBOL id, toSExp e]
                  | PT.ExpApp(e1, e2) => mkExpApp [toSExp e1, toSExp e2]
                  | PT.ExpVar id => mkExpVar [SYMBOL id]
                  | PT.ExpCon id => mkExpCon [SYMBOL id]
                  | PT.ExpInt n => mkExpInt [INT n]
                  | PT.ExpStr s => mkExpStr [STRING s]
                  | PT.ExpTuple es => mkExpTuple (List.map toSExp es)
                  | PT.ExpCase(exp, rules) => mkExpCase [
                        toSExp exp, LIST(List.map (fn r => rule2sexp(marks, r)) rules)
                      ]
                  | PT.ExpScope scope => scope2sexp (marks, scope)
                (* end case *))
          in
            toSExp exp
          end

    and rule2sexp (marks, ty) = (case ty
           of PT.RuleMark m => mark2sexp rule2sexp (marks, m)
            | PT.RuleCase(pat, scope) => mkRuleCase [
                  pat2sexp (marks, pat), scope2sexp (marks, scope)
                ]
          (* end case *))

    and pat2sexp (marks, pat) = let
          fun toSExp pat = (case pat
                 of PT.PatMark m => mark2sexp pat2sexp (marks, m)
                  | PT.PatVar id => mkPatVar [SYMBOL id]
                  | PT.PatCon(id, NONE) => mkPatCon [SYMBOL id]
                  | PT.PatCon(id, SOME pat) => mkPatCon [SYMBOL id, toSExp pat]
                  | PT.PatListCons(p1, p2) => mkPatListCons [toSExp p1, toSExp p2]
                  | PT.PatTuple pats => mkPatTuple (List.map toSExp pats)
                  | PT.PatWild => wildPat
                (* end case *))
          in
            toSExp pat
          end

    and scope2sexp (marks, (binds, exp)) = mkScope [
            LIST(List.map (fn b => bind2sexp(marks, b)) binds),
            exp2sexp(marks, exp)
          ]

    fun dumpToFile (stem, marks, prog) =
          DumpUtil.dump "pt" (stem, prog2sexp (marks, prog))

  end
