(* dump-bind-tree.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Dump the BindTree data structure in S-Expression syntax.
 *)

(* we map type variables, type constructors, data constructors, and
 * variables to unique IDs to check the correctness of bindings.
 *)
functor IdGen (Tbl : MONO_HASH_TABLE) : sig
    type item = Tbl.Key.hash_key
    type id = SExp.value
  (* create a new item to ID mapping *)
    val new : unit -> item -> id
  end = struct

    type item = Tbl.Key.hash_key
    type id = SExp.value

    fun new () = let
          val nextId = ref 0
          val idTbl = Tbl.mkTable (32, Fail "IdGen")
          val find = Tbl.find idTbl
          val ins = Tbl.insert idTbl
          in
            fn item => (case find item
                 of NONE => let
                      val id = !nextId
                      val sexp = SExp.INT(IntInf.fromInt id)
                      in
                        nextId := id + 1;
                        ins (item, sexp);
                        sexp
                      end
                  | SOME id => id
                (* end case *))
          end

  end (* functor IdGen *)

structure DumpBindTree : sig

  (* `dumpToFile (stem, marks, pt)` dumps the parse tree `pt` to `stem.pt`.
   * If `marks` is true, then source-position marks are included in the
   * output.
   *)
    val dumpToFile : string * bool * BindTree.program -> unit

  end = struct

    structure BT = BindTree

    datatype sexp = datatype SExp.value

  (* Generators for mapping binding-tree identifiers to unique IDs.
   * We do this in a left-to-right pre-order traversal of binding
   * sites to ensure a canonical representation.
   *)
    structure TyVarGen = IdGen (BT.TyVar.Tbl)
    structure TycIdGen = IdGen (BT.TycId.Tbl)
    structure ConIdGen = IdGen (BT.ConId.Tbl)
    structure VarIdGen = IdGen (BT.VarId.Tbl)

    type env = {
        marks : bool,
        idOfTV : BT.TyVar.t -> sexp,
        idOfTyc : BT.TycId.t -> sexp,
        idOfCon : BT.ConId.t -> sexp,
        idOfVar : BT.VarId.t -> sexp
      }

    fun newEnv marks : env = {
            marks = marks,
            idOfTV = TyVarGen.new(),
            idOfTyc = TycIdGen.new(),
            idOfCon = ConIdGen.new(),
            idOfVar = VarIdGen.new()
          }

  (* initialize the ID map for the initial basis *)
    fun initEnv marks = let
          val env as {idOfTyc, idOfCon, idOfVar, ...} = newEnv marks
          fun registerTyc tyc = ignore(idOfTyc tyc)
          fun registerCon dc = ignore(idOfCon dc)
          fun registerVar x = ignore(idOfVar x)
          in
          (* Basis type constructors *)
            registerTyc BindBasis.tycBool;
            registerTyc BindBasis.tycInt;
            registerTyc BindBasis.tycList;
            registerTyc BindBasis.tycRef;
            registerTyc BindBasis.tycString;
            registerTyc BindBasis.tycUnit;
          (* BindBasis data constructors *)
            registerCon BindBasis.conTrue;
            registerCon BindBasis.conFalse;
            registerCon BindBasis.conCons;
            registerCon BindBasis.conNil;
          (* binary operators *)
            registerVar BindBasis.opASSIGN;
            registerVar BindBasis.opEQ;
            registerVar BindBasis.opNEQ;
            registerVar BindBasis.opLTE;
            registerVar BindBasis.opLT;
            registerVar BindBasis.opCONCAT;
            registerVar BindBasis.opADD;
            registerVar BindBasis.opSUB;
            registerVar BindBasis.opMUL;
            registerVar BindBasis.opDIV;
            registerVar BindBasis.opMOD;
          (* unary operators *)
            registerVar BindBasis.opNEG;
            registerVar BindBasis.opDEREF;
          (* other variables *)
            registerVar BindBasis.varChr;
            registerVar BindBasis.varFail;
            registerVar BindBasis.varNewRef;
            registerVar BindBasis.varPrint;
            registerVar BindBasis.varSize;
            registerVar BindBasis.varSub;
            env
          end

  (* walk the program and assign unique IDs for bound identifiers *)
    fun assignIds (marks, prog) = let
          val env = initEnv marks
          fun assignTVar tv = ignore (#idOfTV env tv)
          fun assignTycId tyc = ignore (#idOfTyc env tyc)
          fun assignConId dc = ignore (#idOfCon env dc)
          fun assignVarId x = ignore (#idOfVar env x)
          fun doProg (BT.ProgMark{tree, ...}) = doProg tree
            | doProg (BT.Prog(dcls, exp)) = (List.app doDcl dcls; doExp exp)
          and doDcl (BT.DclMark{tree, ...}) = doDcl tree
            | doDcl (BT.DclData(tyc, tvs, cons)) = (
                assignTycId tyc;
                List.app assignTVar tvs;
                List.app doCon cons)
            | doDcl (BT.DclVal bind) = doBind bind
          and doCon (BT.ConMark{tree, ...}) = doCon tree
            | doCon (BT.Con(dc, _)) = assignConId dc
          and doBind (BT.BindMark{tree, ...}) = doBind tree
            | doBind (BT.BindFun(f, params, e)) = (
                assignVarId f;
                List.app doPat params;
                doExp e)
            | doBind (BT.BindVal(pat, e)) = (
                doPat pat;
                doExp e)
            | doBind (BT.BindExp e) = doExp e
          and doExp (BT.ExpMark{tree, ...}) = doExp tree
            | doExp (BT.ExpIf(e1, e2, e3)) = (doExp e1; doExp e2; doExp e3)
            | doExp (BT.ExpOrElse(e1, e2)) = (doExp e1; doExp e2)
            | doExp (BT.ExpAndAlso(e1, e2)) = (doExp e1; doExp e2)
            | doExp (BT.ExpBin(e1, _, e2)) = (doExp e1; doExp e2)
            | doExp (BT.ExpListCons(e1, e2)) = (doExp e1; doExp e2)
            | doExp (BT.ExpUn(_, e)) = doExp e
            | doExp (BT.ExpApp(e1, e2)) = (doExp e1; doExp e2)
            | doExp (BT.ExpTuple es) = List.app doExp es
            | doExp (BT.ExpCase(e, rules)) = (doExp e; List.app doRule rules)
            | doExp (BT.ExpScope scope) = doScope scope
            | doExp _ = ()
          and doRule (BT.RuleMark{tree, ...}) = doRule tree
            | doRule (BT.RuleCase(pat, scope)) = (doPat pat; doScope scope)
          and doPat (BT.PatMark{tree, ...}) = doPat tree
            | doPat (BT.PatVar x) = assignVarId x
            | doPat (BT.PatCon(dc, optPat)) = Option.app doPat optPat
            | doPat (BT.PatListCons(p1, p2)) = (doPat p1; doPat p2)
            | doPat (BT.PatTuple ps) = List.app doPat ps
            | doPat BT.PatWild = ()
          and doScope (binds, e) = (List.app doBind binds; doExp e)
          in
            doProg prog;
            env
          end

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

  (* convert bind-tree identifiers to S-expressions *)
    fun tv2sexp (env : env) tv = LIST[STRING(BT.TyVar.nameOf tv), #idOfTV env tv]
    fun tyc2sexp (env : env) tyc = LIST[STRING(BT.TycId.nameOf tyc), #idOfTyc env tyc]
    fun con2sexp (env : env) dc = LIST[STRING(BT.ConId.nameOf dc), #idOfCon env dc]
    fun var2sexp (env : env) x = LIST[STRING(BT.VarId.nameOf x), #idOfVar env x]

  (* conditionally wrap a SPAN around a subtree *)
    fun mark2sexp tree2sexp (env : env, {span=(l, r), tree}) =
          if #marks env
            then mkSPAN [pos2sexp l, pos2sexp r, tree2sexp(env, tree)]
            else tree2sexp (env, tree)

    fun prog2sexp (env : env, BT.ProgMark m) = mark2sexp prog2sexp (env, m)
      | prog2sexp (env, BT.Prog(dcls, exp)) = let
          val prog = List.foldr (fn (d, sexps) => dcl2sexp(env, d) :: sexps)
                [exp2sexp(env, exp)]
                  dcls
          in
            mkProg prog
          end

    and dcl2sexp (env : env, dcl) = (case dcl
           of BT.DclMark m => mark2sexp dcl2sexp (env, m)
            | BT.DclData(id, tvs, cons) => mkDclData (
                tyc2sexp env id ::
                LIST(List.map (tv2sexp env) tvs) ::
                List.map (fn con => conDcl2sexp (env, con)) cons)
            | BT.DclVal bind => bind2sexp (env, bind)
          (* end case *))

    and ty2sexp (env : env, ty) = let
          fun toSExp ty = (case ty
                 of BT.TyMark m => mark2sexp ty2sexp (env, m)
                  | BT.TyVar tv => tv2sexp env tv
                  | BT.TyCon(tyc, tys) =>
                      mkTyCon (tyc2sexp env tyc :: List.map toSExp tys)
                  | BT.TyFun(ty1, ty2) => mkTyFun [toSExp ty1, toSExp ty2]
                  | BT.TyTuple tys => mkTyTuple (List.map toSExp tys)
                (* end case *))
          in
            toSExp ty
          end

    and conDcl2sexp (env, con) = (case con
           of BT.ConMark m => mark2sexp conDcl2sexp (env, m)
            | BT.Con(conid, NONE) => mkCon [con2sexp env conid]
            | BT.Con(conid, SOME ty) => mkCon [con2sexp env conid, ty2sexp (env, ty)]
          (* end case *))

    and bind2sexp (env, bind) = (case bind
           of BT.BindMark m => mark2sexp bind2sexp (env, m)
            | BT.BindFun(id, params, exp) => mkBindFun [
                  var2sexp env id,
                  LIST(List.map (fn p => pat2sexp (env, p)) params),
                  exp2sexp (env, exp)
                ]
            | BT.BindVal(pat, exp) => mkBindVal [
                  pat2sexp (env, pat), exp2sexp (env, exp)
                ]
            | BT.BindExp exp => mkBindExp [exp2sexp (env, exp)]
          (* end case *))

    and exp2sexp (env, exp) = let
          val var2sexp = var2sexp env
          fun toSExp exp = (case exp
                 of BT.ExpMark m => mark2sexp exp2sexp (env, m)
                  | BT.ExpIf(e1, e2, e3) => mkExpIf [toSExp e1, toSExp e2, toSExp e3]
                  | BT.ExpOrElse(e1, e2) => mkExpOrElse [toSExp e1, toSExp e2]
                  | BT.ExpAndAlso(e1, e2) => mkExpAndAlso [toSExp e1, toSExp e2]
                  | BT.ExpBin(e1, id, e2) => mkExpBin [toSExp e1, var2sexp id, toSExp e2]
                  | BT.ExpListCons(e1, e2) => mkExpListCons [toSExp e1, toSExp e2]
                  | BT.ExpUn(id, e) => mkExpUn [var2sexp id, toSExp e]
                  | BT.ExpApp(e1, e2) => mkExpApp [toSExp e1, toSExp e2]
                  | BT.ExpVar id => mkExpVar [var2sexp id]
                  | BT.ExpCon id => mkExpCon [con2sexp env id]
                  | BT.ExpInt n => mkExpInt [INT n]
                  | BT.ExpStr s => mkExpStr [STRING s]
                  | BT.ExpTuple es => mkExpTuple (List.map toSExp es)
                  | BT.ExpCase(exp, rules) => mkExpCase [
                        toSExp exp, LIST(List.map (fn r => rule2sexp(env, r)) rules)
                      ]
                  | BT.ExpScope scope => scope2sexp (env, scope)
                (* end case *))
          in
            toSExp exp
          end

    and rule2sexp (env, ty) = (case ty
           of BT.RuleMark m => mark2sexp rule2sexp (env, m)
            | BT.RuleCase(pat, scope) => mkRuleCase [
                  pat2sexp (env, pat), scope2sexp (env, scope)
                ]
          (* end case *))

    and pat2sexp (env, pat) = let
          val var2sexp = var2sexp env
          val con2sexp = con2sexp env
          fun toSExp pat = (case pat
                 of BT.PatMark m => mark2sexp pat2sexp (env, m)
                  | BT.PatVar id => mkPatVar [var2sexp id]
                  | BT.PatCon(id, NONE) => mkPatCon [con2sexp id]
                  | BT.PatCon(id, SOME pat) => mkPatCon [con2sexp id, toSExp pat]
                  | BT.PatListCons(p1, p2) => mkPatListCons [toSExp p1, toSExp p2]
                  | BT.PatTuple pats => mkPatTuple (List.map toSExp pats)
                  | BT.PatWild => wildPat
                (* end case *))
          in
            toSExp pat
          end

    and scope2sexp (env, (binds, exp)) = mkScope [
            LIST(List.map (fn b => bind2sexp(env, b)) binds),
            exp2sexp(env, exp)
          ]

    fun dumpToFile (stem, env, prog) = let
          val env = assignIds (env, prog)
          in
            DumpUtil.dump "bt" (stem, prog2sexp (env, prog))
          end

  end
