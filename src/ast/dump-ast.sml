(* dump-ast.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Dump the AST data structure in S-Expression syntax.
 *)

(* we map type constructors, data constructors, and variables to unique IDs
 * to check the correctness of bindings.
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

structure DumpAST : sig

  (* `dumpToFile (stem, ast)` dumps the abstract syntax tree `ast` to `stem.ast`. *)
    val dumpToFile : string * AST.program -> unit

  end = struct

    structure T = AST
    structure Ty = Types
    structure Tyc = TyCon

    datatype sexp = datatype SExp.value

  (* Generators for mapping various kinds of identifiers to unique IDs.  Note
   * that we include meta variables here.  Unique IDs are assigned in a
   * left-to-right pre-order traversal of binding sites to ensure a canonical
   * representation.
   *)
    structure TyVarIdGen = IdGen (TyVar.Tbl)
    structure MetaVarIdGen = IdGen (MetaVar.Tbl)
    structure TycIdGen = IdGen (Tyc.Tbl)
    structure ConIdGen = IdGen (DataCon.Tbl)
    structure VarIdGen = IdGen (Var.Tbl)

    type env = {
        idOfTV : TyVar.t -> sexp,
        idOfMV : MetaVar.t -> sexp,
        idOfTyc : Tyc.t -> sexp,
        idOfCon : DataCon.t -> sexp,
        idOfVar : Var.t -> sexp
      }

    fun newEnv () : env = {
            idOfTV = TyVarIdGen.new(),
            idOfMV = MetaVarIdGen.new(),
            idOfTyc = TycIdGen.new(),
            idOfCon = ConIdGen.new(),
            idOfVar = VarIdGen.new()
          }

  (* initialize the ID map for the initial basis *)
    fun initEnv () = let
          val env as {idOfTyc, idOfTV, idOfCon, idOfVar, ...} = newEnv ()
          fun registerTyc tyc = ignore(idOfTyc tyc)
          fun registerCon dc = ignore(idOfCon dc)
          fun registerVar x = ignore(idOfVar x)
          in
          (* Basis type constructors *)
            registerTyc Basis.tycBool;
            registerTyc Basis.tycInt;
            registerTyc Basis.tycList;
            registerTyc Basis.tycRef;
            registerTyc Basis.tycString;
            registerTyc Basis.tycUnit;
          (* Basis data constructors *)
            registerCon Basis.conTrue;
            registerCon Basis.conFalse;
            registerCon Basis.conCons;
            registerCon Basis.conNil;
          (* operators *)
            registerVar Basis.opASSIGN;
            registerVar Basis.opEQ;
            registerVar Basis.opNEQ;
            registerVar Basis.opLTE;
            registerVar Basis.opLT;
            registerVar Basis.opCONCAT;
            registerVar Basis.opADD;
            registerVar Basis.opSUB;
            registerVar Basis.opMUL;
            registerVar Basis.opDIV;
            registerVar Basis.opMOD;
            registerVar Basis.opNEG;
            registerVar Basis.opDEREF;
          (* other variables *)
            registerVar Basis.varArguments;
            registerVar Basis.varChr;
            registerVar Basis.varExit;
            registerVar Basis.varFail;
            registerVar Basis.varNewRef;
            registerVar Basis.varPrint;
            registerVar Basis.varSize;
            registerVar Basis.varSub;
            env
          end

  (* walk the program and assign unique IDs for bound identifiers *)
    fun assignIds (T.PROG{datatycs, body}) = let
          val env = initEnv ()
          fun assignTVId tv = ignore (#idOfTV env tv)
          fun assignMVId mv = ignore (#idOfMV env mv)
          fun assignTycId tyc = ignore (#idOfTyc env tyc)
          fun assignConId dc = ignore (#idOfCon env dc)
          fun assignVarId x = ignore (#idOfVar env x)
          fun doDataTyc (dt : Ty.data_ty) = (
                assignTycId (Ty.DataTyc dt);
                List.app assignTVId (#params dt);
                List.app doDCon (! (#cons dt)))
          and doDCon (dc as Ty.DCon{argTy, ...}) = (
                assignConId dc;
                Option.app doTy argTy)
          and doTy (Ty.TyVar tv) = ()
            | doTy (Ty.TyMeta mv) = assignMVId mv
            | doTy (Ty.TyFun(ty1, ty2)) = (doTy ty1; doTy ty2)
            | doTy (Ty.TyTuple tys) = List.app doTy tys
            | doTy (Ty.TyCon(_, tys)) = List.app doTy tys
            | doTy Ty.TyError = ()
          fun doExp (T.E(e, ty)) = (
                doTy ty;
                case e
                 of T.E_LET(bind, e) => (doBind bind; doExp e)
                  | T.E_COND(e1, e2, e3) => (doExp e1; doExp e2; doExp e3)
                  | T.E_CASE(e, rules) => (doExp e; List.app doRule rules)
                  | T.E_APPLY(e1, e2) => (doExp e1; doExp e2)
                  | T.E_TUPLE es => List.app doExp es
                  | _ => ()
                (* end case *))
          and doBind (T.B_VAL(p, e)) = (doPat p; doExp e)
            | doBind (T.B_FUN(f, ps, e)) = (
                assignVarId f;
                List.app doPat ps;
                doExp e)
          and doRule (p, e) = (doPat p; doExp e)
          and doPat (T.P_CON(_, p)) = doPat p
            | doPat (T.P_CONST _) = ()
            | doPat (T.P_TUPLE ps) = List.app doPat ps
            | doPat (T.P_VAR x) = assignVarId x
          in
            List.app doDataTyc datatycs;
            doExp body;
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
    val mkProg = mkNode "PROG"
    val mkDataTyc = mkNode "DATA"
    val mkDCon = mkNode "CON"
    val mkTyVar = mkNode "TyVar"
    val mkTyMeta = mkNode "TyMeta"
    val mkTyFun = mkNode "TyFun"
    val mkTyTuple = mkNode "TyTuple"
    val mkTyCon = mkNode "TyCon"
    val tyError = SYMBOL(Atom.atom "TyError")
    val mkE = mkNode "E"
    val mkLET = mkNode "LET"
    val mkCOND = mkNode "COND"
    val mkCASE = mkNode "CASE"
    val mkAPPLY = mkNode "APPLY"
    val mkTUPLE = mkNode "TUPLE"
    val mkDCON = mkNode "DCON"
    val mkINT = mkNode "INT"
    val mkSTR = mkNode "STR"
    val mkVAR = mkNode "VAR"
    val mkRULE = mkNode "RULE"
    val mkVAL = mkNode "VAL"
    val mkFUN = mkNode "FUN"
    val mkP_CON = mkNode "P_CON"
    val mkP_CONST = mkNode "P_CONST"
    val mkP_TUPLE = mkNode "P_TUPLE"
    val mkP_VAR = mkNode "P_VAR"
    end (* local *)

    fun list f xs = LIST(List.map f xs)

  (* convert basic values to an S-expression *)
    fun tv2sexp (env : env) tv = LIST[STRING(TyVar.nameOf tv), #idOfTV env tv]
    fun tyc2sexp (env : env) tyc = LIST[STRING(Tyc.nameOf tyc), #idOfTyc env tyc]
    fun dc2sexp (env : env) dc = LIST[STRING(DataCon.nameOf dc), #idOfCon env dc]
    fun var2sexp (env : env) x = LIST[STRING(Var.nameOf x), #idOfVar env x]

    fun prog2sexp (env : env) (T.PROG{datatycs, body}) =
          mkProg (List.map (datatyc2s env) datatycs @ [exp2sexp env body])

    and datatyc2s (env : env) (dty : Ty.data_ty) = (case #params dty
           of [] => mkDataTyc [
                  tyc2sexp env (Ty.DataTyc dty),
                  list (con2sexp env) (! (#cons dty))
                ]
            | tvs => mkDataTyc [
                  tyc2sexp env (Ty.DataTyc dty),
                  list (tv2sexp env) tvs,
                  list (con2sexp env) (! (#cons dty))
                ]
          (* end case *))

    and con2sexp (env : env) dc = (case DataCon.argTypeOf dc
           of SOME ty => mkDCon [dc2sexp env dc, ty2sexp env ty]
            | NONE => mkDCon [dc2sexp env dc]
          (* end case *))

    and tyscm2sexp (env : env) ([], ty) = ty2sexp env ty
      | tyscm2sexp env (tvs, ty) = LIST [
            STRING "FORALL", list (tv2sexp env) tvs, ty2sexp env ty
          ]

    and ty2sexp (env : env) ty = (case TypeUtil.prune ty
           of Ty.TyVar tv => mkTyVar [tv2sexp env tv]
            | Ty.TyMeta mv => mkTyMeta [#idOfMV env mv]
            | Ty.TyFun(ty1, ty2) => mkTyFun [ty2sexp env ty1, ty2sexp env ty2]
            | Ty.TyTuple tys => mkTyTuple (List.map (ty2sexp env) tys)
            | Ty.TyCon(tyc, tys) => mkTyCon [tyc2sexp env tyc, list (ty2sexp env) tys]
            | Ty.TyError => tyError
          (* end case *))

    and exp2sexp (env : env) (T.E(e, ty)) : SExp.value = (case e
           of T.E_LET(bind, e) => mkLET[bind2sexp env bind, exp2sexp env e]
            | T.E_COND(e1, e2, e3) => mkCOND [
                  exp2sexp env e1, exp2sexp env e2, exp2sexp env e3
                ]
            | T.E_APPLY(e1, e2) => mkAPPLY [exp2sexp env e1, exp2sexp env e2]
            | T.E_CASE(e, rules) =>
                mkCASE [exp2sexp env e, list (rule2sexp env) rules]
            | T.E_TUPLE es => mkTUPLE (List.map (exp2sexp env) es)
            | T.E_VAR x => mkVAR [var2sexp env x]
            | T.E_CONST(T.C_DCON dc) => mkDCON [dc2sexp env dc]
            | T.E_CONST(T.C_INT n) => mkINT [INT n]
            | T.E_CONST(T.C_STR s) => mkSTR [STRING s]
          (* end case *))

    and bind2sexp (env : env) (T.B_VAL(p, e)) =
          mkVAL [pat2sexp env p, exp2sexp env e]
      | bind2sexp (env : env) (T.B_FUN(f, ps, e)) = mkFUN (
          var2sexp env f :: STRING ":" :: tyscm2sexp env (Var.typeOf f)
            :: List.map (pat2sexp env) ps
            @ [exp2sexp env e])

    and rule2sexp (env : env) (p, e) = LIST[pat2sexp env p, exp2sexp env e]

    and pat2sexp (env : env) p = (case p
           of T.P_CON(dc, p) => mkP_CON [dc2sexp env dc, pat2sexp env p]
            | T.P_CONST dc => mkP_CONST [dc2sexp env dc]
            | T.P_TUPLE xs => mkP_TUPLE (List.map (pat2sexp env) xs)
            | T.P_VAR x => mkP_VAR [var2sexp env x]
          (* end case *))

    fun dumpToFile (stem, prog) =
          DumpUtil.dump "ast" (stem, prog2sexp (assignIds prog) prog)

  end
