(* simplify-type.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Support for determining the representation of type and data constructors, as well
 * as translation from AST types to primitive types.  This information is computed
 * by demand.
 *)

structure SimplifyType : sig

  (* information about type-constructor and data-constructor representations *)
    type t

  (* initialize the representation information for the Basis type constructors and
   * data constructors.
   *)
    val initial : unit -> t

  (* get representation info *)
    val tycRepOf : t -> TyCon.t -> PrimType.t
    val conRepOf : t -> DataCon.t -> SimpleDataCon.t

  (* return the list of SimpleAST data constructors for a type constructor (only
   * valid after the type constructor has been analyzed).  The list will have
   * nullary constructors first, followed by constructor functions.
   *)
    val consOf : t -> TyCon.t -> SimpleDataCon.t list

  (* translate an AST type to a primitive type *)
    val cvtTy : t -> Types.ty -> PrimType.t

  (* translate an AST type scheme to a primitive type *)
    val cvtScheme : t -> Types.ty_scheme -> PrimType.t

  (* analyse a data type and its constructors *)
    val analyze : t -> Types.data_ty -> unit

  end = struct

    structure B = Basis
    structure DC = DataCon
    structure TycTbl = TyCon.Tbl
    structure ConTbl = DC.Tbl
    structure SDCon = SimpleDataCon
    structure PTy = PrimType

  (* con_rep = Enum of int | Immediate | Box | TaggedBox of int *)
    datatype con_rep = datatype SDCon.con_rep

    datatype t = Info of {
        tycRep : PTy.t TycTbl.hash_table,
        conRep : SDCon.t ConTbl.hash_table
      }

    fun tycRepOf (Info{tycRep, ...}) = TycTbl.lookup tycRep
    fun conRepOf (Info{conRep, ...}) = ConTbl.lookup conRep

    fun initial () = let
          val tycRep = TycTbl.mkTable (32, Fail "TyCon rep table")
          val conRep = ConTbl.mkTable (32, Fail "DataCon rep table")
          val insTycRep = TycTbl.insert tycRep
          val insConRep = ConTbl.insert conRep
          fun mkCon (dc, argTy, rep) = (dc, SDCon.new(DC.nameOf dc, argTy, rep))
          val consTy = PTy.Tuple[PTy.Any, PTy.Any]
          in
            List.app insTycRep [
                (B.tycBool, PTy.Int),
                (B.tycInt, PTy.Int),
                (B.tycList, PTy.Any),
                (B.tycRef, PTy.Ref),
                (B.tycString, PTy.String),
                (B.tycUnit, PTy.Int)
              ];
            List.app insConRep [
                (B.conTrue, SimpleBasis.conTrue),
                (B.conFalse, SimpleBasis.conFalse),
                (B.conCons, SimpleBasis.conCons),
                (B.conNil, SimpleBasis.conNil)
              ];
            Info{tycRep = tycRep, conRep = conRep}
          end

  (* get the AST argument type of a data constructor function *)
    fun argTyOf dc = DC.argTypeOf

  (* convert an AST type to a primitive type using the given mapping for
   * type constructors
   *)
    fun cvtType tycRepOf = let
          fun cvt ty = (case TypeUtil.prune ty
                 of Types.TyVar _ => PTy.Any
                  | Types.TyMeta _ => PTy.Any (* unresolved meta type *)
                  | Types.TyCon(tyc, _) => tycRepOf tyc
                  | Types.TyFun(ty1, ty2) => PTy.Fun([cvt ty1], cvt ty2)
                  | Types.TyTuple tys => PTy.Tuple(List.map cvt tys)
                  | Types.TyError => raise Fail "unexpected ErrorTy"
                (* end case *))
          in
            cvt
          end

    fun consOf info tyc = List.map (conRepOf info) (TyCon.consOf tyc)

    fun cvtTy (Info{tycRep, ...}) = cvtType (TycTbl.lookup tycRep)

    fun cvtScheme info = let
          val cvt = cvtTy info
          in
            fn (_, ty) => cvt ty
          end

    fun analyze (info as Info{tycRep, conRep}) dt = let
          val tyc = Types.DataTyc dt
          val insTycRep = TycTbl.insert tycRep
          val tycRepOf = TycTbl.lookup tycRep
          val insConRep = ConTbl.insert conRep
          fun mkCon (dc, rep) = insConRep (dc, SDCon.new(DC.nameOf dc, NONE, rep))
          fun mkConFn (dc, argTy, rep) =
                insConRep (dc, SDCon.new(DC.nameOf dc, SOME argTy, rep))
          val cvtTy = cvtType tycRepOf
        (* convert the argument type of a data-constructor function *)
          fun cvtArgTy dc = (case DC.argTypeOf dc
                 of SOME ty => cvtTy ty
                  | NONE => raise Fail "impossible: not a function"
                (* end case *))
        (* assign enumeration values for nullary data constructors *)
          fun assignEnumTags ubDCs =
                List.appi (fn (i, dc) => mkCon(dc, Enum i)) ubDCs
        (* assign enumeration values for data-constructor functions *)
          fun assignBoxTags bDCs = List.appi
                (fn (i, dc) => mkConFn (dc, cvtArgTy dc, TaggedBox i))
                  bDCs
          in
          (* we initially assign the tyc AnyTy in case `tyc` is recursive *)
            insTycRep (tyc, PTy.Any);
            case List.partition DC.isNullary (! (#cons dt))
             of ([], [dc]) => let
                (* a single constructor is represented as its argument *)
                  val argTy = cvtArgTy dc
                  in
                    insTycRep (tyc, argTy);
                    mkConFn (dc, argTy, Immediate)
                  end
              | ([], bDCs) => (
                (* multiple data-constructor functions are represented as tagged boxes *)
                  insTycRep (tyc, PTy.Obj);
                  assignBoxTags bDCs)
              | (ubDCs, []) => (
                (* only nullary constructors, which are represented as integers *)
                  insTycRep (tyc, PTy.Int);
                  assignEnumTags ubDCs)
              | (ubDCs, [dc]) => let
                (* representation of single data-constructor function depends on
                 * its argument type.
                 *)
                  val argTy = cvtArgTy dc
                  in
                    (* the tyc's representation is already AnyTy *)
                    assignEnumTags ubDCs;
                    case PTy.kindOf argTy
                     of PTy.Boxed => (* we represent the constructor as its argument pointer *)
                          mkConFn (dc, argTy, Immediate)
                      | _ => (* we need a box around the argument *)
                          mkConFn (dc, argTy, Box)
                    (* end case *)
                  end
              | (ubDCs, bDCs) => (
                (* multiple data-constructor functions are represented as tagged boxes *)
                  insTycRep (tyc, PTy.Any);
                  assignEnumTags ubDCs;
                  assignBoxTags bDCs)
            (* end case *)
          end

  end
