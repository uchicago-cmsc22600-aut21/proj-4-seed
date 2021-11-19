(* data-con.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Operations of data constructors
 *)

structure DataCon : sig

    type t = Types.dcon

    (* create a new data constructor and add it to the list of data constructors
     * of its data type constructor.
     *)
    val new : Types.tyc * BindTree.conid * Types.ty option -> t

    (* return the name of a data constructor *)
    val nameOf : t -> string

    (* return a unique string representation of a variable *)
    val toString : t -> string

    (* return true if this is a nullary data constructor *)
    val isNullary : t -> bool

    (* return the data-type constructor that the data constructor belongs to *)
    val tycOf : t -> Types.tyc

    (* return the optional argument type of a data constructor *)
    val argTypeOf : t -> Types.ty option

    (* return the polymorphic type of the data constructor.  This will be a
     * function type if the data constructor is not nullary.
     *)
    val typeOf : t -> Types.ty_scheme

    (* are two data constructors the same? *)
    val same : t * t -> bool

    (* sets, finite maps, and hash tables keyed by data constructors *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    structure Ty = Types

    datatype t = datatype Ty.dcon

    fun new (Ty.DataTyc dt, id, optTy) = let
          val {cons, ...} = dt
          val dcon = DCon{
                  id = Stamp.new(),
                  name = BindTree.ConId.nameOf id,
                  argTy = optTy,
                  dty = dt
                }
          in
            cons := dcon :: !cons;
            dcon
          end
      | new (tyc as Ty.AbsTyc{name, ...}, _, _) =
          raise Fail("compiler error: attempt to add dcon to abstract type " ^ name)

    fun nameOf (DCon{name, ...}) = name

    fun toString (DCon{name, id, ...}) = name ^ Stamp.toString id

    fun isNullary (DCon{argTy=NONE, ...}) = true
      | isNullary _ = false

    fun tycOf (DCon{dty, ...}) = Types.DataTyc dty

    fun argTypeOf (DCon{argTy, ...}) = argTy

    fun typeOf (DCon{argTy, dty, ...}) = let
          val tyParams = #params dty
          val resTy = Ty.TyCon(Ty.DataTyc dty, List.map Ty.TyVar tyParams)
          in
            case argTy
             of NONE => (tyParams, resTy)
              | SOME argTy => (tyParams, Ty.TyFun(argTy, resTy))
            (* end case *)
          end

  (* are two data constructors the same? *)
    fun same (DCon{id=a, ...}, DCon{id=b, ...}) = Stamp.same(a, b)

    structure Key =
      struct
        type ord_key = t
        fun compare (DCon{id=a, ...}, DCon{id=b, ...}) = Stamp.compare(a, b)
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (DCon{id, ...}) = Stamp.hash id
        val sameKey = same
      end)

  end
