(* tycon.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure TyCon : sig

    type t = Types.tyc

  (* return a new data tycon *)
    val new : BindTree.tycon * TyVar.t list -> t

  (* finish the definition of a data-type constructor by sorting its constructors
   * into canonical order.  This function should be called after all of the
   * constructors have been added to the type.  It returns the completed datatype.
   *)
    val finish : t -> Types.data_ty

  (* return the name of a tycon *)
    val nameOf : t -> string

  (* return a unique string representation of a tycon *)
    val toString : t -> string

  (* return the arity of the type constructor *)
    val arityOf : t -> int

  (* `isPrim tyc` returns true if `tyc` is a primitive type *)
    val isPrim : t -> bool

  (* `consOf tyc` returns the list of data constructors of the datatype `tyc`.
   * It returns `[]` if `tyc` is an abstract type constructor.
   *)
    val consOf : t -> Types.dcon list

  (* are two type constructors the same? *)
    val same : t * t -> bool

  (* total ordering on type constructors *)
    val compare : t * t -> order

  (* sets, finite maps, and hash tables keyed by type constructors *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    structure Ty = Types

    datatype t = datatype Ty.tyc

    fun new (tyc, tvs) = DataTyc{
            id = Stamp.new(),
            name = BindTree.TycId.nameOf tyc,
            params = tvs,
            cons = ref []
          }

    fun finish (DataTyc(dt as {cons, ...})) = let
        (* we order constructors first by constants before functions and,
         * second, by name.
         *)
          fun gt (Ty.DCon{argTy=SOME _, ...}, Ty.DCon{argTy=NONE, ...}) = true
            | gt (Ty.DCon{argTy=NONE, ...}, Ty.DCon{argTy=SOME _, ...}) = false
            | gt (Ty.DCon{name=a, ...}, Ty.DCon{name=b, ...}) = String.>(a, b)
          in
            cons := ListMergeSort.sort gt (!cons);
            dt
          end
      | finish _ = raise Fail "not a datatype"

    fun nameOf (AbsTyc{name, ...}) = name
      | nameOf (DataTyc{name, ...}) = name

    fun toString (AbsTyc{name, id, ...}) = name ^ Stamp.toString id
      | toString (DataTyc{name, id, ...}) = name ^ Stamp.toString id

    fun arityOf (AbsTyc{arity, ...}) = arity
      | arityOf (DataTyc{params, ...}) = List.length params

    fun isPrim (AbsTyc _) = true
      | isPrim _ = false

    fun consOf (DataTyc{cons, ...}) = !cons
      | consOf (AbsTyc _) = []

  (* are two type constructors the same? *)
    fun same (AbsTyc{id=a, ...}, AbsTyc{id=b, ...}) = Stamp.same(a, b)
      | same (DataTyc{id=a, ...}, DataTyc{id=b, ...}) = Stamp.same(a, b)
      | same _ = false

    fun compare (AbsTyc{id=a, ...}, AbsTyc{id=b, ...}) = Stamp.compare(a, b)
      | compare (DataTyc{id=a, ...}, DataTyc{id=b, ...}) = Stamp.compare(a, b)
      | compare (AbsTyc _, DataTyc _) = LESS
      | compare (DataTyc _, AbsTyc _) = GREATER

    structure Key =
      struct
        type ord_key = t
        val compare = compare
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (AbsTyc{id, ...}) = Stamp.hash id
          | hashVal (DataTyc{id, ...}) = Stamp.hash id
        val sameKey = same
      end)

  end
