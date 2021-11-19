(* simple-data-con.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Simple AST data constructors.
 *)

structure SimpleDataCon : sig

    type t

    (* representation of data constructors *)
    datatype con_rep
      = Enum of int             (* tagged integer (for nullary constructors) *)
      | Immediate               (* representation is argument *)
      | Box                     (* wrapped in a box *)
      | TaggedBox of int        (* representation as a pair of a tag and value *)

    (* define a new data-constructor *)
    val new : string * PrimType.t option * con_rep -> t

    (* return the constructor's name *)
    val nameOf : t -> string

    (* return a unique string representation of the constructor *)
    val toString : t -> string

    (* compare constructors for equality *)
    val same : t * t -> bool

    (* return true if the constructor is nullary (which means that its rep will be
     * `Enum n` for some `n`.
     *)
    val isNullary : t -> bool

    (* return the representation of the constructor *)
    val repOf : t -> con_rep

    (* sets, finite maps, and hash tables keyed by variables *)
    structure Set : ORD_SET where type Key.ord_key = t
    structure Map : ORD_MAP where type Key.ord_key = t
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

  end = struct

    datatype t = DC of {
        name : string,
        id : Stamp.t,
        argTy : PrimType.t option,
        rep : con_rep
      }

    and con_rep
      = Enum of int
      | Immediate
      | Box
      | TaggedBox of int

    fun new (name, argTy, rep) = DC{
            name = name,
            id = Stamp.new(),
            argTy = argTy,
            rep = rep
          }

    fun nameOf (DC{name, ...}) = name

    fun toString (DC{name, id, ...}) = name ^ Stamp.toString id

    fun same (DC{id=a, ...}, DC{id=b, ...}) = Stamp.same(a, b)

    fun isNullary (DC{rep=Enum _, ...}) = true
      | isNullary _ = false

    fun repOf (DC{rep, ...}) = rep

    structure Key =
      struct
        type ord_key = t
        fun compare (DC{id=a, ...}, DC{id=b, ...}) = Stamp.compare(a, b)
      end
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
        type hash_key = t
        fun hashVal (DC{id, ...}) = Stamp.hash id
        val sameKey = same
      end)

  end

