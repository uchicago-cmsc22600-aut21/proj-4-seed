(* llvm-reg.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure LLVMReg : sig

  (* the abstract type of LLVM pseudo registers *)
    type t

  (* create a new LLVM register with the given name and type *)
    val newNamed : string * LLVMType.t -> t

  (* create a new register with a generated name *)
    val new : LLVMType.t -> t

  (* return the type of a register *)
    val typeOf : t -> LLVMType.t

  (* produce a string representation of a register *)
    val toString : t -> string

  (* are two registers the same? *)
    val same : t * t -> bool

  end = struct

    structure Rep = LLVMRep

    datatype t = datatype Rep.reg

    val cnt = ref 1

    fun newNamed (name, ty) = let
          val id = !cnt
          in
            cnt := id + 1;
            Reg{name = Rep.mangle name, ty = ty, id = id}
          end

  (* we need the "r" prefix to avoid the requirement that registers be defined in
   * numeric order.
   *)
    fun new ty = newNamed ("r", ty)

    fun typeOf (Reg{ty, ...}) = ty

    fun toString (Reg{name, id, ...}) = concat["%", name, Int.toString id]

    fun same (Reg{id=a, ...}, Reg{id=b, ...}) = (a = b)

  end
