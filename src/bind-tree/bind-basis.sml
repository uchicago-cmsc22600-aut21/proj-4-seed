(* bind-basis.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * BindTree identifiers for builtin types, constructors, and variables.
 *)

structure BindBasis =
  struct

    local
      val newTyc = BindTree.TycId.new
      val newCon = BindTree.ConId.new
      val newVar = BindTree.VarId.new
    in

    (* type constructors *)
    val tycBool = newTyc (Atom.atom "Bool")
    val tycInt = newTyc (Atom.atom "Int")
    val tycList = newTyc (Atom.atom "List")
    val tycRef = newTyc (Atom.atom "Ref")
    val tycString = newTyc (Atom.atom "String")
    val tycUnit = newTyc (Atom.atom "Unit")

    (* data constructors *)
    val conFalse = newCon (Atom.atom "False")
    val conNil = newCon (Atom.atom "Nil")
    val conTrue = newCon (Atom.atom "True")
    val conCons = newCon OpNames.consId

    (* variables *)
    val varArguments = newVar (Atom.atom "arguments")
    val varChr = newVar (Atom.atom "chr")
    val varExit = newVar (Atom.atom "exit")
    val varFail = newVar (Atom.atom "fail")
    val varNewRef = newVar (Atom.atom "newRef")
    val varPrint = newVar (Atom.atom "print")
    val varSize = newVar (Atom.atom "size")
    val varSub = newVar (Atom.atom "sub")

    (* binary operators *)
    val opASSIGN = newVar OpNames.asgnId
    val opEQ = newVar OpNames.eqlId
    val opNEQ = newVar OpNames.neqId
    val opLT = newVar OpNames.ltId
    val opLTE = newVar OpNames.lteId
    val opCONCAT = newVar OpNames.strcatId
    val opADD = newVar OpNames.addId
    val opSUB = newVar OpNames.subId
    val opMUL = newVar OpNames.mulId
    val opDIV = newVar OpNames.divId
    val opMOD = newVar OpNames.modId

    (* unary operators *)
    val opDEREF = newVar OpNames.derefId
    val opNEG = newVar OpNames.negId

    end (* local *)
  end
