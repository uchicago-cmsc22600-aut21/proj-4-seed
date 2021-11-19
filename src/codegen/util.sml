(* util.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Code generation utility functions.
 *)

structure Util : sig

  (* convert the type of an argument to its corresponding LLVM type.  This
   * conversion has the effect of mapping Boxed and Mixed types to pointers
   * and Unboxed types to integers.
   *)
    val cvtArgType : PrimType.t ->  LLVMType.t

  (* convert CFG types to LLVM types; this function will convert function
   * types to LLVM function types, but it uses cvtArgType for the argument
   * types.
   *)
    val cvtType : PrimType.t -> LLVMType.t

  (* allocate parameters registers for a fragment; these will be in the same
   * order as the fragment's parameter variables.  Note that this function
   * should only be used for entry and join fragments; in other cases the
   * LLVM variables that hold the parameter values will be defined in the
   * predecessor block (or earlier).
   *)
    val getParamRegs : CFGFrag.t -> LLVMReg.t list

  (* get the LLVM type of the function being applied from a CFG application *)
    val getAppliedFunType : CFG.application -> LLVMType.t

  end = struct

    structure PTy = PrimType
    structure LTy = LLVMType

    val intTy = MLLRuntime.intTy
    val ptrTy = MLLRuntime.ptrTy

    fun cvtArgType ty = (case PTy.kindOf ty
           of PTy.Unboxed => intTy
            | _ => ptrTy
          (* end case *))

    fun cvtType PTy.Any = ptrTy
      | cvtType PTy.Obj = ptrTy
      | cvtType PTy.Int = intTy
      | cvtType PTy.String = ptrTy
      | cvtType PTy.Ref = ptrTy
      | cvtType (PTy.Tuple _) = ptrTy
      | cvtType (PTy.Fun(tys, ty)) = LTy.Func(cvtArgType ty, List.map cvtArgType tys)

    fun cvtVar x = LLVMReg.newNamed (CFGVar.nameOf x, cvtArgType (CFGVar.typeOf x))

    fun getParamRegs frag = List.map cvtVar (CFGFrag.paramsOf frag)

    fun getAppliedFunType (retTy, _, args) = LTy.Func(
            cvtArgType retTy,
            List.map (fn arg => cvtArgType(CFGUtil.typeOfValue arg)) args)

  end
