(* llvm-module.sml
 *
 * COPYRIGHT (c) 2021 Kavon Farvardin and John Reppy
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure LLVMModule : sig

  (* An LLVM module is the container for all of the function and static data definitions
   * that will be produced by the code generator.
   *)
    type t

  (* create a new LLVM module *)
    val new : unit -> t

  (* write the LLVM module to the given output file *)
    val output : string * t -> unit

  (* define a static string literal, where the integer is a header word and the
   * string contains the data.  The allocated string will have the property that
   * it is zero-terminated and padded to a multiple of 8 bytes.
   *)
    val defineString : t * string * IntInf.int * string -> LLVMGlobal.t

  (* define a labeled block of static data.
   *    - the var list must consist of Globals or IConsts.
   *    - The type of the LLVMGlobal for this table should be a [n x i64]*, where
   *      n = length(var list)
   *)
    val defineData : t * LLVMGlobal.t * LLVMVar.t list -> unit

  (* declare an external function (the global's type must be LLVMType.Func) *)
    val declareExtern : t * LLVMGlobal.t -> unit

  (* declare an external variable (the type must be a pointer) *)
    val declareExternVar : t * LLVMGlobal.t -> unit

  end = struct

    structure Rep = LLVMRep
    structure Reg = LLVMReg
    structure Ty = LLVMType
    structure Var = LLVMVar
    structure LG = LLVMGlobal
    structure Label = LLVMLabel

    datatype t = datatype Rep.module

    fun new () = Module {
            globals = ref[],
            funs = ref[]
          }

  (* define a static string.  We pack its bytes into a sequence of 64-bit integers *)
    fun defineString (Module{globals, ...}, name, hdr, s) = let
          fun getWord (0, bytes) = (0, bytes)
            | getWord (n, []) = (0, [])
            | getWord (n, b::bytes) = let
                val (w, bytes) = getWord (n-1, bytes)
                in
                  (IntInf.<< (w, 0w8) + Int.toLarge(Char.ord b), bytes)
                end
          fun cvtToInts ([], words) = List.rev words
            | cvtToInts (bytes, words) = let
                val (w, bytes) = getWord (8, bytes)
              (* need to convert to an integer that is representable as a 64-bit
               * signed int
               *)
                val w = if (w <= 0x7fffffffffffffff)
                        then w
                        else w - 0x10000000000000000 (* subtract 2^64 *)
                in
                  cvtToInts (bytes, Var.const(Ty.Int64, w)::words)
                end
          val words = Var.const(Ty.Int64, hdr) :: cvtToInts (String.explode (s ^ "\000"), [])
          val glob = LLVMGlobal.new (name, Ty.Ptr(Ty.Array(List.length words, Ty.Int64)))
          in
            globals := Rep.Data(glob, words) :: !globals;
            glob
          end

    fun defineData (Module{globals,...}, glob, data) = globals := Rep.Data(glob, data) :: !globals

    fun declareExtern (Module{globals,...}, f) = globals := Rep.ExternFn f :: !globals

    fun declareExternVar (Module{globals,...}, v) = globals := Rep.ExternVar v :: !globals

    fun varUse v = concat[Ty.toString(Var.typeOf v), " ", Var.toString v]
    fun regUse r = concat[Ty.toString(Reg.typeOf r), " ", Reg.toString r]

    fun castKind (fromTy, toTy) = (case (fromTy, toTy)
           of (Ty.Int1, Ty. Ptr _) => "inttoptr"
            | (Ty.Int8, Ty.Ptr _) => "inttoptr"
            | (Ty.Int32, Ty.Ptr _) => "inttoptr"
            | (Ty.Int64, Ty.Ptr _) => "inttoptr"
            | (Ty.Int64, Ty.Func _) => "inttoptr"
            | (Ty.Ptr _, Ty.Int1) => "ptrtoint"
            | (Ty.Ptr _, Ty.Int32) => "ptrtoint"
            | (Ty.Ptr _, Ty.Int64) => "ptrtoint"
            | (Ty.Func _, Ty.Int64) => "ptrtoint"
            | (Ty.Int64, Ty.Int32) => "trunc"
            | (Ty.Int64, Ty.Int8) => "trunc"
            | (Ty.Int64, Ty.Int1) => "trunc"
            | (Ty.Int32, Ty.Int64) => "zext"
            | (Ty.Int32, Ty.Int8) => "trunc"
            | (Ty.Int32, Ty.Int1) => "trunc"
            | (Ty.Int8, Ty.Int64) => "zext"
            | (Ty.Int8, Ty.Int32) => "zext"
            | (Ty.Int8, Ty.Int1) => "trunc"
            | (Ty.Int1, Ty.Int64) => "zext"
            | (Ty.Int1, Ty.Int32) => "zext"
            | (Ty.Int1, Ty.Int8) => "zext"
            | _ => "bitcast"
          (* end case *))

  (* implements lib/IR/Function.cpp, function "getMangledTypeStr" *)
    fun mangledName ty = (case ty
           of Ty.Func(ret, args) => String.concat [
                  "p0f_", mangledName ret, String.concatWithMap "" mangledName args, "f"
                ]
            | Ty.Ptr ty => "p1" ^ mangledName ty
            | Ty.Array (i, ty) => "a" ^ Int.toString i ^ mangledName ty
            (* This is left as a TODO LLVM's source code.
             * Literal structs, aka, those without a typedef'd name, cannot be mangled.
             * If it were typedef'd, then we would just write its name.
             *)
            | Ty.Struct _ => raise Fail "it's not possible yet to mangle literal struct types"
            | Ty.VarArg => "vararg"
            | ty => Ty.toString ty
          (* end case *))

    fun output (file, module as Module{globals, funs}) = let
          val outS = TextIO.openOut file
          fun pr s = TextIO.output(outS, s)
        (* "print line", 1st arg is number of tabs to print before the line.
         * prints a newline at the end.
         *)
          fun prl 0 lst = (List.app pr lst; pr "\n")
            | prl n lst = (pr "\t"; prl (n-1) lst)
        (* a table for statepoint globals *)
          val spTbl = AtomTable.mkTable (List.length(!funs), Fail "statepoint table")
        (* print a function definition *)
          fun doFun (Rep.Func{exported, name, params, entry, body=ref bodyBlks}) = let
                val Ty.Func(retTy, _) = LG.typeOf name
                val params = String.concatWithMap "," regUse params
                in
                  prl 0 [
                      "define ", if exported then "" else "fastcc ",
                      Ty.toString retTy, " ", LG.nameOf name, "(", params,
                      ") gc \"statepoint-example\" {"
                    ];
                  Option.app doBlk (! entry);
                  List.app doBlk (List.rev bodyBlks);
                  pr "}\n\n"
                end
          and doBlk (Rep.Blk{name, closed, phis, body=ref instrs, ...}) = let
                fun prPhi (Rep.Phi(lhs, rhs)) = prl 1 [
                        Reg.toString lhs, " = phi ", Ty.toString(Reg.typeOf lhs), " ",
                        String.concatWithMap ", "
                          (fn (v, lab) => concat[
                              "[ ", Var.toString v, ", %", Label.nameOf lab, " ]"
                            ])
                          rhs
                      ]
                in
                  if (!closed)
                    then ()
                    else print(concat[
                        "WARNING: block ", Label.nameOf name, " is not closed\n"
                      ]);
                  prl 0 [Label.nameOf name, ":"];
                  List.app prPhi (!phis);
                  List.app ((prl 1) o instrToString) instrs
                end
        (****** START OF INSTR FUNS *******)
          and instrToString (Rep.Instr{result, rator, args}) = (case rator
                 of Rep.AddOp => binOp "add" result args
                  | Rep.SubOp => binOp "sub" result args
                  | Rep.MulOp => binOp "mul" result args
                  | Rep.DivOp => binOp "sdiv" result args
                  | Rep.RemOp => binOp "srem" result args
                  | Rep.AShftROp => binOp "ashr" result args
                  | Rep.ShftLOp => binOp "shl" result args
                  | Rep.OrOp => binOp "or" result args
                  | Rep.XorOp => binOp "xor" result args
                  | Rep.AndOp => binOp "and" result args
                  | Rep.EquOp => binOp "icmp eq" result args
                  | Rep.NEqOp => binOp "icmp ne" result args
                  | Rep.GteOp => binOp "icmp sge" result args
                  | Rep.GtOp => binOp "icmp sgt" result args
                  | Rep.LtOp => binOp "icmp slt" result args
                  | Rep.LteOp => binOp "icmp sle" result args
                  | Rep.ULtOp => binOp "icmp ult" result args
                  | Rep.LoadOp => loadOp result args
                  | Rep.GetElemPtrOp => gepOp result args
                  | Rep.CastOp => castOp result args
                  | Rep.CallOp live => callOp (result, args, live)
                  | Rep.TailCallOp => tailCallOp (result, args)
                  | Rep.RetValOp => retValOp result args
                  | Rep.RelocOp => relocOp result args
                (* non-result ops *)
                  | Rep.StoreOp => storeOp args
                  | Rep.Return => retOp args
                  | Rep.Goto => gotoOp args
                  | Rep.CondBr => condBrOp args
                  | Rep.CommentOp => commentOp args
                (* end case *))
(* WARNING: this code works for LLVM 10.0.1, but not for 12.0.1 *)
          and callOp (SOME reg, func :: args, live) = let
              (* we skipped non pointers when assigning indices in llvm-block, so we
               * must do the same here for the live list.
               *)
                fun isGCPtr v = (case Var.typeOf v
                       of Ty.Ptr _ => true
                        | _ => false
                      (* end case *))
                val live = List.filter isGCPtr live
                val funcTy = Var.typeOf func
                val spName = Atom.atom("@llvm.experimental.gc.statepoint." ^ mangledName funcTy)
              (* add the name to the table of statepoint externs *)
                val glob = (case AtomTable.find spTbl spName
                       of NONE => let
                            val spTy = Ty.Func(Ty.Token, [
                                    Ty.Int64,
                                    Ty.Int32,
                                    funcTy,
                                    Ty.Int32,
                                    Ty.Int32,
                                    Ty.VarArg
                                  ])
                            val glob = Rep.Glob{name = spName, ty = spTy}
                            in
                              AtomTable.insert spTbl (spName, glob);
                              glob
                            end
                        | SOME glob => glob
                      (* end case *))
                in [
                  Reg.toString reg,
                  " = call token (i64,i32,", Ty.toString funcTy, ",i32,i32,...) ",
                  LG.nameOf glob, "(i64 0,i32 0,\n\t\t",
                (* function's name *)
                  Ty.toString funcTy, " ", Var.toString func, ",\n\t\t",
                (* function's arity *)
                  "i32 ", Int.toString(List.length args), ",i32 0",
                (* start of args *)
                  if List.null args then "" else ",\n\t\t",
                  String.concatWithMap ", " varUse args,
                  ",\n",
                (* end of args *)
                  "\t\ti32 0,i32 0",
                (* live heap pointers *)
                  if List.null live then "" else ",\n\t\t",
                  String.concatWithMap ", " varUse live,
                  ")"
                ] end
            | callOp _ = raise Fail "ill formed call"
        (* emit the intrinsic call to extract the result from the token *)
          and retValOp (SOME reg) [tok] = let
                val retTy = Reg.typeOf reg
                val spName = Atom.atom("@llvm.experimental.gc.result." ^ mangledName retTy)
              (* add the name to the table of statepoint externs *)
                val glob = (case AtomTable.find spTbl spName
                       of NONE => let
                            val glob = Rep.Glob{name = spName, ty = Ty.Func(retTy, [Ty.Token])}
                            in
                              AtomTable.insert spTbl (spName, glob);
                              glob
                            end
                        | SOME glob => glob
                      (* end case *))
                in [
                  Reg.toString reg, " = call ", Ty.toString retTy, " ",
                  LG.nameOf glob, "(token ", Var.toString tok, ")"
                ] end
            | retValOp _ _ = raise Fail "ill formed retVal"
        (* *)
          and relocOp (SOME reg) [tok, off1, off2] = let
                val retTy = Reg.typeOf reg
                val spName = Atom.atom("@llvm.experimental.gc.relocate." ^ mangledName retTy)
                val glob = (case AtomTable.find spTbl spName
                       of NONE => let
                            val glob = Rep.Glob{
                                    name = spName,
                                    ty = Ty.Func(retTy, [Ty.Token, Ty.Int32, Ty.Int32])
                                  }
                            in
                              AtomTable.insert spTbl (spName, glob);
                              glob
                            end
                        | SOME glob => glob
                      (* end case *))
                in [
                  Reg.toString reg, " = call ", Ty.toString retTy, " ", LG.nameOf glob,
                  "(token ", Var.toString tok, ",i32 ", Var.toString off1,
                  ",i32 ", Var.toString off2, ")"
                ] end
            | relocOp _ _ = raise Fail "ill formed reloc"
        (* emit a tail call *)
          and tailCallOp (SOME reg, func::args) = let
                val Ty.Func(retTy, _) = Var.typeOf func
                in [
                  Reg.toString reg, " = tail call ", Ty.toString retTy,
                  " ", Var.toString func, " (", String.concatWithMap ", " varUse args, ")"
                ] end
            | tailCallOp _ = raise Fail "ill formed tail call"
        (* print a return instruction*)
          and retOp [v] = ["ret ", Ty.toString(Var.typeOf v), " ", Var.toString v]
            | retOp [] = ["ret void"]
            | retOp _ = raise Fail "ill formed return"
        (* print an unconditional branch instruction *)
          and gotoOp ([Rep.Label lab]) = ["br label %", Label.nameOf lab]
            | gotoOp _ = raise Fail "ill formed goto"
        (* print a conditional branch instruction *)
          and condBrOp [cond, Rep.Label tru, Rep.Label fals] = [
                  "br i1 ", Var.toString cond,
                  ", label %", Label.nameOf tru,
                  ", label %", Label.nameOf fals
                ]
            | condBrOp _ = raise Fail "ill formed condBr"
        (* print a comment *)
          and commentOp [Rep.Comment msg] = ["; ", msg]
            | commentOp _ = raise Fail "ill formed comment"
        (* print a type cast instruction*)
          and castOp (SOME reg) [a] = let
                val fromTy = Var.typeOf a
                val toTy = Reg.typeOf reg
                val castName = castKind(fromTy, toTy)
                in [
                  Reg.toString reg, " = ", castName, " ", Ty.toString fromTy, " ",
                  Var.toString a, " to ", Ty.toString toTy
                ] end
            | castOp _ _ = raise Fail "ill formed cast"
        (* print a binary operator *)
          and binOp rator (SOME reg) [a, b] = [
                  Reg.toString reg, " = ", rator, " ",
                  Ty.toString(Var.typeOf a), " ", Var.toString a, ", ", Var.toString b
                ]
            | binOp _ _ _ = raise Fail "ill formed binop"
        (* print a load instruction *)
          and loadOp (SOME reg) ([ptr]) = [
                  Reg.toString reg, " = load ", Ty.toString(Reg.typeOf reg), ", ",
                  Ty.toString(Var.typeOf ptr), " ", Var.toString ptr
                ]
            | loadOp _ _ = raise Fail "ill formed load"
        (* print a store instruction *)
          and storeOp ([valu, ptr]) = [
                  "store ", Ty.toString(Var.typeOf valu), " ",
                  Var.toString valu, ", ", Ty.toString(Var.typeOf ptr), " ", Var.toString ptr
                ]
            | storeOp _ = raise Fail "ill formed store"
        (* print a getelementptr instruction *)
          and gepOp (SOME reg) (ptr :: offsets) = let
                val ptrTy = Var.typeOf ptr
                val SOME pointeeTy = Ty.deref ptrTy
                val offsets = String.concatWithMap ", " varUse offsets
                in [
                    Reg.toString reg, " = getelementptr ", Ty.toString pointeeTy, ", ",
                    Ty.toString ptrTy, " ", Var.toString ptr, ", ", offsets
                ] end
            | gepOp _ _ = raise Fail "ill formed GEP"
        (* print an external function declaration *)
          fun prExternFn f = let
                val Ty.Func(retTy, argTys) = LG.typeOf f
                in
                  prl 0 [
                      "declare ", Ty.toString retTy, " ", LG.nameOf f, "(",
                      String.concatWithMap "," Ty.toString argTys, ")"
                    ]
                end
        (* print out global definitions *)
          fun prGDef (Rep.ExternFn f) = prExternFn f
            | prGDef (Rep.ExternVar v) = let
              (* all global identifiers are pointers to some value, so when
               * we declare it here, we strip the pointer off.
               *)
                val SOME ty = Ty.deref(LG.typeOf v)
                in
                  prl 0 [LG.nameOf v, " = external global ", Ty.toString ty]
                end
            | prGDef (Rep.Data(g, vars)) = let
              (* the consts must be >= 0 so no need to change the ~ to - *)
                fun varToStr (Rep.IConst(_, i)) = "\ti64 " ^ IntInf.toString i
                  | varToStr (Rep.Global glob) = String.concat [
                        "\ti64 ptrtoint (", Ty.toString(LLVMGlobal.typeOf glob), " ",
                        LG.nameOf glob, " to i64)"
                      ]
                  | varToStr _ = raise Fail "illegal data"
                in
                  prl 0 [
                      LG.nameOf g, " = global [",
                      Int.toString(List.length vars), " x i64] [\n",
                      String.concatWithMap ",\n" varToStr vars, "\n]\n"
                    ]
                end
          in
            List.app doFun (List.rev (!funs));
            pr "\n";
            List.app prGDef (List.rev (!globals));
            pr "\n";
            AtomTable.app prExternFn spTbl;
            TextIO.closeOut outS
          end (* output *)

  end
