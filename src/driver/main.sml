(* main.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Driver for ML-Lite compiler.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure Opts = Options
    structure UU = UnixUtil

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

    val isSuccess = OS.Process.isSuccess

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm =
          if Error.anyErrors errStrm
            then raise Error.ERROR
            else ()

  (* verbose printing *)
    val verbose = Opts.verbose
    fun say msg = if !verbose then print(concat("# " :: msg)) else ()

  (* conditionally dump the various IRs to a file *)
    local
      fun dumpIf dump flg arg = if !flg then dump arg else ()
    in
    val dumpPT = dumpIf DumpParseTree.dumpToFile Options.dumpPT
    val dumpBT = dumpIf DumpBindTree.dumpToFile Options.dumpBT
    val dumpAST = dumpIf DumpAST.dumpToFile Options.dumpAST
    val dumpSimple = dumpIf (DumpSimpleAST.dumpToFile "smpl") Options.dumpSimple
    val dumpOpt = dumpIf (DumpSimpleAST.dumpToFile "opt") Options.dumpOpt
    val dumpCFG = dumpIf DumpCFG.dumpToFile Options.dumpCfg
    end (* local *)

  (* process an input file *)
    fun doFile (errStrm, filename) = let
          val base = OS.Path.base filename
        (* parse the input file *)
          val parseTree = let
                val inS = TextIO.openIn filename
                val _ = say ["parsing ", filename, "\n"]
                val pt = Parser.parse (inS, errStrm)
                in
                  TextIO.closeIn inS;
                  checkForErrors errStrm;
                  valOf pt
                end
        (* output parse tree *)
          val _ = dumpPT (base, !Opts.withMarks, parseTree)
        (* binding analysis *)
          val _ = say ["binding analysis ", filename, "\n"]
          val bindTree = Binding.analyze (errStrm, parseTree)
          val _ = checkForErrors errStrm
        (* output binding tree *)
          val _ = dumpBT (base, !Opts.withMarks, bindTree)
        (* type check the program *)
          val _ = say ["type checking ", filename, "\n"]
          val ast = TypeChecker.check (errStrm, bindTree)
          val _ = checkForErrors errStrm
        (* output AST *)
          val _ = dumpAST (base, ast)
        (* convert to SimpleAST *)
          val _ = say ["simplify ", filename, "\n"]
          val simple = Simplify.translate ast
        (* output SimpleAST *)
          val _ = dumpSimple (base, simple)
        (* do a consistency check on the SimpleAST *)
          val _ = if !Options.checkSimple andalso CheckSimple.check ("simplify", false, simple)
		then raise Error.ERROR
                else ()
        (* conditionally optimize the Simple AST code *)
          val simple = let
                val simple = if !Options.optimize
                      then (
                        say ["optimizing ", filename, "\n"];
                        SimpleOpt.transform say simple)
                      else (
                        say ["let-floating ", filename, "\n"];
                        LetFloat.transform simple)
                in
                (* do a consistency check on the SimpleAST *)
                  if !Options.checkSimple
                  andalso CheckSimple.check ("optimization", !Options.optimize, simple)
                    then raise Error.ERROR
                    else ();
                  dumpOpt (base, simple);
                  simple
                end
        (* closure convert to CFG *)
          val _ = say ["closure conversion for ", filename, "\n"]
          val cfg = Closure.transform simple
        (* output CFG IR *)
          val _ = dumpCFG (base, cfg)
        (* check CFG invariants *)
          val _ = CheckCFG.check cfg
          in
          (* code generation *)
            say ["code generation for ", filename, "\n"];
            CodeGen.gen (base, cfg);
          (* final steps to produce an executable *)
            if isSuccess (UU.run ("llc", [base ^ ".ll", "-o", base ^ ".s"]))
            andalso isSuccess (UU.patchAssembly (base ^ ".s"))
            andalso isSuccess (UU.run ("cc", [base ^ ".s", UU.runtimePath(), "-o", base]))
              then ()
              else raise Error.ERROR
          end

    fun handleExn Error.ERROR = OS.Process.failure
      | handleExn exn = (
          err (concat [
              "uncaught exception ", General.exnName exn,
              " [", General.exnMessage exn, "]\n"
            ]);
          List.app
            (fn s => err (concat ["  raised at ", s, "\n"]))
              (SMLofNJ.exnHistory exn);
          OS.Process.failure)

  (* make an error stream for an input file and limit the number of reported errors
   * to 25.
   *)
    fun mkErrStream srcFile = let
	  val errStrm = Error.mkErrStream srcFile
	  in
	    Error.setErrorLimit (errStrm, 25);
	    errStrm
	  end

    fun main (_, opts) = let
          val file = Options.process opts
          in
          (* check that the input file exists *)
            if not (OS.FileSys.access(file, [OS.FileSys.A_READ]))
              then (
                err (concat[
                    "source file \"", file,
                    "\" does not exist or is not readable\n"
                  ]);
                OS.Process.failure)
          (* process the file *)
            else if !Options.dumpTokens
              then let (* scanner test *)
              (* scan the input and print the tokens to <file>.toks *)
                val errStrm = mkErrStream file
                val base = OS.Path.base file
                val outFile = OS.Path.joinBaseExt{base = base, ext = SOME "toks"}
                val inS = TextIO.openIn file
                val outS = TextIO.openOut outFile
                val _ = say ["testing scanner; output to ", outFile, "\n"]
                val sts = Parser.lexer (inS, errStrm, outS)
                in
                  TextIO.closeIn inS; TextIO.closeOut outS;
                  if sts then OS.Process.failure else OS.Process.success
                end
              else let
              (* parse the input and print the parse tree to <file>.pt *)
                val errStrm = mkErrStream file
                fun finish () = if Error.anyErrors errStrm
                      then (
                        Error.report (TextIO.stdErr, errStrm);
                        OS.Process.failure)
                      else (
                        if Error.anyWarnings errStrm
                          then Error.report (TextIO.stdErr, errStrm)
                          else ();
                        OS.Process.success)
                in
                  (doFile (errStrm, file); finish())
                    handle ex => (ignore (finish()); handleExn ex)
                end
          end (* main *)
            handle ex => handleExn ex

  end
