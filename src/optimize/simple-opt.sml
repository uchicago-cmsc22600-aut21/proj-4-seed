(* simple-opt.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure SimpleOpt : sig

  (* `transform say prog` performs the optimization passes on the program.
   * The function `say` is used to report progress.
   *)
     val transform : (string list -> unit) -> SimpleAST.program -> SimpleAST.program

  end = struct

  (* list of passes *)
    val passes = [
            ("let-float", LetFloat.transform),
(* [EXTRA CREDIT]
            ("contract", Contract.transform),
            ("uncurry", Uncurry.transform),
*)
            ("contract", Contract.transform)
          ]

    fun transform say prog = let
          val _ = Census.init prog
          fun apply ((name, transform), prog) = (
                say [name, "\n"];
                transform prog)
          in
            List.foldl apply prog passes
          end

  end
