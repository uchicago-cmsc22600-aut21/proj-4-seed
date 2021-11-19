(* op-names.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Operator names
 *)

structure OpNames =
  struct

  (* conditional operators *)
    val orelseId = Atom.atom "||"
    val andalsoId = Atom.atom "&&"

  (* infix operators *)
    val asgnId = Atom.atom ":="
    val eqlId = Atom.atom "=="
    val neqId = Atom.atom "!="
    val ltId = Atom.atom "<"
    val lteId = Atom.atom "<="
    val strcatId = Atom.atom "^"
    val consId = Atom.atom "::"
    val addId = Atom.atom "+"
    val subId = Atom.atom "-"
    val mulId = Atom.atom "*"
    val divId = Atom.atom "/"
    val modId = Atom.atom "%"
    val derefId = Atom.atom "!"
    val negId = Atom.atom "unary -"     (* !! different from subId !! *)

  end
