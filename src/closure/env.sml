(* env.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * The translation environment for closure conversion.  The environment has both
 * imperative and functional components.
 *)

structure Env : sig

    type t

  (* create a new environment *)
    val new : unit -> t

  (* add a CFG function to the list of functions *)
    val addFun : t * CFGFunct.t -> unit

  (* get the list of functions from the environment; the functions are returned in the
   * reverse order in which they were added.  Note that this operation clears the list.
   *)
    val getFuns : t -> CFGFunct.t list

  (* `newFun (env, f, isSelfTailRec)` returns an environment for translating the
   * function `f`.  The returned environment has a definition for the header label
   * if the function is self-tail recursive, an empty list of fragments, and an empty
   * variable mapping.  Note that the argument environment's fragment list is **not**
   * modified by this function, so have multiple functions under translation at the
   * same time.
   *)
    val newFun : t * SimpleVar.t * bool -> t

  (* get the current function *)
    val currentFun : t -> SimpleVar.t

  (* get the header-fragment label; this raises `Fail` if no such label is set *)
    val getHeaderLable : t -> CFGLabel.t

  (* record a non-entry fragment in the current function *)
    val addFrag : t * CFG.frag -> unit

  (* get the fragments for the current function; the fragments are returned in the
   * reverse order in which they were added.  Note that this operation clears the list.
   *)
    val getFrags : t -> CFG.frag list

  (* reset the environment to process a new fragment; the resulting environment has
   * an empty variable mapping.
   *)
    val newFrag : t -> t

  (* lookup the CFG value that a SimpleAST variable maps to in the environment *)
    val lookup : t * SimpleVar.t -> CFG.value

  (* bind a variable to a CFG value *)
    val bind : t * SimpleVar.t * CFG.value -> t

  end = struct

    structure SV = SimpleVar
    structure VMap = SimpleVar.Map
    structure CV = CFGVar

    datatype t = Env of {
        funs : CFGFunct.t list ref,     (* the translated functions *)
        frags : CFGFrag.t list ref,     (* the fragments for the current function *)
        curFn : SV.t,                   (* the current function being translated *)
        hdrLab : CFGLabel.t option,     (* the optional header label for the current function *)
        vMap : CFG.value VMap.map
      }

    fun new () = Env{
            funs = ref [],
            frags = ref [],
            curFn = SV.new("*dummy*", PrimType.Any),
            hdrLab = NONE,
            vMap = VMap.empty
          }

    fun addFun (Env{funs, ...}, func) = (funs := func :: !funs)

    fun getFuns (Env{funs, ...}) = !funs before (funs := [])

    fun newFun (Env{funs, ...}, f, isSelfTailRec) = Env{
            funs = funs,
            frags = ref [],
            curFn = f,
            hdrLab = if isSelfTailRec
              then SOME(CFGLabel.new(SV.toString f ^ "_hdr"))
              else NONE,
            vMap = VMap.empty
          }

    fun currentFun (Env{curFn, ...}) = curFn

    fun getHeaderLable (Env{hdrLab=SOME lab, ...}) = lab
      | getHeaderLable (Env{curFn, ...}) = raise Fail(concat[
            "no header label for '", SV.toString curFn, "'"
          ])

    fun addFrag (Env{frags, ...}, frag) = (frags := frag :: !frags)

    fun getFrags (Env{frags, ...}) = !frags before (frags := [])

    fun newFrag (Env{funs, frags, curFn, hdrLab, ...}) = Env{
            funs = funs,
            frags = frags,
            curFn = curFn,
            hdrLab = hdrLab,
            vMap = VMap.empty
          }

    fun lookup (Env{vMap, ...}, x) = (case VMap.find(vMap, x)
           of SOME v => v
            | NONE => raise Fail(concat["lookup: no mapping for '", SV.toString x, "'"])
          (* end case *))

    fun bind (Env{funs, frags, curFn, hdrLab, vMap}, x, v) = Env{
            funs = funs,
            frags = frags,
            curFn = curFn,
            hdrLab = hdrLab,
            vMap = VMap.insert (vMap, x, v)
          }

  end
