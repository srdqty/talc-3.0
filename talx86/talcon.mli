(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker,                       *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Modified by Dan to remove statistics info *)

(* Utilities for checking the well-formedness of constructors, substituting
 * within constructors, normalizing constructors, and comparing constructors
 *)

open Numtypes;;
open Identifier;;
open Tal;;
open Talctxt;;

(*** Kind utilities ***)

(* Is k a well-formed kind? *)
(* val kindwf : ctxt -> kind -> unit  *)

(* k1 <= k2 *)
val kindleq : ctxt -> kind -> kind -> unit

(* k1 = k2 *)
val kindeq : ctxt -> kind -> kind -> unit

(* meet(k1,k2) *)
val kindmeet : ctxt -> kind -> kind -> kind

(* join(k1,k2) *)
val kindjoin : ctxt -> kind -> kind -> kind

(* apply subst and check kind *)
val check_kind : ctxt -> kind -> kind

(*** Constructor checking ***)

(* apply the substitution and check the constructor *)
val check : ctxt -> con -> kind * con
(* check as above and weak-head normalize *)
val check_whnorm : ctxt -> con -> kind * con
(* check as above on machine states *)
val verify_gamma : ctxt -> machine_state -> machine_state

(*** Type constructor well formedness and kinding - assumes checked ***)

(* |- pc : k *)
val primcon_kind : ctxt -> primcon -> kind
(* D |- c : k *)
val con_kind : ctxt -> con -> kind

(*** Type constructor free vars & subsitution & normalisation ***)

(* return the set of free variables *)
val freevars_kind : kind -> identifier Set.set
val ksubsts : (identifier,kind) Dict.dict -> kind -> kind
(* substitute for kindvariables k2{j:=k1} *)
val ksubst : kind -> identifier -> kind -> kind
val unroll_kind : kind -> kind


val is_closed : con -> bool 
val freevars : con -> (identifier Set.set * identifier Set.set)
(* substs d c = c{d} - capture avoiding *)
val substs : (identifier,kind) Dict.dict * (identifier,con) Dict.dict -> con -> con
(* subst c1 x c2 = c2{x:=c1} - capture avoiding *)
val subst : con -> identifier -> con -> con
(* expand type abbreviations *)
val expand_abbrevs : ctxt -> con -> con
(* weak-head normal form:  assumes constructor is well-formed *)
val whnorm : ctxt -> con -> con
(* normalize the constructor:  assumes constructor is well-formed *)
val normalize : ctxt -> con -> con

(* de-constructors -- these always weak-head normalize and then match
 * to extract values, failing if the constructor isn't of the form 
 * expected.  This should be called in lieu of explicitly whnorm'ing
 * and matching to avoid errors unless a more extensive match is needed.
 *)
val dchptr  : ctxt -> con -> (int32 list * con option * (con*variance) option)
val dcons   : ctxt -> con -> (con * con)
val dsptr   : ctxt -> con -> con
val dexist  : ctxt -> con -> (identifier * kind * con * con)
val dforall : ctxt -> con -> (identifier * kind * con)
val dprod   : ctxt -> con -> (con list)
val dlog    : ctxt -> con -> (log * con list)
val dif     : ctxt -> con -> (con * con)
val dname   : ctxt -> con -> con
val dvar    : ctxt -> con -> identifier
val dsum    : ctxt -> con -> (con list)
val dint    : ctxt -> con -> int32
val dcode   : ctxt -> con -> con
val dms     : ctxt -> con -> machine_state
val dcodems : ctxt -> con -> machine_state
val darray  : ctxt -> con -> (con * con)
val dfield  : ctxt -> con -> (con * variance)
val dsing   : ctxt -> con -> con

type ms_or_label = MachineState of machine_state | Lab of identifier

val dcodems_or_label : ctxt -> con -> ms_or_label

(*** Type constructor equalivalence ***)

(* compare two constructors up to alpha-equivalence *)
val alphaeqcon : ctxt -> con -> con -> unit
(* ctxt |- c1 = c2 *)
val eqcon : ctxt -> con -> con -> unit

(*** Type constructor subtyping ***)

(* ctxt |- c1 <= c2 *)
val leqcon : ctxt -> con -> con -> unit
(* ctxt |- gamma1 <= gamma2 *)
val machine_state_leq : ctxt -> machine_state -> machine_state -> unit;;

(* meet(c1,c2) *)
val conmeet : ctxt -> con -> con -> con
(* join(c1,c2) *)
val conjoin : ctxt -> con -> con -> con
(* meet(gamma1,gamma2) *)
val machine_state_meet : ctxt -> machine_state -> machine_state -> machine_state
(* join(gamma1,gamma2) *)
val machine_state_join : ctxt -> machine_state -> machine_state -> machine_state

(*** Utilities ***)

(* prove con given constraints in ctxt *)
val prove : ctxt -> con -> unit

(* split arithmetic expression into (const,non-consts) such that the
 * original con = const + sum(non-consts) *)
val split_arithmetic : ctxt -> con -> int32 * con list
 
(* get the index of a union element *)
val sum_index : (unit -> 'a) -> con -> int32

(* replace all singleton unions directly under c with their single element:
   assume constructor is well formed *)
val from_union : ctxt -> con -> con

(* unroll a recursive type definition: assumes constructor is well formed *)
val unroll_rec : bool -> ctxt -> con -> con

(* sizeof d c : returns the size (in bytes) of values who have type c *)
(* assumes c is normalized *)
val sizeof : ctxt -> con -> int32

(* sizeof_stack d c: returns the size (in bytes) of a stack with type c *)
val sizeof_stack : ctxt -> con -> int32

(* convert function type into list of type variables, logical or arithmetic
 * precondition, and register state *)
val separate_fun_type :
  ctxt -> con -> (identifier * kind) list * con * con

(*** Field/Stack Slot Utilities ***)

(* get_mem_offset_p ctxt c offset sz
 * Get update function and types at the offset from start of c
 * If offset exactly reaches a type boundary then ensure that a sequence of 
 *   types at that offset have size sz (return that sequence).  
 * If the offset does not reach a type boundary exactly then return the 
 *   residual offset (don't check the size).
 *)
val get_mem_offset_p :
    ctxt -> con -> int32 -> (con -> con) * con * int32;;
val get_mem_offset_p_checked :
    ctxt -> con -> int32 -> int32 -> (con -> con) * (con list) * int32 * bool;;
(* same as get_mem_offset_p but must be exact *)
val get_mem_offset : ctxt -> con -> int32 -> (con -> con) * con;;
val get_mem_offset_checked : ctxt -> con -> int32 -> int32 -> (con -> con) * (con list) * bool;;
(* same as above, but returns the tail of the list, instead of 
   the object at that position *)
val get_mem_from_offset : ctxt -> con -> int32 -> (con -> con) * con;;


(* verify that the stack constructor c1 is a tail of stack constructor c2.
 * assumes c1,c2 WH normalized and returns a function, which when given a 
 * mutated version of c1, generates the corresponding mutated version of c2.
 * That is, we verify that there exists a c3 such that Cappend(c1,c3) = c2
 * and return a function which maps c1' to Cappend(c1',c3).
 *)
val verify_stack_tail : ctxt -> con -> con -> (con -> con);;
(* verify_stack_tail ctxt cs offset c ->
     result of writing a c at offset in stack of type cs *)
val write_stack_offset : ctxt -> con -> int32 -> con -> con;;
(* Return the new stack type after poping i bytes *)
val get_stack_tail : ctxt -> int32 -> con -> con;;

(* get the alias info and type of a given name in the current capability *)
val get_name : ctxt -> identifier -> (alias_info * con)
val get_name_alias : ctxt -> identifier -> alias_info
val get_name_type : ctxt -> identifier -> con
(* change the alias info and/or type of a given name in the current 
 * capability -- ensures the name is in the capability. *)
val change_name : ctxt -> identifier -> (alias_info * con) -> ctxt
(* insert the given name and associated alias info and type into the current
 * capability -- ensures the name is not already in the capability. *)
val add_name : ctxt -> identifier -> (alias_info * con) -> ctxt
(* remove the given name from the capability -- check that it's actually
 * present. *)
val remove_name : ctxt -> identifier -> ctxt

(* EOF: talcon.mli *)
