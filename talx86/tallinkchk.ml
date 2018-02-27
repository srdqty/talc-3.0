(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* tallinkchk.ml
 * TAL Link Verifier
 *
 * Checks various linking operations are correct.
 *)

open Utilities;;
open Identifier;;
open Tal;;
open Talctxt;;
open Talcon;;

type imex = tal_int_type*tal_int_type
type inex = I | E;;

(* 
 * coalese  
 * an applicative data structure for associating a key with multiple values  
 *)
type ('a,'b) coalese = ('a,'b) coalese_item list ref
and ('a,'b) coalese_item = {key : 'a; mutable vals : 'b list}
;;

let coalese_new () : ('a,'b) coalese = ref [];;
let coalese_add k v c =
  let rec loop c1 =
    match c1 with
      [] -> c := {key=k; vals=[v]} :: !c
    | ci::c1 ->
	if ci.key=k then ci.vals <- v :: ci.vals else loop c1 in
  loop !c
;;
(*
let coalese_map f c =
  List.map (fun {key=k; vals=vs} -> f k vs) !c
;;
let coalese_iter f c =
  List.iter (fun {key=k; vals=vs} -> f k vs) !c
;;
*)
let coalese_fold f a c =
  List.fold_left (fun a {key=k; vals=vs} -> f a k vs) a !c


let sub_con_def ctxt cd1 cd2 =
  match cd1,cd2 with
    _,AbsCon -> ()
  | AbsCon,(BoundCon _ | ConcCon _) -> generate_error ctxt Con_def_nleq
  | BoundCon c1,BoundCon c2 -> leqcon ctxt c1 c2
  | BoundCon _,ConcCon _ -> generate_error ctxt Con_def_nleq
  | ConcCon c1,BoundCon c2 -> leqcon ctxt c1 c2
  | ConcCon c1,ConcCon c2 -> eqcon ctxt c1 c2
;;

let con_def_meet ctxt cd1 cd2 =
  match cd1,cd2 with
    AbsCon,_ -> cd2
  | _,AbsCon -> cd1
  | BoundCon c1,BoundCon c2 -> BoundCon (conmeet ctxt c1 c2)
  | ConcCon c1,BoundCon c2 -> leqcon ctxt c1 c2; cd1
  | BoundCon c1,ConcCon c2 -> leqcon ctxt c2 c1; cd2
  | ConcCon c1,ConcCon c2 -> eqcon ctxt c1 c2; cd1
;;

(*** Link Checking ***)

(* Phase 1: Build a context to check in *)

(* for each label in the coalese, compute the most specific kind for that label *)
let link_build_ctxt (cons : (identifier, (inex * kind * int_con_def)) coalese) =
  let aux ctxt l iekcds =
    let ctxt = set_loc ctxt (Loccon l) in
    match iekcds with
      [] -> failwith "Tallinkchk.verify_link - internal error 1"
    | (_,k,_)::iekcds ->
	let k =
	  List.fold_left (fun k1 (_,k2,_) -> kindmeet ctxt k1 k2) k iekcds in
	add_con ctxt l k in
  let ctxt = set_verify_ctxt empty_ctxt "building checking context" in
  let ctxt = coalese_fold aux ctxt cons in
  ctxt
;;

(* Phase 2: Process the constructors *)
(* returns the context, plus a list of the imported cons and
   a list of the exported cons. *)
let link_process_cons ctxt cons =
  let ctxt = set_verify_ctxt ctxt "" in
  let check_cons_loop ctxt l (ie1,k1,cd1 as iekcd1) (ie2,k2,cd2 as iekcd2) =
    let aux ctxt k1 cd1 k2 cd2 =
      kindleq ctxt k1 k2; sub_con_def ctxt cd1 cd2 in
    match ie1,ie2 with
      I,I ->
	let ctxt = set_verify_ctxt ctxt "verifying two imports consistency" in
	let k = kindmeet ctxt k1 k2 in
	let cd = con_def_meet ctxt cd1 cd2 in
	(I,k,cd)
    | I,E -> 
	let ctxt = 
	  set_verify_ctxt ctxt "verifying export/import consistency" in
	aux ctxt k2 cd2 k1 cd1; iekcd2
    | E,I -> 
	let ctxt = 
	  set_verify_ctxt ctxt "verifying export/import consistency" in
	aux ctxt k1 cd1 k2 cd2; iekcd1
    | E,E ->
	(* check for equality; return either one if they are equal *)
	let ctxt = 
	  set_verify_ctxt ctxt "verifying export/export consistency" in
	aux ctxt k2 cd2 k1 cd1; aux ctxt k1 cd1 k2 cd2; iekcd1
(*
   generate_error (set_verify_ctxt ctxt "") (Redefined_label l); iekcd1
*)
  in
  let check_cons (ctxt,icons,econs) l iekcds =
    let ctxt = set_loc ctxt (Loccon l) in
    match iekcds with
      [] -> failwith "Tallinkchk.verify_link - internal error 2"
    | iekcd::iekcds ->
	let (ie,k,cd) = List.fold_left (check_cons_loop ctxt l) iekcd iekcds in
	let ctxt = add_con_def ctxt l cd in
	if ie=E then (ctxt,icons,(l,k,cd)::econs)
 	else (ctxt,(l,k,cd)::icons,econs) in
  coalese_fold check_cons (ctxt,[],[]) cons
;;

(* Phase 3: Process the values *)

let link_process_vals ctxt vals =
  let ctxt = set_verify_ctxt ctxt "" in
  let check_vals_loop ctxt l (ie1,c1 as iec1) (ie2,c2 as iec2) =
    let aux c1 c2 =
      let ctxt = set_verify_ctxt ctxt "verifying export/import consistency" in
      leqcon ctxt c1 c2 in
    match ie1,ie2 with
      I,I ->
 	let ctxt = set_verify_ctxt ctxt "verifying two imports consistency" in
	(I,conmeet ctxt c1 c2)
    | I,E -> aux c2 c1; iec2
    | E,I -> aux c1 c2; iec1
    | E,E -> generate_error (set_verify_ctxt ctxt "") (Redefined_label l ); iec1 in
  let check_vals (ivals,evals) l iecs =
    let ctxt = set_loc ctxt (Locval l) in
    match iecs with
      [] -> failwith "Tallinkchk.verify_link - internal error 3"
    | iec::iecs ->
	let (ie,c) = List.fold_left (check_vals_loop ctxt l) iec iecs in
	if ie=E then (ivals,(l,c)::evals)
 	else ((l,c)::ivals,evals) in
  coalese_fold check_vals ([],[]) vals
;;

(* The whole thing *)
(*
 * Verify the link compatibility of a list of tal_implementations, 
 * computing the resulting import/export interface.
 *)
let verify_link (intts:imex list) : imex =
  
  (* first for each con label, gather all of the imports/exports 
     relating to that label *)
  let cons = coalese_new () in
  let aux1 inex (l,k,cd) = coalese_add l (inex,k,cd) cons in
  let aux2 (imp,exp) =
    List.iter (aux1 I) imp.it_cons; List.iter (aux1 E) exp.it_cons in
  List.iter aux2 intts;

  (* Use this information to build the linking context, and then 
     check the label cons *)
  let ctxt = link_build_ctxt cons in
  let (ctxt,icons,econs) = link_process_cons ctxt cons in

  (* now gather all of the imports/exports for each val label *)
  let vals = coalese_new () in
  let aux1 inex (l,c) = coalese_add l (inex,c) vals in
  let aux2 (imp,exp) =
    List.iter (aux1 I) imp.it_vals; List.iter (aux1 E) exp.it_vals in
  List.iter aux2 intts;

  let (ivals,evals) = link_process_vals ctxt vals in

  ({it_cons=icons; it_vals=ivals}, {it_cons=econs; it_vals=evals})
;;

(*** Program Checking ***)

let rec assoc2 k l =
  match l with
    [] -> raise Not_found
  | (k1,v1,v2)::l -> if k=k1 then (v1,v2) else assoc2 k l
;;

(* 
 *  Int_1 <= Int_2
 *
 * However, if the last argument is true, then we allow extra 
 * con labels to be intt2 that are not in intt1. These extra labels
 * are gathered together and returned.
 *)
let sub_intt 
      (s1:string) 
      (error_msg:string) 
      (intt1:tal_int_type)
      (intt2:tal_int_type)
      (allow_extra_cons:bool)
      : int_con list =
  let aux ctxt (l,k,_) = add_con (set_loc ctxt (Loccon l)) l k in
  let ctxt =
    set_verify_ctxt empty_ctxt ("building checking context for "^s1) in
  let ctxt = List.fold_left aux ctxt intt1.it_cons in
  let ctxt = set_verify_ctxt ctxt ("verifying "^s1) in
  let extras = ref [] in 
  let check_con ctxt (l,k2,cd2) =
    let ctxt = set_loc ctxt (Loccon l) in
    let ls = Identifier.id_to_string l in
    (try
       let (k1,cd1) = assoc2 l intt1.it_cons in
      (try kindleq ctxt k1 k2
      with exc -> (Printf.eprintf "kinds not right for con label %s\n" ls;
		      raise exc));
      (try sub_con_def ctxt cd1 cd2
      with exc -> (Printf.eprintf "con defs not right for con label %s\n" ls;
		     raise exc));
      (* kindleq ctxt k1 k2; sub_con_def ctxt cd1 cd2 *)
    with Not_found -> 
       if allow_extra_cons then 
	  extras := (l,k2,cd2)::!extras 
       else 
	  begin
	     Printf.eprintf "con label %s not found\n" ls;
	     generate_error ctxt (Intt_nleq (error_msg,l))
	  end);
    add_con_def ctxt l cd2 in
  let ctxt = List.fold_left check_con ctxt intt2.it_cons in
  let check_val (l,c2) =
    let ctxt = set_loc ctxt (Locval l) in
    (try
       let c1 = List.assoc l intt1.it_vals in
       leqcon ctxt c1 c2
    with Not_found -> (Printf.eprintf "val label %s not found\n" 
			    (Identifier.id_to_string l);
			 generate_error ctxt (Intt_nleq (error_msg,l)))) in
  List.iter check_val intt2.it_vals;
  !extras
;;

let sub_intt_exports e1 e2 =
   match sub_intt "exports" "missing label: " e1 e2 false with 
      [] -> ()
    | _  -> failwith "Internal Error: tallinkchk.ml sub_intt_exports"
let sub_intt_imports imps1 imps2 = 
   sub_intt "imports" "extra label: " imps2 imps1 true
      
let sub_module (imps1,exps1) (imps2,exps2) : int_con list =
   sub_intt_exports exps1 exps2;
   sub_intt_imports imps1 imps2
;;

(*
let prog_imex = ref ({it_cons=[]; it_vals=[]},{it_cons=[]; it_vals=[]});;
*)

let verify_program (progt:imex) (progt2:imex) : int_con list =
   sub_module progt progt2
;;

(* tallinkchk.ml *)
