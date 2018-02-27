(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker,                       *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Hacked by Dan to profile and try to improve running time.
   Changes:
     1. Removed statistics since they're work and the profiler does it better
     2. Re-wrote kindleq so common case could be inlined.
 *)

(* talcon.ml
 * TAL kind & type constructor verifier and manipulation utilities
 *
 * Kinds: subkind & equal kinds.
 * Type constructors: well formed of kind, nomalise, alpha equality
 * Utilities: unroll, size, stack size, seperate function type
 *)
 
open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;
open Talctxt;;

let debug s =
  Format.print_string s; Format.print_newline()

open Talpp
open Format

let debug2 f = 
  let fmt = Format.std_formatter in 
  let o = std_options in 
  Format.pp_open_hvbox fmt 0;
  f fmt o;
  Format.pp_print_newline fmt ();
  Format.pp_print_flush fmt (); 
  () 
;;


(*************************************************************************)
(* kindwf and check_kind                                                 *)
(*************************************************************************)

let rec kindwf' ctxt expand k = 
   match k.rkind with 
      Karrow (k1,k2) ->
	 defkind(Karrow (kindwf' (reverse_polarity ctxt) expand k1, 
			 kindwf' ctxt expand k2))
    | Kprod ks ->
	 defkind(Kprod (List.map (kindwf' ctxt expand) ks))
    | Ksum ks ->
	 defkind(Ksum (List.map (kindwf' ctxt expand) ks))
    | Kvar i -> 
	 if check_kindvar ctxt i 
	 then k
	 else if expand then 
	    begin
	       try 
		  let k' = Dict.lookup (get_kindabbrevs ctxt) i in
		  (*
		    k.rkind <- k'.rkind;
		    k.freekindvars <- k'.freekindvars
		  *) k'
	       with Dict.Absent -> 
		 (generate_error ctxt (Kindwf(k,"unbound kind variable "^
					      (id_to_string i))); 
		  raise Talfail)
	    end 
         else 
	    (generate_error ctxt (Kindwf(k,"unbound kind variable "^
					 (id_to_string i))); raise Talfail)
    | Kmu (ks,j) -> 
	 let ctxt = List.fold_left (fun ctxt (j,k) -> 
	    add_kind ctxt j) ctxt ks in 
	 if List.mem j (List.map fst ks) then 
	    defkind(Kmu 
		      (List.map (fun (j,k) -> (j,kindwf' ctxt expand k)) ks,j))
	 else 
	 (generate_error ctxt (Kindwf(k,"variable "^(id_to_string j)^
				      " not bound in recursive type"));
	  raise Talfail)
    | _ -> k (* everything else is good *)


(* kindwf ctxt k returns () if k is a well-formed kind                   *)
let kindwf ctxt k = kindwf' ctxt false k

(* check_kind checks k to make sure it is well-formed and expands abbrevs
   Instead of returning a new kind, it just mutates to expand abbrevs *)
let rec check_kind ctxt k = kindwf' ctxt true k

(* *****************************************************************
   Alpha equality of kinds (and a few preliminary functions for constructor
   alpha equality. See the note before constructor aeq.)
   *****************************************************************)
type alphactxt = ((identifier*identifier) list) * ((identifier*identifier) list);;

let empty_ctxt = ([],[])
;;
let extend (km,vm) x1 x2 : alphactxt =
   km,((x1,x2)::vm)
;;
let extend_kind (km,vm) x1 x2 : alphactxt = 
   (x1,x2)::km,vm
;;
let rec cmp error c x1 x2 =
  match c with
    [] -> if (id_compare x1 x2)<>0 then error ()
   | (y1,y2)::c ->
      if (id_compare x1 y1)=0 then
 	(if (id_compare x2 y2)<>0 then error ())
      else if (id_compare x2 y2)=0 then
 	 error ()
      else cmp error c x1 x2
;;
let compare error (k,c) x1 x2 = 
cmp error c x1 x2
;;

let rec kindaeq error ctxt kmap k1 k2 =
  if k1 == k2 then () else
  match k1.rkind,k2.rkind with
    (Kbyte s1,Kbyte s2) when s1=s2 -> ()
  | (Ktype,Ktype) -> ()
  | (Kmemi i1,Kmemi i2) when i1=i2 -> ()
  | (Kmem,Kmem) -> ()
  | (Kstack,Kstack) -> ()
  | (Kint,Kint) -> ()
  | (Kbool, Kbool) -> ()
  | (Karrow(k1a,k1b),Karrow(k2a,k2b)) ->
       (kindaeq error ctxt kmap  k1a k2a; kindaeq error ctxt kmap  k1b k2b)
  | (Kprod k1s,Kprod k2s) -> kindsaeq error ctxt kmap (Kindeq (k1,k2)) k1s k2s
  | (Kname, Kname) -> ()
  | (Kcap, Kcap) -> ()
  | (Kms, Kms) -> ()
  | (Ksum k1s, Ksum k2s) -> kindsaeq error ctxt kmap (Kindeq (k1,k2)) k1s k2s
  | (Kvar i, Kvar j) -> cmp error kmap i j
  | (Kmu (ks1,i), Kmu (ks2,j)) ->
       if List.length ks1 <> List.length ks2 
       then error () (*  generate_error ctxt (Kindeq (k1,k2))*)
       else 
	  let kmap = List.fold_right2 (fun (j,_) (j2,_) kmap -> (j,j2)::kmap) 
	     	ks1 ks2 kmap in 
	  List.iter2 (fun (_,k1) (_,k2) -> kindaeq error ctxt kmap k1 k2) ks1 ks2;
	  cmp error kmap i j
  | (_,_) -> (* generate_error ctxt (Kindeq (k1,k2)) *)
       error ()
and kindsaeq error ctxt kmap ve k1s k2s =
  match k1s,k2s with
    ([],[]) -> ()
   | (k1::k1s,k2::k2s) -> (kindaeq error ctxt kmap  k1 k2; 
			     kindsaeq error ctxt kmap ve k1s k2s)
   | (_,_) -> (* generate_error ctxt ve *) error ()
;;
exception NotEq
let kindeq ctxt k1 k2 = 
   kindaeq (fun () -> raise NotEq) ctxt [] k1 k2 

let rec filter_schema l = 
   match l with [] -> []
    | ( (j,k)::tl ) -> 
	 match tl with 
	    [] -> [(j,k)]
	  | (j2,k2)::_ -> if id_compare j j2 = 0  then 
	       try  (kindeq (empty_ctxt) k k2 ;
		  filter_schema tl)
	       with _ -> 
		  ((debug2 (fun fmt o -> 
		     print_kind fmt o k;
		     pp_print_string fmt " =/= ";
		     print_kind fmt o k2));
		     failwith "two kinds in a schema are not the same")
	  else (j,k)::(filter_schema tl)


(*************************************************************************)
(* k1 <= k2, k1 = k2, kind meet, kind join                               *)
(*************************************************************************)


let rec kindaleq error ctxt kmap k1 k2 =
   if k1 == k2 then () else kindaleq' error ctxt kmap k1 k2
and kindaleq' error ctxt kmap k1 k2 =
  match k1.rkind,k2.rkind with
    (Kbyte s1,Kbyte s2) when s1=s2 -> ()
  | (Kbyte _,Ktype) -> ()
  | (Kmemi i1,Kmemi i2) when i1=i2 -> ()
  | (Kmemi _,Kmem) -> ()
  | (Karrow(k1a,k1b),Karrow(k2a,k2b)) -> 
      (kindaleq error ctxt kmap k2a k1a; kindaleq error ctxt kmap k1b k2b)
  | (Kprod k1s,Kprod k2s) -> kindsaleq error ctxt kmap (Kindleq (k1,k2)) k1s k2s true
  | (Ksum k1s, Ksum k2s) -> kindsaleq error ctxt kmap (Kindleq (k1,k2)) k2s k1s false
  | (_,_) -> kindaeq error ctxt kmap k1 k2
and kindsaleq error ctxt kmap ve k1s k2s prod =
  match k1s,k2s with
    ([],[]) -> ()
  | (k1::k1s,k2::k2s) -> 
       ((if prod then kindaleq error ctxt kmap k1 k2 else kindaleq error ctxt kmap k2 k1); 
	  kindsaleq error ctxt kmap ve k1s k2s prod)	
  | (_,_) -> (* generate_error ctxt ve *) error ()
;;

(* kindleq k1 k2:
 *   if     k1 <= k2 then ()
 *   if not k1 <= k2 then generate error Kindleq (k1,k2)
 *)

exception NotLeq
let kindleq ctxt k1 k2 = 
   kindaleq (fun () -> (* was raise NotLeq *) generate_error ctxt (Kindleq(k1,k2))) ctxt [] k1 k2 

(* kindmeet k1 k2:
 *   if the meet of k1 & k2 exists return it
 *   otherwise generate error Kindmeet (k1,k2)
 * kindjoin k1 k2:
 *   if the join of k1 & k2 exists return it
 *   otherwise generate error Kindjoin (k1,k2)
 *)

let rec kindmeet ctxt k1 k2 =
  if k1==k2 then k1
  else match k1.rkind,k2.rkind with
    Kbyte s1,Kbyte s2 when s1=s2 -> k1
  | Kbyte _,Ktype -> k1
  | Ktype,Kbyte _ -> k2
  | Ktype,Ktype -> ktype
  | Kmemi i1,Kmemi i2 when i1=i2 -> k1
  | Kmemi _,Kmem -> k1
  | Kmem,Kmemi _ -> k2
  | Karrow (k11,k12),Karrow (k21,k22) ->
       karrow (kindjoin ctxt k11 k21) (kindmeet ctxt k12 k22)
  | Kprod k1s,Kprod k2s -> kprod (kindsmeet ctxt (Kindmeet (k1,k2)) k1s k2s)
  | Ksum k1s, Ksum k2s -> ksum (kindsmeet ctxt (Kindmeet (k1,k2)) k1s k2s)
  | Kmu (ks1,j1), Kmu (ks2,j2) -> kindeq ctxt k1 k2 ; k1
  | _,_ -> generate_error ctxt (Kindmeet (k1,k2)); raise Talfail
and kindsmeet ctxt ve k1s k2s =
  match k1s,k2s with
    [],[] -> []
  | k1::k1s,k2::k2s -> (kindmeet ctxt k1 k2)::(kindsmeet ctxt ve k1s k2s)
  | _,_ -> generate_error ctxt ve; raise Talfail
and kindjoin ctxt k1 k2 =
  if k1==k2 then k1
  else match k1.rkind,k2.rkind with
    Kbyte s1,Kbyte s2 -> if s1=s2 then k1 else ktype
  | Ktype,(Kbyte _|Ktype) | (Kbyte _|Ktype),Ktype -> ktype
  | Kmemi i1,Kmemi i2 when i1=i2 -> k1
  | (Kmemi _|Kmem),(Kmemi _|Kmem) -> kmem
  | Karrow (k11,k12),Karrow (k21,k22) ->
       karrow (kindmeet ctxt k11 k21)(kindjoin ctxt k12 k22)
  | Kprod k1s,Kprod k2s -> kprod (kindsjoin ctxt (Kindjoin (k1,k2)) k1s k2s)
  | Ksum k1s, Ksum k2s -> ksum (kindsjoin ctxt (Kindjoin (k1,k2)) k1s k2s)
  | Kmu _, Kmu _ -> kindeq ctxt k1 k2; k1 
  | _,_ -> generate_error ctxt (Kindjoin (k1,k2)); raise Talfail
and kindsjoin ctxt ve k1s k2s =
  match k1s,k2s with
    [],[] -> []
  | k1::k1s,k2::k2s -> (kindjoin ctxt k1 k2)::(kindsjoin ctxt ve k1s k2s)
  | _,_ -> generate_error ctxt ve; raise Talfail
;;



(*************************************************************************)
(* Free variables of a kind/constructor                                  *)
(*************************************************************************)

let singleton = Set.singleton id_compare
let empty_set = Set.empty id_compare

(* add a to the free kind set *)
let rec combine a (b,c) = 
   (Set.union a b, c)
let mappair f (a1,a2) (b1,b2) = 
   (f a1 b1, f a2 b2)
let unions_kind l : identifier Set.set = 
   List.fold_left Set.union empty_set l
let kdelete s l = 
   List.fold_left Set.delete s l

let unions (l : (identifier Set.set * identifier Set.set) list) = 
   List.fold_left (mappair Set.union)(empty_set,empty_set) l
let cdelete (ks,cs) a = (ks,Set.delete cs a)

(* Calculates the free variables of a kind and updates the freevars
 * field.  No assumptions about well formedness or canonical form.
 *)

let rec rk_freevars (k : rkind) : identifier Set.set = 
   match k with 
      (Kbyte _ | Ktype | Kmemi _ | Kmem | Kstack 
    | Kint | Kbool | Kname | Kcap | Kms )  -> empty_set
    | Karrow (k1, k2) -> Set.union (freevars_kind k1) (freevars_kind k2)
    | Kprod ks -> unions_kind (List.map freevars_kind ks)
    | Ksum ks -> unions_kind (List.map freevars_kind ks)
    | Kvar i -> singleton i
    | Kmu (ks1,i) -> 
	 let (js,ks) = List.split ks1 in 
	 kdelete (unions_kind (List.map freevars_kind ks)) js
and freevars_kind (k:kind) = 
   match k.freekindvars with 
      None -> let s = rk_freevars k.rkind in k.freekindvars <- Some s; s
    | Some s -> s



(* Calculates the free variables of a constructor and updates the freevars
 * field.  No assumptions about well formedness or canonical form.
 *)

let rec rc_freevars (c : rcon) : (identifier Set.set) * (identifier Set.set) =
  match c with
    Cvar a -> empty_set, singleton a
  | Clam (v,k,c) -> 
       let ks,cs = (freevars c) in 
       Set.union ks (freevars_kind k), Set.delete cs v
  | Capp (c1,c2) -> mappair Set.union (freevars c1) (freevars c2)
  | Ctuple cs ->  combine empty_set (unions (List.map freevars cs))
  | Cproj (_,c) ->  combine empty_set ( freevars c)
(* ---- LX ---- *)
  | Cinj(_,c,k) ->  combine (freevars_kind k) (freevars c)
  | Ccase(c,a,cs) -> 
       combine empty_set (mappair Set.union (freevars c)
			       (cdelete (unions (List.map freevars cs)) a))
  | Cfold(k,c) -> combine (freevars_kind k) (freevars c)
  | Cpr (i,l) -> 
       let (kset, cset) = 
	  List.fold_right (fun (j,a,k1,f,k2,c) set -> 
	     let kset = (Set.union (freevars_kind k1) (freevars_kind k2)) in
	     let cset = (mappair Set.union set (freevars c)) in
	     combine kset cset) l (empty_set, empty_set) in
       let (kset, cset) = 
	  List.fold_right (fun (j,a,k1,f,k2,c) (kset,cset) -> 
	     (Set.delete kset j, Set.delete (Set.delete cset a) f)) l (kset, cset) in 
       (kset, cset)
  | Cvoid k -> freevars_kind k,empty_set
(* -- end LX -- *) 
  | Clab _ -> empty_set,empty_set
  | Cprim _ ->  empty_set,empty_set
  | Crec fs -> 
      let s =
 	List.fold_left
	    (fun (ks,cs) (_,k,c) -> 
	       combine (freevars_kind k) (mappair Set.union (ks,cs) (freevars c)))
	    (empty_set,empty_set) fs in
      List.fold_left (fun (ks,cs) (x,_,_) -> (ks,Set.delete cs x)) s fs
  | Cforall (v,k,c) -> combine (freevars_kind k) (cdelete (freevars c) v)
  | Cexist (v,k,c1, c2) -> combine (freevars_kind k) 
	  (cdelete (mappair Set.union (freevars c1) (freevars c2)) v)
(* machine state *)
  | Cms ms -> 
      let s1 = freevars (ms_get_cap ms)
      in ms_fold_reg (fun r c s -> mappair Set.union s (freevars c)) ms s1
  | Cmsjoin(c1,c2) -> mappair Set.union (freevars c1) (freevars c2)
(* --- *)
  | Ccode c -> freevars c
  | Chptr (_,co,tco) ->
      let fvs = match co with None -> empty_set,empty_set | Some c -> freevars c in
      (match tco with None -> fvs | Some (c,_) -> mappair Set.union fvs (freevars c))
  | Cfield (c,_) -> freevars c
  | Cprod cs -> unions (List.map freevars cs)
  | Csum cs -> unions (List.map freevars cs)
  | Carray (c1,c2) -> mappair Set.union (freevars c1) (freevars c2)
  | Csing c -> freevars c
  | Csptr c -> freevars c
  | Cempty -> empty_set,empty_set
  | Ccons (c1,c2) -> mappair Set.union (freevars c1) (freevars c2)
  | Cappend (c1,c2) -> mappair Set.union (freevars c1) (freevars c2)
(* arithmetic and logic *)
  | Clog (_,cs) -> unions (List.map freevars cs)
  | Cif (c1,c2) -> mappair Set.union (freevars c1) (freevars c2)
(* alias stuff *)
  | Cname c -> freevars c
  | Cjoin cs -> unions (List.map freevars cs)
  | Ccap d ->
      Dict.fold_dict 
	  (fun x (_,c) (ks,s) ->
	     let fk,fc =  (freevars c) in
	     (Set.union fk ks, Set.union fc s)) d
	  (empty_set, empty_set)
  | Ctagof c -> freevars c
 (* Cyclone *)
  | Ctmpl(c1,c2_opt,labels,holes) ->
      let c2_fv =
        match c2_opt with None -> empty_set,empty_set | Some c2 -> freevars c2 in
      List.fold_right
        (fun (i,c) fvs -> mappair Set.union (freevars c) fvs)
        (labels@holes)
        (mappair Set.union (freevars c1) c2_fv)
  | Ctptr _ -> empty_set, empty_set
  | Ctrgn(c1,None,_) -> freevars c1
  | Ctrgn(c1,Some c2,t) ->
      let regions = List.map (fun (i,_,_) -> i) t in
      let cons =
        List.concat
          (List.map
             (fun (_,labels,holes) ->
               (List.map (fun (_,c) -> c) labels)
               @(List.map (fun (_,c) -> c) holes))
             t) in
      List.fold_left
        (fun (k,c) i -> (k,Set.insert c i))
        (List.fold_right
           (fun c fvs -> mappair Set.union (freevars c) fvs)
           cons
           (mappair Set.union (freevars c1) (freevars c2)))
         regions
(* End Cyclone *)
  | Csubst(c,s) -> subst_freevars (freevars c) s
  | Cr ci ->
       (match ci with 
	  RCon c -> freevars c
	| RKind k -> if (Set.is_empty (freevars_kind k)) 
	             then empty_set, empty_set
	             else failwith "SCW-BUG: What do I do here???"
	| _ -> empty_set, empty_set )
  | Ctypeof _ -> empty_set, empty_set      

and subst_freevars ((sk,sc) as s) es =
  match es with
    Enil -> s
  | Es(x,c) -> 
      if Set.member sc x then
	 mappair Set.union (cdelete s x ) (freevars c)
      else s
  | Eo(es1,es2) -> subst_freevars (subst_freevars s es1) es2
and freevars (c : con) : (identifier Set.set) * (identifier Set.set) = 
   match c.freevars with
     None -> let s = rc_freevars c.rcon in 
    c.freevars <- (Some s);  s
   | Some s -> s
;;

let is_closed c = 
   let (k,c) = freevars c in 
   Set.is_empty k & Set.is_empty c 

(*************************************************************************)
(* Capture avoiding substitution for kinds and  constructors             *)
(*************************************************************************)

(* These functions do capture avoiding substitution on constructors.
 * There are two main forms for each:
 *   ksubst k1 x k2 = k2{x:=k1}           (for kinds)
 *   ksubsts d k = k{d}
 *
 *   subst c1 x c2 = c2{x:=c1}            (for constructors)
 *   substs d c = c{d}
 *
 * But many auxilary functions to make this all go fast.  First, there are
 * functions optimised for single variable case rcsubsta & substa.  substa
 * checks for cutoff and rcsubsta does the actual substitution.  Second, there
 * are functions for multiple substitutions rcsubstsa & substsa.  Again,
 * substsa does a cutoff check and rcsubsta does the substitution.  The
 * functions only rename if necessary.  Since renaming requires multiple
 * substitutions, rcsubsta fails over to rcsubstsa via subst12d if renaming is
 * required.  Renaming is actually done by the function rename.
 *
 * Requires free variable calculation.  No assumptions about well formedness or
 * canonical form.
 *)
let defvarkind x =
   { rkind=Kvar x;
     freekindvars=Some(Set.singleton id_compare x);
      kabbrev = None
  }
;;
let defvarcon x = cvar x
;;

let get_kdict ((kd,cd),(ks,cs)) = (kd,ks)

(* do renaming if necessary *)
let rename_kind j ((kd,ks) as z) =
   if Dict.member kd j then 
      (Dict.delete kd j, ks),j 
   else if Set.member ks j then 
      let j' = id_unique j in
      ((Dict.insert kd j (defvarkind j'),ks), j')
   else (z,j)

(* Do renaming if necessary.
 * Current substitutions are d where s=fv(ran(d)).
 * x of kind k binding in c is to be renamed if necessary.
 * If x in dom(d) then remove that substitution.
 * If x in s then renaming necessary.
 * Otherwise no conflicts to keep same name.
 * Ouput: new d,s pair, new (x,k,c) triple.
 *)

let rename1 (x as t) ((((kd,cd) as d),((ks,cs) as s)) as z) =
  if Dict.member cd x then
    (((kd,Dict.delete cd x),s),t)
  else if Set.member cs x then
     let x' = id_unique x in
     (* as x' is unique, we don't need to add it to the free vars of d even though 
	we map x to it. *)
     (((kd,Dict.insert cd x (defvarcon x')),s), x')
  else (z,t)
;;

let rename (x,k,c) z = 
   let (z,x) = rename1 x z in 
   (z, (x,k,c))
;;

(* as above but renaming occurs in the 2 constructors c1 and c2 *)
let rename2 ((x,k,c1,c2) as t) ((d,s) as z) =
   let (z,x) = rename1 x z in 
   (z, (x,k,c1,c2))
;;

(* Rename then apply fk to k and f to c in t
 *  - discards new d,s pair and returns new (x,k,c) triple
 *)
let rename_then fk fc p t =
  let (p',(x,k,c)) = rename t p in
  ( x, fk (get_kdict p') k, fc p' c)
;;


(* Given a dict(only for kinds) and a kind actually apply the substitution to the kind    kd - dict from kvars to kinds
   ks - set of freevars in kinds in dict (so we know when we have to rename) 
*)
let rec rksubstsa ((kd,ks) as p) kind = 
   match kind.rkind with 
      (Kbyte _ | Ktype | Kmemi _ | Kmem  | Kstack 
    | Kint | Kbool | Kname | Kcap | Kms)  -> kind
    | Karrow (k1,k2) -> 
	 defkind(Karrow( ksubstsa p k1, ksubstsa p k2))
    | Kprod ks ->defkind (Kprod (List.map (ksubstsa p) ks))
    | Ksum ks ->defkind (Ksum (List.map (ksubstsa p) ks))
    | Kvar v -> (try Dict.lookup kd v with Dict.Absent -> kind)
    | Kmu (schema,v) ->
	 let js,ks = List.split schema in 
	 let z,js,v = List.fold_left 
	       (fun (p,js,v') j -> let p,jnew = rename_kind j p in 
	       (p,jnew::js, if id_compare j v = 0 then jnew else v')) (p,[],v) js in
	 let js = List.rev js in 
	 let ks = List.map (ksubstsa z) ks in 
	 defkind (Kmu(List.combine js ks, v))

(* Multiple substitutions - check for cutoff otherwise rksubstsa *)
(* k{d} where s= fv(ran (d)) *)
and ksubstsa (d,_ as p) k = 
   match k.freekindvars with 
      None -> rksubstsa p k
    | Some kfvs -> 
	 let aux j k ((d,fvs) as z) = 
	    if Set.member kfvs j then
	       (Dict.insert d j k), Set.union fvs (freevars_kind k)
	    else z in
	 let d,_ as p =  Dict.fold_dict aux d (Dict.empty id_compare,empty_set) in
	 if Dict.is_empty d then
	    k
	 else 
      	    rksubstsa p k

(* Actually do multiple substitutions *)
(* con{d} where fvs=fv(ran(d)) *)
let dcon rc = defcon rc;;
let wcon rc = wcon rc;;

let rec rcsubstsa (((kd,cd) as d,(kfvs,cfvs) as fvs) as p) con = 
  match con.rcon with
    Cvar a ->
      (try Dict.lookup cd a with Dict.Absent -> con)
  | Clam(x,k,c) -> 
     let (x',k',c') = rename_then ksubstsa substsa p (x,k,c) in wcon(Clam(x',k',c'))
  | Capp(c1,c2) -> dcon(Capp(substsa p c1, substsa p c2))
  | Ctuple cs -> wcon(Ctuple(List.map (substsa p) cs))
  | Cproj(i,c) -> dcon(Cproj(i,substsa p c))
(* ---- LX ---- *)
  | Cinj(i,c,k) -> dcon(Cinj(i, substsa p c, ksubstsa (get_kdict p) k))
  | Ccase(c,a,cs) -> let (p',x) = rename1 a p in 
    dcon (Ccase (substsa p c, x, List.map (substsa p') cs))
  | Cfold (k,c) -> dcon(Cfold(ksubstsa (get_kdict p) k, substsa p c))
  | Cpr (f, l) -> 
       (* Create a new dictionary, possibly renaming the bound kind variables
	  and bound con variables. *)
       let (l, (((kd,cd),(ks,cs)) as p))  = 
	  List.fold_right (fun (j,a,k,f,k',c) (rest,(((kd,cd),(ks,cs)) as p))  -> 
	     let ((kd,ks), j') = rename_kind j (kd,ks) in 
	     let (p,a') = rename1 a ((kd,cd),(ks,cs)) in
	     let (p,f') = rename1 f p in 
	     ((j',a',k,f',k',c)::rest,p)) l ([],p) in 
(*       let l = List.rev l in *)
       let kp = kd,ks in 
       let l = List.map (fun  (j,a,k,f,k',c) -> 
	  (j, a, ksubstsa kp k, f, ksubstsa kp k', substsa p c)) l in
       let f =  (try Dict.lookup cd f with Dict.Absent -> cvar f) in 
       (match f.rcon with Cvar f -> 
	  dcon(Cpr (f,l))
	| _ -> failwith "BUG: substitution for bound variable")
  | Cvoid k -> cvoid (ksubstsa (get_kdict p) k)
(* -- end LX -- *)
  | Clab _ -> con
  | Cprim _ -> con
  | Crec fs ->
      let g f (p,fs) = let (p',f') = rename f p in (p',f'::fs) in
      let (p',fs') = List.fold_right g fs (p,[]) in
      wcon (Crec (List.map (fun (x',k,c) -> (x',ksubstsa (get_kdict p') k,substsa p' c)) fs'))
  | Cforall (x,k,c) -> 
      let (x',k',c') = rename_then ksubstsa substsa p (x,k,c) in
      wcon(Cforall(x',k',c'))
  | Cexist (x,k,c1,c2) -> 
      let (p',(x',k',c1',c2')) = rename2 (x,k,c1,c2) p in
      wcon(Cexist(x', ksubstsa (get_kdict p') k', (substsa p' c1'), (substsa p' c2')))
  | Cms ms -> wcon(Cms(ms_map (substsa p) ms))
  | Cmsjoin (c1,c2) -> dcon(Cmsjoin(substsa p c1,substsa p c2))
  | Ccode c -> wcon(Ccode(substsa p c))
  | Chptr (is,co,tco) ->
      let co = match co with None -> co | Some c -> Some (substsa p c) in
      let tco =
	match tco with None -> tco | Some (c,v) -> Some (substsa p c,v) in
      chptr is co tco
  | Cfield (c,v) -> wcon (Cfield (substsa p c,v))
  | Cprod cs -> dcon (Cprod (List.map (substsa p) cs))
  | Csum cs -> wcon (Csum (List.map (substsa p) cs))
  | Carray (c1,c2) -> wcon (Carray (substsa p c1,substsa p c2))
  | Csing c -> wcon (Csing (substsa p c))
  | Csptr c -> wcon (Csptr (substsa p c))
  | Cempty -> con
  | Ccons (c1,c2) -> wcon(Ccons(substsa p c1,substsa p c2))
  | Cappend (c1,c2) -> dcon(Cappend(substsa p c1, substsa p c2))
(* arithmetic and logic *)
  | Clog (l,cs) -> dcon (Clog (l,List.map (substsa p) cs))
  | Cif (c1,c2) -> wcon(Cif (substsa p c1, substsa p c2))
(* alias stuff *)
  | Cname c -> wcon (Cname (substsa p c))
  | Ctagof c -> wcon (Ctagof (substsa p c))
  | Cjoin cs -> dcon(Cjoin(List.map (substsa p) cs))
  | Ccap cap_dict -> 
     (* jgm: we're in big trouble here because it's crucial that
      * we never substitute anything but a Kname variable for a
      * Kname variable.  I don't think this will cause a problem
      * in practice but it's very unsettling. *)
      let f x (ai,c) d' = 
	let x' = 
	  try 
	    begin
	       match (Dict.lookup cd x).rcon with
		Cvar x' -> x'
	      |	_ -> failwith "Substituted a non-variable for a name"
	    end
	      with Dict.Absent -> x in
	let c' = substsa p c in
	Dict.insert_new d' x' (ai,c')
      in wcon (Ccap(Dict.fold_dict f cap_dict (Dict.empty id_compare)))
(* Cyclone *)
  | Ctmpl(c1,c2_opt,labels,holes) ->
      let c2_subst =
        match c2_opt with None -> None | Some c2 -> Some(substsa p c2) in
      wcon(Ctmpl(substsa p c1,
                   c2_subst,
                   List.map (fun (i,c) -> (i,substsa p c)) labels,
                   List.map (fun (i,c) -> (i,substsa p c)) holes))
  | Ctptr _ -> con
  | Ctrgn(c1,None,t) ->
      wcon(Ctrgn(substsa p c1, None,
                   List.map
                     (fun (v,labels,holes) ->
                       (v,
                        List.map
                          (fun (i,c) -> (i,substsa p c))
                          labels,
                        List.map
                          (fun (i,c) -> (i,substsa p c))
                          holes))
                     t))
  | Ctrgn(c1,Some c2,t) ->
      wcon(Ctrgn(substsa p c1, Some(substsa p c2), 
                   List.map
                     (fun (v,labels,holes) ->
                       (v,
                        List.map
                          (fun (i,c) -> (i,substsa p c))
                          labels,
                        List.map
                          (fun (i,c) -> (i,substsa p c))
                          holes))
                     t))
(* End Cyclone *)
  | Csubst(c,es) -> 
      let rec sub_esub (d,fvs as p) es = 
	match es with
	  Enil -> p
	| Es(x,c) -> 
	    let  c' = substsa p c in
	    let d' = (kd, Dict.insert cd x c') in
	    let fvs' = mappair Set.union fvs (freevars c') in
	    (d',fvs') 
	| Eo(es1,es2) -> sub_esub (sub_esub p es2) es1
      in substsa (sub_esub p es) c
  | Cr ci -> 
       (match ci with 
	  RCon c -> wcon (Cr (RCon (substsa p c)))
	| _ -> con)
  | Ctypeof _ -> con
       
(* Multiple substitutions - check for cutoff otherwise rcsubstsa *)
(* c{d} where s=fv(ran(d)) *)
and substsa ((kd,cd),_ as p) c = 
  match c.freevars with
    None -> rcsubstsa p c
   | Some (kfvs,cfvs) ->
	  
      let auxk x k (((kd,cd),(kfvs2, cfvs2)) as z) = 
	 if Set.member kfvs x then
	  ((Dict.insert kd x k,cd),(Set.union (freevars_kind k) kfvs2,cfvs2))
	else z in
      let aux x c (((kd,cd),(kfvs2, cfvs2)) as z) = 
	 if Set.member cfvs x then
	  ((kd,Dict.insert cd x c),mappair Set.union (kfvs2,cfvs2) (freevars c))
	else z in
      let p = Dict.fold_dict auxk kd 
	    ((Dict.empty id_compare,Dict.empty id_compare),(empty_set,empty_set)) in 
      let (kd,cd),_ as p  = Dict.fold_dict aux cd p in
      if Dict.is_empty kd & Dict.is_empty cd then
	c
      else 
      	rcsubstsa p c
;;

(* Fail over function when rcsubsta needs to do renaming *)
(* c2{x:=c1} where fvs=fv(c1) *)
let subst12d c1 x fvs c2 =
   let c =  substsa 
	 ((Dict.empty id_compare ,Dict.singleton id_compare x c1),fvs) c2 in
   c	 

(* Actually do single substitution *)
(* con{a:=ca} where fvs=fv(ca) *)
let rec rcsubsta ca a ((kfvs,cfvs) as fvs:(identifier Set.set) * (identifier Set.set)) con = 
  match con.rcon with
    Cvar x ->
      if (id_compare x a)=0 then ca else con
  | Clam(x,k,c) -> 
      if (id_compare x a)=0 then con
      else if Set.member cfvs x then subst12d ca a fvs con 
      else wcon(Clam(x,k,substa ca a fvs c))
  | Capp(c1,c2) -> dcon(Capp(substa ca a fvs c1, substa ca a fvs c2))
  | Ctuple cs -> wcon(Ctuple(List.map (substa ca a fvs) cs))
  | Cproj(i,c) -> dcon(Cproj(i,substa ca a fvs c))
(* ---- LX ---- *)
  | Cinj(i,c,k) -> dcon(Cinj(i, substa ca a fvs c, k))
  | Ccase( c,x,cs) -> 
       if (id_compare x a) = 0
       then dcon(Ccase (substa ca a fvs c, x, cs)) 
       else if Set.member cfvs x then 
	  subst12d ca a fvs con
       else dcon (Ccase (substa ca a fvs c, x, List.map (substa ca a fvs) cs))
  | Cfold (k,c) -> dcon(Cfold(k, substa ca a fvs c))
  | Cpr (j,l) -> subst12d ca a fvs con 
       (* immediately fall-over *)
  | Cvoid _ -> con
(* -- end LX -- *)
  | Clab _ -> con
  | Cprim _ -> con
  | Crec fs ->
      if List.exists (fun (x,_,_) -> (id_compare x a)=0) fs then
	con
      else if List.exists (fun (x,_,_) -> Set.member cfvs x) fs then
	subst12d ca a fvs con
      else 
	wcon(Crec(List.map (fun (x,k,c) -> (x,k,substa ca a fvs c)) fs))
  | Cforall (x,k,c) -> 
      if (id_compare x a)=0 then con
      else if Set.member cfvs x then subst12d ca a fvs con
      else wcon(Cforall(x,k,substa ca a fvs c))
  | Cexist (x,k,c1,c2) -> 
      if (id_compare x a)=0 then con
      else if Set.member cfvs x then subst12d ca a fvs con
      else wcon(Cexist(x,k,substa ca a fvs c1,substa ca a fvs c2))
  | Cms ms -> wcon(Cms(ms_map (substa ca a fvs) ms))
  | Cmsjoin (c1,c2) -> dcon(Cmsjoin(substa ca a fvs c1,substa ca a fvs c2))
  | Ccode c -> wcon(Ccode(substa ca a fvs c))
  | Chptr (is,co,tco) ->
      let co = match co with None -> co | Some c -> Some (substa ca a fvs c) in
      let tco =
	match tco with None -> tco
 	| Some (c,v) -> Some (substa ca a fvs c,v) in
      chptr is co tco
  | Cfield (c,v) -> wcon (Cfield (substa ca a fvs c,v))
  | Cprod cs -> dcon (Cprod (List.map (substa ca a fvs) cs))
  | Csum cs -> wcon (Csum (List.map (substa ca a fvs) cs))
  | Carray (c1,c2) -> defcon (Carray (substa ca a fvs c1,substa ca a fvs c2))
  | Csing c -> wcon (Csing (substa ca a fvs c))
  | Csptr c -> wcon (Csptr (substa ca a fvs c))
  | Cempty -> con
  | Ccons (c1,c2) -> wcon(Ccons(substa ca a fvs c1,substa ca a fvs c2))
  | Cappend (c1,c2) -> dcon(Cappend(substa ca a fvs c1, substa ca a fvs c2))
(* arithmetic and logic *)
  | Clog (l,cs) -> dcon (Clog (l,List.map (substa ca a fvs) cs))
  | Cif (c1,c2) -> wcon (Cif (substa ca a fvs c1, substa ca a fvs c2))
(* alias stuff *)
  | Cname c -> wcon (Cname (substa ca a fvs c))
  | Ctagof c -> wcon (Ctagof (substa ca a fvs c))
  | Cjoin cs -> dcon (Cjoin(List.map (substa ca a fvs) cs))
  | Ccap d ->
     (* jgm: we're in big trouble here because it's crucial that
      * we never substitute anything but a Kname variable for a
      * Kname variable.  I don't think this will cause a problem
      * in practice but it's very unsettling. *)
      let f x (ai,c) d' = 
	let x' = 
	  if (id_compare x a)=0 then
	    begin
	      match ca.rcon with
		Cvar x' -> x'
	      |	_ -> failwith "Substituted a non-variable for a name"
	    end
	  else x in
	let c' = substa ca a fvs c in
	Dict.insert_new d' x' (ai,c')
      in wcon (Ccap(Dict.fold_dict f d (Dict.empty id_compare)))
(* Cyclone *)
  | Ctptr _ -> con
  | Ctmpl (c1,c2_opt,labels,holes) ->
      let c2_substa =
        match c2_opt with None -> None | Some c2 -> Some(substa ca a fvs c2) in
      wcon(Ctmpl(substa ca a fvs c1,
                   c2_substa,
                   List.map (fun (i,c) -> (i,substa ca a fvs c)) labels,
                   List.map (fun (i,c) -> (i,substa ca a fvs c)) holes))
  | Ctrgn (c1,None,t) ->
      wcon(Ctrgn(substa ca a fvs c1,
                   None,
                   List.map
                     (fun (v,labels,holes) ->
                       (v,
                        List.map
                          (fun (i,c) -> (i,substa ca a fvs c))
                          labels,
                        List.map
                          (fun (i,c) -> (i,substa ca a fvs c))
                          holes))
                     t))
  | Ctrgn (c1,Some c2,t) ->
      wcon(Ctrgn(substa ca a fvs c1,
                   Some(substa ca a fvs c2),
                   List.map
                     (fun (v,labels,holes) ->
                       (v,
                        List.map
                          (fun (i,c) -> (i,substa ca a fvs c))
                          labels,
                        List.map
                          (fun (i,c) -> (i,substa ca a fvs c))
                          holes))
                     t))
(* End Cyclone *)
  | Csubst(c,es) -> subst12d ca a fvs con
  | Cr ci -> 
       (match ci with 
	  RCon c -> wcon (Cr (RCon (substa ca a fvs c)))
	| _ -> con)
  | Ctypeof _ -> con
      
(* Single substitution - check for cutoff otherwise rcsubsta *)
(* c{a:=ca} where fvs=fv(ca) *)
and substa ca a fvs c =
  match c.freevars with
    None -> rcsubsta ca a fvs c
  | Some (kfvs,cfvs) ->
       if Set.member cfvs a then
	  rcsubsta ca a fvs c
       else 
 	  c
;;

(*** Substitution entry points ***)

(* k{a:=ka} -- kind single substitution *)
let ksubst ka a k = ksubstsa ((Dict.singleton id_compare a ka),(freevars_kind ka)) k;;

(* k{d}  -- kind multiple substition *)
let ksubsts kd k =
   let aux x c s = Set.union (freevars_kind k) s in 
   let s = Dict.fold_dict aux kd empty_set in
   ksubstsa (kd,s) k


(* an application of ksubsts *)
let unroll_kind k = 
   match k.rkind with 
      Kmu (schema, j) -> 
	 let body = List.assoc j schema in
	 let mus = List.map (fun (j,k) -> (j, defkind (Kmu (schema, j)))) schema in
	 let kd = Dict.inserts (Dict.empty id_compare) mus  in
	 ksubsts kd body
    | _ -> failwith "Unroll applied to non mu"

(* c{a:=ca} -- constructor single substitution *)
let subst ca a c = 
   let kfvs,cfvs as fvs = (freevars ca) in 
   let c = substa ca a fvs  c in 
   c	 
;;

(* c{d} -- constructor multiple substitution *)
let substs (kd,cd as d) c =
  let aux x c s = mappair Set.union s (freevars c) in
  let s = Dict.fold_dict (fun j k (ks,cs) -> (Set.union (freevars_kind k) ks,cs)) 
	kd (empty_set,empty_set) in
  let s = Dict.fold_dict aux cd s in
  substsa (d,s) c
;;

(*************************************************************************)
(* ctxt |- c : k                                                         *)
(*************************************************************************)

(* These functions return the least kind of a constructor in a given context.
 * They assume that the constructor is well formed and has been checked and
 * put into a canonical form.
 *)

let error ctxt c s = generate_error ctxt (Conwf (c,s));;

let km2t4 = karrow kmem k4byte

let primcon_kind ctxt pc = 
  match pc with
    PCbytes sc -> kbyte sc
  | PCfloat32  -> kbyte Byte4
  | PCfloat64  -> kbyte Byte8
  | PCjunk  i  -> kmemi i
  | PCjunkbytes sc -> kbyte sc
  | PCint   i  -> kint
  | PCtrue | PCfalse -> kbool
;;

let rec con_kind ctxt con =
  match con.rcon with
    Cvar a -> get_variable_kind ctxt a 
  | Clam(a,k1,c) -> karrow k1 (con_kind (add_var ctxt a k1) c)
  | Capp(c1,c2) ->
      (match (con_kind ctxt c1).rkind with
	Karrow(_,k2) -> k2
      |	_ -> error ctxt con "Capp: not a Karrow"; raise Talfail)
  | Ctuple cs -> kprod (List.map (con_kind ctxt) cs)
  | Cproj(i,c') ->
      (match (con_kind ctxt c').rkind with
	Kprod ks ->
	  (try List.nth ks i
	  with Failure _ -> 
	    error ctxt con "Cproj: index out of range"; raise Talfail)
      |	_ -> error ctxt con "Cproj: not a Kprod"; raise Talfail)
  | Cinj(i,c,k) -> k
  | Ccase(c,a,cs) -> 
       let k = con_kind ctxt c in 
       (match k.rkind with 
	  Ksum ks ->
	     (try 
	     	let l = 
		   List.map2 (fun k c -> 
		      con_kind (add_var ctxt a k) c) ks cs  in 
		(match l with 
		   (hd :: rest) -> 
		      List.fold_right (kindjoin ctxt) rest hd
		 | [] -> failwith "Ccase: compiler bug" )
	     with _ ->  error ctxt con "Ccase: wrong num branches";  raise Talfail)
	| _ ->     
	     error ctxt con "Ccase: not a Ksum"; raise Talfail)
  | Cfold (k,c) -> k	  
  | Cpr (i,l) -> 
       let schema = List.map (fun (j,a,k,f,k',c) -> (j,k)) l in 
       let schema = filter_schema schema in
       let rec getk' l = match l with (j,a,k,f,k',c)::rest -> if 
	  id_compare f i = 0 then (j,k') else getk' rest
	| [] -> failwith "Shouldn't get here" in 
       let (j,k') = getk' l in 
       
       let mus = List.map (fun (j,k) -> (j, defkind (Kmu (schema, j)))) schema in
       let kd = Dict.inserts (Dict.empty id_compare) mus  in
       let mu = defkind (Kmu(schema, j)) in 
       let ret = defkind(Karrow (mu, ksubsts kd k'))in 
       ret
  | Cvoid k -> k
(* -- end LX -- *)
  | Clab l -> get_label_kind ctxt l
  | Cprim pc -> primcon_kind ctxt pc
  | Crec fs -> kprod (List.map (fun (_,k,_) -> k) fs)
  | Cfield (c,_) ->
      (match (con_kind ctxt c).rkind with
	Kbyte s -> kmemi (scale_to_int32 s)
      |	Ktype -> kmem
      |	_ -> error ctxt con "Cfield: not a T/Ti"; kmem)
  | Cprod cs ->
      let rec aux n cs =
	match cs with
	  [] -> kmemi n
	| c::cs ->
	    match (con_kind ctxt c).rkind with
	      Kmemi i -> aux (n+$i) cs
	    | _ -> kmem in
      aux i32_0 cs
  | Csum cs ->
      (match cs with
	[] -> kmem
      |	c::cs ->
	  match (con_kind ctxt c).rkind with
	    Kmemi i ->
	      let rec aux cs =
		match cs with
		  [] -> kmemi i
		| c::cs ->
		    match (con_kind ctxt c).rkind with
		       Kmemi j when i=$j -> aux cs
		    | _ -> kmem in
	      aux cs
	  | _ -> kmem)
  | Carray (c1,c2) ->
      (match (con_kind ctxt c2).rkind with
	 Kmemi i ->
	  (match c1.rcon with
	    Cprim (PCint j) -> kmemi (i*$j)
	  | _ -> kmem)
      |	_ -> kmem)
  | Cms _ -> kms
  | Cmsjoin (_,_) -> kms
  | Cempty -> kstack
  | Ccons (_,_) -> kstack
  | Cappend (_,_) -> kstack
(* arithmetic and logic *)
  | Clog (l,cs) -> log_kind l
  | Cif (_,c2) -> con_kind ctxt c2
(* alias stuff *)
  | Cname x -> k4byte
  | Ccap d -> kcap
  | Cjoin cs -> kcap
  | Ctagof _ -> k4byte
(* Cyclone *)
  | Ctmpl _ | Ctrgn _ | Ctptr _ -> k4byte
(* End Cyclone *)
  | Csubst(c,s) -> con_kind (subst_kind ctxt s) c
  | Cr _ -> kmem
  (* Everything else is T4 *)
  | Cforall (_,_,_) | Cexist (_,_,_,_) | Ccode _ | Chptr (_,_,_) | Csing _ 
  | Csptr _ -> k4byte
  | Ctypeof l -> 
      get_label_con_opt ctxt l;  (* Ensure l is bound! *)
      k4byte
(* enter the substitution's variables into the context right to left *)
and subst_kind ctxt s = 
  match s with
    Enil -> ctxt
  | Es(x,c) -> add_var ctxt x (con_kind ctxt c)
  | Eo(s1,s2) -> subst_kind (subst_kind ctxt s2) s1
and log_kind l =
  match l with
    Cadd | Csub | Cmuls | Cmulu -> kint
  | Cand | Cor | Cimp | Ciff | Cnot | Clts | Cltu | Cltes | Clteu -> kbool
;;

(*************************************************************************)
(* Check constructor well formedness and canonicalise                    *)
(*************************************************************************)

(* Check the well formedness of a constructor, put it into canonical form,
 * and return its kind.  Also computes the normal form flag and free variable
 * set.
 *
 * Requires substitution, free variable calculation, constructor kinding, and
 * kind functions.
 *)

(* Canonical form:
 *   + Abbrevs expanded
 *   + Tags are non-pointer integers
 *   + Tags sorted and unique
 *   + Sum branches have an index
 *   + Sum branches sorted
 *)

(* l is sorted, removes duplicates *)
let remove_duplicates l =
  let rec aux l =
    match l with
      [] | [_] -> l
    | x1::(x2::rest as tl) ->
 	if x1=x2 then aux (x1::rest) else x1::(aux tl) in
  aux l
;;

(* Assumes the list is sorted.  Returns true if no two adjacent elements
   are equal, and false otherwise. *)
let all_unique cmp l =
  let rec aux l =
    match l with
    | [] -> true
    | [hd] -> true
    | hd1::(hd2::rest as tl) -> if (cmp hd1 hd2 == 0) then false else aux tl in
  aux l
;;

(* A sum branch must start with a known tag.
 * The forms S(i)^_ and *[S(i)^_,...] are acceptable
 *)
let sum_index error c =
  match c.rcon with
    Cfield ({rcon=Csing {rcon=Cprim (PCint i)}},_) -> i
  | Cprod ({rcon=Cfield ({rcon=Csing {rcon=Cprim (PCint i)}},_)}::_) -> i
  | _ -> error (); raise Talfail
;;

let expand_abbrevs ctxt c = substs (get_kindabbrevs ctxt, get_abbrevs ctxt) c;;

(* checks the kind of a constructor, expands abbrevs entered in the context,
 * and returns the kind and new constructor.
 *)
let check ctxt c =
  let rec ck (ctxt : ctxt) (con : con) = 
    match con.abbrev with
      Some x -> con_kind ctxt con
    | _ -> 
    let c = con.rcon in
    match c with
      Cvar a -> 
	begin
	  (*
	  try 
	    let c = Dict.lookup (get_abbrevs ctxt) a in
	    con.rcon <- c.rcon;
	    con.con_state <- c.con_state;
	    con.freevars <- c.freevars;
	    con.abbrev <- (Some a);
	    (* con.hash <- c.hash; *)
	    con_kind ctxt c
	  with Dict.Absent -> *) get_variable_kind ctxt a
	end
    | Clam(a,k1,c) -> 
	check_kind ctxt k1;
	let k2 = ck (add_var ctxt a k1) c in
	karrow k1 k2
    | Capp(c1,c2) ->
	let k1 = ck ctxt c1 in
	let k2 = ck ctxt c2 in
	begin
	  match k1.rkind with
	    Karrow(ka,kb) -> 
	      kindleq ctxt k2 ka; kb
	  | _ -> error ctxt con "Capp: not a Karrow"; raise Talfail
	end
    | Ctuple cs ->
	 (* This case was incorrect in talc-1.5 *)
	 let ks = List.map (ck ctxt) cs in 
	 kprod ks 
    | Cproj(i,c') ->
	 let k = ck ctxt c' in
	 begin
	    match k.rkind with
	       Kprod ks ->
	      	  (try (List.nth ks i) with
		     Failure _ -> error ctxt con "Cproj: index out of range"; 
		      	raise Talfail)
	     | _ -> (error ctxt con "Cproj: not a Kprod"; raise Talfail)
	end
(* ---- LX ---- *)
    | Cinj(i,c',k) -> 
	 let ki = ck ctxt c' in 
	 begin 
	    check_kind ctxt k;
	    match k.rkind with 
	       Ksum ks -> 
		  (try  kindleq ctxt (List.nth ks i) ki;
		     k
		  with Failure _ -> error ctxt con "Cinj: index out of range";
		     raise Talfail)
	     | Kvar _ -> failwith "Cinj: should have been expanded"
	     | _ -> error ctxt con "Cinj: not a Ksum"; raise Talfail
	 end
    | Ccase(c,a,cs) ->
	 let k = ck ctxt c in 
	 (match k.rkind with 
	    Ksum ks -> 
	       let results = 
		  try (List.map2 (fun c k ->
		     ck (add_var ctxt a k) c)) cs ks

		  with Invalid_argument _ -> 
		     error ctxt con "Ccase: wrong number of branches in sum";
		     raise Talfail in
	       let result = (match results with 
               	  (hd::rest) -> 
	     	     List.fold_right (kindjoin ctxt) rest hd (** kindjoin ?? **) 
	      	|	[] -> error ctxt con "Ccase: no arms in sum";raise Talfail) in 
	       result
    	  | _ -> error ctxt con "Ccase: not a Ksum"; raise Talfail)
    | Cfold (k,c) ->
	 (let kunfold = ck ctxt c in
	 check_kind ctxt k;
	 match k.rkind with 
            Kmu(ks,j) -> 
	       let k' = unroll_kind k in 
               kindleq ctxt kunfold k' ;
		k
	   | _ -> error ctxt con "Cfold: not given a Kmu"; raise Talfail)
    | Cpr (i,l) ->
	 let ctxt = set_verify_ctxt ctxt "cpr well-formedness check" in
	 let schema = List.map (fun (j,a,k,f,k',c) -> (j,k)) l in 
	 let schema = filter_schema schema 
	 in
	 let rec getk' l = match l with (j,a,k,f,k',c)::rest -> 
	    if id_compare f i = 0 then (j,k') else getk' rest
	  | [] -> error ctxt con "Cpr: var does not appear in kind of recurrence function";
               raise Talfail in 
	 let mus = List.map (fun (j,k) -> (j, defkind (Kmu (schema, j)))) schema in
	 List.iter (fun (i,k) -> check_kind ctxt k; ()) mus;
	 let kd = Dict.inserts (Dict.empty id_compare) mus  in
	 let (j,_) = getk' l in 
	 let mu = defkind (Kmu(schema, j)) in 
	 (* create the context for checking the bodies of the pr
	  * by adding each kind variable j and con variable f *)  
	 let innerctxt1 = List.fold_right (fun (j,a,k,f,k',c) ctxt -> 
	    add_kind ctxt j) l ctxt in
	 let innerctxt = List.fold_right (fun  (j,a,k,f,k',c) ctxt ->
	    check_kind ctxt k';
	    (add_var ctxt f (karrow (kvar j) k'))) l innerctxt1 in 
	 let l = List.map (fun (j,a,k,f,k',c) ->
	    let rek = ck (add_var innerctxt a k) c in
            kindeq ctxt (ksubsts kd rek) (ksubsts kd k');
            (j,a,k,f,k',c)) l in 
	 let (j,k') = getk' l in
	 karrow mu (ksubsts kd k') 
    | Cvoid k ->
	 (match k.rkind with 
	    Karrow (_,_) | Kprod _ | Ksum _ | Kvar _ | Kmu (_,_) -> 
	       error ctxt con "void: only available at base kinds";
	       raise Talfail 
	  | _ ->  k)
    (* -- end LX -- *)


    | Clab l -> let k = get_label_kind ctxt l in k
    | Cprim pc -> primcon_kind ctxt pc
    | Crec fs ->
	let g ctxt (a,k,_) = 
	   begin
	      check_kind ctxt k; 
	      add_var ctxt a k 
	   end in
	let ctxt' = List.fold_left g ctxt fs in
	let check_f (a,k,c) = 
	  let k' = ck ctxt' c in
	  kindleq ctxt' k' k in
	List.iter check_f fs;
	kprod (List.map (fun (_,k,_) -> k) fs)
    | Cforall(a,k,c) ->
	check_kind ctxt k;
	let k' = ck (add_var ctxt a k) c in
	kindleq ctxt k' k4byte;
	k4byte
    | Cexist(a,k,c1,c2) ->
	check_kind ctxt k;
	let ctxt = add_var ctxt a k in
	let k1 = ck ctxt c1 in
	let k2 = ck ctxt c2 in
	kindleq ctxt k1 kbool;
	kindleq ctxt k2 k4byte;
	k4byte
    | Cms ms ->
	(* JGM: we could be a bit stronger here and verify that in fact
	 * the TLA is a pointer to a tuple, but then what the hell --
	 * you can't use it if it's not.
	 *)
	let ck_dict c = 
	  let k = ck ctxt c in
	  begin
	    kindleq ctxt k k4byte
	  end in
	ms_app_reg (fun r -> ck_dict) ms;
	kindleq ctxt (ck ctxt (ms_get_cap ms)) kcap;
 	kms
    | Cmsjoin(c1,c2) -> 
	kindleq ctxt (ck ctxt c1) kms; 
	kindleq ctxt (ck ctxt c2) kms; kms
    | Ccode c -> kindleq ctxt (ck ctxt c) kms; k4byte
    | Chptr (is,co,tco) ->
	let is = remove_duplicates (Sort.list (fun i j -> i<=j) is) in
	if not (List.for_all is_non_pointer_integer is) then
	  error ctxt con "Chptr: possible pointer tag";
	(match co with
	  None -> ()
	| Some c -> 
	    let k' = ck ctxt c in
	    kindleq ctxt k' kmem);
        (match tco with
	  None -> ()
	| Some (c,v) ->
	    ck ctxt c; ());
	k4byte
    | Cfield (c,v) ->
	let k' = ck ctxt c in
	let k =
	  match k'.rkind with
	    Kbyte s -> kmemi (scale_to_int32 s)
	  | Ktype -> kmem
	  | _ -> error ctxt con "Cfield: not a type"; kmem in
	k
    | Cprod cs ->
	let rec aux sz cs =
	  match cs with
	    [] ->
	      (match sz with None -> kmem | Some i -> kmemi i)
	  | c::cs ->
	      let k = ck ctxt c in
	      match k.rkind with
		Kmem -> aux None cs 
	      |	Kmemi i ->
		  let sz =
		    match sz with None -> None | Some j -> Some (i+$j) in
		  aux sz cs
	      |	_ ->
		  error ctxt con "Cprod: not a Tm";
		  aux None cs in
	aux (Some i32_0) cs
    | Csum [] -> kmem
    | Csum (c1::_ as cs) ->
	let sz =
	  match (ck ctxt c1).rkind with
	    Kmem -> None
	  | Kmemi i -> Some i
	  | _ -> error ctxt con "Csum: not a Tm"; None in
	let rec aux sz cs new_cs =
	  match cs with
	    [] ->
	      ((match sz with None -> kmem | Some i -> kmemi i),new_cs)
	  | c::cs ->
	      let k = ck ctxt c in
	      let k =
	      	match k.rkind with
		  Kmem -> None
	      	| Kmemi i -> (match sz with Some j when i=$j -> sz | _ -> None)
	      	| _ -> error ctxt con "Csum: not a Tm"; None in
	      let err () = error ctxt con "Csum: no tag" in
	      let i = sum_index err c in
	      aux sz cs ((i,c)::new_cs) in
	let k,ics = aux sz cs [] in
	let _,cs = List.split (Sort.list (fun (i,_) (j,_) -> i<=$j) ics) in
	con.rcon <- Csum cs;
	k
    | Carray (c1,c2) ->
	let k1 = ck ctxt c1
	and k2 = ck ctxt c2 in
	kindleq ctxt k1 kint; kindleq ctxt k2 kmem;
	con_kind ctxt con
    | Csing c ->
	let k' = ck ctxt c in
	kindleq ctxt k' kint;
	k4byte
    | Csptr c ->
	let k' = ck ctxt c in
	kindleq ctxt k' kstack;
	k4byte
    | Cempty -> 
	kstack
    | Ccons(c1,c2) ->
	let k1 = ck ctxt c1 in
	let k2 = ck ctxt c2 in
	(match k1.rkind with
	  Ktype | Kbyte _ | Kmem | Kmemi _ -> ()
	| _ -> error ctxt con "Ccons: head must be T or Tm");
 	kindleq ctxt k2 kstack; 
	kstack
    | Cappend(c1,c2) ->
	let k1 = ck ctxt c1 in
	let k2 = ck ctxt c2 in	
	kindleq ctxt k1 kstack; kindleq ctxt k2 kstack;
	kstack
(* arithmetic and logic *)
    | Clog (l,cs) -> ck_log ctxt l cs
    | Cif (c1,c2) ->
	let k1 = ck ctxt c1 in
	let k2 = ck ctxt c2 in
	kindleq ctxt k1 kbool;
	k2
(* alias stuff *)
    | Cname c ->
	let k = ck ctxt c in
	kindleq ctxt k kname;
	k4byte
    | Cjoin cs ->
	List.iter (fun c -> kindleq ctxt (ck ctxt c) kcap) cs;
	kcap;
    | Ctagof c ->
	let k = ck ctxt c in
	kindleq ctxt k kname;
	k4byte
    | Ccap d ->
	(* must check that each id x is a Kname and that the cons to which
	 * they map are of kind k4byte.  To avoid rewriting the dictionary
	 * in place, we prohibit abbreviations that conflict with a given
	 * Kname variable in the domain of the dictionary.
	 *)
	let f x (ai,c) unit = 
	  begin
	    if Dict.member (get_abbrevs ctxt) x then
	      error ctxt con "abbreviation maps Kname";
	    kindleq ctxt (get_variable_kind ctxt x) kname;
	    kindleq ctxt (ck ctxt c) k4byte
	  end
	in Dict.fold_dict f d (); kcap
(* Cyclone *)
    | Ctptr _ -> k4byte
    | Ctmpl(c1,c2_opt,labels,holes) ->
	let k1 = ck ctxt c1 in
	kindleq ctxt k1 k4byte;
        (match c2_opt with
          None -> ()
        | Some c2 ->
            let k2 = ck ctxt c2 in
	    kindleq ctxt k2 k4byte);
        let ck_id_con (i,c) =
          begin
            let k = ck ctxt c in
	    kindleq ctxt k k4byte
          end in
	let id_con_cmp (i1,_) (i2,_) = id_compare i1 i2 in
	let labels = List.sort id_con_cmp labels in
	let holes = List.sort id_con_cmp holes in
	if not (all_unique id_con_cmp holes) 
	then error ctxt con "Duplicate holes in template.";
	if not (all_unique id_con_cmp labels) 
	then error ctxt con "Duplicate labels in template.";
        List.iter ck_id_con labels;
        List.iter ck_id_con holes;
	k4byte
    | Ctrgn(c1,None,t) ->
	let k1 = ck ctxt c1 in
	let id_br_templ_cmp (i1,_,_) (i2,_,_) = id_compare i1 i2 in
	let t = List.sort id_br_templ_cmp t in
	if not (all_unique id_br_templ_cmp t) then 
	  error ctxt con "Duplicate template pointers in region.";
	let ck_br_templ (a,labels,holes) =
          let ck_id_con (i,c) =
            begin
              let k = ck ctxt c in
              kindleq ctxt k k4byte
            end in
	  let id_con_cmp (i1,_) (i2,_) = id_compare i1 i2 in
	  let labels = List.sort id_con_cmp labels in
	  let holes = List.sort id_con_cmp holes in
	  if not (all_unique id_con_cmp holes) 
	  then error ctxt con "Duplicate holes in code region template.";
	  if not (all_unique id_con_cmp labels) 
	  then error ctxt con "Duplicate labels in code region template.";
          List.iter ck_id_con labels;
          List.iter ck_id_con holes 
	in
	List.iter ck_br_templ t;
	k4byte
    | Ctrgn(c1,Some c2,t) ->
	let k1 = ck ctxt c1 in
	let k2 = ck ctxt c2 in
        List.iter
          (fun (a,labels,holes) ->
            let ck_id_con (i,c) =
              begin
                let k = ck ctxt c in
                kindleq ctxt k k4byte
              end in
              (* XXX - SHOULD CHECK THAT LABELS & HOLES ARE UNIQUE *)
              List.iter ck_id_con labels;
              List.iter ck_id_con holes)
          t;
 	k4byte
(* End Cyclone *)
    | Csubst(c,es) ->
	let rec ck_subst ctxt es = 
	  match es with
	    Enil -> ctxt
	  | Es(x,c) -> let k = ck ctxt c in add_var ctxt x k
	  | Eo(es1,es2) ->
	      let ctxt = ck_subst ctxt es2 in
	      let ctxt = ck_subst ctxt es1 in
	      ctxt in
	let ctxt = ck_subst ctxt es in
	let k = ck ctxt c in
	k
    | Cr ci ->
	 (match ci with 
	    RCon c -> ignore (ck ctxt c)
	  | _ -> ());
	 kmem
    | Ctypeof l -> get_label_con_opt ctxt l; k4byte
    	    
  and ck_log ctxt l cs =
    match l with
      Cadd | Cmuls | Cmulu -> 
      	List.iter (fun c -> kindleq ctxt (ck ctxt c) kint) cs; kint
    | Csub ->
      	(match cs with
	  [c1;c2] ->
	    kindleq ctxt (ck ctxt c1) kint;
	    kindleq ctxt (ck ctxt c2) kint;
	    kint
      	| _ -> error ctxt (clog l cs) "Csub not binary operator";raise Talfail)
    | Cand | Cor ->
      	List.iter (fun c -> kindleq ctxt (ck ctxt c) kbool) cs; kbool
    | Cimp | Ciff ->
      (match cs with 
	[c1;c2] -> 
	  kindleq ctxt (ck ctxt c1) kbool;
	  kindleq ctxt (ck ctxt c2) kbool;
	  kbool
      |	_ -> error ctxt (clog l cs) "Cimp or Ciff not binary operators";
	  raise Talfail)
    | Cnot ->
      	(match cs with 
	  [c1] -> kindleq ctxt (ck ctxt c1) kbool; kbool
      	| _ ->  error ctxt (clog l cs) "Cnot not unary operator";raise Talfail)
    | Clts | Cltu | Cltes | Clteu ->
      	(match cs with
	  [c1;c2] -> 
	    kindleq ctxt (ck ctxt c1) kint;
	    kindleq ctxt (ck ctxt c2) kint;
	    kbool
      	| _ -> error ctxt (clog l cs) "Relop not binary operator";
	    raise Talfail)
  in let c = expand_abbrevs ctxt c in
  (ck ctxt c,c)
;;

(* check a machine state *)
let verify_gamma ctxt gamma =
(*  let ctxt = set_verify_ctxt ctxt "checking machine state" in *)
  let f _ c = let (k,_) = check ctxt c in kindleq ctxt k k4byte in
  ms_app_reg f gamma; 
  let (k,_) = check ctxt (ms_get_cap gamma) in kindleq ctxt k kcap;
  gamma
;;

(*************************************************************************)
(*** From here on, all functions assume the constructors are checked!  ***)
(*************************************************************************)

(*************************************************************************)
(* Normalization of constructors                                         *)
(*************************************************************************)

(* JGM: to avoid hair with the capability stuff, I've commented out all
 * of the explicit substitution stuff.

(* Given variable x and explicit substitution es, returns true if x 
 * occurs either in the domain of es, or else x occurs free in the
 * co-domain of es.  Used as a heuristic to decide when to rename a
 * bound variable.
 *)
let rec variable_conflict x es = 
  match es with
    Enil -> false
  | Es(y,c') -> ((id_compare x y) = 0) or (Set.member (freevars c') x)
  | Eo(es1,es2) -> (variable_conflict x es1) or (variable_conflict x es2)
;;

(* given a set of free variables s and an explicit substitution es,
 * determines whether the substitution will have any effect.  Returns
 * true if the substitution will have no effect.
 *)
let rec nosubst s es = 
  match es with
    Enil -> true
  | Es(y,c') -> not (Set.member s y)
  | Eo(es1,es2) -> nosubst s es1 & nosubst s es2
;;

(* delete stuff from the substitution that's not needed -- if no deletion
 * occurs then leave the substitution alone to preserve sharing.  In practice,
 * not effective...
 *)
let trim_subst s es = 
  let changed = ref false in
  let rec trimit s es = 
    match es with
      Enil -> (s,es)
    | Es(x,c) ->
	if Set.member s x then
	  (Set.union (Set.delete s x) (freevars c),es)
	else 
	  (changed := true; (s,Enil))
    | Eo(es1,es2) ->
	let (s,es1) = trimit s es1 in
	let (s,es2) = trimit s es2 in
	(s,match es1,es2 with
	  Enil,_ -> (changed := true; es2)
	| _,Enil -> (changed := true; es1)
	| es1,es2 -> Eo(es1,es2))
  in let (_,es') = trimit s es in
  if (!changed) then es' else es

(* push a substitution down -- used by whcon.  Can probably be 
 * improved a lot.
 *)
let rec push_subst (c : con) (es : esubst) : con = 
  if nosubst (freevars c) es then c else 
  match c.rcon with
    Cvar x ->
      (* Look up the variable in the substitution *)
      (match es with
	Enil -> c
      |	Es(y,c') -> if (id_compare x y) = 0 then c' else c
      | Eo(es1,es2) -> push_subst (push_subst c es1) es2)
  | Clam(x,k,c1) -> 
      (* If x occurs in the substitution, rename it.  Could do better
       * when x is in the domain of the substitution by just trimming
       * the substitution, but we expect this to be rare.  Note that
       * Crec, Cforall, and Cexist use the same or similar code.
       *)
      if variable_conflict x es then 
	let x' = id_unique x in
	let es = Eo(Es(x,defvarcon x'),es) in
	clam x' k (csubst c1 es)
      else clam x k (csubst c1 es) 
  | Crec(vkcs) ->
      let rec f vkcs es = 
	match vkcs with
	  [] -> ([],es)
	| (x,k,c)::rest -> 
	    if variable_conflict x es then
	      let x' = id_unique x in
	      let es' = Eo(Es(x,defvarcon x'),es) in
	      let rest',es' = f rest es' in
	      ((x',k,csubst c es')::rest',es')
	    else
	      let rest',es' = f rest es in
	      ((x,k,csubst c es')::rest',es') in
      let vkcs',_ = f vkcs es in crec vkcs'
  | Cforall(x,k,c1) ->
      if variable_conflict x es then 
	let x' = id_unique x in
	let es = Eo(Es(x,defvarcon x'),es) in
	cforall x' k (csubst c1 es)
      else cforall x k (csubst c1 es) 
  | Cexist(x,k,c1,c2) ->
      if variable_conflict x es then 
	let x' = id_unique x in
	let es = Eo(Es(x,defvarcon x'),es) in
	cexist x' k (csubst c1 es) (csubst c2 es)
      else cexist x k (csubst c1 es) (csubst c2 es)
    (* push a composed substitution *)
  | Csubst(c,es') -> push_subst c (Eo(es',es))
    (* the rest of the cases just push the substitution into the sub-terms *)
  | Capp(c1,c2) -> capp (csubst c1 es) (csubst c2 es)
  | Ctuple cs -> ctuple (List.map (fun c -> csubst c es) cs)
  | Cproj(i,c) -> cproj (csubst c es) i
  | Clab _ -> c (* shouldn't occur due to nosubst check *)
  | Cprim _ -> c (* shouldn't occur due to nosubst check *)
  | Ccode rs -> ccode(ms_map (fun c -> csubst c es) rs)
  | Chptr(is,co,tco) ->
      let co = match co with None -> None | Some c -> Some (csubst c es) in
      let tco =
 	match tco with None -> None | Some (c,v) -> Some (csubst c es,v) in 
      chptr is co tco
  | Cfield(c,v) -> cfield (csubst c es) v
  | Cprod cs -> cprod(List.map (fun c -> csubst c es) cs)
  | Csum cs -> csum(List.map (fun c -> csubst c es) cs)
  | Carray(c1,c2) -> carray (csubst c1 es) (csubst c2 es)
  | Csing c -> csing(csubst c es)
  | Csptr c -> csptr(csubst c es)
  | Cempty -> c (* shouldn't occur due to nosubst check *)
  | Ccons(c1,c2) -> ccons (csubst c1 es) (csubst c2 es)
  | Cappend(c1,c2) -> cappend (csubst c1 es) (csubst c2 es)
  (* arithmetic and logic *)
  | Clog (l,cs) -> clog l (List.map (fun c -> csubst c es) cs)
  | Cif (c1,c2) -> cif (csubst c1 es) (csubst c2 es) 
  (* Cyclone *)
  | Ctmpl(c,copt,xcs1,xcs2) ->
      let f = List.map (fun (x,c) -> (x,csubst c es)) in
      ctmpl(csubst c es,
	    (match copt with None -> None | Some c -> Some (csubst c es)),
	    f xcs1,f xcs2)
  | Ctptr _ -> c
  | Ctrgn(c,copt,xl) ->
      let f = List.map (fun (x,c) -> (x,csubst c es)) in
      ctrgn(csubst c es,
	    (match copt with None -> None | Some c -> Some (csubst c es)),
	    List.map (fun (x,xc1,xc2) -> (x,f xc1,f xc2)) xl)
*)

(* weak-head normalization *)
let rec whnorm ctxt c =
  let defcon cs rc = 
    let con = defcon rc in
    con.con_state <- cs;
    con.freevars <- Some(rc_freevars rc);
    con in
  let copy_con c1 c2 = 
    c1.rcon <- c2.rcon;
    c1.con_state <- c2.con_state;
    c1.freevars <- c2.freevars;
    (* c1.hash <- c2.hash; *)
    c2 in
  (* JGM: to avoid hair with the capability stuff, I've commented out
   * all of the explicit substitution stuff so ignore the following
   * comment.
   *)
  (* Uncomment the following line to use explicit substitutions.  Doing
   * substitutions eagerly seems to win for now.  Perhaps hash-consing
   * would improve things.
   *)
  (* let subst c2 x c3 = push_subst c3 (Es(x,c2)) in *)
  let rec wh (c : con) : con =
    if c.con_state <> NotNorm then c else  
    let c' = try hash_find c.rcon with Not_found -> c in 
    if c' != c then (copy_con c (wh c')) else
    match c.rcon with
      Capp(c1,c2) ->
	begin
	  wh c1;
	  match c1.rcon with
	    Clam(x,k,c3) -> 
(*
	      debug "lambda";
	      debug2 
		(fun fmt o -> print_con fmt {o with expand_abbrevs=true} c);
*)
	      let c' = 
	      	(match c2.rcon with
		  Cvar y -> 
		    if id_compare x y = 0 then wh c3
		    else wh (subst c2 x c3)
		| _ -> 
		    let c4 = (subst c2 x c3) in
		  (*  debug2 (fun fmt o -> print_con fmt o c4); *)
		    wh c4) in
	      copy_con c c'
	   (* -- LX -- *)
	   | Cpr (i, l) -> 
		let c' = 
		   (match (wh c2).rcon with 
		      Cfold (k,arg) ->
			 (match k.rkind with 
			    Kmu (schema, i2) ->
			    (* assume that i is equivalent to i2 *)
			       let js,ks = List.split schema in
			       let schema2 = List.map (fun (j,a,k,f,k',body) -> (j,k)) l in 
			       let schema2 = filter_schema schema2 in
			       let kd = List.fold_right2 
				     (fun (j,k) j2 kd -> 
				     	Dict.insert kd j
					(* Note, Kmu(schema,j2) should be alpha-equivalent to
					   Kmu (get_schema l, j), where get_schema is the 
					   map (fun (j,_,k,_,_,_) -> j,k) l *)
					   {rkind=Kmu(schema, j2); 
					      freekindvars=k.freekindvars;
					      kabbrev=Some j2})
				     schema2 js (Dict.empty id_compare) in 
			       let cd = List.fold_right
				     (fun (j,_,_,f,_,_) cd ->
				     	Dict.insert cd f { c1 with rcon = Cpr(f,l)}) l 
				     (Dict.empty id_compare) in 
			       let rec aux l = match l with (_,a,_,f,_,body)::rest -> 
				  if id_compare f i = 0 then a,body else aux rest
			     	| [] -> failwith "BUG: Pr not well-formed" in
			       let a,body = aux l in 
			       let cd = Dict.insert cd a arg in 
			       let ret = wh (substs (kd,cd) body) in 
(*			       (debug2 (fun fmt o -> 
				  pp_print_string fmt "after norm of : "; print_con fmt o c; 
				  pp_print_string fmt " the result is : ";
				  print_con fmt o ret)); *)
			       ret 

			  | _ -> failwith "BUG: cfold not well-formed")
		    | _ -> c.con_state <- WeakHead;  c) in
		copy_con c c'
	   (* -- end LX -- *)	   
	   | _ -> c.con_state <- WeakHead; c
	end
    | Cproj(i,c1) ->
	begin
	    wh c1;
	    match c1.rcon with
	      Ctuple cs -> 
		let c' = wh (List.nth cs i) in
		copy_con c c'
	    | _ -> c.con_state <- WeakHead; c
	end
    (* -- LX -- *)	    
    | Ccase(c1, a, cs) -> 
	 begin
	    wh c1;
	    match c1.rcon with 
	       Cinj(i,c2,k) ->
		  let branch = (try List.nth cs i
		  with Failure _ -> failwith "Ccase not well formed in wh") in 
		  let c' = wh (subst c2 a branch) in
		  copy_con c c'
	     | c1 -> c.con_state <- WeakHead; c
	 end
    (* -- end LX -- *)
    (* Flatten out products *)
    | Cprod c1s -> 
	 List.iter (fun c -> ignore (wh c)) c1s;
	 let aux acc c1 = 
	    match c1.rcon with 
	       Cprod c2 -> List.rev_append c2 acc 
	     | _ -> c1 :: acc in
	 let c' = wcon (Cprod (List.rev (List.fold_left aux [] c1s))) in
(*	 debug2 (fun fmt o -> 
	    pp_print_string fmt "Prod before:";
	    print_con fmt o c); *)
	 copy_con c c'
    | Cmsjoin(c1,c2) ->
	begin
	  wh c1;
	  wh c2;
	  match c1.rcon,c2.rcon with
	    Cms ms1,Cms ms2 ->
	      let ms = ms_join ms1 ms2 in
	      let c' = wcon (Cms ms) in
	      copy_con c c'
	  | _,_ -> c.con_state <- WeakHead; c
	end
    | Cappend(c1,c2) ->
	begin
	  wh c1;
	  match c1.rcon with
	    Cempty -> 
	      let c' = wh c2 in
	      copy_con c c'
	  | Ccons(f,c1) -> 
	      let c' = wcon (Ccons(f,defcon NotNorm (Cappend(c1,c2)))) in
	      copy_con c c'
	  | Cappend(ca,cb) -> 
	      let c' = wcon (Cappend(ca,defcon NotNorm (Cappend(cb,c2)))) in
	      wh(copy_con c c')
	  | c1 -> c.con_state <- WeakHead; c
	end
    | Clog (l,cs) ->
	(* Normalization obeys the following rules:
         * 1. plus,and,or are associative and commutative (see Cjoin below)
         * 2. mul is assumed binary (no normalization otherwise) and 
         *    has normal form const*x when const > 1.
	 * 3. 0/1/true/false are units for plus/mul/and/or
         * 4. 0*x = 0
         * 5. i*(j*c) = k*c where k = i*j
         * 6. fold other constants that don't overflow for plus/mul/sub/and/or
	 * 7. fold constants in inequalities: i < j, i <= j, i <# j, i <=# j  
         * 8. multiplication is distributed over addition: 
	 *    a*(b+c) -> a*b + a*c
         *) 
	let set_rc    rc = c.rcon <- rc                                      in
	let set_cs    cs = c.con_state <- cs                                 in
	let set_fv    fv = c.freevars <- fv                                  in
	let set_con   c' = copy_con c c'; () in
	let set_rc    rc = let c' = wcon rc in copy_con c c'; () in
	let set_const rc = let c' = prcon rc in copy_con c c'; () in
	let set_int  i32 = set_const (Cprim (PCint i32)) in
	(* constant fold subtraction *)
	let process_sub cs =
	  match cs with
	    [c1;c2] -> set_rc (Clog (Cadd, [c1;cmuls (~-$ i32_1) c2])); wh c;()
	  | _ -> failwith "whnorm: non-binary subtraction" in
 
	(* flatten cons *)
	let f_aux unit_con cs next =
	  match next.rcon with
	    Clog (l',cs') -> 
	      if l = l' then cs' @ cs
	      else next::cs
	  | _ ->
	      if next = unit_con then cs
	      else next::cs in
	let flatten unit_con cs = List.fold_left (f_aux unit_con) [] cs in

	(* flattening/constant folding/sorting for assoc-commut ops *)
	let process_assoc l cs unit_con op =
	  let cs = flatten unit_con cs in
	  let separate (consts,others) c =
	    match c.rcon with 
	      Cprim _ -> (c::consts,others)
	    | _ -> (consts,c::others) in
	  let consts,others = List.fold_left separate ([],[]) cs in
	  let cs = 
	    try (List.fold_left op unit_con consts)::others
	    with Invalid_argument _ -> consts@others in
	  begin
	    match cs with
	      [] -> set_const unit_con.rcon
	    | [c] -> set_con c
	    | c1::((c2::cs') as rest) -> 
	      	if c1.rcon = unit_con.rcon then 
		  match cs' with 
		    [] -> set_con c2;
		  | _ -> set_rc (Clog (l,Sort.list (<=) rest))
	      	else (set_rc (Clog (l,Sort.list (<=) cs)))
	  end in

	(* constant fold/reorder to const*x *)
	let rec fold_mul l cs mulop =
	    match cs with
	      [{rcon=Cprim (PCint i)};{rcon=Cprim (PCint j)}] -> 
		(try set_int (mulop i j) with Invalid_argument _ -> ())
	    | [({rcon=Cprim (PCint i)} as c1);c2] -> 
		if i =$ i32_0 then set_int i32_0
		else if i =$ i32_1 then set_con c2
		else 
		  (match c2.rcon with
		    Clog (l',[{rcon=Cprim (PCint j)};c3]) when l' = l ->
		      (* fold i*(j*c) -> k*c where k = i*j *)
		      (try 
			let k = mulop i j in
			set_con (clog l [pcint k;c3])
		      with Invalid_argument _ -> ())
		  | _ -> ())		      
	    | [c1;({rcon=Cprim (PCint i)} as c2)] -> 
		fold_mul l [c2;c1] mulop
	    | _ -> () in

	(* attempt to distribute mul over add and return true if distributed *)
	let distribute_mul l cs =
	  match cs with
	  | [c1;{rcon=Clog (Cadd,cs')}] -> 
	      set_rc (Clog (Cadd,List.map (fun c' -> clog l [c1;c']) cs'));
	      true
	  | [{rcon=Clog (Cadd,cs')};c2] -> 
	      set_rc (Clog (Cadd,List.map (fun c' -> clog l [c2;c']) cs'));
	      true
	  | _ -> false in	      

        let process_mul l cs mulop =
	  fold_mul l cs mulop;
	  match c.rcon with
	    Clog ((Cmuls | Cmulu) as l,cs) -> 
	      if distribute_mul l cs then (wh c; ())
	      else ()
	  | _ -> () in

	(* constant folding inequalities *)
	let process_ineq l cs int32op =
	  match cs with
	    [{rcon=Cprim (PCint i)};{rcon=Cprim (PCint j)}] -> 
	      if int32op i j then set_const (Cprim PCtrue)
	      else set_const (Cprim PCfalse)
	  | _ -> () in
		
	(* weakhead normalize constructors *)
	begin
	  List.iter (fun c -> wh c; ()) cs;
	  (match l with
	    Cadd ->
	      let addop c1 c2 =
	    	match c1.rcon,c2.rcon with
		  Cprim (PCint i), Cprim (PCint j) -> pcint (add32_over i j)
	    	| _,_ -> invalid_arg "addop" in
	      process_assoc l cs (pcint (int_to_int32 0)) addop
	  | Cand ->
	      let andop c1 c2 = 
	      	match c1.rcon,c2.rcon with
		  Cprim PCtrue,_  -> c2
	      	| _,Cprim PCtrue  -> c1
	      	| Cprim PCfalse,_ -> pcfalse
	      	| _,Cprim PCfalse -> pcfalse
	      	| _,_ -> invalid_arg "andop" in
	      process_assoc l cs pctrue andop
	  | Cor ->
	      let orop c1 c2 = 
	      	match c1.rcon,c2.rcon with
		  Cprim PCfalse,_ -> c2
	      	| _,Cprim PCfalse -> c1
	      	| Cprim PCtrue,_  -> pctrue
	      	| _,Cprim PCtrue  -> pctrue
	      	| _,_ -> invalid_arg "orop" in
	      process_assoc l cs pcfalse orop
	  | Cmuls -> process_mul l cs signed_mul32_over 
	  | Cmulu -> process_mul l cs unsigned_mul32_over
	  | Csub  -> process_sub cs
	  | Clts  -> process_ineq l cs (<$)
	  | Cltes -> process_ineq l cs (<=$)
	  | Cltu  -> process_ineq l cs (unsigned_lt32)
	  | Clteu -> process_ineq l cs (unsigned_lte32)
	  | _ -> ()); (* no reductions for other operators *) 
	  c
	end
    | Cjoin cs ->
	(* The intention here is to simulate Cjoin being associative and
	 * commutative.  The associativity is achieved by "flattening"
	 * into a list.  The commutivity is simulated by "sorting" the
	 * constructors and collapsing the Ccap dictionaries into a
	 * single dictionary, raising an exception if a name is duplicated.
	 * Equality is very conservative here and will require the
	 * constructors to be essentially syntactically equal using
	 * this "normal form".  Of course, we've probably broken 
	 * confluence....*)
	(* first, recursively weak-head normalize the constructors, 
	 * pull out the nested dictionaries and merge them so they
	 * can be placed at the beginning.
	 *)
	let rec f (cs,d) c = 
	  begin
	    wh c;
	    match c.rcon with
	      Cjoin cs' -> List.fold_left f (cs,d) cs' 
	    | Ccap d' -> 
		let g x info d = Dict.insert_new d x info in
		let d = Dict.fold_dict g d' d in (cs,d)
	    | _ -> (c::cs,d)
	  end in
	let (cs,d) = List.fold_left f ([],Dict.empty id_compare) cs in
	let cs = Sort.list (<=) cs in
	(* finally, collapse the dictionary and other constructors, avoiding
	 * a join or empty dictionary if possible. Note that the ordering
	 * constraints ensure that if we have a dictionary in a join, then
	 * it's at the beginning of the list. *)
	(match cs,Dict.is_empty d with
	  [],true -> 
	    copy_con c cempty_cap
	| [],false ->
	    let dcon = wcon(Ccap d) in
	    copy_con c dcon
	| [c'],true -> 
	    copy_con c c'
	| cs,true ->
	    let c' = wcon(Cjoin cs) in
	    copy_con c c'
	| cs,false -> 
	    let dcon = wcon(Ccap d) in
	    let c' = wcon(Cjoin(dcon::cs)) in
	    copy_con c c')
    | Csubst(c1,es) -> (*c.rcon <- (push_subst c1 es).rcon; wh c*)
	failwith "explicit substitution found"
    | _ -> c.con_state <- WeakHead; c
  in
  wh c
;;

(***********************************************************************)
(* deconstructors                                                      *)
(***********************************************************************)
let fail ctxt c s = (error ctxt c s ; raise Talfail)

let dchptr ctxt c = 
   match (whnorm ctxt c).rcon with
     Chptr (x,y,z) -> (x,y,z)
   | _ -> fail ctxt c "expecting Chptr"
;;
let dcons ctxt c = 
   match (whnorm ctxt c).rcon with
     Ccons(hd,tl) -> (hd,tl)
   | _ -> fail ctxt c "expecting Ccons"
;;
let dsptr ctxt c = 
   match (whnorm ctxt c).rcon with
     Csptr c -> c
   | _ -> fail ctxt c "expecting Csptr"
;;
let dexist ctxt c = 
   match (whnorm ctxt c).rcon with
     Cexist (v,k,c1,c2) -> (v,k,c1,c2)
   | _ -> fail ctxt c "expecting Cexist"
;;
let dforall ctxt c = 
   match (whnorm ctxt c).rcon with
     Cforall (v,k,c) -> (v,k,c)
   | _ -> fail ctxt c "expecting Cforall"
;;
let dprod ctxt c = 
   match (whnorm ctxt c).rcon with
     Cprod cs -> cs
   | _ -> fail ctxt c "expecting Cprod"
;;
let dlog ctxt c =
  match (whnorm ctxt c).rcon with
    Clog (l,cs) -> (l,cs)
  | _ -> fail ctxt c "expecting Clog"
;;
let dif ctxt c =
  match (whnorm ctxt c).rcon with
    Cif (c1,c2) -> (c1,c2)
  | _ -> fail ctxt c "expecting Cif"
;;
let dname ctxt c = 
   match (whnorm ctxt c).rcon with
     Cname c -> c
   | _ -> fail ctxt c "expecting Cname"
;;
let dvar ctxt c = 
   match (whnorm ctxt c).rcon with
     Cvar x -> x
   | _ -> fail ctxt c "expecting Cvar"
;;
let dsum ctxt c = 
   match (whnorm ctxt c).rcon with
     Csum cs -> cs
   | _ -> fail ctxt c "expecting Csum"
;;
let dint ctxt c = 
   match (whnorm ctxt c).rcon with
     Cprim (PCint i) -> i
   | _ -> fail ctxt c "expecting Cprim (PCint i)"
;;
let dcode ctxt c = 
   match (whnorm ctxt c).rcon with
     Ccode c -> c
   | _ -> fail ctxt c "expecting Ccode"
;;
let dms ctxt c = 
  match (whnorm ctxt c).rcon with
    Cms ms -> ms
  | _ -> fail ctxt c "expecting Cms"
;;
let dcodems ctxt c = dms ctxt (dcode ctxt c)
;;
let darray ctxt c = 
   match (whnorm ctxt c).rcon with
     Carray (cs,ce) -> (cs,ce)
   | _ -> fail ctxt c "expecting Carray"
;;
let dfield ctxt c = 
   match (whnorm ctxt c).rcon with
     Cfield (c,v) -> (c,v)
   | _ -> fail ctxt c "expecting Cfield"
;;
let dsing ctxt c = 
   match (whnorm ctxt c).rcon with
     Csing c -> c
   | _ -> fail ctxt c "expecting Csing"
;;
let dr ctxt c  = 
   match (whnorm ctxt c).rcon with 
      Cr c -> c
    | _ -> fail ctxt c "expecting Cr"
;;

type ms_or_label = MachineState of machine_state | Lab of identifier

let dcodems_or_label ctxt c =
  match (whnorm ctxt c).rcon with
  | Ctypeof l -> Lab(l)
  | Ccode c ->
      (match (whnorm ctxt c).rcon with
      |	Cms ms -> MachineState ms
      |	_ -> fail ctxt c "expecting Cms")
  | _ -> fail ctxt c "expecting Ccode"
;;
      

(* Full normalisation *)
let normalize ctxt c =
  let rec norm c =
    if c.con_state <> Normalized then begin
      let rec aux c =
      	match c with
      	  Cvar _ -> ()
      	| Clam (_,_,c) -> norm c
      	| Capp (c1,c2) -> norm c1; norm c2
      	| Ctuple cs -> List.iter norm cs
      	| Cproj (_,c) -> norm c
        (* ---- LX ---- *)
	| Cinj(_,c,_) -> norm c
	| Ccase (c,_,cs) -> norm c; List.iter norm cs
	| Cfold (_,c) -> norm c
	| Cpr (i,l) -> List.iter (fun (j,a,k,f,k',c) -> norm c) l
	| Cvoid _ -> ()
        (* -- end LX -- *)
      	| Clab _ -> ()
      	| Cprim _ -> ()
      	| Crec fs -> List.iter (fun (x,k,c) -> norm c) fs
      	| Cforall (_,_,c) -> norm c
      	| Cexist (_,_,c1,c2) -> norm c1; norm c2
      	| Cms ms -> (ms_app (fun c -> norm c) ms)
	| Cmsjoin(c1,c2) -> norm c1; norm c2
	| Ccode c -> norm c
      	| Chptr (_,co,tco) ->
	    (match co with None -> () | Some c -> norm c);
	    (match tco with None -> () | Some (c,_) -> norm c)
      	| Cfield (c,_) -> norm c
      	| Cprod cs -> List.iter norm cs
      	| Csum cs -> List.iter norm cs
      	| Carray (c1,c2) -> norm c1; norm c2
      	| Csing c -> norm c
      	| Csptr c -> norm c
      	| Cempty -> ()
      	| Ccons (c1,c2) -> norm c1; norm c2
      	| Cappend (c1,c2) -> norm c1; norm c2
	(* arithmetic and logic *)
	| Clog (l,cs) -> List.iter norm cs 
	| Cif (c1,c2) -> norm c1; norm c2
	(* alias stuff *)
	| Cname c -> norm c
	| Cjoin cs -> List.iter norm cs
	| Ccap d -> Dict.app_dict (fun x (ai,c) -> norm c) d
	| Ctagof c -> norm c
(* Cyclone *)
        | Ctptr _ -> ()
        | Ctmpl(c1,c2_opt,labels,holes) ->
            begin
              norm c1;
              (match c2_opt with None -> () | Some c2 -> norm c2);
              List.iter (fun (i,c) -> norm c) labels;
              List.iter (fun (i,c) -> norm c) holes
            end
        | Ctrgn(c1,c2_opt,t) ->
            begin
              norm c1;
              (match c2_opt with
	      |	None -> () 
	      |	Some c2 -> norm c2);
              List.iter
                (fun (_,labels,holes) ->
                  List.iter
                    (fun (i,c) -> norm c) labels;
                  List.iter
                    (fun (i,c) -> norm c) holes)
                t
            end
(* End Cyclone *)
	| Csubst _ -> failwith "substitution found in whnormal form"
	| Cr ci -> (match ci with 
	     RCon c -> norm c
	   | _ -> ())
	| Ctypeof _ -> ()
      in
      aux (whnorm ctxt c).rcon; c.con_state <- Normalized
    end in    
  norm c; c
;;

(* Check and then return kind & weak head normal form - con *)
let check_whnorm ctxt c : kind * con =
  let (k,c) = check ctxt c in
  (k,whnorm ctxt c)

(*************************************************************************)
(* Equality of type constructors, alpha equality of type constructors    *)
(*************************************************************************)

(* Computes c1 =a c2 & c1 = c2
 * Assumes checked and canonicalised constructors.
 * Requires normalisation function.
 *)

(* Alpha equality and also subtyping is done in an "alpha context" which
 * maps type variables from c1 to type variables in c2 that are supposed to
 * be equal.
 *
 * An alpha context consists of a list of pairs (x,y).  If an x appears in the
 * list then it is bound in c1 by an outer constructor and maps to the y of c2.
 * If a y appears in the list then it is bound in c2 by an outer constructor
 * and maps to the x of c1.  If an x is absent from the list then it is free in
 * c1. If a y is absent from the list then it is free in c2.
 * Note that type variables cannot be blindly compared as they may map to
 * different variables or may be bound in c1 but free in c2 or vice versa.
 * 
 * JGM:  I got rid of the kind context because it was never used.
 *)

(* compare two constructors up to alpha-equivalence *)
let rec aeq error ctxt  ((kmap,cmap) as varmap) c1 c2 = 
  if varmap=([],[]) & c1 == c2 then () else
  begin
  match c1.rcon,c2.rcon with
    (Cvar x,Cvar y) ->  compare error varmap x y
  | (Clam(x1,k1,c1),Clam(x2,k2,c2)) ->
      kindaeq error ctxt kmap k1 k2;
      aeq error ctxt (extend varmap x1 x2) c1 c2
  | (Capp(c1a,c1b),Capp(c2a,c2b)) ->
      aeq error ctxt varmap c1a c2a;
      aeq error ctxt varmap c1b c2b
  | (Ctuple cs1, Ctuple cs2) -> aeqs error ctxt varmap cs1 cs2
  | (Cproj(i1,c1),Cproj(i2,c2)) ->
      if i1 = i2 then aeq error ctxt varmap c1 c2 else error ()
  (* ---- LX ---- *)
  | (Cinj(i1,c1,k1), Cinj(i2,c2,k2)) -> 
       kindaeq error ctxt kmap k1 k2;
       if i1 = i2 then aeq error ctxt varmap c1 c2 else error ()
  | (Ccase (c1,a1,cs1), Ccase (c2,a2,cs2)) ->
       (aeq error ctxt varmap c1 c2;
       try List.iter2 (aeq error ctxt (extend varmap a1 a2)) cs1 cs2
       with Invalid_argument _ -> error ())
  | (Cfold (k1,c1), Cfold (k2,c2)) -> 
       kindaeq error ctxt kmap k1 k2;
       aeq error ctxt varmap c1 c2
  | Cpr (i1,l1), Cpr (i2,l2) -> 
       (try 
       if List.length l1 <> List.length l2 then error ()
       else 
	  let (kmap,cmap) as varmap = List.fold_right2 
	     (fun (j1,a1,k1,f1,k1',c1)(j2,a2,k2,f2,k2',c2) varmap -> 
		let varmap = extend_kind varmap j1 j2 in 
		extend varmap f1 f2) l1 l2 varmap  in 
	  compare error varmap i1 i2;
	  List.iter2 
	     (fun (j1,a1,k1,f1,k1',c1) (j2,a2,k2,f2,k2',c2) ->
		kindaeq error ctxt kmap k1 k2; 
		kindaeq error ctxt kmap k1' k2';
	  	aeq error ctxt (extend varmap a1 a2) c1 c2) l1 l2
       with
	  e -> ((debug2 (fun fmt o -> 
	     pp_print_string fmt "Cprs not aeq";
	     print_con fmt o c1; 
	     pp_print_string fmt " and ";
	     print_con fmt o c2));
		  raise e))
  | (Cvoid k1, Cvoid k2) ->
       kindaeq error ctxt kmap k1 k2
  (* -- end LX -- *)
  | (Clab l1,Clab l2) -> if (id_compare l1 l2)<>0 then error ()
  | (Cprim pc1,Cprim pc2) -> if pc1 <> pc2 then error ()
  | (Crec fs1,Crec fs2) -> 
      let varmap2 = 
	List.fold_right2 
	  (fun (x1,k1,_) (x2,k2,_) varmap ->
	     (kindaeq error ctxt kmap k1 k2; extend varmap x1 x2))
	  fs1 fs2 varmap in
      List.iter2 (fun (_,_,c1) (_,_,c2) ->aeq error ctxt varmap2 c1 c2) fs1 fs2
  | (Cforall(x1,k1,c1),Cforall(x2,k2,c2)) ->
      kindaeq error ctxt kmap k1 k2;
      aeq error ctxt (extend varmap x1 x2) c1 c2
  | (Cexist(x1,k1,c1,c1'),Cexist(x2,k2,c2,c2')) ->
      kindaeq error ctxt kmap k1 k2;
      let varmap = extend varmap x1 x2 in
      aeq error ctxt varmap c1 c2;
      aeq error ctxt varmap c1' c2'
  | (Cms ms1,Cms ms2) -> 
      (try
	ms_app_reg (fun r c1 -> aeq error ctxt varmap c1 (ms_get_reg ms2 r)) ms1;
	ms_app_reg (fun r _ -> ms_get_reg ms1 r) ms2;
	if not (fpstack_equal (ms_get_fpstack ms1) (ms_get_fpstack ms2)) then 
	  error ();
	aeq error ctxt varmap (ms_get_cap ms1) (ms_get_cap ms2)
      with Dict.Absent -> error())
  | (Cmsjoin(c11,c12),Cmsjoin(c21,c22)) ->
      aeq error ctxt varmap c11 c21;
      aeq error ctxt varmap c12 c22
  | (Ccode c1,Ccode c2) -> aeq error ctxt varmap c1 c2
  | (Chptr (is1,co1,tco1),Chptr (is2,co2,tco2)) ->
      if is1<>is2 then error ();
      (match co1,co2 with
	None,None -> ()
      | Some c1,Some c2 -> aeq error ctxt varmap c1 c2
      | _,_ -> error ());
      (match tco1,tco2 with
	None,None -> ()
      | Some (c1,v1),Some (c2,v2) ->
	  if v1<>v2 then error () else aeq error ctxt varmap c1 c2
      | _,_ -> error ())
  | (Cfield (c1,v1),Cfield (c2,v2)) ->
      if v1=v2 then aeq error ctxt varmap c1 c2 else error ()
  | (Cprod cs1,Cprod cs2) -> aeqs error ctxt varmap cs1 cs2
  | (Csum cs1,Csum cs2) -> aeqs error ctxt varmap cs1 cs2
  | (Carray (c1a,c1b),Carray (c2a,c2b)) ->
      aeq error ctxt varmap c1a c2a;
      aeq error ctxt varmap c1b c2b
  | (Csing c1,Csing c2) -> aeq error ctxt varmap c1 c2
  | (Csptr c1,Csptr c2) -> aeq error ctxt varmap c1 c2
  | (Cempty,Cempty) -> ()
  | (Ccons(hd1,c1),Ccons(hd2,c2)) ->
      aeq error ctxt varmap hd1 hd2;
      aeq error ctxt varmap c1 c2
  | (Cappend(c1a,c1b),Cappend(c2a,c2b)) ->
      aeq error ctxt varmap c1a c2a;
      aeq error ctxt varmap c1b c2b
  (* arithmetic and logic *)
  | Clog (l1,cs1),Clog (l2,cs2) ->
      if not (l1 = l2) then error ();
      aeqs error ctxt varmap cs1 cs2
  | Cif (c1,c1'),Cif (c2,c2') ->
      aeq error ctxt varmap c1 c2;
      aeq error ctxt varmap c1' c2'
  (* alias stuff *)
  | Cname c1,Cname c2 -> aeq error ctxt varmap c1 c2
  | Cjoin cs1,Cjoin cs2 -> aeqs error ctxt varmap cs1 cs2
  | Ccap d1,Ccap d2 ->
      begin
	try 
	  Dict.app_dict 
	    (fun x (ai1,c1) -> 
	      let (ai2,c2) = Dict.lookup d2 x in
	      if ai1 <> ai2 then error();
	      aeq error ctxt varmap c1 c2) d1;
	  Dict.app_dict (fun x _ -> (Dict.lookup d1 x; ())) d2
	with Dict.Absent -> error()
      end
  | Ctagof c1,Ctagof c2 -> aeq error ctxt varmap c1 c2
(* Cyclone *)
(* XXX - type-checking here appears weak. *)
  | Ctmpl(c11,c12_opt,labels1,holes1),Ctmpl(c21,c22_opt,labels2,holes2) ->
      begin
        aeq error ctxt varmap c11 c21;
        (match c12_opt,c22_opt with
          None,None -> ()
        | Some c12,Some c22 ->
            aeq error ctxt varmap c12 c22
        | _ -> error());
        let id_con_list_eq l1 l2 =
          try
            List.iter2
              (fun (i1,c1) (i2,c2) ->
		(* FMS: Was i1 <> i2 ?? *)
                if i1=i2 then aeq error ctxt varmap c1 c2
                else error())
              l1
              l2
          with Invalid_argument _ -> error()
        in
        id_con_list_eq labels1 labels2;
        id_con_list_eq holes1 holes2
      end
  | Ctrgn(c11,c12_opt,t1),Ctrgn(c21,c22_opt,t2) -> (* FMS: replaced. *)
      begin
        aeq error ctxt varmap c11 c21;
        (match c12_opt,c22_opt with
	| None,None -> ()
	| Some c12, Some c22 -> aeq error ctxt varmap c12 c22
	| _,_ -> error());
        let id_con_list_eq l1 l2 =
          try
            List.iter2
              (fun (i1,c1) (i2,c2) ->
                if i1=i2 then aeq error ctxt varmap c1 c2
                else error())
              l1
              l2
          with Invalid_argument _ -> error()
        in
	let br_templ_eq (i1,l1,h1) (i2,l2,h2) =
	  if i1=i2 then 
	    (id_con_list_eq l1 l2;
	     id_con_list_eq h1 h2)
	  else error ()
	in
	try 
	  List.iter2 br_templ_eq t1 t2
	with Invalid_argument _ -> error()
      end      
(* End Cyclone *)
  | Csubst(c1,es1),Csubst(c2,es2) ->
      let rec aeq_subst es1 es2 varmap = 
	match es1,es2 with
	  Enil,Enil -> varmap
	| Es(x1,c1),Es(x2,c2) ->
	    aeq error ctxt varmap c1 c2; extend varmap x1 x2
	| Eo(es11,es12),Eo(es21,es22) ->
	    aeq_subst es11 es21 (aeq_subst es12 es22 varmap)
	| _,_ -> error(); varmap
      in aeq error ctxt (aeq_subst es1 es2 varmap) c1 c2
  | Cr ci1,Cr ci2 ->
       (match ci1,ci2 with
	  RCon c1, RCon c2 -> 
	     aeq error ctxt empty_ctxt c1 c2
	| RKind k1, RKind k2 -> 
	     kindaeq error ctxt [] k1 k2
	| RLabel l1, RLabel l2 ->
	     if not (id_compare l1 l2  == 0) 
	     then error()
	| _,_ -> error ())
  | (Ctypeof l1,Ctypeof l2) -> if (id_compare l1 l2)<>0 then error ()
  | (_,_) -> error ()

  end
and aeqs error ctxt varmap cs1 cs2 =
  try List.iter2 (aeq error ctxt varmap) cs1 cs2
  with Invalid_argument _ -> error ()
;;

let aeqcon error ctxt c1 c2 =
  aeq error ctxt empty_ctxt c1 c2
    (* could set c1 == c2 and force more sharing *)
;;

exception NotEq
let eqerror () = raise NotEq
let eqerror' _ _ = raise NotEq
let dieerror ctxt c1 c2 () = generate_error ctxt (Neqcon (c1,c2))

let eqcon ctxt c1 c2 =
  if c1 != c2 then
    aeqcon (dieerror ctxt c1 c2) ctxt (normalize ctxt c1) (normalize ctxt c2)
  else ()
(*
  if c1 <> c2 then
    try
      let ctxt' = error_handler ctxt eqerror' in
      aeqcon eqerror ctxt' c1 c2
    with NotEq -> 
      let c1 = normalize ctxt c1
      and c2 = normalize ctxt c2 in 
      aeqcon (dieerror ctxt c1 c2) ctxt c1 c2
*)
;;

let alphaeqcon ctxt c1 c2 = aeqcon (dieerror ctxt c1 c2) ctxt c1 c2

(*************************************************************************)
(* Alias/Name/Capability utilities                                       *)
(*************************************************************************)
(* given a variable x of kind Kname, look it up in the current capability
 * to get its alias info and type. *)
let get_name ctxt x = 
  let cap = whnorm ctxt (get_cap ctxt) in
  let err () = generate_error ctxt (Bad_Name x); raise Talfail in
  let d = 
    match cap.rcon with
      Ccap d -> d
    | Cjoin (c::cs) ->
	begin
	  match c.rcon with
	    Ccap d -> d
	  | _ -> err()
	end
    | _ -> err() in
  try Dict.lookup d x with Dict.Absent -> err()
;;

let get_name_alias ctxt x = fst(get_name ctxt x);;
let get_name_type  ctxt x = snd(get_name ctxt x);;

(* change the alias info and type information associated with a given
 * name.  Checks to see that the name is already in the capability.
 *)
let change_name ctxt x p = 
  let cap = whnorm ctxt (get_cap ctxt) in
  let err () = generate_error ctxt (Bad_Name x); raise Talfail in
  let check d = try Dict.lookup d x with Dict.Absent -> err() in
  let cap' = 
    match cap.rcon with
      Ccap d -> check d; wcon(Ccap(Dict.insert d x p))
    | Cjoin (c::cs) ->
	begin
	  match c.rcon with
	    Ccap d -> 
	      check d; wcon(Cjoin((wcon(Ccap(Dict.insert d x p)))::cs))
	  | _ -> err()
	end
    | _ -> err() in
  set_cap ctxt cap'
;;  

(* insert the alias info and type information associated with a given
 * name.  Checks to see that the name is not already in the capability.
 *)
let add_name ctxt x p = 
  let cap = whnorm ctxt (get_cap ctxt) in
  let err () = generate_error ctxt (Bad_Name x); raise Talfail in
  let other () = cjoin [ccap (Dict.insert (Dict.empty id_compare) x p);cap] in
  try 
  let cap' = 
    match cap.rcon with
      Ccap d -> wcon(Ccap(Dict.insert_new d x p))
    | Cjoin (c::cs) ->
	begin
	  match c.rcon with
	    Ccap d -> wcon(Cjoin((wcon(Ccap(Dict.insert_new d x p)))::cs))
	  | _ -> other()
	end
    | _ -> other() in
  set_cap ctxt cap'
  with Dict.Present -> err()
;;  

(* remove a name from the current capability *)
let remove_name ctxt x = 
  let cap = whnorm ctxt (get_cap ctxt) in
  let err () = generate_error ctxt (Bad_Name x); raise Talfail in
  try 
  let cap' = 
    match cap.rcon with
      Ccap d -> 
	let d' = Dict.delete_present d x in
	if Dict.is_empty d' then cempty_cap
	else wcon(Ccap(d'))
    | Cjoin (c::cs) ->
	begin
	  match c.rcon with
	    Ccap d -> 
	      let d' = Dict.delete_present d x in
	      if Dict.is_empty d' then 
		begin
		  match cs with
		    [] -> cempty_cap
		  | [c] -> c
		  | _ -> wcon(Cjoin cs)
		end
	      else wcon(Cjoin((wcon(Ccap(d')))::cs))
	  | _ -> err()
	end
    | _ -> err() in
  set_cap ctxt cap'
  with Dict.Absent -> err()

(* change a name from unique to may alias *)
let forgetunique ctxt x = 
  let (ai,c) = get_name ctxt x in
  match ai with
    Unique -> change_name ctxt x (MayAlias,c)
  | _ -> 
      (error ctxt (wcon (Cvar x)) "Forgetunique:  name is not unique";
       raise Talfail)
;;
(*************************************************************************)
(* Type constructor subtyping                                            *)
(*************************************************************************)

(* Computes c1 <=a c2 & c1 <= c2
 * Assumes checked and canonicalised constructors.
 * Requires normalisation function.
 *)

(* Subtyping is done in an "alpha context" which
 * maps type variables from c1 to type variables in c2 that are supposed to
 * be equal or perhaps in some subtype relationship.
 *
 * An alpha context consists of a list of triple (x,y,rel) where rel indicates
 * the relationship equal (ACeq), less than (AClt), or greater than (ACgt).
 * If an x appears in the
 * list then it is bound in c1 by an outer constructor and maps to the y of c2.
 * If a y appears in the list then it is bound in c2 by an outer constructor
 * and maps to the x of c1.  If an x is absent from the list then it is free in
 * c1. If a y is absent from the list then it is free in c2.
 * Note that type variables cannot be blindly compared as they may map to
 * different variables or may be bound in c1 but free in c2 or vice versa.
 *)

type ac = ACeq | AClt | ACgt;;
type salphactxt = (identifier*identifier) list * (identifier*identifier*ac) list;;

let sempty_ctxt : salphactxt = ([],[]);;
let sextend_eq old dir x1 x2 : salphactxt =
  if id_compare x1 x2 == 0 
  then old 
  else let (km,vm) = old in
  if dir 
  then (km,(x1,x2,ACeq)::vm) 
  else (km,(x2,x1,ACeq)::vm)
;;
let sextend_lt old dir x1 x2 : salphactxt =
  if id_compare x1 x2 == 0
  then old
  else let (km,vm) = old in
  if dir 
  then (km,(x1,x2,AClt)::vm) 
  else (km,(x2,x1,ACgt)::vm)
;;

let sextend_kind (km,vm) dir x1 x2 : salphactxt = 
   if dir then (x1,x2)::km,vm else (x2,x1)::km,vm

let rec compare error (km,vm) dir x1 x2 =
  let y1,y2 = if dir then x1,x2 else x2,x1 in
  let chk_rel dir rel =
    match dir,rel with
      _,ACeq | true,AClt | false,ACgt -> ()
    | true,ACgt | false,AClt -> error () in
  let rec aux vm =
    match vm with
      [] -> if (id_compare y1 y2)<>0 then error ()
    | (z1,z2,rel)::_ when (id_compare y1 z1)=0 ->
      	if (id_compare y2 z2)<>0 then error ();
      	chk_rel dir rel
    | (z1,z2,rel)::_ when (id_compare y2 z2)=0 ->
	error (); (* we know (id_compare y1 z1)<>0 *)
      	chk_rel dir rel
    | _::vm -> aux vm in
  aux vm
;;

let rec subset l1 l2 =
  match l1,l2 with
    [],_ -> true
  | _::_,[] -> false
  | i1::l1',i2::l2' ->
      if i1=$i2 then
 	subset l1' l2'
      else if i1>$i2 then
 	subset l1 l2'
      else
      	false
;;

let rec mem i l =
  match l with
    [] -> false
  | i'::_ when i=$i' -> true
  | _::l -> mem i l
;;

let try_aeq error ctxt (kmap, cmap) dir c1 c2 =
  let varmap = (kmap, List.map (fun (x1,x2,_) -> (x1,x2)) cmap) in
  if dir then aeq error ctxt varmap c1 c2 else aeq error ctxt varmap c2 c1
;;

let kindaeq_d dir error ctxt (kmap,cmap) c1 c2 = 
   if dir then kindaeq error ctxt kmap c1 c2 else kindaeq error ctxt kmap c2 c1

let kindaleq_d dir error ctxt (kmap,cmap) c1 c2 = 
   if dir then kindaleq error ctxt kmap c1 c2 else kindaleq error ctxt kmap c2 c1

(* calculates the size (in bytes) of values who have type c *)	
let rec sizeof ctxt c =
  match (con_kind ctxt c).rkind with
    Kbyte s -> scale_to_int32 s
  | Kmemi i -> i
  | _ ->
      let c = normalize ctxt c in
      match (con_kind ctxt c).rkind with
    	Kbyte s -> scale_to_int32 s
      | Kmemi i -> i
      | _ -> generate_error ctxt (Unknown_size c); raise Talfail
;;

(* ctxt |- c1 <= c2
 * in alpha context varmap
 * exactsize = no width subtyping?
 * dir = varmap maps c1->c2 (true) or c2->c1 (false)
 *)
let rec leqc error ctxt1 ctxt2 ((kmap, cmap) as varmap) dir exactsize c1 c2 =
  let leqc' = leqc error ctxt1 ctxt2 varmap dir exactsize in
  let leqc_extend v1 k1 v2 k2 = 
    let ctxt1' = add_var ctxt1 v1 k1 in
    let ctxt2' = add_var ctxt2 v2 k2 in
    let varmap' = sextend_eq varmap dir v1 v2 in
    leqc error ctxt1' ctxt2' varmap'
  in
  let leqc_extend' v1 k1 v2 k2 = leqc_extend v1 k1 v2 k2 dir exactsize in
  let try_aeq' =  try_aeq error ctxt1 varmap dir in
  let kindaeq_d' = kindaeq_d dir error ctxt1 varmap in
  let kindaleq_d' = kindaleq_d dir error ctxt1 varmap in
  if kmap<>[] or cmap<>[] or c1!=c2 then
  match c1.rcon,c2.rcon with
    Cvar x1,Cvar x2 -> compare error varmap dir x1 x2
  | Clam (v1,k1,c1),Clam (v2,k2,c2) ->
      kindaeq_d' k1 k2;
      leqc_extend' v1 k1 v2 k2 c1 c2
  | Capp (c11,c12),Capp (c21,c22) ->
      leqc' c11 c21; 
      try_aeq' c12 c22
  | Ctuple cs1,Ctuple cs2 ->
      (try List.iter2 leqc' cs1 cs2
      with Invalid_argument _ -> error ())
  | Cproj (i1,c1),Cproj (i2,c2) -> 
      if i1=i2 then leqc' c1 c2 else error ()
(* ---- LX ---- *)
(* Only alpha-equivalence at this point *)
  | (Cinj _, Cinj _) -> 
       try_aeq' c1 c2 
  | (Ccase _, Ccase _) ->
       try_aeq' c1 c2 
  | (Cfold _, Cfold _) -> 
       try_aeq' c1 c2
  | (Cpr _, Cpr _) -> 
      try_aeq' c1 c2 
  | (Cvoid k1, Cvoid k2) ->
       kindaeq_d' k1 k2
(* -- end LX -- *)
  | Clab l1,Clab l2 -> if (id_compare l1 l2)<>0 then error ()
  | _,Cprim (PCjunk i) ->
      (match (con_kind ctxt1 c1).rkind with
	 Kmemi j when i=j -> ()
      |	_ -> error ())
  | _,Cprim (PCjunkbytes sc2) ->
      (match (con_kind ctxt1 c1).rkind with
	Kbyte sc1 when sc1=sc2 -> ()
      |	_           -> error())
  | Cprim pc1,Cprim pc2 -> if pc1<>pc2 then error ()
  | Crec fs1,Crec fs2 ->
      (try try_aeq eqerror ctxt1 varmap dir c1 c2
      with NotEq ->
	try
	  let aux (vm,ctxt1,ctxt2) (x1,k1,_) (x2,k2,_) = 
	    (sextend_lt vm dir x1 x2,
	     add_var ctxt1 x1 k1,
	     add_var ctxt2 x2 k2)
	  in
	  let (varmap,ctxt1,ctxt2) = 
	    List.fold_left2 aux (varmap,ctxt1,ctxt2) fs1 fs2 in
	  let kindaleq_d' = kindaleq_d dir error ctxt1 varmap in
	  let leqc' = leqc error ctxt1 ctxt2 varmap dir exactsize in
	  let aux (_,k1,c1) (_,k2,c2) =
	    kindaleq_d' k1 k2; leqc' c1 c2 in
	  List.iter2 aux fs1 fs2
	with Invalid_argument _ -> error ())
  | Cforall (v1,k1,c1),Cforall (v2,k2,c2) ->
      kindaleq_d' k2 k1; (* FMS: Shouldn't dir be changed here? *)
      leqc_extend' v1 k1 v2 k2 c1 c2
  | Cexist (v1,k1,c1,c1'),Cexist (v2,k2,c2,c2') ->
      kindaleq_d' k1 k2;
      leqc_extend' v1 k1 v2 k2 c1 c2;
      leqc_extend' v1 k1 v2 k2 c1' c2'
  | Cms ms1,Cms ms2 -> leqms error ctxt2 ctxt1 varmap (not dir) ms2 ms1
  | Ccode c1,Ccode c2 -> leqc' c1 c2
  | Cmsjoin(c11,c12),Cmsjoin(c21,c22) ->
      leqc' c11 c21; leqc' c12 c22
  | Chptr (is1,co1,tco1),Chptr (is2,co2,tco2) ->
      if not (subset is1 is2) then error ();
      (match co1,co2 with
	None,_ -> ()
      | Some _,None -> error ()
      | Some c1,Some c2 -> leqc error ctxt1 ctxt2 varmap dir false c1 c2);
      (match tco1,tco2 with
	_,None -> ()
      | None,Some _ -> error ()
      | Some (c1,v1),Some (c2,v2) ->
	  leqcv error ctxt1 ctxt2 varmap dir false c1 v1 c2 v2)
  | Cfield (c1,v1),Cfield (c2,v2) ->
      leqcv error ctxt1 ctxt2 varmap dir exactsize c1 v1 c2 v2
  | Cprod cs1,Cprod cs2 ->
      let rec aux cs1 cs2 =
      	match cs1,cs2 with
	  _,[] -> if exactsize & cs1<>[] then error ()
      	| [],_ -> error ()
      	| c1::cs1,c2::cs2 ->
	    leqc error ctxt1 ctxt2 varmap dir (exactsize or cs2<>[]) c1 c2;
	    aux cs1 cs2 in
      aux cs1 cs2
  | Csum _,Csum [] ->
      if exactsize then kindaleq_d' (con_kind ctxt1 c1) (con_kind ctxt2 c2)
  | Csum cs1,Csum (c2::_ as cs2) ->
      let sz = 
	match (con_kind ctxt2 c2).rkind with 
	  Kmemi i -> Some i 
	| _ -> None in
      let rec aux1 sz cs1 cs2 =
	match cs2 with
	  [] ->
	    (if exactsize then
	      match sz with
		None -> ()
	      |	Some i ->
		  let chk c =
		    match (con_kind ctxt1 c).rkind with Kmemi j when i=j -> ()
		    | _ -> error () in
		  List.iter chk cs1)
	| c2::cs2 ->
	    let i2 = sum_index error c2 in
	    aux2 sz cs1 i2 c2 cs2
      and aux2 sz cs1 i2 c2 cs2 =
	match cs1 with
	  [] -> error ()
	| c1::cs1 ->
	    let i1 = sum_index error c1 in
	    if i1=i2 then begin
	      leqc' c1 c2;
	      let sz =
	    	match sz with
		  None -> sz
	    	| Some i ->
		    match (con_kind ctxt2 c2).rkind with Kmemi j when i=j -> sz
		    | _ -> None in
	      aux1 sz cs1 cs2
	    end else if i1<i2 then begin
	      if exactsize then
		(match sz with None -> ()
		| Some i ->
		    match (con_kind ctxt1 c1).rkind with Kmemi j when i=j -> ()
		    | _ -> error ());
	      aux2 sz cs1 i2 c2 cs2
	    end else
	      error () in
      aux1 sz cs1 cs2
  | Carray (c11,c12),Carray (c21,c22) ->
      if exactsize then try_aeq' c11 c21
      else leqc error ctxt1 ctxt2 varmap dir false c11 c21;
      leqc' c12 c22
  | Csing c1,Csing c2 -> leqc' c1 c2
  | Csing _,Cprim (PCbytes Byte4) -> ()
  | Csing {rcon=Cprim (PCint i)},Chptr (is,_,None) ->
      if not (mem i is) then error ()
  | Csptr c1,Csptr c2 -> leqc error ctxt1 ctxt2 varmap dir false c1 c2
  | Csptr c1,Cprim (PCbytes Byte4) -> () 
  | Cempty,Cempty -> ()
  | Ccons (c11,c12),Ccons (c21,c22) ->
      (* We special case for junk on the stack. *)
      let is_junk_con c = 
	match c.rcon with
	| Cprim(PCjunkbytes sc)  -> Some (scale_to_int32 sc)
	| Cprim(PCjunk i) -> Some i
	| _               -> None in
      let con_size c =
	match (con_kind ctxt1 c).rkind with
	  Kmemi j -> Some j
	| Kbyte s -> Some (scale_to_int32 s)
	| _       -> None in
      (match (con_size c11, is_junk_con c21) with
      |	(Some i1, Some i2) ->
	  if i1 =$ i2 then leqc' c12 c22 else
	  if i1 < i2 then  leqc' c12 (ccons (pcjunk (i2 -$ i1)) c22) 
	  else             leqc' (ccons (pcjunk (i1 -$ i2)) c12) c22
      |	_ -> (leqc error ctxt1 ctxt2 varmap dir true c11 c21;
	      leqc' c12 c22))
  | Cappend (c11,c12),Cappend (c21,c22) -> 
      leqc error ctxt1 ctxt2 varmap dir true c11 c21;
      leqc' c12 c22
  (* arithmetic and logic *)
  | _,Cprim PCtrue -> ()
  | Clog (l1,cs1), Clog (l2,cs2) ->
      (* determine whether conjuncts on rhs are a subset of conjuncts on lhs *)
      let try_catch_aeq =
	let error () = raise NotEq in
	let ctxt = error_handler ctxt1 (fun _ _ -> raise NotEq) in
	(fun c1 c2 ->
 	  try try_aeq error ctxt1 varmap dir c1 c2; true
	  with NotEq -> false) 
      in
      let rec belongs c cs cs_original =
	match cs with
	  [] -> error ()
	| c'::cs' -> 
	    if try_catch_aeq c c' then ()
	    else belongs c cs' cs_original in
      let rec includes cs1 cs2 =
	match cs1 with
	  [] -> ()
	| c::cs1' -> belongs c cs2 cs2; includes cs1' cs2 in
      begin
	match l1,l2 with
	  Cand,Cand -> includes cs2 cs1
	| Cand,_ -> includes [c2] cs1
	| _,Cand -> includes cs2 [c1] 
	| _,_ -> try_aeq error ctxt1 varmap dir c1 c2
      end
  | Cif (c1,c1'), Cif (c2,c2') ->
      (* implication: contravariant in first argument, covariant in second *)
      leqc error ctxt2 ctxt1 varmap (not dir) exactsize c2 c1;
      leqc error ctxt1 ctxt2 varmap dir exactsize c1' c2'
  (* alias stuff *)
  | Cname c1,Cname c2 -> leqc' c1 c2
  (* this allows us to treat Cname(x) <= c2 when x MayAlias and maps to c1
   * and c1 <= c2, allowing us to implicitly coerce a register or stack slot
   * to forget the aliasing information. *)
  | Cname {rcon=Cvar x},_ ->
      let (a,xcon) = get_name ctxt1 x in
      (match a with
	MayAlias -> leqc' xcon c2
      |	_ -> error ())
  (* This allows us to drop constructors from the left-hand join. It relies
   * upon the fact that the constructors are sorted and that we're more
   * likely to drop stuff at the beginning of cs1 (namely a ground capability)
   *)
  | Cjoin cs1,Cjoin cs2 ->
      if exactsize then
	try
	  List.iter2 leqc' cs1 cs2
	with Invalid_argument _ -> error ()
      else 
	let rec iter rcs1 rcs2 = 
	  match rcs1,rcs2 with
	    _,[] -> ()
	  | c1::rest1,c2::rest2 -> 
	      begin
		try 
		  let ctxt1' = error_handler ctxt1 eqerror' in
		  let ctxt2' = error_handler ctxt2 eqerror' in
		  leqc eqerror ctxt1' ctxt2' varmap dir exactsize c1 c2; 
		  iter rest1 rest2
		with _ -> iter rest1 rcs2
	      end
	  | [],_ -> error() in
	iter (List.rev cs1) (List.rev cs2)
  (* this is necessary in case c2 occurs in the list of cs1 *)
  (* Dave: or if c2 is the empty capability *)
  | Cjoin cs1,_ ->
      if exactsize then error () else
      let rec iter rcs1 = 
	  match rcs1 with
	  | c1::rest1 -> 
	      begin
		try
		  let ctxt1' = error_handler ctxt1 eqerror' in
		  let ctxt2' = error_handler ctxt2 eqerror' in
		  leqc eqerror ctxt1' ctxt2' varmap dir exactsize c1 c2
		with _ -> iter rest1
	      end
	  | [] -> error() in
      (match c2.rcon with
	Ccap d -> 
	  if Dict.is_empty d then ()
	  else iter cs1 
      |	_ -> iter cs1)
  | Ccap d1,Ccap d2 -> 
      let aux x (ai2,c2) = 
	try 
	  let (ai1,c1) = Dict.lookup d1 x in
	  if ai1 <> ai2 then error();
	  leqc error ctxt1 ctxt2 varmap dir false c1 c2
	with Dict.Absent -> error () in
      Dict.app_dict aux d2;
      (* JGM:  should check for exactsize here... *)
  | Ctagof c1,Ctagof c2 -> leqc' c1 c2
(* Cyclone *)
  | Ctptr x1,Ctptr x2 -> if (id_compare x1 x2)<>0 then error ()
  | Ctmpl(c11,c12_opt,l1,h1),Ctmpl(c21,c22_opt,l2,h2) ->
      (* XXX - must check holes and labels as well!!! *)
      begin
        leqc error ctxt2 ctxt1 varmap (not dir) false c21 c11;
        (match c12_opt,c22_opt with
          None,_ -> ()
        | Some c12,Some c22 ->
            leqc error ctxt1 ctxt2 varmap dir false c12 c22
        | _ -> error ());
	(* We reorder holes and labels here.  
	   XXX - Would be better if they were maintained in sorted order. *)
	let id_con_cmp (i1,_) (i2,_) = id_compare i1 i2 in
	let l1 = List.sort id_con_cmp l1 in
	let l2 = List.sort id_con_cmp l2 in
	let h1 = List.sort id_con_cmp h1 in
	let h2 = List.sort id_con_cmp h2 in

        let id_con_list_leq l1 l2 =
	  try
            List.iter2
              (fun (i1,c1) (i2,c2) ->
                if i1=i2 then leqc error ctxt1 ctxt2 varmap dir false c1 c2
                else error())
              l1
              l2
          with Invalid_argument _ -> error()
	in
	id_con_list_leq l1 l2;
	id_con_list_leq h2 h1
      end
  | Ctrgn(c11,c12_opt,t1),Ctrgn(c21,c22_opt,t2) ->
      (* FMS: Changed direction of subtyping on both pre and post condition
	 with little thought based on experience.  XXX - revisit. *)
      begin
	leqc error ctxt1 ctxt2 varmap dir false c11 c21;
	(match c12_opt,c22_opt with
	| (None,_) -> ()
	| (Some c12,Some c22) ->
	    leqc error ctxt2 ctxt1 varmap (not dir) false c22 c12
	| (_,_) -> error ());
	
	let id_con_list_leq l1 l2 =
          try
            List.iter2
              (fun (i1,c1) (i2,c2) ->
		if i1=i2 then leqc error ctxt1 ctxt2 varmap dir false c1 c2
		else error())
              l1
              l2
          with Invalid_argument _ -> error()
	in
	let br_templ_leq (i1,l1,h1) (i2,l2,h2) =
	  if i1=i2 then 
	    let id_con_cmp (i1,_) (i2,_) = id_compare i1 i2 in
	    let l1 = List.sort id_con_cmp l1 in
	    let l2 = List.sort id_con_cmp l2 in
	    let h1 = List.sort id_con_cmp h1 in
	    let h2 = List.sort id_con_cmp h2 in
	    (id_con_list_leq l1 l2;
	     id_con_list_leq h2 h1)
	  else error ()
	in
	let br_templ_cmp (i1,_,_) (i2,_,_) = id_compare i1 i2 in
	let t1 = List.sort br_templ_cmp t1 in
	let t2 = List.sort br_templ_cmp t2 in
	try 
	  List.iter2 br_templ_leq t1 t2
	with Invalid_argument _ -> error()      
      end
(* End Cyclone *)
  | Csubst _,Csubst _ -> 
      try_aeq' c1 c2
  | (Cr _, Cr _) -> 
       try_aeq' c1 c2
  | Ctypeof l1,Ctypeof l2 -> 
      if (id_compare l1 l2) <> 0 then error () else () 
  | _,_ -> error ()
and leqms error ctxt1 ctxt2 varmap dir ms1 ms2 =
   let aux r c2 =
    try leqc error ctxt1 ctxt2 varmap dir false (ms_get_reg ms1 r) c2
    with Dict.Absent -> error () in
   ms_app_reg aux ms2;
   fpstack_leq' ctxt1 (ms_get_fpstack ms1) (ms_get_fpstack ms2);
   (match ms_get_cc ms1,ms_get_cc ms2 with
     _,CCnoinfo -> ()
   | _,_ -> failwith "Talcon.leqrs - cc not fully implemented");
   leqc error ctxt1 ctxt2 varmap dir false (ms_get_cap ms1) (ms_get_cap ms2)
and leqcv error ctxt1 ctxt2 varmap dir exactsize c1 v1 c2 v2 =
  match v2 with
    Read ->
      if v1=Read or v1=ReadWrite then
 	leqc error ctxt1 ctxt2 varmap dir exactsize c1 c2
      else
 	error ()
  | Write ->
      if v1=Write or v1=ReadWrite then
 	leqc error ctxt2 ctxt1 varmap (not dir) exactsize c2 c1
      else
 	error ()
  | ReadWrite ->
      if v1=ReadWrite then try_aeq error ctxt1 varmap dir c1 c2 else error ()
and fpstack_leq' ctxt fps1 fps2 =
  if not (fpstack_leq fps1 fps2) then
    generate_error ctxt (FPstackleq (fps1,fps2))
;;

let leqc_norm error ctxt c1 c2 =
  if c1 != c2 then
    let c1 = normalize ctxt c1 
    and c2 = normalize ctxt c2 in
    leqc (error c1 c2) ctxt ctxt empty_ctxt true false c1 c2
  else () 
;;

let leqcon ctxt c1 c2 =
    let error c1 c2 () = generate_error ctxt (Nleqcon (c1,c2)) in
    leqc_norm error ctxt c1 c2
;;

let machine_state_leq ctxt gamma1 gamma2 = 
  let error r c1 c2 () = generate_error ctxt (Msnleq (r,c1,c2)) in
  let check1 r c2 =
    try
      let c1 = ms_get_reg gamma1 r in
      leqc_norm (error r) ctxt c1 c2
    with Dict.Absent -> generate_error ctxt (Msabsentreg (r,gamma1,gamma2))
    | Talfail -> () in
  ms_app_reg check1 gamma2;
  fpstack_leq' ctxt (ms_get_fpstack gamma1) (ms_get_fpstack gamma2);
  (match ms_get_cc gamma1,ms_get_cc gamma2 with
    _,CCnoinfo -> ()
  | _,_ -> failwith "Talcon.leqrs - cc not fully implemented");
  leqcon ctxt (ms_get_cap gamma1) (ms_get_cap gamma2)
;;

(* NG - For now conmeet & conjoin check only for types in a subtype relation
 *      and reg_state_meet & reg_state_join are complete except they call
 *      conmeet & conjoin
 *)
let rec conmeet ctxt c1 c2 =
  let ctxt' = error_handler ctxt (fun _ _ -> raise NotEq) in
  try leqcon ctxt' c1 c2; c1
  with NotEq -> try leqcon ctxt' c2 c1; c2
  with NotEq -> generate_error ctxt (Conmeet (c1,c2)); raise Talfail
and conjoin ctxt c1 c2 =
  let ctxt' = error_handler ctxt (fun _ _ -> raise NotEq) in
  try leqcon ctxt' c1 c2; c2
  with NotEq -> try leqcon ctxt' c2 c1; c1
  with NotEq -> generate_error ctxt (Conjoin (c1,c2)); raise Talfail
and machine_state_meet ctxt rs1 rs2 =
  let aux r c1 rs =
    try
      ms_set_reg rs r (conmeet ctxt c1 (ms_get_reg rs2 r))
    with Dict.Absent -> rs in
  let ms = ms_fold_reg aux rs1 ms_empty in
  let fps1, fps2 = ms_get_fpstack rs1, ms_get_fpstack rs2 in
  if fpstack_equal fps1 fps2 then ms_set_fpstack ms fps1
  else (generate_error ctxt (Msmeet (rs1,rs2)); raise Talfail)
and machine_state_join ctxt rs1 rs2 =
  let aux r c1 rs =
    ms_set_reg rs r
      (try conjoin ctxt c1 (ms_get_reg rs r)
      with Dict.Absent -> c1) in
  let ms = ms_fold_reg aux rs1 rs2 in
  let fps1, fps2 = ms_get_fpstack rs1, ms_get_fpstack rs2 in
  if fpstack_equal fps1 fps2 then ms_set_fpstack ms fps1
  else (generate_error ctxt (Msjoin (rs1,rs2)); raise Talfail)
;;

(*************************************************************************)
(* Utilities                                                             *)
(*************************************************************************)

(* prove con given ctxt *)
let prove ctxt con =
  let prop = get_prop ctxt in
  leqcon ctxt prop con

(* separate arithmetic expression into (i32 constant,non-const con list) where
 * the original expresssion = constant + sum(non-const list)
 * Assumes whnorm.
 *)
let split_arithmetic ctxt c =
  match c.rcon with
    Cprim (PCint i) -> (i,[])
  | Cvar _ -> (i32_0,[c])
  | Clog (Cadd,cs) -> 
      let rec get_const cs =
	match cs with
	  [] -> (i32_0,[])
	| {rcon=Cprim (PCint i)}::rest -> (i,rest)
	| c::rest -> let (i,cs) = get_const rest in (i,c::cs) in
      get_const cs
  | Clog (Csub,cs) ->
      (match cs with
	[{rcon=Cprim (PCint i)};c2] -> (i,[whnorm ctxt (cmuls (~-$ i32_1) c2)])
      |	[c1;{rcon=Cprim (PCint i)}] -> (~-$ i,[c1])
      |	[c1;c2] -> (i32_0,c1::[whnorm ctxt (cmuls (~-$ i32_1) c2)])
      | _ -> invalid_arg "split_arithmetic: bad con: subtraction not binary")
  | Clog (l,cs) -> (i32_0,[c])
  | _ -> invalid_arg "split_arithmetic: bad con: not arithmetic expression"

(* replace all singleton unions with single element *)
let from_union ctxt c =
  let rec aux c =
    match c.rcon with
      Cprod cs -> cprod (List.map aux cs)
    | Csum [c] -> aux c
    | Csum cs -> csum (List.map aux cs)
    | _ -> c in
  match  c.rcon with
    Chptr (is,Some c,tco) -> chptr is (Some (aux c)) tco
  | _ -> c
;;
    
(* unroll a recursive type 
   no explicit substitutions though
*)
let rec unroll_rec exact ctxt c =
  match c.rcon with
    Cproj (i,c1) ->
      (let c1 = whnorm ctxt c1 in
      match c1.rcon with
	Crec [(v,k,c2)] ->  subst c v c2 (* csubst c2 (Es(v,c)) *)
      |	Crec fs ->
	   (* 
	  let aux (es,n) (v,_,_) =
	    let uc = if n=i then c else defcon (Cproj (n,c1)) in
	    (Eo(Es(v,uc),es),n+1) in
	  let (es,_) = List.fold_left aux (Enil,0) fs in
	  let (_,_,c2) =
	    try List.nth fs i
	    with Failure _ ->
	      generate_error ctxt (BadUnroll c); raise Talfail in
	  csubst c2 es *)
	   let aux (d,n) (v,_,_) =
	    let uc = if n=i then c else cproj c1 n in
	    (Dict.insert d v uc,n+1) in
	  let (d,_) = List.fold_left aux (Dict.empty id_compare,0) fs in
	  let (_,_,c2) =
	    try List.nth fs i
	    with Failure _ ->
	      generate_error ctxt (BadUnroll c); raise Talfail in
	  substs (Dict.empty id_compare, d) c2
      |	_ -> whnorm ctxt (cproj (unroll_rec exact ctxt c) i))
  | Capp (c1,c2) ->
      let c1 = whnorm ctxt c1 in
      whnorm ctxt (capp (unroll_rec exact ctxt c1) c2)
  | Clab l ->
      (match get_label_def ctxt l with
	AbsCon -> generate_error ctxt (BadUnroll c); raise Talfail
      |	BoundCon c -> if exact then generate_error ctxt (BadUnroll c); c
      |	ConcCon c -> c)
  | Chptr (is,Some c,tco) ->
      let c = whnorm ctxt c in
      whnorm ctxt (chptr is (Some(unroll_rec exact ctxt c)) tco)
  | _ -> generate_error ctxt (BadUnroll c); raise Talfail
;;


(* Calclulate the size of a stack type *)
let rec sizeof_stack ctxt c =
  match (whnorm ctxt c).rcon with
    Cempty -> i32_0
  | Ccons (c1,c2) -> (sizeof ctxt c1) +$ (sizeof_stack ctxt c2)
  | Cappend (c1, c2) -> (sizeof_stack ctxt c1) +$ (sizeof_stack ctxt c2)
  | _ -> generate_error ctxt (Unknown_size c); i32_0
;;

(* From a tal function type c, separate abstracted type variables and value
   variables *)
let rec separate_fun_type ctxt c =
  match (whnorm ctxt c).rcon with
    Cforall (v,k,c) -> 
      let (vks, prop, mstate) = separate_fun_type ctxt c in
      ((v,k) :: vks, prop, mstate)
  | Cif (c1,{rcon=Ccode mstate}) -> ([], c1, mstate)
  | Ccode mstate -> ([], pctrue, mstate)
  | _ -> generate_error ctxt (Conwf (c,"not a function type")); raise Talfail
;;

(*************************************************************************)
(* Field/Stack Slot Utilities                                            *)
(*************************************************************************)

	  
(* JGM's attempt to decode what is going on here -- why we need something
 * this complicated is beyond me.
 *
 * In essence, we're trying to extract a field type from a "product" of
 * some kind.  The offset is a byte offset where the field should live.
 * The depth is an int option which is not clearly documented and has
 * something to do with coercing (nested) structs to arrays (and is otherwise
 * always None.)  This returns a triple where the first component is an
 * "update" function which when given a constructor to go at position
 * offset yields the same type but with that field changed.  The second
 * component is the actual field type.  The third component is the 
 * "left over" offset?  Perhaps the actual depth of the thing.
 *)

(* Get the cons at the offset within a prod or a stack.
   If sizeopt is None then return the tail of the prod/stack.  In this
     case the function should be thrown away.
   If sizeopt is Some j then return a list of cons that have total size j,
     beginning at the offset.

   The offset returned is the leftover portion if the position is not 
   at an object boundary. (For example if the offset is to the 
   middle of an array, the array is returned, plus the part into the middle.)
 
   Dave: sizeopt is an optional size for the object at offset that is being retrieved
*)

type get_mem_style =
    ReturnRest         (* return rest of constructor after offset *)
  | ReturnCon          (* return con at offset, don't check size *)
  | CheckSize of int32 (* return con at offset, do check size *)

exception NoSizeCheck
let size_check_ctxt ctxt = error_handler ctxt (fun _ _ -> raise NoSizeCheck)

let rec get_rest_stack ctxt c sz =
  if sz =$ i32_0 then
    ([],c)
  else if sz <$ i32_0 then
    (raise NoSizeCheck)
  else
    (let c = whnorm ctxt c in
     match c.rcon with 
       Ccons (c1,c2) ->
	let sz1 = sizeof ctxt c1 in
	let hd,tl = get_rest_stack ctxt c2 (sz-$sz1) in
	(c1::hd,tl)
     | _ -> (raise NoSizeCheck))

let rec get_rest_cs ctxt cs sz =
  if sz =$ i32_0 then
    ([],cs)
  else if sz <$ i32_0 then
    (raise NoSizeCheck)
  else
    (match cs with
      [] ->  (raise NoSizeCheck)
    | c::cs ->
	let sz1 = sizeof ctxt c in
	let hd,tl = get_rest_cs ctxt cs (sz -$ sz1) in
	(c::hd,tl))
    
let rec get_mem_offset_p ctxt c offset gms =
  let c = whnorm ctxt c in
  match c.rcon with
    Cprod cs ->
      let (f,c,i,checked) = get_mem_offset_l ctxt cs offset gms in
      ((fun c -> cprod (f c)),c,i,checked)
  | Ccons (c1,c2) ->
      if offset =$ i32_0 then
	 match gms with
	   ReturnRest -> (id, [ccons c1 c2], i32_0, false)
	 | ReturnCon  -> ((fun c -> ccons c c2), [c1], i32_0, false)
	 | CheckSize sz -> 
	     let ctxt = size_check_ctxt ctxt in
	     (try
	       let sz1 = sizeof ctxt c1 in
	       let hd,tl = get_rest_stack ctxt c2 (sz-$sz1) in
	       ((fun c -> ccons c tl), c1::hd, i32_0, true)
	     with _ ->
	       ((fun c -> ccons c c2), [c1], i32_0, false))
      else
      	let c1s = sizeof ctxt c1 in
      	if c1s <=$ offset then
	  let (f,c,i,checked) = get_mem_offset_p ctxt c2 (offset-$c1s) gms in
	  ((fun c -> ccons c1 (f c)),c,i,checked)
      	else
	  ((fun c -> ccons c c2), [c1], offset, false)
   | _ ->
      (id,[c],offset,false)
   
and get_mem_offset_l ctxt cs offset gms =
   match cs with
      [] -> 
	(match gms with
	  ReturnRest -> ((fun c -> [c]), [cprod []], i32_0, false)
      	| _ -> (generate_error ctxt (Bad_offset offset); raise Talfail))
    | c::cs ->
	 if offset =$ i32_0 then
	    (match gms with
	      ReturnRest -> ((fun c -> [c]), [cprod (c::cs)], i32_0, false)
	    | ReturnCon  -> ((fun c -> c::cs), [c], i32_0, false)
	    | CheckSize sz ->
		let ctxt = size_check_ctxt ctxt in
		(try
		  let sz1 = sizeof ctxt c in
	      	  let hd,tl = get_rest_cs ctxt cs (sz-$sz1) in
		  ((fun c -> c::tl), c::hd, i32_0, true)
		with _ ->
		  ((fun c -> c::cs), [c], i32_0, false)))
	 else
      	    let csize = sizeof ctxt c in
      	    if csize <=$ offset then
	       let (f,cons,i,checked) = get_mem_offset_l ctxt cs (offset-$csize) gms in
	       ((fun c' -> c::(f c')),cons,i,checked)
      	    else
	       ((fun c -> c::cs), [c], offset, false)
	       

let get_mem_offset ctxt cs offset =
  let (f,c,i,_) = get_mem_offset_p ctxt cs offset ReturnCon in
  if i <>$ i32_0 then
    (generate_error ctxt (Bad_offset i); raise Talfail)
  else
    match c with
      [c] -> (f,c)
    | _ -> failwith  "Talcon.get_mem_offset: should return single con"
;;

let get_mem_offset_checked ctxt cs offset sz =
  let (f,c,i,check) = get_mem_offset_p ctxt cs offset (CheckSize sz) in
  if i <>$ i32_0 then
    (generate_error ctxt (Bad_offset i); raise Talfail)
  else
    (f,c,check)
;;

let get_mem_from_offset ctxt cs offset = 
   let (f,c,i,_) = get_mem_offset_p ctxt cs offset ReturnRest in
  if i <>$ i32_0 then
    (generate_error ctxt (Bad_offset i); raise Talfail)
  else
    (match c with
      [c] -> (f,c)
    | _ -> failwith "Talcon.get_mem_from_offset: should return single con")
;;

let get_mem_offset_p_checked ctxt c offset sz = 
  get_mem_offset_p ctxt c offset (CheckSize sz)
;;

let get_mem_offset_p ctxt c offset = 
  let (f,cs,i,_) = get_mem_offset_p ctxt c offset ReturnCon in
  match cs with
    [c] -> (f,c,i)
  | _ -> failwith "Talcon.get_mem_offset_p: should return single con"
;;

(* verify that the stack constructor c1 is a tail of stack constructor c2.
 * assumes c1,c2 normalized and returns a function, which when given a 
 * mutated version of c1, generates the corresponding mutated version of c2.
 * (see assign_simple_path below).  That is, we verify that there exists
 * a c3 such that Cappend(c1,c3) = c2 and return a function which maps
 * c1' to Cappend(c1',c3).
 *
 * JGM: this is a stupid algorithm for doing this, but will probably
 * work well in practice.
 *)
exception Tryrest
let verify_stack_tail ctxt c1 c2 =
  let ctxt' = error_handler ctxt (fun _ _ -> raise Tryrest) in
  let rec aux c2 =
    try
      leqcon ctxt' c2 c1; (* Dan: It should be okay for c1 to be a super-type *)
      id
    with Tryrest -> 
      (match (whnorm ctxt c2).rcon with
	Ccons(ca,cb) -> 
	  let f = aux cb in (fun c -> ccons ca (f c))
      | Cappend(ca,cb) ->
	  let f = aux cb in (fun c -> defcon(Cappend(ca,f c)))
      | _ -> generate_error ctxt (Not_tail (c1,c2)); id) in
  aux c2
;;

let write_stack_rest ctxt c1 size c2 =
  let c1s = sizeof ctxt c1 in
  let rec aux size c2 =
    if size =$ c1s then
      c2
    else if size >$ c1s then
      ccons (pcjunk (size-$c1s)) c2
    else
      match (whnorm ctxt c2).rcon with
	Ccons (c1,c2) -> aux (size+$(sizeof ctxt c1)) c2
      |	_ -> generate_error ctxt Stack_write_alignment; raise Talfail in
  ccons c1 (aux size c2)
;;

let rec write_stack_offset ctxt cs i c =
  match (whnorm ctxt cs).rcon with
    Ccons (c1,c2) ->
      let c1s = sizeof ctxt c1 in
      if i <$ c1s then
	let c2 = write_stack_rest ctxt c (c1s-$i) c2 in
	if i >$ i32_0 then ccons (pcjunk i) c2 else c2
      else
	ccons c1 (write_stack_offset ctxt c2 (i-$c1s) c)
  | _ -> generate_error ctxt (Bad_offset i); raise Talfail
;;

let rec get_stack_tail ctxt i con =
  match i,(whnorm ctxt con).rcon with
    i,_ when i =$ i32_0 -> con
  | i,Ccons (c,con) ->
      let sc = sizeof ctxt c in
      if sc >$ i then
	ccons (pcjunk (sc-$i)) con
      else if sc =$ i then
	con
      else
	get_stack_tail ctxt (i-$sc) con
  | _,_ -> generate_error ctxt (Bad_offset i); raise Talfail
;;


(* EOF: talcon.ml *)
