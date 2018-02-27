(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dan Grossman                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* talpp.ml
 *
 * Pretty printer for x86 tal language.
 *
 * TODO: 1. GAS support
 *       2. Movsx/movzx instructions
 *)

open Utilities;;
open Numtypes;;
open Format;;
open Identifier;;
open Tal;;

type style = MASM | GAS
type detail = FullDetail | PartialDetail
type options = { style          : style; 
		 kinds          : bool; 
		 cons           : bool; 
		 expand_abbrevs : bool;
		 expand_pr      : bool;
		 detail         : detail;} (* Overridden internally. *)

let std_options = {style=MASM; 
		    kinds          = true; 
		    cons           = true ; 
		    expand_abbrevs = true;
                    expand_pr      = false;
		    detail         = FullDetail };;

(* Control output of types in error messages. *)
let full_types = ref false

(* Utilities *)

let rec sepi fmt i s p l =
  match l with
    [] -> ()
  | [x] -> p x
  | x::l ->
      p x; pp_print_string fmt s; pp_print_break fmt 0 i; sepi fmt i s p l
;;

let rec sepb fmt s p l =
  match l with
    [] -> ()
  | [x] -> p x
  | x::l ->
      p x; pp_print_break fmt 1 0; pp_print_string fmt s; sepb fmt s p l
;;

let sep fmt s p l = sepi fmt 0 s p l;;
let sepc fmt p l = sepi fmt 0 "," p l;;
let sepci fmt i p l = sepi fmt i "," p l;;

(* Misc *)

let print_scale fmt s = 
  match s with
    Byte1 -> pp_print_char fmt '1'
  | Byte2 -> pp_print_char fmt '2'
  | Byte4 -> pp_print_char fmt '4'
  | Byte8 -> pp_print_char fmt '8'
;;

let string_of_log l =
  match l with
    Cadd -> "++"
  | Csub -> "-"
  | Cmuls -> "*#"
  | Cmulu -> "*u"
  | Cand -> " && " (* extra space makes 'em more legible *)
  | Cor -> " | "
  | Cimp -> " =!> "
  | Ciff -> " iff "
  | Cnot -> "~"
  | Clts -> "<#"
  | Cltu -> "<"
  | Cltes -> "<=#"
  | Clteu -> "<="

let string_of_reg r =
  match r with
    Eax -> "a"
  | Ebx -> "b"
  | Ecx -> "c"
  | Edx -> "d"
  | Esi -> "si"
  | Edi -> "di"
  | Ebp -> "bp"
  | Esp -> "sp"
  | Virt i -> id_to_string i
;;

let print_reg_part fmt o r part =
  (match o.style with
    MASM ->
      (match r with
	Eax | Ebx | Ecx | Edx ->
	  let r = String.uppercase (string_of_reg r) in
	  (match part with
	    RPe -> fprintf fmt "E%sX" r
	  | RPx -> fprintf fmt "%sX" r
	  | RPh -> fprintf fmt "%sH" r
	  | RPl -> fprintf fmt "%sL" r)
      |	Esi | Edi | Ebp | Esp ->
	  let r = String.uppercase (string_of_reg r) in
	  (match part with
	    RPe -> fprintf fmt "E%s" r
	  | RPx -> fprintf fmt "%s" r
	  | RPh | RPl -> invalid_arg "Talpp.print_reg_part")
      |	Virt i ->
	  let hd =
	    match part with
	      RPe -> "R" | RPx -> "R16" | RPh -> "RH" | RPl -> "RL" in
	  fprintf fmt "%s(%s)" hd (id_to_string i))
  | GAS -> failwith "GAS unimplemented")
;;

let print_reg fmt o r = print_reg_part fmt o r RPe;;

(* print fpstack as part of register file. 
 * return true if comma is required before next register is printed 
 *)
let print_internal_fpstack fmt o fps need_comma =
  let rec aux fps need_comma i =
    let pp_reg () = 
      if need_comma then pp_print_char fmt ',';
      pp_print_string fmt ("ST"^string_of_int i) in
    if i < 8 then 
      match fpstack_get_fpreg fps i with
      |	FPfull -> pp_reg (); aux fps true (i+1)
      |	FPempty -> aux fps need_comma (i+1)
      |	FPany -> pp_reg (); pp_print_string fmt ("?"); aux fps true (i+1)
    else () in
  if fpstack_isempty fps then need_comma
  else (aux fps need_comma 0; true)

(* print fpstack on its own *)
let print_fpstack fmt o fps =
  pp_print_char fmt '{';
  print_internal_fpstack fmt o fps false;
  pp_print_char fmt '}'
  
(* Kinds *)

let rec print_kind_a fmt k =
  match k.rkind with
    Kbyte s -> pp_print_char fmt 'T'; print_scale fmt s
  | Ktype -> pp_print_char fmt 'T'
  | Kmemi i -> fprintf fmt "Tm %s" (string_of_int32 i)
  | Kmem -> pp_print_string fmt "Tm"
  | Kstack -> pp_print_string fmt "Ts"
  | Kint -> pp_print_string fmt "Sint"
  | Kbool -> pp_print_string fmt "Tbool"
  | Karrow (k1,k2) ->
      print_kind_b fmt true k1;
      pp_print_string fmt "-!>";
      pp_print_cut fmt ();
      (match k2.rkind with
	 Karrow (_,_) -> print_kind_a fmt k2
       | _ -> print_kind_b fmt false k2)
  | Kprod ks ->
      pp_print_string fmt "*[";
      sepci fmt 2 (print_kind_b fmt false) ks;
      pp_print_string fmt "]"
  | Kname -> pp_print_string fmt "Tn"
  | Kcap -> pp_print_string fmt "Tcap"
  | Kms -> pp_print_string fmt "Tms"
(* LX *)
  | Ksum ks -> 
      pp_print_string fmt "+[";
      sepci fmt 2 (print_kind_b fmt false) ks;
      pp_print_string fmt "]"
  | Kvar id ->  id_prn fmt id
  | Kmu ([i1,k],i) -> 
       pp_print_string fmt "mu "; id_prn fmt i;
       pp_print_char fmt '.'; print_kind_a fmt k
  | Kmu (ks,i) -> 
       pp_print_string fmt "Kmu "; id_prn fmt i;
       pp_print_string fmt " with [";
       List.iter (fun (i,s) -> id_prn fmt i; pp_print_string fmt " ") ks;
       pp_print_string fmt "]"
(* end LX *)

and print_kind_b fmt p k =
  match k.rkind with
     Karrow (_,_) ->
      if p then pp_print_string fmt "(";
      pp_open_hovbox fmt 0; print_kind_a fmt k; pp_close_box fmt ();
      if p then pp_print_string fmt ")"
  | _ -> pp_open_hovbox fmt 0; print_kind_a fmt k; pp_close_box fmt ()
and print_kind fmt o k =
  if o.kinds then print_kind_b fmt false k
and print_ckind fmt o k =
  if o.kinds then begin
    pp_print_string fmt ":"; print_kind_b fmt false k
  end
;;

(* Constructors *)

let print_primcon fmt pc =
  match pc with
    PCbytes Byte1 -> pp_print_string fmt "B1"
  | PCbytes Byte2 -> pp_print_string fmt "B2"
  | PCbytes Byte4 -> pp_print_string fmt "B4"
  | PCbytes Byte8 -> pp_print_string fmt "B8"
  | PCfloat32 -> pp_print_string fmt "F4"
  | PCfloat64 -> pp_print_string fmt "F8"
  | PCjunk i -> fprintf fmt "junk %s" (string_of_int32 i)
  | PCjunkbytes Byte1  -> fprintf fmt "junk1"
  | PCjunkbytes Byte2  -> fprintf fmt "junk2"
  | PCjunkbytes Byte4  -> fprintf fmt "junk4"
  | PCjunkbytes Byte8  -> fprintf fmt "junk8"
  | PCint i  -> fprintf fmt "%s" (string_of_int32 i)
  | PCtrue   -> fprintf fmt "true"
  | PCfalse  -> fprintf fmt "false"
;;

let print_variance fmt v =
  match v with
    Read -> pp_print_string fmt "^r"
  | Write -> pp_print_string fmt "^w"
  | ReadWrite -> pp_print_string fmt "^rw"
;;

let log_prec l =
  match l with
    Cadd -> 10
  | Csub -> 10
  | Cmuls -> 11
  | Cmulu -> 11
  | Cand -> 6
  | Cor -> 7
  | Cimp -> 5
  | Ciff -> 5
  | Cnot -> 12
  | Clts -> 9
  | Cltu -> 9
  | Cltes -> 9
  | Clteu -> 9

let prec c =
  match c with
    Cvar _ -> 100
  | Clam (_,_,_) -> 1
  | Capp (_,_) -> 2
  | Ctuple _ -> 100
  | Cproj (_,_) -> 3
  | Clab _ -> 100
  | Cprim _ -> 100
  | Crec _ -> 100
  | Cforall (_,_,_) -> 4
  | Cexist (_,_,_,_) -> 4
  | Ccode _ -> 4
  | Cmsjoin (_,_) -> 5
  | Cms _ -> 100
  | Chptr (_,_,_) -> 4
  | Cfield (_,_) -> 7
  | Cprod _ -> 100
  | Csum _ -> 100
  | Carray _ -> 100
  | Csing _ -> 100
  | Csptr _ -> 2
  | Cempty -> 100
  | Ccons (_,_) -> 6
  | Cappend (_,_) -> 5
  | Clog (l,_) -> log_prec l
  | Cif (_,_) -> 4
  | Cjoin _ -> 100
  | Ccap _ -> 100
  | Cname _ -> 100
  | Ctagof _ -> 100
(* Cyclone *)
  | Ctmpl _ -> 100
  | Ctptr _ -> 5
  | Ctrgn _ -> 100
  | Csubst (_,_) -> 100
(* End Cyclone *)
(* LX *)
  | Cinj(_,_,_) -> 3
  | Ccase(_,_,_) -> 100
  | Cfold (_,_) -> 100
  | Cvoid _ -> 100
  | Cpr (_,_) -> 100
(* end LX *)
  | Cr _ -> 100
  | Ctypeof _ -> 100
;;

let rec print_con_a fmt o inprec con =
  let finished = 
    match o.detail with
    | PartialDetail when con.hash < 0 -> 
	(print_con_short fmt o inprec con; true)
    | _ -> false in
  if finished then () else 
  let finished = 
    match con.abbrev, o.expand_abbrevs with 
      Some v,false -> ( id_prn fmt v; true)
    | _,_ ->  false in
  if finished then () else 
  let c = con.rcon in
  let myprec = prec c in
  let lp () =
    if inprec > myprec then pp_print_string fmt "("; pp_open_hovbox fmt 0 in
  let rp () =
    pp_close_box fmt (); if inprec > myprec then pp_print_string fmt ")" in
  match c with
    Cvar v -> id_prn fmt v
  | Clam (v,k,c) -> 
      lp ();
      pp_print_string fmt "fn "; pp_open_hovbox fmt 0;
      print_con_b fmt o myprec v k c;
      rp ()
  | Capp (c1,c2) ->
      lp (); print_con_a fmt o myprec c1; pp_print_break fmt 1 2;
      print_con_a fmt o (myprec+1) c2; rp ()
  | Ctuple cs ->
      pp_open_hovbox fmt 0; pp_print_char fmt '[';
      sepci fmt 1 (print_con_a fmt o 0) cs;
      pp_print_char fmt ']'; pp_close_box fmt ()
  | Cproj (i,c) ->
      lp ();
      print_con_a fmt o myprec c; pp_print_string fmt ("."^(string_of_int i));
      rp ()
  | Clab l -> pp_print_char fmt '`'; id_prn fmt l
  | Cprim pc -> print_primcon fmt pc
  | Crec vkcs ->
      let f (v,k,c) =
	pp_open_hovbox fmt 0; id_prn fmt v; print_ckind fmt o k;
 	pp_print_char fmt '.'; pp_print_break fmt 0 2; print_con_a fmt o 0 c;
 	pp_close_box fmt () in
      pp_print_string fmt "rec(";
      pp_open_hovbox fmt 0; sepc fmt f vkcs;  pp_close_box fmt ();
      pp_print_string fmt ")"
  | Cforall (v,k,c) ->
      lp ();
      pp_print_string fmt "All["; pp_open_hovbox fmt 0;
      print_con_c fmt o myprec v k c;
      rp ()
  | Cexist (v,k,c1,c2) ->
      lp ();
      pp_print_string fmt "Exist["; pp_open_hovbox fmt 0;
      print_con_d fmt o myprec v k c1 c2;
      rp ()
  | Ccode c -> 
      lp();
      pp_print_string fmt "code"; pp_print_break fmt 1 2;
      print_con_a fmt o (myprec+1) c; rp()
  | Cms ms -> print_machine_state fmt o ms
  | Cmsjoin(c1,c2) -> 
      lp ();
      print_con_a fmt o (myprec+1) c1; pp_print_char fmt '&';
      pp_print_cut fmt (); print_con_a fmt o myprec c2;
      rp ();
  | Chptr (is,co,tco) ->
      let print_tags is =
 	sepc fmt (fun i -> fprintf fmt "%s" (string_of_int32 i)) is in
      lp ();
      fprintf fmt "^";
      (match tco with
	None ->
	  if is<>[] then begin
	    let (o,c) = if co=None then '[',']' else '(',')' in
	    fprintf fmt "T%c@[<hov>" o; print_tags is; fprintf fmt "@]%c" c
	  end
      |	Some (c,v) ->
	  fprintf fmt "T%a(@[<hov>" print_variance v; print_con_a fmt o 0 c;
	  if is<>[] then begin
	    fprintf fmt ",@;<0 0>"; print_tags is
	  end;
	  fprintf fmt "@])");
      (match co with
	None -> ()
      |	Some c -> print_con_a fmt o myprec c);
      rp ()
  | Cfield (c,v) ->
      lp (); print_con_a fmt o myprec c; print_variance fmt v; rp ()
  | Cprod cs ->
      pp_print_string fmt "*[";
      pp_open_hovbox fmt 0; sepc fmt (print_con_a fmt o 0) cs;
      pp_close_box fmt (); pp_print_char fmt ']';
  | Csum cs ->
      pp_print_string fmt "+[";
      pp_open_hovbox fmt 0; sepc fmt (print_con_a fmt o 0) cs;
      pp_close_box fmt (); pp_print_char fmt ']';
  | Carray(cl,ce) ->
      pp_open_hovbox fmt 0; pp_print_string fmt "array(";
      print_con_a fmt o 0 cl; pp_print_char fmt ','; pp_print_break fmt 0 2;
      print_con_a fmt o 0 ce; pp_print_string fmt ")"; pp_close_box fmt ()
  | Csing c ->
      pp_print_string fmt "S("; print_con_a fmt o 0 c; pp_print_string fmt ")"
  | Csptr c ->
      lp (); pp_print_string fmt "sptr"; pp_print_break fmt 1 2;
      print_con_a fmt o (myprec+1) c; rp ()
  | Cempty -> pp_print_string fmt "se"
  | Ccons (c1, c2) ->
      lp ();
      print_ccons fmt o myprec con;
      rp ();
(*      lp ();
      print_con_a fmt o (myprec+1) c1; pp_print_string fmt "::";
      pp_print_cut fmt (); print_con_a fmt o myprec c2;
      rp () *)
  | Cappend (c1,c2) -> 
      lp ();
      print_con_a fmt o (myprec+1) c1; pp_print_string fmt "#";
      pp_print_cut fmt ();
      print_con_a fmt o myprec c2;
      rp ()
  | Cif (c1,c2) ->
      lp ();
      pp_print_string fmt "All["; print_con_a fmt o 0 c1; 
      pp_print_string fmt "]."; pp_print_break fmt 0 2;
      print_con_a fmt o myprec c2;
      rp ()
  | Clog (l,cs) -> print_log_a fmt o inprec l cs
  | Cname c -> 
      pp_print_string fmt "Nm("; print_con_a fmt o 0 c; pp_print_string fmt ")"
  | Ctagof c ->
      pp_print_string fmt "Tagof("; print_con_a fmt o 0 c;
      pp_print_string fmt ")"
  | Cjoin cs ->
      pp_print_string fmt "&["; 
      pp_open_hovbox fmt 0; sepc fmt (print_con_a fmt o 0) cs;
      pp_close_box fmt (); pp_print_char fmt ']';
  | Ccap d ->
      let entries = Dict.fold_dict (fun x p es -> (x,p)::es) d [] in
      pp_print_string fmt "cap[";
      pp_open_hovbox fmt 0; 
      sepc fmt 
	(fun (x,(ai,c)) -> 
	  (id_prn fmt x;
	   pp_print_char fmt 
	     (match ai with
	       MayAlias -> ':'
	     |	Unique -> '!');
	   print_con_a fmt o 0 c)) entries;
      pp_close_box fmt (); pp_print_char fmt ']';
(* Cyclone *)
  | Ctmpl(c1,c2_opt,labels,holes) ->
      let f (v,c) =
	pp_open_hovbox fmt 0; id_prn fmt v; pp_print_char fmt ':';
        pp_print_break fmt 0 2; print_con_a fmt o 0 c; pp_close_box fmt () in
      pp_print_string fmt "tmpl(";
      pp_open_hovbox fmt 0;
      print_con_a fmt o 0 c1; pp_print_char fmt ',';
      (match c2_opt with None -> pp_print_string fmt "*"
      | Some c2 -> print_con_a fmt o 0 c2);
      pp_print_char fmt ',';
      pp_print_char fmt '{'; sepc fmt f labels; pp_print_char fmt '}';
      pp_print_char fmt ',';
      pp_print_char fmt '{'; sepc fmt f holes; pp_print_char fmt '}';
      pp_close_box fmt ();
      pp_print_string fmt ")" 
  | Ctrgn(c1,c2_opt,t) ->
      let f (v,c) =
	pp_open_hovbox fmt 0; id_prn fmt v; pp_print_char fmt ':';
        pp_print_break fmt 0 2; print_con_a fmt o 0 c; pp_close_box fmt () in
      let g (v,labels,holes) =
        pp_print_char fmt '(';
        pp_open_hovbox fmt 0;
        id_prn fmt v;
        pp_print_char fmt ',';
        pp_print_char fmt '{'; sepc fmt f labels; pp_print_char fmt '}';
        pp_print_char fmt ',';
        pp_print_char fmt '{'; sepc fmt f holes; pp_print_char fmt '}';
        pp_close_box fmt ();
        pp_print_char fmt ')' in
      pp_print_string fmt "cgregion(";
      pp_open_hovbox fmt 0;
      print_con_a fmt o 0 c1; pp_print_char fmt ',';
      (match c2_opt with None -> pp_print_string fmt "*"
      | Some c2 -> print_con_a fmt o 0 c2);
      pp_print_char fmt ',';
      pp_print_char fmt '{'; sepc fmt g t; pp_print_char fmt '}';
      pp_close_box fmt ();
      pp_print_char fmt ')'
  | Ctptr v ->
      lp (); pp_print_string fmt "tptr"; pp_print_break fmt 1 2;
      id_prn fmt v; rp ()
(* End Cyclone *)
(* LX *)
  | Cinj(i,c,k) ->
       pp_open_box fmt 0;
       lp(); pp_print_string fmt "inj";  pp_print_break fmt 0 2;
       pp_print_int fmt i; pp_print_break fmt 0 2;
       lp(); print_con_a fmt o 0 c;  rp(); pp_print_break fmt 0 2;
       pp_print_char fmt '[';  print_kind fmt o k; pp_print_char fmt ']'; 
       rp();
       pp_close_box fmt ()
  | Ccase(c,a,cs) -> 
       pp_print_string fmt "case ("; print_con_a fmt o 0 c; 
       pp_print_string fmt ")"; id_prn fmt a;
       pp_print_string fmt "[";
       pp_open_hovbox fmt 0; sepc fmt (print_con_a fmt o 0) cs;
       pp_close_box fmt (); pp_print_char fmt ']';
  | Cfold(k,c) ->  
       pp_print_string fmt "roll[";
       print_kind fmt o k; pp_print_char fmt ']';
       lp();
       print_con_a fmt o (myprec+1) c;
       rp ();
  | Cpr(i,l) ->
       let rec find l = 
	  match l with
	     ((_,_,_,f,_,_) as hd::rest) ->
	  	if id_compare f i = 0 
	  	then hd else find rest
           | [] -> failwith "ill-formed Cpr in talpp.ml"
       in 
       let (j,a,k,f,k',c) = find l in 
       if (not o.expand_pr) then 
       id_prn fmt f 
       else 
	  begin 
	     lp(); id_prn fmt f;
	     pp_print_string fmt " : "; id_prn fmt j ; 
	     pp_print_string fmt " -!>"; print_kind fmt o k'; pp_print_string fmt " = fn ";
	     id_prn fmt a; pp_print_string fmt ":"; print_kind fmt o k ;  pp_print_string fmt ".";
	     print_con_a fmt o (myprec+1) c; rp()
	  end 
	  	  
(* just print out the id for now... maybe later we'll 
		       print more information *)
  | Cvoid k -> 
       pp_print_string fmt "void[";
       print_kind fmt o k;
       pp_print_string fmt "]"
(* end LX *)
  | Csubst(c,s) -> 
      let print_one_subst v c = 
	pp_open_hovbox fmt 0; id_prn fmt v; pp_print_char fmt '=';
	print_con_a fmt o 0 c; pp_close_box fmt() in
      let rec print_subst s = 
	match s with
	  Enil -> ()
	| Es(v,c) -> print_one_subst v c
	| Eo(s1,s2) -> print_subst s1; print_subst s2 in
      pp_print_string fmt "let ";
      pp_open_hovbox fmt 0;
      print_subst s;
      pp_close_box fmt ();
      pp_print_string fmt " in ";
      print_con_a fmt o 0 c; 
      pp_print_string fmt " end"
  | Cr (ri) ->
       pp_print_string fmt "Rep";
       (match ri with 
	  RCon c -> pp_print_string fmt "(type "; print_con_a fmt o 0 c
	| RKind k -> pp_print_string fmt "(kind ";
	     print_kind fmt o k
	| RLabel l -> pp_print_string fmt "(label ";
	     id_prn fmt l);
       pp_print_string fmt ")"
  | Ctypeof l -> (pp_print_string fmt "typeof "; 
		  id_prn fmt l)
and print_con_b fmt o myprec v k con =
  match con.abbrev, o.expand_abbrevs with 
     Some v,false -> (id_prn fmt v)
   | _,_ ->
  	let c = con.rcon in
  	id_prn fmt v; print_ckind fmt o k; pp_print_break fmt 1 0;
  	match c with
	   Clam (v,k,c) -> print_con_b fmt o myprec v k c
	 | _ ->
	      pp_close_box fmt (); pp_print_char fmt '.'; pp_print_break fmt 1 2;
	      print_con_a fmt o myprec con
and print_con_c fmt o myprec v k con =
  match con.abbrev, o.expand_abbrevs with 
     Some v,false -> (id_prn fmt v)
   | _,_ ->
  	let c = con.rcon in
  	id_prn fmt v; print_ckind fmt o k;
  	match c with
	   Cforall (v,k,c) -> pp_print_break fmt 1 0; print_con_c fmt o myprec v k c
	 | Cif (c1,c2) -> 
	      pp_print_break fmt 1 0; print_con_a fmt o myprec c1; pp_close_box fmt ();
	      pp_print_string fmt "]."; pp_print_break fmt 0 2;
	      print_con_a fmt o myprec c2
	 | _ ->
	      pp_close_box fmt (); pp_print_string fmt "]."; pp_print_break fmt 0 2;
	      print_con_a fmt o myprec con
and print_con_d fmt o myprec v k c1 c2 =
  let rc1 = c1.rcon in
  let rc2 = c2.rcon in
  id_prn fmt v; print_ckind fmt o k;
  match rc1,rc2 with
    Cprim PCtrue,Cexist (v,k,c1,c2) -> 
      pp_print_break fmt 1 0; print_con_d fmt o myprec v k c1 c2
  | Cprim PCtrue,_ ->
      pp_close_box fmt (); pp_print_string fmt "]."; pp_print_break fmt 0 2;
      print_con_a fmt o myprec c2
  | _ ->
      pp_print_string fmt "|"; pp_print_break fmt 0 2;
      print_con_a fmt o 0 c1;
      pp_close_box fmt (); pp_print_string fmt "]."; pp_print_break fmt 0 2;
      print_con_a fmt o myprec c2
and print_ccons fmt o myprec c =
  match c.rcon with 
  | Ccons(c1,c2) -> print_con_a fmt o (myprec+1) c1; pp_print_string fmt "::";
      pp_print_cut fmt (); print_ccons fmt o myprec c2
  | _ -> print_con_a fmt o myprec c
and print_log_a fmt o inprec l cs =
  let myprec = log_prec l in
  let lp () =
    if inprec > myprec then pp_print_string fmt "("; pp_open_hovbox fmt 0 in
  let rp () =
    pp_close_box fmt (); if inprec > myprec then pp_print_string fmt ")" in
  match l,cs with
    Cnot,[c] ->
      lp ();
      pp_print_string fmt "~";
      print_con_a fmt o (myprec+1) c; 
      rp ()
  | _,_ ->  
      lp ();
      sepi fmt 0 (string_of_log l) (print_con_a fmt o (myprec+1)) cs;
      rp ()
and print_machine_state fmt o ms =
  pp_print_string fmt "{"; pp_open_hovbox fmt 0;
  let cap = ms_get_cap ms in
  let need_comma =
    if cap <> cempty_cap then
      (pp_print_string fmt "cap: "; print_con_a fmt o 0 (ms_get_cap ms); 
       true)
    else 
      false in

  (* Floating point *)
  let need_comma = 
    print_internal_fpstack fmt o (ms_get_fpstack ms) need_comma in

  let s =
    match ms_get_cc ms with
      CCnoinfo -> 
	if need_comma then ("," : (unit, Format.formatter, unit) format)
	else ("" : (unit, Format.formatter, unit) format)
    | CCcmp (c1,c2) ->
      (* parser does not accept this *)
	if need_comma then pp_print_char fmt ',';
      	fprintf fmt "cc: cmp(@[<hv>"; print_con_a fmt o 0 c1;
      	fprintf fmt ",@;<0 0>"; print_con_a fmt o 0 c2; fprintf fmt "@])";
	(",@;<0 0>" : (unit, Format.formatter, unit) format)
    | CCtest (_,_) ->
      	failwith "Talpp.print_machine_state - cc unimplemented" in
  let f (r,c) =
    print_reg fmt o r; pp_print_string fmt ": "; print_con_a fmt o 0 c in
  let rlist = ms_fold_reg (fun r c rl -> (r,c)::rl) ms [] in
  if rlist <> [] then fprintf fmt s;
  sepc fmt f rlist; pp_close_box fmt (); pp_print_string fmt "}"
(* Print a short version of a type for error messages -- one word. *)
and print_con_short fmt o inprec con =
  let pp_str = pp_print_string fmt in
  let pp_short s = pp_str ("*"^s^"*") in
  match con.rcon with
  | Clam _ -> pp_short "lambda"
  | Capp _ -> pp_short "app"
  | Ctuple _ -> pp_short "tuple"
  | Cproj _ -> pp_short "proj"
  | Cinj _ -> pp_short "inj"
  | Ccase _ -> pp_short "case"
  | Cfold _ -> pp_short "fold"
  | Cpr _ -> pp_short "pr"
  | Cvoid _ -> pp_short "void"
  | Crec _ -> pp_short "rec"
  | Cforall _ -> pp_short "forall"
  | Cexist _ -> pp_short "exist"
  | Ccode _ -> pp_short "code"
  | Cms _ -> pp_short "ms"
  | Cmsjoin _ -> pp_short "msjoin"
  | Chptr _ -> pp_short "hptr"
  | Cfield _ -> pp_short "field"
  | Cprod _ -> pp_short "prod"
  | Csum _ -> pp_short "sum"
  | Carray _ -> pp_short "array"
  | Csptr _ -> pp_short "sptr"
  | Ccons _ -> pp_short "cons"
  | Cappend _ -> pp_short "append"
  | Clog _ -> pp_short "log"
  | Cif _ -> pp_short "if"
  | Ccap _ -> pp_short "cap"
  | Ctagof _ -> pp_short "tagof"
  | Ctmpl _ -> pp_short "tmpl"
  | Ctptr _ -> pp_short "tptr"
  | Ctrgn _ -> pp_short "rgn"
  | Csubst _ -> pp_short "subst"
  | Cr _ -> pp_short "R"
  | _ -> print_con_a fmt {o with detail = FullDetail} inprec con (* default is print full. *)
;;
    
let print_con fmt o c =
  if o.cons then
     (match c.abbrev, o.expand_abbrevs with 
     	Some v,false -> (id_prn fmt v)
      | _,_ -> print_con_a fmt o 0 c)
		      
(* Cyclone *)
  else pp_print_string fmt "XX" (* TEMPORARY; USEFUL TO PUSH THROUGH MASM *)
(* End Cyclone *)
and print_ccon fmt o c =
  if o.cons then begin pp_print_string fmt ":"; print_con_a fmt o 0 c end
;;

(*** Code ***)
let print_annotate fmt o a =
  if o.cons 
  then
    match a with
      Con       c         -> print_con  fmt o c
    | AReg      r         -> print_reg  fmt o r
    | StackTail (r,i)     -> print_reg  fmt o r;
                             pp_print_char fmt ' '; pp_print_int fmt i 
    | StackSlice(r,i,j,c) -> print_reg fmt o r;
	                     pp_print_char fmt ' '; pp_print_int fmt i;
	                     pp_print_char fmt ' '; pp_print_int fmt j;
	                     pp_print_char fmt ' '; print_con fmt o c

(* 'a Coerce *)

let rec strip_tapps clist cons =
  match clist with
    (Tapp con)::clist -> strip_tapps clist (con::cons)
  | _ -> (clist,cons)
;;

let rec print_coerce fmt f o (raw,clist) =
  match clist with
    [] -> f fmt o raw
  | (Pack (c1,c2))::clist ->
      (* c1 is hidden type, c2 is existential *)
      pp_open_hovbox fmt 0;
      pp_print_string fmt "pack(<"; print_con fmt o c1;
      pp_print_string fmt ">,"; pp_print_break fmt 0 2;
      print_coerce fmt f o (raw,clist);
      pp_print_char fmt ','; pp_print_break fmt 0 2;
      pp_print_char fmt '<'; print_con fmt o c2; pp_print_string fmt ">)";
      pp_close_box fmt ()
  | (Tapp a)::clist -> (* changed by Dan for anotations *)
      let (clist,cons) = strip_tapps clist [a] in
      pp_open_hovbox fmt 0;
      pp_print_string fmt "tapp(";
      print_coerce fmt f o (raw,clist);
      pp_print_char fmt ','; pp_print_break fmt 0 2; pp_print_char fmt '<';
      pp_open_hovbox fmt 0; sepc fmt (print_annotate fmt o) cons;
      pp_close_box fmt (); pp_print_string fmt ">)";
      pp_close_box fmt ();
  | (Roll t)::clist ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "roll(<"; print_con fmt o t;
      pp_print_string fmt ">,"; pp_print_break fmt 0 2;
      print_coerce fmt f o (raw,clist); pp_print_string fmt ")";
      pp_close_box fmt ()
  | (Unroll)::clist ->
      pp_print_string fmt "unroll("; print_coerce fmt f o (raw,clist);
      pp_print_string fmt ")"
  | (Tosum t)::clist -> 
      pp_open_hovbox fmt 0;
      pp_print_string fmt "sum("; pp_print_char fmt '<'; print_con fmt o t;
      pp_print_string fmt ">,"; pp_print_break fmt 0 2;
      print_coerce fmt f o (raw,clist); pp_print_string fmt ")";
      pp_close_box fmt ()
  | (RollTosum t)::clist -> 
      pp_open_hovbox fmt 0;
      pp_print_string fmt "rollsum("; pp_print_char fmt '<'; print_con fmt o t;
      pp_print_string fmt ">,"; pp_print_break fmt 0 2;
      print_coerce fmt f o (raw,clist); pp_print_string fmt ")";
      pp_close_box fmt ()
  | (Fromsum)::clist ->
      pp_print_string fmt "rec("; print_coerce fmt f o (raw,clist);
      pp_print_string fmt ")"
  | (Toarray (off,d,c))::clist ->
      fprintf fmt "@[<hv>array(%s,@;<0 2>%d,@;<0 2><" (string_of_int32 off) d;
      print_con fmt o c; fprintf fmt ">,@;<0 2>";
      print_coerce fmt f o (raw,clist); fprintf fmt ")@]"
  | (Slot (i,sz))::clist ->
      fprintf fmt "@[<hv>slot(%s,@;<0 2>%s,@;<0 2>"
 	(string_of_int32 i) (string_of_int32 sz);
      print_coerce fmt f o (raw,clist); fprintf fmt ")@]"
  | (Subsume t)::clist ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "subsume("; pp_print_char fmt '<'; print_con fmt o t;
      pp_print_string fmt ">,"; pp_print_break fmt 0 2;
      print_coerce fmt f o (raw,clist); pp_print_string fmt ")";
      pp_close_box fmt ()
  | Forgetname::clist ->
      pp_print_string fmt "forgetname("; print_coerce fmt f o (raw,clist);
      pp_print_string fmt ")"
  | Prove::clist ->
      pp_print_string fmt "prove("; print_coerce fmt f o (raw,clist);
      pp_print_string fmt ")"
  | VirtLabel(_)::clist -> failwith "virtual label!"
;;

let print_label_coerce fmt o lc =
  print_coerce fmt (fun fmt _ l -> id_prn fmt l) o lc
;;

let print_reg_coerce fmt o rc = print_coerce fmt print_reg o rc;;

let print_genop fmt o gop =
  if o.style=GAS then failwith "GAS unimplemented";
  let print_opt opt =
    match opt with
      None -> ()
    | Some (s,r') -> 
	pp_print_char fmt '+';
	if not (s = Byte1) then (print_scale fmt s; pp_print_char fmt '*');
	print_reg fmt o r' in
  match gop with
    Immed i -> fprintf fmt "%s" (string_of_int32 i)
  | Reg r -> print_reg fmt o r
  | Addr l -> id_prn fmt l
  | Prjr (r,i,opt) ->
      pp_print_char fmt '['; print_reg_coerce fmt o r;
      print_opt opt;
      if not (i =$ i32_0) then (fprintf fmt "+%s" (string_of_int32 i));
      pp_print_char fmt ']'
  | Prjl (l,i,opt) ->
      pp_print_char fmt '['; print_label_coerce fmt o l;
      print_opt opt;
      if not (i =$ i32_0) then (fprintf fmt "+%s" (string_of_int32 i));
      pp_print_char fmt ']'
;;

let print_genop_part fmt o gop reg_part =
  match gop with
  | Reg r -> print_reg_part fmt o r reg_part	
  | _ -> print_genop fmt o gop
;;

let print_genop_coerce fmt o cgop = print_coerce fmt print_genop o cgop;;

(* MASM gets projections of code labels wrong and also needs size information
 * in some instances.  Thus the phrase DWORD PTR must be inserted in certain
 * circumstances.
 *
 * print_unary_op, print_unary_op_coerce, print_anop, print_anop_coerce,
 * print_binop, and print_binop2 do these insertions
 *)

let print_unary_op fmt o gop =
  if o.style=GAS then failwith "GAS unimplemented";
  begin match gop with
    Prjr (_,_,_) | Prjl (_,_,_) -> pp_print_string fmt "DWORD PTR "
  | _ -> ()
  end;
  print_genop fmt o gop
;;

let print_unary_op_coerce fmt o (gop,_ as cgop) =
  if o.style=GAS then failwith "GAS unimplemented";
  begin match gop with
    Prjr (_,_,_) | Prjl (_,_,_) -> pp_print_string fmt "DWORD PTR "
  | _ -> ()
  end;
  print_genop_coerce fmt o cgop
;;

let print_anop fmt o gop =
  if o.style=GAS then failwith "GAS unimplemented";
  begin match gop with
    Prjl (_,_,_) -> pp_print_string fmt "DWORD PTR "
  | _ -> ()
  end;
  print_genop fmt o gop
;;

let print_anop_coerce fmt o (gop,_ as cgop) =
  if o.style=GAS then failwith "GAS unimplemented";
  begin match gop with
    Prjl (_,_,_) -> pp_print_string fmt "DWORD PTR "
  | _ -> ()
  end;
  print_genop_coerce fmt o cgop
;;

let print_binop fmt o o1 o2 =
  let ptr1 =
    match o1 with
      Prjl(_,_,_) -> true
    | Prjr(_,_,_) -> (match o2 with Immed _ -> true | _ -> false)
    | _ -> false
  and ptr2 =
    match o2 with
      Prjl(_,_,_) -> true
    | Prjr(_,_,_) -> (match o1 with Immed _ -> true | _ -> false)
    | _ -> false in
  if ptr1 then pp_print_string fmt "DWORD PTR ";
  print_genop fmt o o1; pp_print_char fmt ',';
  if ptr2 then pp_print_string fmt "DWORD PTR ";
  print_genop fmt o o2
;;

let print_binop2 fmt o o1 (o2,_ as co2) =
  let ptr1 =
    match o1 with
      Prjl(_,_,_) -> true
    | Prjr(_,_,_) -> (match o2 with Immed _ -> true | _ -> false)
    | _ -> false
  and ptr2 =
    match o2 with
      Prjl(_,_,_) -> true
    | Prjr(_,_,_) -> (match o1 with Immed _ -> true | _ -> false)
    | _ -> false in
  if ptr1 then pp_print_string fmt "DWORD PTR ";
  print_genop fmt o o1; pp_print_char fmt ',';
  if ptr2 then pp_print_string fmt "DWORD PTR ";
  print_genop_coerce fmt o co2
;;

let print_binop3 fmt o (o1,_ as co1) (o2,_ as co2) =
  let ptr1 =
    match o1 with
      Prjl(_,_,_) -> true
    | Prjr(_,_,_) -> (match o2 with Immed _ -> true | _ -> false)
    | _ -> false
  and ptr2 =
    match o2 with
      Prjl(_,_,_) -> true
    | Prjr(_,_,_) -> (match o1 with Immed _ -> true | _ -> false)
    | _ -> false in
  if ptr1 then pp_print_string fmt "DWORD PTR ";
  print_genop_coerce fmt o co1; pp_print_char fmt ',';
  if ptr2 then pp_print_string fmt "DWORD PTR ";
  print_genop_coerce fmt o co2
;;

let print_binop_part fmt o o1 part1 o2 part2 =
  let ptr1 =
    match o1 with
      Prjl(_,_,_) -> true
    | Prjr(_,_,_) -> (match o2 with Immed _ -> true | _ -> false)
    | _ -> false
  and ptr2 =
    match o2 with
      Prjl(_,_,_) -> true
    | Prjr(_,_,_) -> (match o1 with Immed _ -> true | _ -> false)
    | _ -> false in
  if ptr1 then 
    (match part1 with
      RPe -> pp_print_string fmt "DWORD PTR "; print_genop fmt o o1;
    | RPl -> pp_print_string fmt "BYTE PTR ";  print_genop fmt o o1;
    | _   -> failwith "memory half words unimplemented");
  print_genop_part fmt o o1 part1; pp_print_char fmt ',';
  if ptr2 then 
    (match part2 with
      RPe -> pp_print_string fmt "DWORD PTR "; print_genop fmt o o2;
    | RPl -> pp_print_string fmt "BYTE PTR ";  print_genop fmt o o2;
    | _   -> failwith "memory half words unimplemented");
  print_genop_part fmt o o2 part2
;;

let print_cc fmt o cc =
  if o.style=GAS then failwith "GAS unimplemented";
  let s =
    match cc with
      Above -> "A" | AboveEq -> "AE" | Below -> "B" | BelowEq -> "BE"
    | Eq -> "E" | Greater -> "G" | GreaterEq -> "GE" | Less -> "L"
    | LessEq -> "LE" | NotEq -> "NE" | NotOverflow -> "NO" | NotSign -> "NS"
    | Overflow -> "O" | ParityEven -> "PE" | ParityOdd -> "PO" | Sign -> "S"
  in pp_print_string fmt s
;;

(* Floating Point *)

let print_fpreg fmt i =
  if i = 0 then pp_print_string fmt "ST"
  else pp_print_string fmt ("ST("^(string_of_int i)^")")

let print_fpregs fmt b i =
  if b then (print_fpreg fmt 0; pp_print_string fmt ","; print_fpreg fmt i)
  else (print_fpreg fmt i; pp_print_string fmt ","; print_fpreg fmt 0)

let print_fpmem fmt o scale g =
  match scale,g with
    Byte2,Reg Eax -> pp_print_string fmt "AX"
  | _ ->
      (match scale with
    	Byte2 -> pp_print_string fmt "WORD PTR "
      | Byte4 -> pp_print_string fmt "DWORD PTR "
      | Byte8 -> pp_print_string fmt "QWORD PTR "
      | _ -> invalid_arg "print_fpmem: bad scale");
      print_genop fmt o g

let implicit_st op =
  match op with
    Fcom | Fcomp | Fucom | Fucomp -> true
  | _ -> false

let print_fpargs fmt o implicit_st args =
  match args with
    FPstack i -> print_fpreg fmt i
  | FPstack2 (b,i) -> 
      if implicit_st then print_fpreg fmt i
      else print_fpregs fmt b i
  | FPgenop (s,g) -> print_fpmem fmt o s g

let print_fpnoargs fmt o op =
  if o.style=GAS then failwith "GAS unimplemented";
  let s =
    match op with
      F2xm1 -> "F2XM1"
    | Fabs -> "FABS"
    | Fchs -> "FCHS"
    | Fclex -> "FCLEX"
    | Fnclex -> "FNCLEX"
    | Fcompp -> "FCOMPP"
    | Fucompp -> "FUCOMPP"
    | Fcos -> "FCOS"
    | Fdecstp -> "FDECSTP"
    | Fincstp -> "FINCSTP"
    | Finit -> "FINIT"
    | Fninit  -> "FNINIT"
    | Fld1 -> "FLD1"
    | Fldz -> "FLDZ"
    | Fldpi -> "FLDPI"
    | Fldl2e -> "FLDL2E"
    | Fldl2t -> "FLDL2T"
    | Fldlg2 -> "FLDLG2"
    | Fldln2 -> "FLDLN2"
    | Fnop -> "FNOP"
    | Fpatan -> "FPATAN"
    | Fprem -> "FPREM"
    | Fprem1 -> "FPREM1"
    | Fptan -> "FPTAN"
    | Frndint -> "FRNDINT"
    | Fscale -> "FSCALE"
    | Fsin -> "FSIN"
    | Fsincos -> "FSINCOS"
    | Fsqrt -> "FSQRT"
    | Ftst -> "FTST"
    | Fwait -> "FWAIT"
    | Fxam -> "FXAM"
    | Fxtract -> "FXTRACT"
    | Fyl2x -> "FYL2X"
    | Fyl2xp1 -> "FYL2XP1"
  in pp_print_string fmt s

let print_fpsomeargs fmt o op =
  if o.style = GAS then failwith "GAS unimplemented";
  let s = 
    match op with
      Fadd -> "FADD"
    | Fcom -> "FCOM"
    | Fcomp -> "FCOMP"
    | Fdiv -> "FDIV"
    | Fdivr -> "FDIVR"
    | Fmul -> "FMUL"
    | Fsub -> "FSUB"
    | Fsubr -> "FSUBR"
    | Fucom -> "FUCOM"
    | Fxch -> "FXCH"
    | Fiadd -> "FIADD"
    | Ficom -> "FICOM"
    | Ficomp -> "FICOMP"
    | Fidiv -> "FIDIV"
    | Fidivr -> "FIDIVR"
    | Fimul -> "FIMUL"
    | Fisub -> "FISUB"
    | Fisubr -> "FISUBR"
    | Faddp -> "FADDP"
    | Fdivp -> "FDIVP"
    | Fdivrp -> "FDIVRP"
    | Fmulp -> "FMULP"
    | Fsubp -> "FSUBP"
    | Fsubrp -> "FSUBRP"
    | Fucomp -> "FUCOMP"
    | Fst -> "FST"
    | Fstp -> "FSTP"
    | Fist -> "FIST"
    | Fistp -> "FISTP"
    | Fld -> "FLD"
    | Fild -> "FILD"
    | Ffree -> "FFREE"
    | Fcomi -> "FCOMI"
    | Fcomip -> "FCOMIP"
    | Fucomi -> "FUCOMI"
    | Fucomip -> "FUCOMIP"
    | Fstsw -> "FSTSW"
    | Fnstsw -> "FNSTSW"
  in
  pp_print_string fmt s
	  
(* Instructions *)

let print_arithbin fmt o x =
  if o.style=GAS then failwith "GAS unimplemented";
  let s = 
    match x with
      Adc -> "ADC" | Add -> "ADD" | And -> "AND" | Imul2 -> "IMUL"
    | Or -> "OR" | Sbb -> "SBB" | Sub -> "SUB" | Xor -> "XOR"
  in pp_print_string fmt s
;;

let print_arithun fmt o x =
  if o.style=GAS then failwith "GAS unimplemented";
  let s = 
    match x with
      Dec -> "DEC" | Inc -> "INC" | Neg -> "NEG" | Not -> "NOT"
  in pp_print_string fmt s
;;

let print_arithmd fmt o x =
  if o.style=GAS then failwith "GAS unimplemented";
  let s = 
    match x with
      Div -> "DIV" | Idiv -> "IDIV" | Imul1 -> "IMUL" | Mul -> "MUL"
  in pp_print_string fmt s
;;

let print_arithsr fmt o x =
  if o.style=GAS then failwith "GAS unimplemented";
  let s = 
    match x with
      Rcl -> "RCL" | Rcr -> "RCR" | Rol -> "ROL" | Ror -> "ROR" | Sal -> "SAL"
    | Sar -> "SAR" | Shl -> "SHL" | Shr -> "SHR"
  in pp_print_string fmt s
;;

let print_conv fmt o c =
  if o.style=GAS then failwith "GAS unimplemented";
  match c with
    Cbw -> pp_print_string fmt "\tCBW"
  | Cdq -> pp_print_string fmt "\tCDQ"
  | Cwd -> pp_print_string fmt "\tCWD"
  | Cwde -> pp_print_string fmt "\tCWDE"
;;

let scale_to_reg_part i =
  if i =$ i32_1 then RPl
  else if i =$ i32_2 then RPx
  else if i =$ i32_4 then RPe
  else invalid_arg "Talpp.scale_to_reg_part"
;;

let print_array_arg fmt o genop =
  let print_opt opt = 
    match opt with
      None -> ()
    | Some (s,r) -> 
	pp_print_string fmt "+"; 
	print_scale fmt s;
	pp_print_string fmt "*";
       	print_reg fmt o r
  in
  match genop with
    Prjr (rc,n,opt) ->
      print_reg_coerce fmt o rc; 
      print_opt opt;
      fprintf fmt "+%s" (string_of_int32 n)
  | Prjl (lc,n,opt) ->
      print_label_coerce fmt o lc; 
      print_opt opt;
      fprintf fmt "+%s" (string_of_int32 n)
  | _ -> invalid_arg "Talpp.print_array_arg"
;;

let print_mallocarg fmt o ma =
  let rec aux ma =
    match ma with
      Mprod mas -> 
      	pp_print_char fmt '['; pp_open_hovbox fmt 0;
      	sepc fmt aux mas;
      	pp_close_box fmt (); pp_print_char fmt ']'
    | Mbytes s -> pp_print_char fmt ':'; print_scale fmt s
    | Mbytearray (scale,size) ->
	fprintf fmt "array(%s,B" (string_of_int32 size); print_scale fmt scale;
	fprintf fmt ")" in
  pp_print_char fmt '<'; aux ma; pp_print_char fmt '>'
;;

let print_proofarg fmt o pf =
  let print_rule (rule, cs) =
    id_prn fmt rule;
    pp_print_char fmt '<';
    sepi fmt 1 "," (print_con fmt o) cs;
    pp_print_char fmt '>' in
  pp_open_hvbox fmt 0;
  sepi fmt 1 "" print_rule pf;
  pp_close_box fmt ()

let rec print_instruction fmt o i =
  let pp_string = pp_print_string fmt in
  let pp_char = pp_print_char fmt in
  if o.style=GAS then failwith "GAS unimplemented";
  (match i with
    ArithBin (x,o1,o2) ->
      pp_char '\t'; print_arithbin fmt o x; pp_char '\t';
      print_binop fmt o o1 o2
  | ArithUn (x,op) ->
      pp_char '\t'; print_arithun fmt o x; pp_char '\t';
      print_unary_op fmt o op
  | ArithMD (x,op) ->
      pp_char '\t'; print_arithmd fmt o x; pp_char '\t';
      print_unary_op fmt o op
  | ArithSR (x,op,io) ->
      pp_char '\t'; print_arithsr fmt o x; pp_char '\t';
      print_unary_op fmt o op; pp_char ',';
      (match io with None -> pp_string "CL"
      | Some i -> fprintf fmt "%s" (string_of_int32 i))
  | Bswap r -> pp_string "\tBSWAP\t"; print_reg fmt o r
  | Call op -> pp_string "\tCALL\t"; print_unary_op_coerce fmt o op
  | Clc -> pp_string "\tCLC"
  | Cmc -> pp_string "\tCMC"
  | Cmovcc (cc,r,op) ->
      pp_string "\tCMOV"; print_cc fmt o cc; pp_char '\t';
      print_reg fmt o r; pp_char ','; print_anop_coerce fmt o op
  | Cmp (o1,o2) -> pp_string "\tCMP\t"; print_binop3 fmt o o1 o2
  | Conv c -> print_conv fmt o c
  | Imul3 (r,op,i) ->
      pp_string "\tIMUL\t"; print_reg fmt o r;
      pp_char ','; print_anop fmt o op;
      pp_char ','; fprintf fmt "%s" (string_of_int32 i)
  | Int n ->
      pp_string "\tINT\t"; fprintf fmt "%s" (string_of_int8 n)
  | Into -> pp_string "\tINTO"
  | Jcc (cc,l,iopt) ->
      pp_string "\tJ"; print_cc fmt o cc; pp_char '\t';
      print_label_coerce fmt o l;
      begin
      	match iopt with
	  None -> ()
      	| Some instructions ->
	    pp_string "\tvirtual<";
	    sepi fmt 2 " " (print_instruction fmt o) instructions;
	    pp_string ">"
      end
  | Jecxz (l,iopt) -> 
      pp_string "\tJECXZ\t"; 
      print_label_coerce fmt o l;
      begin
      	match iopt with
	  None -> ()
      	| Some instructions ->
	    pp_string "\tvirtual<";
	    sepi fmt 2 " " (print_instruction fmt o) instructions;
	    pp_string ">"
      end
  | Jmp op -> pp_string "\tJMP\t"; print_anop_coerce fmt o op
  | Lahf -> pp_string "\tLAHF"
  | Lea (r,op) ->
      pp_string "\tLEA\t"; print_reg fmt o r; pp_char ',';
      print_anop fmt o op
  | Loopd (l,bo) ->
      pp_string "\tLOOP";
      (match bo with
 	None -> ()
      | Some false -> pp_string "NE"
      | Some true -> pp_char 'E');
      pp_string "D\t"; print_label_coerce fmt o l
  | Mov (o1,o2) ->
      pp_string "\tMOV\t"; 
      print_binop2 fmt o o1 o2
  | Movpart (zx,o1,rp1,o2,rp2) when rp1 <> rp2 ->
      if zx then pp_string "\tMOVZX\t" else pp_string "\tMOVSX\t";
      print_binop_part fmt o o1 rp1 o2 rp2 
  | Movpart (_,o1,rp1,o2,rp2) -> (* rp1=rp2 *)
      pp_string "\tMOV\t";
      print_binop_part fmt o o1 rp1 o2 rp2
  | Nop -> pp_string "\tNOP"
  | Pop op -> pp_string "\tPOP\t"; print_unary_op fmt o op
  | Popad -> pp_string "\tPOPAD"
  | Popfd -> pp_string "\tPOPFD"
  | Push op -> pp_string "\tPUSH\t"; print_unary_op_coerce fmt o op
  | Pushad -> pp_string "\tPUSHAD"
  | Pushfd -> pp_string "\tPUSHFD"
  | Rdtsc -> pp_string "\tRDTSC"
  | Retn no ->
      pp_string "\tRETN";
      (match no with None -> ()
      | Some n -> fprintf fmt "\t%s" (string_of_int32 n))
  | Sahf -> pp_string "\tSAHF"
  | Setcc (cc,op) ->
      pp_string "\tSET"; print_cc fmt o cc; pp_char '\t';
      (match op with Reg r -> print_reg_part fmt o r RPl
       | _ -> failwith "bad args to SETcc")
  | Shld (op,r,io) -> 
      pp_string "\tSHLD\t"; print_anop fmt o op;
      pp_char ','; print_reg fmt o r; pp_char ',';
      (match io with None -> pp_string "CL"
      | Some c -> fprintf fmt "%s" (string_of_int32 c))
  | Shrd (op,r,io) -> 
      pp_string "\tSHRD\t"; print_anop fmt o op;
      pp_char ','; print_reg fmt o r; pp_char ',';
      (match io with None -> pp_string "CL"
      | Some c -> fprintf fmt "%s" (string_of_int32 c))
  | Stc -> pp_string "\tSTC"
  | Test (o1,o2) -> pp_string "\tTEST\t"; print_binop fmt o o1 o2
  | Xchg (op,r) ->
      pp_string "\tXCHG\t"; print_anop fmt o op;
      pp_char ','; print_reg fmt o r
(* Abstract ops *)
  | Coerce gc ->
      pp_string "\tCOERCE\t"; print_genop_coerce fmt o gc
  | CoerceName nc ->
      let print_name fmt o name =
	pp_string "name(";
 	id_prn fmt name;
        pp_char ')' in
      pp_string "\tCOERCE\t"; 
      print_coerce fmt print_name o nc
  | Comment s ->
      pp_string "; "; pp_string s
  | Fallthru cons ->
      pp_string "\tFALLTHRU";
      if cons<>[] then begin 
        pp_string "\t<";
        pp_open_hovbox fmt 0; sepc fmt (print_con fmt o) cons;
        pp_close_box fmt (); pp_char '>'
      end
  | Malloc (x,i,margopt) ->
      pp_string "\tMALLOC\t"; 
      pp_string (id_to_string x); pp_char ',';
      fprintf fmt "%s" (string_of_int32 i);
      (match margopt with
	None -> () | Some marg -> (pp_char ','; print_mallocarg fmt o marg));
  | Proof pf ->
      pp_string "\tPROOF\t";
      pp_char '<';
      print_proofarg fmt o pf;
      pp_char '>'
  | Unpack (v,r,op) ->
      pp_string "\tUNPACK\t"; id_prn fmt v; pp_char ',';
      print_reg fmt o r; pp_char ','; print_anop_coerce fmt o op
  | Sunpack (v,g) ->
      pp_string "\tSUNPACK\t"; id_prn fmt v; pp_char ',';
      print_genop fmt o g
  | Nameobj (x,g) ->
      pp_string "\tNAMEOBJ\t"; id_prn fmt x; pp_char ','; 
      print_genop fmt o g
  | ForgetUnique x ->
      pp_string "\tFORGETUNIQUE\t"; id_prn fmt x;
  | RemoveName x ->
      pp_string "\tREMOVENAME\t"; id_prn fmt x;
(* Floating Point *)
  | FPnoargs op -> pp_char '\t'; print_fpnoargs fmt o op
  | FPsomeargs (op,args) -> 
      pp_char '\t';
      print_fpsomeargs fmt o op;
      pp_char '\t';
      print_fpargs fmt o (implicit_st op) args
(* Cyclone *)
  | CgStart (x,con) ->
      pp_string "\tCGSTART\t<"; id_prn fmt x; 
      pp_char ',';  (print_con fmt o) con;
      pp_string ">"
  | CgDump(reg1, id1, reg2, id2) ->
      pp_string "\tCGDUMP\t"; print_reg fmt o reg1;
      pp_char ','; id_prn fmt id1;
      pp_char ','; print_reg fmt o reg2;
      pp_char ','; id_prn fmt id2
  | CgHole(reg, id1, id2) ->
      pp_string "\tCGHOLE\t"; print_reg fmt o reg;
      pp_char ','; id_prn fmt id1;
      pp_char ','; id_prn fmt id2
  | CgHoleJmp(l,lc) ->
      pp_string "\tCGHOLEJMP";
      pp_char '\t'; id_prn fmt l;
      pp_char ','; print_label_coerce fmt o lc
  | CgHoleJcc(cc,l,lc,iopt) ->
      pp_string "\tCGHOLEJCC "; print_cc fmt o cc;
      pp_char ','; pp_char '\t'; id_prn fmt l;
      pp_char ','; print_label_coerce fmt o lc;
      begin
	match iopt with
	  None -> ()
	| Some instructions ->
	    pp_string "\tvirtual<";
	    sepi fmt 2 " " (print_instruction fmt o) instructions;
	    pp_string ">"
      end
  | CgFill(reg1, reg2, id1, id2, reg3) ->
      pp_string "\tCGFILL\t"; print_reg fmt o reg1;
      pp_char ','; print_reg fmt o reg2;
      pp_char ','; id_prn fmt id1;
      pp_char ','; id_prn fmt id2;
      pp_char ','; print_reg fmt o reg3
  | CgFillJmp(r1,r2,l1a,l1b,r3,l2a,l2b) ->
      pp_string "\tCGFILLJMP\t"; print_reg fmt o r1;
      pp_char ','; print_reg fmt o r2;
      pp_char ','; id_prn fmt l1a;
      pp_char ','; id_prn fmt l1b;
      pp_char ','; print_reg fmt o r3;
      pp_char ','; id_prn fmt l2a;
      pp_char ','; id_prn fmt l2b
  | CgFillJcc(r1,r2,l1a,l1b,r3,l2a,l2b) ->
      pp_string "\tCGFILLJCC\t"; print_reg fmt o r1;
      pp_char ','; print_reg fmt o r2;
      pp_char ','; id_prn fmt l1a;
      pp_char ','; id_prn fmt l1b;
      pp_char ','; print_reg fmt o r3;
      pp_char ','; id_prn fmt l2a;
      pp_char ','; id_prn fmt l2b
  | CgForget(id1,id2) ->
      pp_string "\tCGFORGET\t"; id_prn fmt id1;
      pp_char ','; id_prn fmt id2
  | CgEnd (reg) ->
      pp_string "\tCGEND\t";
      print_reg fmt o reg
(* End Cyclone *)
(* LX *)
  | Letprod (is,c) ->  
       pp_print_string fmt "\tLETPROD\t"; pp_print_string fmt "*[";
       (match is with 
	  (hd::rest) -> id_prn fmt hd;
	     List.iter (fun i -> pp_print_char fmt ','; id_prn fmt i) rest
	| [] -> ());
       pp_print_string fmt "],";
       print_con fmt o c
  | Letroll (i,c) ->
       pp_print_string fmt "\tLETROLL\t";
       id_prn fmt i; pp_print_char fmt ','; print_con fmt o c
  | Vcase (int,c,id,gc) ->
       pp_print_string fmt "\tVCASE\t";
       fprintf fmt "%s" (string_of_int32 int); pp_print_char fmt ','; 
       id_prn fmt id; pp_print_char fmt ',';
       print_con fmt o c; pp_print_char fmt ','; print_genop_coerce fmt o gc

(* end LX *)
	)
;;

let print_code_block fmt o (l,lc,is : code_block) =
  id_prn fmt l;
  pp_print_char fmt ':';
  pp_print_cut fmt ();
  (match lc with
    Some lc -> 
      if o.cons then begin
    	pp_print_string fmt "LABELTYPE <";
    	print_con fmt o lc;
    	pp_print_char fmt '>'; 
    	pp_print_cut fmt ()
      end;
  | None -> ());
  for i = 0 to (Array.length is)-1 do
    print_instruction fmt o is.(i);
    pp_print_cut fmt ()
  done;
;;

(* Cyclone *)
let print_template fmt o (lengthlabel,con,cbs : template) =
  pp_print_string fmt "TEMPLATE_START ";
  id_prn fmt lengthlabel;
  pp_print_string fmt ",<";
  print_con fmt o con;
  pp_print_char fmt '>'; 
  pp_print_cut fmt ();
  List.iter (print_code_block fmt o) cbs;
  pp_print_string fmt "TEMPLATE_END ";
  pp_print_cut fmt ();
  pp_print_cut fmt ();
;;
(* End Cyclone *)

(*** Data ***)

let rec print_data_item fmt o di =
  match di with
    Dlabel cl ->
      pp_print_string fmt "\tDD\t"; print_label_coerce fmt o cl
  | Dbytes (s0) ->
      (* FMS: rewrote to print out dbytes composed of usual characters as
	 "adsfsaf" instead of 012,043,304,203, ... 
	 No need to break after 32 anymore since we don't use MASM *)
      let valid_str_elt c = 
	let  cd = Char.code c in
	let max = Char.code '~' in 
	(cd>= 032 && cd<=max && cd<>034)   
      in
      let s0_len = String.length s0 in
      let s0_valid = 
	let rec loop i = 
	  if(i<0) then true
	  else if valid_str_elt s0.[i] then loop (i-1)
	  else false
	in
	loop (s0_len - 1)
      in
      pp_print_string fmt "\tDB\t";
      if s0_valid then
	(pp_print_string fmt "\034";
	 pp_print_string fmt s0;
	 pp_print_string fmt "\034")
      else 
	for i = 0 to s0_len - 1 do
	  if i>0 then pp_print_char fmt ',';
	  pp_print_int fmt (Char.code s0.[i])
	done
  | D2bytes i -> fprintf fmt "\tDW\t%s" (string_of_int16 i)
  | D4bytes ci ->
      let aux fmt o i = fprintf fmt "%s" (string_of_int32 i) in
      pp_print_string fmt "\tDD\t"; print_coerce fmt aux o ci
  | Drep (ri,s) -> 
       pp_print_string fmt "\tDREP\t";
       (match ri with 
	  RCon c -> pp_print_string fmt "TYPE\t"; print_con fmt o c 
	| RKind k -> pp_print_string fmt "KIND\t"; print_kind fmt o k
	| RLabel i -> pp_print_string fmt "LABEL\t"; id_prn fmt i)
  | Dfloat32 f -> fprintf fmt "\tREAL4\t0x%s" (f32_to_hex f)
  | Dfloat64 f -> fprintf fmt "\tREAL8\t0x%s" (f64_to_hex f)
  | Djunk -> pp_print_string fmt "\tDD\t?"
  | Dup -> pp_print_string fmt "\tTAL_STRUCT"
  | Ddown -> pp_print_string fmt "\tTAL_ENDS"
;;

let print_data_block fmt o (l,align,lc,(dis,clist) : data_block) =
  id_prn fmt l;
  pp_print_char fmt ':';
  pp_print_cut fmt ();
  if align != i32_4 then 
    begin
      fprintf fmt "ALIGN\t%s" (string_of_int32 align);
      pp_print_cut fmt ();
    end;
  if o.cons then begin
    match lc with
      None -> ()
    | Some c ->
    	pp_print_string fmt "LABELTYPE <";
    	print_con fmt o c;
    	pp_print_char fmt '>'; 
    	pp_print_cut fmt ()
  end;
  if clist<>[] then begin
    pp_print_string fmt "COERCE\t";
    print_coerce fmt (fun fmt _ _ -> pp_print_string fmt "?") o ((),clist);
    pp_print_cut fmt ()
  end;
  let aux di = print_data_item fmt o di; pp_print_cut fmt () in
  List.iter aux dis
;;

(*** Compilation Units ***)

let print_tal_int fmt o name m =
  pp_open_vbox fmt 0;
  pp_print_string fmt ("; TAL INTERFACE "^name); pp_print_cut fmt ();
  pp_print_string fmt "; This file was generated by TALC"; pp_print_cut fmt ();
  for i = 0 to (Array.length m.int_abbrevs) - 1 do
    let (l,c) = m.int_abbrevs.(i) in
    pp_print_string fmt "\tTYPE\t";
    pp_print_char fmt '<'; pp_open_hovbox fmt 0; id_prn fmt l;
    pp_print_break fmt 1 2; print_string "= ";
    print_con fmt o c; pp_close_box fmt (); pp_print_char fmt '>';
    pp_print_cut fmt ()
  done;
  if (Array.length m.int_abbrevs)>0 then pp_print_cut fmt ();
  for i = 0 to (Array.length m.int_cons) - 1 do
    let (l,k,cd) = m.int_cons.(i) in
    pp_print_string fmt "\tTYPE\t";
    pp_print_char fmt '<'; pp_open_hovbox fmt 0; id_prn fmt l;
    pp_print_break fmt 1 2; print_char ':'; print_kind fmt o k;
    (match cd with
      AbsCon -> ()
    | BoundCon c ->
	pp_print_break fmt 1 2; print_string "<= "; print_con fmt o c
    | ConcCon c ->
 	pp_print_break fmt 1 2; print_string "= "; print_con fmt o c);
    pp_close_box fmt (); pp_print_char fmt '>';
    pp_print_cut fmt ()
  done;
  if (Array.length m.int_cons)>0 then pp_print_cut fmt ();
  for i = 0 to (Array.length m.int_vals) - 1 do
    let (l,c) = m.int_vals.(i) in
    pp_print_string fmt "\tVAL\t"; id_prn fmt l;
    pp_print_string fmt ",<"; print_con fmt o c; pp_print_char fmt '>';
    pp_print_cut fmt ()
  done;
  pp_close_box fmt ()
;;

let print_tal_int_type fmt o {it_cons=cons; it_vals=vals} =
  let prn_lkco (l,k,cd) =
    pp_open_hovbox fmt 0; id_prn fmt l; pp_print_break fmt 1 2;
    print_ckind fmt o k;
    (if o.cons then
      match cd with
      	AbsCon -> ()
      |	BoundCon c ->
	  pp_print_break fmt 1 2; pp_print_string fmt "<="; print_con fmt o c
      | ConcCon c ->
	  pp_print_break fmt 1 2; pp_print_char fmt '='; print_con fmt o c);
    pp_close_box fmt () in
  let prn_lc (l,c) =
    pp_open_hovbox fmt 0; id_prn fmt l; pp_print_break fmt 1 2;
    print_ccon fmt o c; pp_close_box fmt () in
  pp_print_char fmt '{'; pp_open_hovbox fmt 0;
  pp_open_hovbox fmt 0; sepc fmt prn_lkco cons; pp_close_box fmt ();
  pp_print_char fmt ';'; pp_print_break fmt 1 0; pp_open_hovbox fmt 0;
  sepc fmt prn_lc vals; pp_close_box fmt (); pp_close_box fmt ();
  pp_print_char fmt '}'
;;

let print_tal_pre_mod fmt o name modl =
  let m = modl.pre_imp in
  pp_open_vbox fmt 0;
  pp_print_string fmt ("; TAL IMPLEMENTATION "^name); pp_print_cut fmt ();
  pp_print_string fmt "; This file was generated by TALC"; pp_print_cut fmt ();
  pp_print_string fmt "\tINCLUDE\tTAL.INC"; pp_print_cut fmt ();
(* Cyclone *)
  if (Array.length m.templates)>0 then
    begin
      pp_print_string fmt "\tINCLUDE\tCYCLONE.INC";
      pp_print_cut fmt ()
    end;
(* End Cyclone *)
  pp_print_string fmt "\t_begin_TAL"; pp_print_cut fmt (); pp_print_cut fmt ();
(* Cyclone *)
  if (Array.length m.templates)>0 then
    begin
      pp_print_string fmt "\t_begin_CYCLONE";
      pp_print_cut fmt ()
    end;
  pp_print_cut fmt ();
(* End Cyclone *)
  for i = 0 to (Array.length modl.import_refs) - 1 do
    let name =
      match modl.import_refs.(i) with
	Int_filename s -> s
      |	Int_data (s,_) -> s in
    pp_print_string fmt ("\tTAL_IMPORT\t"^name); pp_print_cut fmt ()
  done;
  if (Array.length modl.import_refs)>0 then pp_print_cut fmt ();
  for i = 0 to (Array.length modl.export_refs) - 1 do
    let name =
      match modl.export_refs.(i) with
	Int_filename s -> s
      |	Int_data (s,_) -> s in
    pp_print_string fmt ("\tTAL_EXPORT\t"^name); pp_print_cut fmt ()
  done;
  if (Array.length modl.export_refs)>0 then pp_print_cut fmt ();
  for i = 0 to (Array.length m.imp_abbrevs) - 1 do
    let (l,c) = m.imp_abbrevs.(i) in
    pp_print_string fmt "\tTYPE\t";
    pp_print_char fmt '<'; pp_open_hovbox fmt 0; id_prn fmt l;
    pp_print_break fmt 1 2; print_string "= ";
    print_con fmt o c; pp_close_box fmt (); pp_print_char fmt '>';
    pp_print_cut fmt ()
  done;
  if (Array.length m.imp_abbrevs)>0 then pp_print_cut fmt ();
  for i = 0 to (Array.length m.con_blocks) - 1 do
    let (l,k,c) = m.con_blocks.(i) in
    pp_print_string fmt "\tTYPE\t";
    pp_print_char fmt '<'; pp_open_hovbox fmt 0; id_prn fmt l;
    pp_print_break fmt 1 2; print_char ':'; print_kind fmt o k;
    pp_print_break fmt 1 2; print_string "= ";
    print_con fmt o c; pp_close_box fmt (); pp_print_char fmt '>';
    pp_print_cut fmt ()
  done;
  if (Array.length m.con_blocks)>0 then pp_print_cut fmt ();
  pp_print_string fmt "\tCODE"; pp_print_cut fmt (); pp_print_cut fmt ();
  for i = 0 to (Array.length m.code_blocks) - 1 do
    print_code_block fmt o m.code_blocks.(i)
  done;
  if (Array.length m.code_blocks)>0 then pp_print_cut fmt ();
(* Cyclone *)
  for i = 0 to (Array.length m.templates) - 1 do
    print_template fmt o m.templates.(i)
  done;
  if (Array.length m.templates)>0 then pp_print_cut fmt ();
(* End Cyclone *)
  pp_print_string fmt "\tDATA"; pp_print_cut fmt (); pp_print_cut fmt ();
  for i = 0 to (Array.length m.data_blocks) - 1 do
    print_data_block fmt o m.data_blocks.(i)
  done;
  if (Array.length m.data_blocks)>0 then pp_print_cut fmt ();
  pp_print_string fmt "\t_end_TAL"; pp_print_cut fmt ();
  pp_print_string fmt "\tEND"; pp_print_cut fmt ();
  pp_close_box fmt ()
;;

open Talctxt;;

let print_tal_loc fmt o l =
  match l with
    Loctop -> pp_print_string fmt "top level"
  | Lockind l -> pp_print_string fmt "kind abbreviation "; id_prn fmt l
  | Loccon l -> pp_print_string fmt "abstract type "; id_prn fmt l
  | Locval l -> pp_print_string fmt "imported label "; id_prn fmt l
  | Loccb l -> pp_print_string fmt "concrete type "; id_prn fmt l
  | Locc (l,i) ->
      pp_print_string fmt "code block "; id_prn fmt l;
      pp_print_string fmt "("; pp_print_int fmt i; pp_print_string fmt ")"
  | Locd (l,i) ->
      pp_print_string fmt "data block "; id_prn fmt l;
      pp_print_string fmt "("; pp_print_int fmt i; pp_print_string fmt ")"
;;

(******************************************************************************)
(* Special purpose print routine to generate better/smaller error messages. *)

exception Found

(* Break the sharing -- where equal set hash to ~1. *)
let highlight_similarities c_ref c =
  let dc rc = { c with rcon = rc; } in
  let rec h c_ref c =
    let rec h_lists f l l' =
      match l,l' with
	c::tl,c'::tl' -> (f c c') :: (h_lists f tl tl')
      | _,_ -> l' in
    if c == c_ref || c = c_ref then  { c with hash = -1; } else
    match c_ref.rcon, c.rcon with
      Clam(_,_,c), Clam(x1,x2,c') -> dc (Clam (x1,x2,h c c'))
    | Capp (c1,c2),Capp (c1',c2') -> dc (Capp (h c1 c1', h c2 c2'))
    | Ctuple cs, Ctuple cs' -> dc (Ctuple (hs cs cs'))
    | Cproj(_,c), Cproj(x,c') -> dc (Cproj(x,h c c'))
    | Cinj(_,c,_),Cinj(x1,c',x2) -> dc (Cinj(x1,h c c',x2))
    | Ccase(c,_,cs),Ccase(c',x,cs') -> dc (Ccase(h c c',x,hs cs cs'))
    | Cfold(_,c),Cfold(x,c') -> dc (Cfold(x,h c c'))
    | Cpr (_,ls), Cpr(x,ls') -> 
	dc (Cpr (x,h_lists 
		   (fun (_,_,_,_,_,c) (x1,x2,x3,x4,x5,c') -> 
		     (x1,x2,x3,x4,x5,h c c')) 
		   ls ls'))
    | Crec ls,Crec ls' -> 
	dc (Crec (h_lists (fun (_,_,c) (x1,x2,c') -> (x1,x2,h c c')) ls ls'))
    | Cforall(_,_,c), Cforall(x1,x2,c') -> dc (Cforall(x1,x2,h c c'))
    | Cexist (_,_,c1,c2),Cexist(x1,x2,c1',c2') ->
	dc (Cexist(x1,x2,h c1 c1', h c2 c2'))
    | Ccode c, Ccode c' -> dc (Ccode (h c c'))
    | Cms ms, Cms ms' -> dc (Cms (h_ms ms ms'))
    | Cmsjoin (c1,c2) , Cmsjoin (c1',c2') -> dc (Cmsjoin (h c1 c1', h c2 c2'))
    | Chptr (_,co,cvo), Chptr(x,co',cvo') ->
	dc (Chptr (x,ho co co',hvo cvo cvo'))
    | Cfield (c,_), Cfield (c',x) -> dc (Cfield (h c c',x))
    | Cprod cs, Cprod cs' -> dc (Cprod (hs cs cs'))
    | Csum cs, Csum cs' -> dc (Csum (hs cs cs'))
    | Carray (c1,c2), Carray (c1',c2') -> dc (Carray (h c1 c1', h c2 c2'))
    | Csing c, Csing c' -> dc (Csing (h c c'))
    | Csptr c, Csptr c' -> dc (Csptr (h c c'))
    | Ccons (c1,c2), Ccons(c1',c2') -> dc (Ccons (h c1 c1', h c2 c2'))
    | Cappend (c1,c2), Cappend (c1',c2') -> dc (Cappend (h c1 c1', h c2 c2'))
    | Clog (_,cs), Clog (x,cs') -> dc (Clog (x, hs cs cs'))
    | Cif (c1,c2), Cif (c1',c2') -> dc (Cif (h c1 c1', h c2 c2'))
    | Cname c, Cname c' -> dc (Cname (h c c'))
    | Ccap d, Ccap d' -> 
	let f i (a,c) d' =
	  try let (a',c') = Dict.lookup d' i in 
	  Dict.insert d' i (a',c')
	  with _ -> d'
	in
	dc (Ccap (Dict.fold_dict f d d'))
    | Cjoin cs, Cjoin cs' -> dc (Cjoin (hs cs cs'))
    | Ctagof c, Ctagof c' -> dc (Ctagof (h c c'))
    | Ctmpl (c,co,ics1,ics2),
      Ctmpl (c',co',ics1',ics2') -> 
	dc (Ctmpl (h c c', ho co co',
		   h_id_cons ics1 ics1', h_id_cons ics2 ics2'))
    | Ctrgn (c,co,iccs),
	Ctrgn (c',co',iccs') ->
	  let hicss iccs iccs' =  
	    h_lists (fun (_,ics1,ics2) (x,ics1',ics2') -> 
	      (x,h_id_cons ics1 ics1', h_id_cons ics2 ics2')) iccs iccs' in
	  dc (Ctrgn (h c c', ho co co', hicss iccs iccs'))
    | Csubst (c,es), Csubst(c',es') -> dc (Csubst (h c c', h_esubst es es'))
    | Cr ri, Cr ri' -> dc (Cr (h_rep_item ri ri'))
    | _,_ -> c
  and ho co co' = 
    match co,co' with 
    | Some c, Some c' -> Some (h c c')
    | _,_ -> co'
  and hvo vco vco' =
    match vco,vco' with
    | Some (c,_), Some(c',v) -> Some(h c c',v)
    | _,_ -> vco'
  and hs cs cs' = 
    match cs,cs' with
      c::tl,c'::tl' -> (h c c') :: (hs tl tl') 
    | _,_ -> cs'
  and h_id_cons ics ics' = 
    match ics, ics' with
      (_,c)::tl, (x,c')::tl' -> (x,h c c')::(h_id_cons tl tl')
    | _,_ -> ics'
  and h_esubst es es' =
    match es,es' with
    | Es (_,c), Es(x,c') -> Es(x,h c c')
    | Eo (es1,es2), Eo (es1',es2') -> Eo (h_esubst es1 es1', h_esubst es2 es2')
    | _ -> es'
  and h_rep_item ri ri' =
    match ri,ri' with
      RCon c, RCon c' -> RCon (h c c')
    | _ -> ri'
  and h_ms ms ms' = 
    let ms' = ms_set_cap ms' (h    (ms_get_cap ms) (ms_get_cap ms')) in
    let ms' = ms_set_cc  ms' (h_cc (ms_get_cc  ms) (ms_get_cc  ms')) in
    let f_reg r c ms' =
      try 
	let c' = ms_get_reg ms' r in
	ms_set_reg ms' r (h c c')
      with _ -> ms' in
    let ms' = ms_fold_reg f_reg ms ms' in
    ms'
  and h_cc cc cc' = 
    match cc,cc' with
    | CCcmp(c1,c2), CCcmp(c1',c2') -> CCcmp(h c1 c1', h c2 c2')
    | CCtest(c1,c2), CCtest(c1',c2') -> CCtest(h c1 c1', h c2 c2')
    | _,_ -> cc'
  in
  h c_ref c
  ;;

(******************************************************************************)
let rec print_verify_error fmt o ve =
  match ve with
    Undefined_label l ->
      pp_print_string fmt "label "; id_prn fmt l;
      pp_print_string fmt " undefined"
  | Undefined_var v ->
      pp_print_string fmt "var "; id_prn fmt v;
      pp_print_string fmt " undefined"
  | Undefined_reg r ->
      pp_print_string fmt "reg "; print_reg fmt o r;
      pp_print_string fmt " undefined"
  | Redefined_label l ->
      pp_print_string fmt "label "; id_prn fmt l;
      pp_print_string fmt " redefined"
  | Kindleq (k1,k2) ->
      pp_open_hovbox fmt 0; print_kind fmt o k1; pp_print_space fmt ();
      pp_print_string fmt "! <="; pp_print_space fmt (); print_kind fmt o k2;
      pp_close_box fmt ()
  | Kindeq (k1,k2) ->
      pp_open_hovbox fmt 0; print_kind fmt o k1; pp_print_space fmt ();
      pp_print_string fmt "!="; pp_print_space fmt (); print_kind fmt o k2;
      pp_close_box fmt ()
  | Kindmeet (k1,k2) ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "no kind meet:"; pp_print_break fmt 1 2;
      print_kind fmt o k1; pp_print_char fmt ','; pp_print_break fmt 1 2;
      print_kind fmt o k2;
      pp_close_box fmt ()
  | Kindjoin (k1,k2) ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "no kind join:"; pp_print_break fmt 1 2;
      print_kind fmt o k1; pp_print_char fmt ','; pp_print_break fmt 1 2;
      print_kind fmt o k2;
      pp_close_box fmt ()
  | Conwf (c,s) ->
      pp_print_string fmt "malformed constructor:"; pp_print_break fmt 1 2;
      pp_print_string fmt s; pp_print_char fmt ':'; pp_print_break fmt 1 2;
      print_con fmt o c
  | Neqcon (c1,c2) ->
      pp_open_hovbox fmt 0; print_con fmt o c1; pp_print_space fmt ();
      pp_print_string fmt "!="; pp_print_space fmt (); print_con fmt o c2;
      pp_close_box fmt ()
  | Nleqcon (c1,c2) ->  
      pp_open_vbox fmt 0;
      print_nleq_con fmt o c1 c2;
      pp_close_box fmt ()
  | Msabsentreg (r,ms1,ms2) ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "Missing register ";
      print_reg fmt o r;
      pp_print_string fmt ".";
      pp_close_box fmt ();
      pp_open_hovbox fmt 0;
      let print_regs ms = 
	pp_open_hbox fmt ();
	pp_print_string fmt "{";
	ms_app_reg (fun r _ -> print_reg fmt o r; pp_print_space fmt ()) ms;
	pp_print_string fmt "}";
	pp_close_box fmt () in
      print_regs ms1;
      pp_print_space fmt (); pp_print_string fmt "! <="; pp_print_space fmt ();
      print_regs ms2;
      pp_close_box fmt ();
  | Msnleq (r,c1,c2) ->
      pp_open_vbox fmt 0;
      pp_open_hbox fmt ();
      pp_print_string fmt "subtyping failed at register";
      pp_print_space fmt (); print_reg fmt o r;
      pp_print_string fmt ".";
      pp_close_box fmt ();
      pp_print_cut fmt ();
      print_nleq_con fmt o c1 c2;
      pp_close_box fmt ();
  | Conmeet (c1,c2) ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "no con meet:"; pp_print_break fmt 1 2;
      print_con fmt o c1; pp_print_char fmt ','; pp_print_break fmt 1 2;
      print_con fmt o c2;
      pp_close_box fmt ()
  | Conjoin (c1,c2) ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "no con join"; pp_print_break fmt 1 2;
      print_con fmt o c1; pp_print_char fmt ','; pp_print_break fmt 1 2;
      print_con fmt o c2;
      pp_close_box fmt ()
  | Msmeet (ms1,ms2) ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "no register state meet:"; pp_print_break fmt 1 2;
      print_machine_state fmt o ms1; pp_print_char fmt ',';
      pp_print_break fmt 1 2; print_machine_state fmt o ms2;
      pp_close_box fmt ()
  | Msjoin (ms1,ms2) ->
      pp_open_hovbox fmt 0;
      pp_print_string fmt "no register state join"; pp_print_break fmt 1 2;
      print_machine_state fmt o ms1; pp_print_char fmt ',';
      pp_print_break fmt 1 2; print_machine_state fmt o ms2;
      pp_close_box fmt ()
  | FPstackeq (fps1,fps2) ->
      pp_print_string fmt "fp stacks not equal: ";
      print_fpstack fmt o fps1;
      pp_print_string fmt "!=";
      print_fpstack fmt o fps2
  | FPstackleq (fps1,fps2) ->
      pp_print_string fmt "fp stacks not less or equal:";
      print_fpstack fmt o fps1;
      pp_print_string fmt "! <=";
      print_fpstack fmt o fps2
  | BadUnroll c ->
      pp_print_string fmt "cannot unroll:"; pp_print_break fmt 1 2;
      print_con fmt o c
  | Unknown_size c ->
      pp_print_string fmt "indeterminant size:"; pp_print_break fmt 1 2;
      print_con fmt o c
  | Bad_offset i ->
      fprintf fmt "bad tuple/stack offset %s" (string_of_int32 i)
  | Bad_depth i ->
      pp_print_string fmt "bad tuple/stack depth "; pp_print_int fmt i
  | Not_tail (c,_) ->
      pp_print_string fmt "not tail of current stack:"; pp_print_break fmt 1 2;
      print_con fmt o c
  | Readonly -> pp_print_string fmt "field is readonly"
  | Stack_write_alignment -> pp_print_string fmt "stack slot write misaligned"
  | Coercion (c,s,_) -> pp_print_string fmt s; pp_print_break fmt 1 2;
      print_con fmt o c
  | No_stack_type -> pp_print_string fmt "stack currently undefined"
  | Genop (s,_) -> pp_print_string fmt s
  | Both_mem (_,_) -> pp_print_string fmt "both operands are for memory"
  | Inst_form s -> pp_print_string fmt s
  | Data_form s -> pp_print_string fmt s
  | Fallsthru -> pp_print_string fmt "code falls through"
  | Cyclic_imports s ->
      let s =
	match s with
	  Int_filename n -> n
	| Int_data (n,_) -> n in
      pp_print_string fmt s; pp_print_char fmt ':'; pp_print_break fmt 1 2;
      pp_print_string fmt "cyclic import"
  | Doesnt_export -> pp_print_string fmt "exported item not defined in module"
  | Ndisjoint_exports l ->
      pp_print_string fmt "non-disjoint exports: "; id_prn fmt l
  | Multiple_exports -> pp_print_string fmt "exported twice"
  | Con_def_nleq -> pp_print_string fmt "constructor definition not subtype"
  | Intt_nleq (s,l) ->
       pp_print_string fmt "interface not subtype:"; pp_print_break fmt 1 2;
       pp_print_string fmt s ; id_prn fmt l
  | Label_requires_type id ->
      pp_print_string fmt "label requires type (cannot be inferred): ";
      id_prn fmt id
  | Label_bad_mode (id,m) ->
      pp_print_string fmt "Label ";
      id_prn fmt id;
      pp_print_string fmt " cannot be ";
      (match m with
	Rel -> pp_print_string fmt "relative."
      |	Abs -> pp_print_string fmt "absolute.")
  | Fallsthru_unlabelled ->
      pp_print_string fmt
 	"code fallsthru with instantiation to code without type"
  | Backward_branch id ->
      pp_print_string fmt
 	"backward branch to label requires type (cannot be inferred): ";
      id_prn fmt id
  | Bad_Name id ->
      pp_print_string fmt "bad name: "; id_prn fmt id
  | Var_defined id ->
      pp_print_string fmt "identifier already defined: "; id_prn fmt id
  | Negative_var i -> pp_print_string fmt "var appears negatively in kind: ";
       id_prn fmt i
  | Kindwf (k,s) -> 
       pp_open_hovbox fmt 0;
       pp_print_string fmt "kind not well-formed :" ; pp_print_break fmt 1 2;
       pp_print_string fmt s; pp_print_char fmt ':';  pp_print_break fmt 1 2;
       print_kind fmt o k; pp_close_box fmt ()
and print_nleq_con fmt o c1 c2 =
  let c1' = highlight_similarities c2 c1 in
  let c2' = highlight_similarities c1 c2 in 
  let o = if !full_types then o else { o with detail = PartialDetail; } in
  pp_open_hovbox fmt 0;
  print_con fmt o c1'; 
  pp_close_box fmt ();
  pp_print_cut fmt (); pp_print_string fmt "! <="; pp_print_cut fmt (); 
  pp_open_hovbox fmt 0;
  print_con fmt o c2';
  pp_close_box fmt ()  
;;

let print_ctxt fmt o ctxt =
  let kindvars = Talctxt.get_kindvars ctxt in
  let ms = Talctxt.get_machine_state ctxt in
  let kindmap = Talctxt.get_var_map ctxt in
  let abbrevs = Talctxt.get_abbrevs ctxt in
  let kindabbrevs = Talctxt.get_kindabbrevs ctxt in
  let psi = Talctxt.get_value_labels ctxt in
  let pp_print_string = pp_print_string fmt in
  let print_map dom codom d =
    pp_open_hvbox fmt 0;
    Dict.app_dict (fun x v -> (dom x; 
			       pp_print_string ":"; 
			       codom v;
			       pp_print_break fmt 1 2)) d;
    pp_close_box fmt () in
  let print_pol b = match b, (get_polarity ctxt) with  
      (false, true | true,false) -> pp_print_string "-"
    | (true,true | false,false) -> pp_print_string "+" in
  let print_opt pval opt = 
    match opt with
      None -> pp_print_string "*";
    | Some v -> pval v in
  begin
    pp_open_hvbox fmt 0;
    pp_print_string "CTXT{"; pp_print_break fmt 0 2;
    pp_print_string "KindAbbrevs{";
    print_map (id_prn fmt) (print_kind fmt o) kindabbrevs;
    pp_print_string "}"; pp_print_break fmt 1 2;

    pp_print_string "Kind Vars{";
    print_map (id_prn fmt) (print_pol) kindvars;
    pp_print_string "}"; pp_print_break fmt 1 2;

    pp_print_string "Kinds{";
    print_map (id_prn fmt) (print_kind fmt o) kindmap;
    pp_print_string "}"; pp_print_break fmt 1 2;

    pp_print_string "Abbrevs{";
    print_map (id_prn fmt) (print_con fmt o) abbrevs;
    pp_print_string "}"; pp_print_break fmt 1 2;
(*
    pp_print_string "CodeLabels{";
    print_map (id_prn fmt) (print_opt (print_con fmt o)) psi;
    pp_print_string "}"; pp_print_break fmt 1 2;
*)
    pp_print_string "Regs"; print_machine_state fmt o ms;
    pp_print_break fmt 1 2;
    pp_print_string "}";
    pp_close_box fmt()
  end

let print_Talverify fmt o (ctxt,ve) =
  pp_open_hvbox fmt 0;
  print_tal_loc fmt o (get_loc ctxt);
  pp_print_char fmt ':'; pp_print_break fmt 1 2;
  pp_print_string fmt (get_verify_ctxt ctxt);
  pp_print_char fmt ':'; pp_print_break fmt 1 2;
  print_verify_error fmt o ve;

(*
	pp_print_break fmt 1 2;
       	print_ctxt fmt o ctxt;
*)
  pp_close_box fmt ()
;;

(* EOF: x86talpp.ml *)
