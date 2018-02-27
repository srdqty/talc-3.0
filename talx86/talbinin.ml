(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* talbinin.ml 
 * 
 * Provides routines for reading in TAL constructs in a binary form.
 *
 * Warning -- we must be very careful about the order of effects for
 * Ocaml...in particular, never write (foo e1 e2 e3) where you expect
 * the effects of e1, e2, and e3 to happen left-to-right.  Similarly,
 * don't write (e1,e2,e3) if you expect them to happen left to right.
 * You must write let x1 = e1 in let x2 = e2 in let x3 = e3 ...  Sigh.
 *)

open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;
open Talbin;;

type tal_info = 
    { ti_imports     : int_ref vector;
      ti_exports     : int_ref vector;
      ti_kindabbrevs : kind_abbrev vector;
      ti_abbrevs     : con_abbrev vector;
      ti_con_blocks  : con_block vector;
      ti_code_annots : (con option * (inst_annot list)) vector;
      ti_data_annots : (int32 * con option * ((data_annot list) coerce)) vector;
      ti_template_annots : (con * (con option * (inst_annot list)) vector) vector;
    } 

module type talbinin = sig
   type in_channel
   val read_tal_info : bool -> in_channel -> tal_info	 
   val read_in_con : in_channel -> con
   val read_in_kind : in_channel -> kind
   val read_in_label : in_channel -> identifier
   val read_in_psi : in_channel -> int_con list
   val read_in_tal_int_type : in_channel -> tal_int_type
end 


module  TalbininFunc (X : sig 
   type in_channel 
   val input_char : in_channel -> char
   val input_byte : in_channel -> int
   val pos_in : in_channel -> int
   val seek_in : in_channel -> int -> unit
   val close_in : in_channel -> unit
end 
) = 
struct 

type in_channel = X.in_channel
open X

(* Debugging utilities. *)
let debug = false;;
let dbg_print s = (if debug then (print_string s; flush stdout))

(* End of debugging utilities. *)
let fail s = 
  (Printf.eprintf "talbinin: %s\n" s; flush stderr; raise Gcdfec.Exit)
;;

let parse_error f c ch= 
  let pos = pos_in ch in
  Printf.eprintf "talbinin(parse error):read_%s found %s at character %d \n" f (Char.escaped c) pos;
  flush stderr; close_in ch; raise Gcdfec.Exit
;;

let implode chars = 
  let s = String.create (List.length chars) in
  let rec loop chars i = 
    match chars with
      [] -> s
    | c::chars -> (String.set s i c; loop chars (i+1)) in
  loop chars 0
;;

type read_env = 
    { id_v : identifier vector;
       kind_v : kind vector;
      con_v : con vector;
      cl_v : (coercion list) vector;
    } 

let get_c ch = X.input_char ch;;

let peek_c ch = 
  let pos = pos_in ch in
  let c = get_c ch in
  seek_in ch pos; c
;;

let match_c ch c = 
  let c' = get_c ch in
  if c' = c then () else 
  (print_string "expecting "; print_char c; print_string ":";
   parse_error "match" c' ch)
;;

(* Just for debugging what does this short look like in hex! *)
let short_to_string i = 
  if i < 0 or i > (1 lsl 16) then "NAN" else
  let i = i lsl 1 in
  let cs =
    if i <= 254 then
      Printf.sprintf "0x%x" (i lor 1)
    else 
	Printf.sprintf "0x%x%x" (i land 0xff) ((i lsr 8) land 0xff) in
  cs
;;

let read_short ch = 
  let rec aux bnum a =
    let i = Char.code(input_char ch) in
    let a = a lor ((i lsr 1) lsl (7 * bnum)) in
    if (i land 1) = 1 then a else aux (bnum+1) a
  in
  aux 0 0
;;

(*
  let i = Char.code(input_char ch) in
  if (i land 1) = 1 then
    i lsr 1
  else 
    let i2 = Char.code(input_char ch) in
    let j = (i2 lsl 8) lor i in
    (j lsr 1)
;;
*)

let read_int ch = 
  let b0 = input_byte ch in
  if b0 != 0xff then b0
  else 
    let b0 = input_byte ch in
    let b1 = input_byte ch in
    let b2 = input_byte ch in
    let b3 = input_byte ch in
    b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
;;

let read_int32 ch = int_to_int32(read_int ch)
;;
  
  
let read_vector get_elt ch = 
  let sz = read_short ch in
  flush stdout;
  if sz < 0 then fail "read_vector: size < 0" else
  if sz = 0 then Array.of_list []
  else 
    let elt = get_elt ch in
    let a = Array.create sz elt in
    for i=1 to (sz - 1) do
      a.(i) <- get_elt ch 
    done;
    a
;;

let read_len_list get_elt ch = 
  let sz = read_short ch in
  let rec loop i elts = 
    if (i >= sz) then (List.rev elts)
    else loop (i+1) ((get_elt ch)::elts)
  in loop 0 []
;;

let read_dict read_dom read_cod ch d = 
  let elts = read_len_list 
      (fun ch -> 
	let d = read_dom ch in
	let c = read_cod ch in (d,c)) ch in
  Dict.inserts d elts
;;

let read_list get_elt ch = 
  let len = read_short ch in
  let rec loop i accum =
    (if i = 0 then (List.rev accum) else loop (i-1) (get_elt ch :: accum))
  in loop len []
;;

let read_s ch = 
  let rec r chars = 
    match get_c ch with
      '\000' -> implode (List.rev chars)
    | c -> r (c::chars)
  in r []
;;

let read_opt read_elt ch = 
  match get_c ch with
    '\000' -> None
  | '\001' -> Some(read_elt ch)
  | c -> parse_error "opt" c ch 
;;

let read_id_table ch = 
  read_vector (fun ch -> id_of_string (read_s ch)) ch
;;

let read_id id_table ch = 
  let i = read_short ch in
  if (i >= 0) & i < (Array.length id_table) then id_table.(i)
  else fail ("id table -- bad index: "^(string_of_int i))
;;

let read_scale ch = 
  match get_c ch with
    '1' -> Byte1
  | '2' -> Byte2
  | '4' -> Byte4
  | '8' -> Byte8
  | c -> parse_error "scale" c ch
;;

let decode_reg c ch= 
  match c with
    'A' -> Eax
  | 'B' -> Ebx
  | 'C' -> Ecx
  | 'D' -> Edx
  | 's' -> Esi
  | 'd' -> Edi
  | 'b' -> Ebp
  | 'S' -> Esp
  | c -> parse_error "reg" c ch
;;

let read_reg ch = decode_reg (get_c ch) ch
;;

let rec read_kind_entry id_table kind_table offset ch =
   let getkind ch = 
      let i = read_short ch in
      if (i >= 0) & (i < offset) then kind_table.(i) 
      else fail ("bad kind index: "^(string_of_int i)^"("^(short_to_string i)^")") in 
   
  match get_c ch with
    '1' -> kbyte Byte1
  | '2' -> kbyte Byte2
  | '4' -> k4byte
  | '8' -> kbyte Byte8
  | 'T' -> ktype
  | 'm' -> kmemi (read_int32 ch)
  | 'M' -> kmem
  | 'S' -> kstack
  | 'I' -> kint
  | 'B' -> kbool
  | '>' -> let k1 = getkind ch in let k2 = getkind ch in karrow k1 k2
  | '*' -> kprod(read_list (getkind) ch)
(* LX *)
  | '+' -> ksum (read_list (getkind) ch)
  | 'V' -> kvar (read_id id_table ch)
  | 'U' -> let id = read_id id_table ch in 
    let schema = read_len_list (fun ch -> 
       let id = read_id id_table ch in 
       let k = getkind ch in (id,k)) ch in 
    kmu schema id 
(* LX *)
  | 'N' -> kname
  | 'C' -> kcap
  | 'G' -> kms
  | c -> parse_error "kind" c ch
;;

let read_variance ch = 
  match get_c ch with
    'R' -> Read
  | 'W' -> Write
  | 'B' -> ReadWrite
  | c -> parse_error "variance" c ch
;;

let read_log ch = 
  match get_c ch with
    '+' -> Cadd
  | '-' -> Csub
  | '*' -> Cmuls
  | 'u' -> Cmulu
  | '&' -> Cand
  | '|' -> Cor
  | '>' -> Cimp
  | '=' -> Ciff
  | '!' -> Cnot
  | '<' -> Clts
  | 'A' -> Cltu
  | 'B' -> Cltes
  | 'C' -> Clteu
  | c -> parse_error "log" c ch
;;

let read_alias_info ch = 
  match get_c ch with
    'U' -> Unique
  | 'M' -> MayAlias
  | c -> parse_error "alias_info" c ch
;;

let prev = ref ' ';;

let read_con_entry id_table con_table kind_table offset (ch : X.in_channel) = 
   let getkind ch = 
      let i = read_short ch in
      try kind_table.(i) 
      with _ -> fail ("bad kind index: %d"^(string_of_int i)) in 
   let getcon ch = 
      let i = read_short ch in 
      if (i >= 0) & (i < offset) then con_table.(i) 
      else fail ("bad con index: %d"^(string_of_int i)) in 
   let getcons ch = read_len_list getcon ch in
  let getid ch = read_id id_table ch in
  let getms ch = 
    (try
    let ms0 =
      (* get esp and ebp -- specially encoded *)
      match get_c ch with
	'\000' -> 
	  let c1 = getcon ch in
	  let c2 = getcon ch in
	  ms_set_reg (ms_set_reg ms_empty Ebp c2) Esp c1 
      |	'\001' -> ms_set_reg ms_empty Esp (getcon ch) 
      |	'\002' -> ms_set_reg ms_empty Ebp (getcon ch)
      |	'\003' -> ms_empty 
      |	c -> parse_error "getms" c ch in
    (* get list of reg*con pairs terminated with 000-002 which encode
     * cc info. *)
    let rec getregs ms = 
      match get_c ch with
	'\000' -> ms
      |	'\001' ->
	  let c1 = getcon ch in
	  let c2 = getcon ch in
	  ms_set_cc ms (CCcmp(c1,c2)) 
      |	'\002' ->
	  let c1 = getcon ch in
	  let c2 = getcon ch in
	  ms_set_cc ms (CCtest(c1,c2))
      |	c -> 
	  let r = decode_reg c ch in 
	  let c = getcon ch in 
	  getregs (ms_set_reg ms r c) in
    (* after regs, and cc info is fpstack and then capability *)
    let get_fpstack ms =
      let bm1 = int_of_char (get_c ch) in
      let bm2 = if bm1 != 0 then int_of_char (get_c ch) else 0 in      
      let rec aux i fps =
	let r =
	  if ((1 lsl i) land bm1) <> 0 then
	    if ((1 lsl i) land bm2) <> 0 then FPfull
	    else FPany
	  else FPempty in
	if i<0 then fps
	else aux (i-1) (fpstack_set_fpreg fps i r)
      in
      ms_set_fpstack ms (aux 7 fpstack_empty) in
    let ms = getregs ms0 in 
    let ms = get_fpstack ms in 
    let ms = ms_set_cap ms (getcon ch) in
    ms
    with exc -> (print_string "..getms.."; raise exc)) in
  let c = get_c ch in
(*
  print_string (Char.escaped c); print_string "\n";
*)
  let con = 
  try 
  match c with
    'V' -> cvar (getid ch)
  | '\\' -> 
      let id = getid ch in let k = getkind ch in 
      let c = getcon ch in clam id k c
  | 'a' -> let c1 = getcon ch in let c2 = getcon ch in capp c1 c2
  | '(' -> ctuple (getcons ch) (* ) *)
  | '#' -> let i = read_short ch in let c = getcon ch in cproj c i 
  (* lx *)
  | 'i' -> 
       let i = read_short ch in let c = getcon ch in 
       let k = getkind ch in 
       cinj i c k
  | ';' ->
       let c1 = getcon ch in let id = getid ch in 
       let cs = getcons ch in 
       ccase c1 id cs
  | 'W' ->
       let k = getkind ch in let c = getcon ch in 
       cfold k c       
  | 'P' -> 
       let id = getid ch in 
       let l = read_len_list (fun ch ->
	  let j = getid ch in 
	  let a = getid ch in 
	  let k = getkind ch in 
	  let f = getid ch in 
	  let k' = getkind ch in 
	  let c = getcon ch in
	  (j,a,k,f,k',c)) ch in 
       cpr id l 
  | '0' -> 
       cvoid (getkind ch)
  (* end lx *)     
  | 'L' -> clab (getid ch)
  (* primcons *)
  | '1' -> cbyte1
  | '2' -> cbyte2
  | '4' -> cbyte4
  | '8' -> cbyte8
  | '3' -> pcfloat32
  | '6' -> pcfloat64
  | 'J' -> pcjunk (read_int32 ch)
  | 'X' -> pcjunkbytes Byte1
  | 'O' -> pcjunkbytes Byte2
  | 'G' -> pcjunkbytes Byte4
  | 'H' -> pcjunkbytes Byte8
  | 'I' -> pcint (read_int32 ch)
  | 'T' -> pctrue
  | 'f' -> pcfalse
  (* end primcons *)
  | 'u' -> 
      crec (read_len_list 
	      (fun ch -> let x = getid ch in let k = getkind ch in
	       let c = getcon ch in (x,k,c)) ch)
  | 'A' -> 
      let x = getid ch in 
      let k = getkind ch in let c = getcon ch in cforall x k c
  | 'E' -> 
      let x = getid ch in let k = getkind ch in 
      let c1 = getcon ch in let c2 = getcon ch in cexistp x k c1 c2
  | 'C' -> ccode (getcon ch)
  | 'm' -> cms (getms ch)
  | '&' -> let c1 = getcon ch in let c2 = getcon ch in cmsjoin c1 c2
  | '!' -> 
      let ints = read_len_list read_int32 ch in
      let copt = read_opt getcon ch in
      let cvopt = 
	read_opt 
	  (fun ch -> let c = getcon ch in let v = read_variance ch in (c,v)) ch
      in chptr ints copt cvopt
  | 'F' -> let c = getcon ch in let v = read_variance ch in cfield c v
  | '*' -> cprod (getcons ch)
  | '+' -> csum (getcons ch)
  | 'v' -> let c1 = getcon ch in let c2 = getcon ch in carray c1 c2
  | 'S' -> csing (getcon ch)
  | 's' -> csptr (getcon ch)
  | 'e' -> cempty
  | ':' -> let c1 = getcon ch in let c2 = getcon ch in ccons c1 c2
  | '@' -> let c1 = getcon ch in let c2 = getcon ch in cappend c1 c2
  | '~' -> let log = read_log ch in let cs = getcons ch in clog log cs 
  | '?' -> let c1 = getcon ch in let c2 = getcon ch in cif c1 c2
  | 'N' -> cname (getcon ch)
  | 'c' -> ccap (read_dict getid 
		   (fun ch -> 
		     let ai = read_alias_info ch in 
		     let c = getcon ch in (ai,c)) 
		   ch (Dict.empty id_compare))
  | 'j' -> cjoin (getcons ch)
  | 't' -> ctagof (getcon ch)
(* Cyclone *)
  | 'q' -> (* Ctmpl *)
      let read_ic ch = let x = getid ch in let c = getcon ch in (x,c) in
      let c1 = getcon ch in
      let c2opt = read_opt getcon ch in
      let ics1 = read_list read_ic ch in
      let ics2 = read_list read_ic ch in
      ctmpl (c1,c2opt,ics1,ics2)
  | 'p' -> let x = getid ch in ctptr x
  | 'r' -> (* Ctrgn *)
      let read_ic ch = let x = getid ch in let c = getcon ch in (x,c) in
      let read_ics = read_list read_ic in
      let read_elt ch = 
	(let x = getid ch in let ics1 = read_ics ch in let ics2 = read_ics ch in
	(x,ics1,ics2))
      in
      let c1 = getcon ch in let c2opt = read_opt getcon ch in
      let rest = read_list read_elt ch in
      ctrgn (c1,c2opt,rest)
(* end Cyclone *)
  | 'R' -> cr (match get_c ch with 
       'c' -> RCon (getcon ch)
     | 'k' -> RKind (getkind ch)
     | 'l' -> RLabel (getid ch)
(*     | 'p' -> RPsi *)
     | c -> parse_error "repitem" c ch)
  | 'y' -> ctypeof (getid ch)
  | c -> parse_error "con" c ch
  with exc -> 
     (print_string "...this constructor char: ";
       	print_string (Char.escaped c);
        print_string "\n"; 
     	print_string "...last constructor char: "; 
     	print_string (Char.escaped (!prev));
     	print_string "\n"; raise exc) in
  prev := c; con
     ;;

let read_con_table id_table kind_table ch = 
  let sz = read_short ch in
(*  Printf.printf "size is %d\n" sz; *)
  let table = Array.create sz cbyte1 in
  for i=0 to (sz - 1) do
(*    Printf.printf "reading con entry : %d\n" i; *)
    table.(i) <- read_con_entry id_table table kind_table i ch
  done;
  table
;;

let read_kind_table id_table ch = 
  let sz = read_short ch in
(*  Printf.printf "size is %d\n" sz; *)
  let table = Array.create sz ktype in
  for i=0 to (sz - 1) do
(*    Printf.printf "reading con entry : %d\n" i; *)
     table.(i) <- read_kind_entry id_table table i ch
  done;
  table
;;
   

let read_con con_table ch = 
  let i = read_short ch in
  if (i >= 0) & i < (Array.length con_table) then con_table.(i)
  else fail ("con table -- bad index: "^(string_of_int i))
;;

let read_kind kind_table ch = 
  let i = read_short ch in
  if (i >= 0) & i < (Array.length kind_table) then kind_table.(i)
  else fail ("kind table -- bad index: "^(string_of_int i))
;;



let read_annotate id_table con_table ch = 
  match get_c ch with
    'C' -> Con(read_con con_table ch)
  | 'R' -> AReg(read_reg ch)
  | 'T' -> let r = read_reg ch in let s = read_short ch in StackTail(r,s)
  | 'S' -> let r = read_reg ch in let s1 = read_short ch in 
           let s2 = read_short ch in let c = read_con con_table ch in
	   StackSlice(r,s1,s2,c)
  | c -> parse_error "annotate" c ch
;;

let read_coercion id_table con_table ch = 
  let getcon = read_con con_table in
  let getid = read_id id_table in
  match get_c ch with
    'P' -> let c1 = getcon ch in let c2 = getcon ch in Pack(c1,c2)
  | 'A' -> Tapp(read_annotate id_table con_table ch)
  | 'R' -> Roll(getcon ch)
  | 'U' -> Unroll
  | 'T' -> Tosum(getcon ch)
  | 'F' -> Fromsum
  | 'r' -> RollTosum(getcon ch)
  | 'v' -> let i = read_int32 ch in let j = read_int ch in 
           let c = getcon ch in Toarray(i,j,c)
  | 's' -> let i = read_int32 ch in let j = read_int32 ch in Slot(i,j)
  | '<' -> Subsume(getcon ch)
  | 'f' -> Forgetname
  | 'p' -> Prove
(*  | 'V' -> let x = read_short ch in VirtLabel(x) *)
  | 'V' -> let x = getid ch in VirtLabel(x)
  | c   -> parse_error "coercion" c ch
;;

let read_coercions_table id_table con_table ch = 
(*
  print_string "reading coercion list table\n";   
*)
  read_vector (read_list (read_coercion id_table con_table)) ch
;;

let read_read_env ch = 
(*
  print_string "reading id table\n"; 
*)
  let id_table = read_id_table ch in
(*
  Printf.printf "Id Table\n";
  Printf.printf "---------\n";
  for i = 0 to (Array.length id_table) - 1 do
    Printf.printf "%d: %s\n" i (id_to_string(id_table.(i)))
  done;
  print_string "reading con table\n";
*)
  let kind_table = read_kind_table id_table ch in 
  let con_table = read_con_table id_table kind_table ch in
(*
  Printf.printf "\nCon Table\n";
  Printf.printf "------------\n";
  for i = 0 to (Array.length con_table) - 1 do
    Printf.printf "%d: " i;
    Talpp.print_con Format.std_formatter Talpp.std_options con_table.(i);
    Format.print_newline();
  done;
*)
  let coercion_list_table = read_coercions_table id_table con_table ch in
(*
  Printf.printf "\nCoercion list table\n";
  Printf.printf "----------------------\n";
  for i = 0 to (Array.length coercion_list_table) - 1 do
    Printf.printf "%d: " i;
    Talpp.print_coerce Format.std_formatter 
      (fun f opts u -> Format.print_string "?") Talpp.std_options 
      ((),coercion_list_table.(i));
    Format.print_newline();
  done;
  print_string "done reading read_env\n";
*)
  { id_v = id_table;
    kind_v = kind_table;
    con_v = con_table;
    cl_v = coercion_list_table
  } 
;;

let read_id read_env ch = read_id read_env.id_v ch
;;
let read_con read_env ch = read_con read_env.con_v ch
;;
let read_kind read_env ch = read_kind read_env.kind_v ch
;;
let read_coercions read_env ch = 
  let i = read_short ch in
  if (i >= 0) & i < (Array.length read_env.cl_v) then read_env.cl_v.(i)
  else fail ("coercion list table -- bad index: "^(string_of_int i))
;;

let rec read_mallocarg read_env ch = 
  match get_c ch with
    '1' -> Mbytes Byte1
  | '2' -> Mbytes Byte2
  | '4' -> Mbytes Byte4
  | '8' -> Mbytes Byte8
  | '*' -> Mprod (read_list (read_mallocarg read_env) ch)
  | 'b' -> let s = read_scale ch in let i = read_int32 ch in Mbytearray(s,i)
  | c -> parse_error "mallocarg" c ch
;;

let read_genop read_env ch = 
  match get_c ch with
    'I' -> Immed (read_int32 ch)
  | 'R' -> Reg (read_reg ch)
  | 'A' -> Addr (read_id read_env ch)
  | 'P' -> 
      let r = read_reg ch in 
      let cs = read_coercions read_env ch in
      let i = read_int32 ch in 
      let sropt = 
	read_opt 
	  (fun ch -> 
	    let s = read_scale ch in
	    let r = read_reg ch in (s,r)) ch in
      Prjr((r,cs),i,sropt)
  | 'p' ->
      let x = read_id read_env ch in 
      let cs = read_coercions read_env ch in
      let i = read_int32 ch in 
      let sropt = 
	read_opt 
	  (fun ch -> 
	    let s = read_scale ch in
	    let r = read_reg ch in (s,r)) ch in
      Prjl((x,cs),i,sropt)
  | c -> parse_error "genop" c ch
;;

let rec read_inst_annot read_env ch = 
  let r_cs = read_coercions read_env in
  let r_c = read_con read_env in
  let r_id = read_id read_env in
  let c = get_c ch in
  (*Printf.printf "annot: %c\n" c; *)
  match c with
    '1' -> An_none 1
  | '2' -> An_none 2
  | '3' -> An_none 3
  | '4' -> An_none 4
  | '5' -> An_none 5
  | '6' -> An_none 6
  | '7' -> An_none 7
  | '8' -> An_none 8
  | '9' -> An_none 9
  | 'N' -> let i = Char.code(get_c ch) in An_none i
  | 'A' -> An_op1(r_cs ch)
  | 'B' -> let c1 = r_cs ch in let c2 = r_cs ch in An_op2(c1,c2)
  | 'C' -> let c1 = r_cs ch in let c2 = r_cs ch in let c3 = r_cs ch in 
           An_op3(c1,c2,c3)
  | 'D' -> let c1 = r_cs ch in let c2 = r_cs ch in let c3 = r_cs ch in 
           let c4 = r_cs ch in An_op4(c1,c2,c3,c4)
  | 'j' -> let c1 = r_cs ch in 
           let is = read_list (read_inst_annot read_env) ch in
	   An_jcc(c1,is)
  | 'c' -> let g = read_genop read_env ch in let c = r_cs ch in An_coerce(g,c)
  | 'n' -> let x = r_id ch in let c = r_cs ch in An_coercename(x,c)
  | 'f' -> An_fallthru(read_len_list r_c ch)
  | 'm' -> let x = r_id ch in 
           let mopt = read_opt (read_mallocarg read_env) ch in
	   An_malloc(x,mopt)
  | 'p' -> An_proof(read_list 
		      (fun ch -> 
			let x = r_id ch in
			let cs = read_list (read_con read_env) ch in
			(x,cs)) ch)
  | 'w' -> let x = r_id ch in let r = read_reg ch in let cs = r_cs ch in
           An_unpacknomove(x,r,cs)
  | 'U' -> let x = r_id ch in let cs1 = r_cs ch in let cs2 = r_cs ch in
           An_unpackmove(x,cs1,cs2)
  | 's' -> let i = r_id ch in let g = read_genop read_env ch in
           An_sunpack(i,g)
  | 'x' -> let x = r_id ch in let g = read_genop read_env ch in
           An_nameobj(x,g)
  | 'F' -> An_forgetunique(r_id ch)
  | 'R' -> An_removename(r_id ch)
(* LX *)
  | 'V' -> let i = read_int32 ch in let c = r_c ch in 
    let id = r_id ch in let g = read_genop read_env ch in 
    let co = r_cs ch in 
    An_vcase (i,c,id,(g,co))
  | 'P' ->
       let is = read_len_list r_id ch in 
       let c = r_c ch in 
       An_letprod (is,c)
  | 'r' -> 
       let i = r_id ch in 
       let c = r_c ch in 
       An_letroll (i,c)
(* end LX *)
(* Cyclone *)
  | 'a' -> 
      let i = r_id ch in
      let c = r_c ch in 
      An_cgstart(i,c)
  | 'k' -> 
      let x1 = r_id ch in let x2 = r_id ch in 
      An_cgdump(x1,x2)
  | 'l' -> 
      let x1 = r_id ch in let x2 = r_id ch in 
      An_cghole(x1,x2)
  | 'h' -> 
      let x1 = r_id ch in let x2 = r_id ch in 
      let cs = r_cs ch in 
      An_cgholejmp(x1,(x2,cs))
  | 'i' ->
      let x1 = r_id ch in let x2 = r_id ch in 
      let cs = r_cs ch in 
      let is = read_list (read_inst_annot read_env) ch in
      An_cgholejcc(x1,(x2,cs),is)
  | 'o' ->
      let x1 = r_id ch in let x2 = r_id ch in 
      An_cgfill(x1,x2)
  | 'u' ->
      let x1 = r_id ch in let x2 = r_id ch in 
      let x3 = r_id ch in let x4 = r_id ch in 
      An_cgfilljmp(x1,x2,x3,x4)
  | 'z' ->
      let x1 = r_id ch in let x2 = r_id ch in 
      let x3 = r_id ch in let x4 = r_id ch in 
      An_cgfilljcc(x1,x2,x3,x4)
  | 'q' ->
      let x1 = r_id ch in
      let x2 = r_id ch in
      An_cgforget (x1,x2)
  | 'Q' ->
      An_cgend
(* end Cyclone *)
  | c -> parse_error "inst_annot" c ch
;;

let read_code_annot read_env ch = 
  let copt = read_opt (read_con read_env) ch in
  let rec loop ias = 
    match peek_c ch with
      ('\000' | '\001' | 'd' | 'Y' ) -> List.rev ias
    | _ -> loop ((read_inst_annot read_env ch)::ias)
  in (copt,loop [])
;;

let read_code_annots read_env ch = read_vector (read_code_annot read_env) ch
;;

let read_template_annot read_env ch =
  let con = read_con read_env ch in
  let blocks = read_code_annots read_env ch in
  get_c ch; (* 'Y' marks the end of the template. *)
  (con,blocks)
;;

let read_template_annots read_env ch = 
  read_vector (read_template_annot read_env) ch;;

let read_data_annot read_env ch = 
  let c = get_c ch in
  (* Printf.printf "%c\n" c; *)
  match c with
    'L' -> An_dlabel (read_coercions read_env ch)
  | 'B' -> An_dbytes(read_int ch)
  | '2' -> An_d2bytes
  | '4' -> An_d4bytes (read_coercions read_env ch)
  | '3' -> An_dfloat32
  | '6' -> An_dfloat64
  | 'J' -> An_djunk
  | 'R' -> An_drep (match get_c ch with 
       'c' -> An_con 
     | 'k' -> An_kind 
     | 'l' -> An_label
     | c -> parse_error "repitem" c ch)
  | 'U' -> An_dup
  | 'D' -> An_ddown
  | c -> parse_error "data_annot" c ch
;;

let read_data_block read_env ch = 
  let align = 
    match peek_c ch with 
    | 'A' -> (get_c ch; read_int32 ch)
    | _ -> i32_4 in
  let copt = read_opt (read_con read_env) ch in
  let c = read_coercions read_env ch in
  let das = read_list (read_data_annot read_env) ch in
  (align,copt,(das,c))
;;

let read_data_annots read_env ch = read_vector (read_data_block read_env) ch
;;

(* read an individual con created by Talbinout.emit_out_con *)

let read_in_con in_channel = 
   let read_env = read_read_env in_channel in 
   read_con read_env in_channel

let read_in_kind in_channel = 
   let read_env = read_read_env in_channel in 
   read_kind read_env in_channel
 

let read_in_label in_channel = 
   id_of_string (read_s in_channel)

let read_int_con read_env ch = 
   match (get_c ch) with 
      'A' -> AbsCon
    | 'B' -> BoundCon (read_con read_env ch)
    | 'C' -> ConcCon (read_con read_env ch)
    | d -> parse_error "int_con" d ch

let read_in_tal_int read_env ch =
  match (get_c ch) with
    'f' -> Int_filename (read_s ch)
  | 'd' -> 
      let filename = read_s ch in
      let abbrevs = 
	read_vector 
	  (fun ch -> 
	    let x = read_id read_env ch in
	    let c = read_con read_env ch in (x,c)) ch in
      let kindabbrevs = 
	 read_vector
	    (fun ch ->
	       let x = read_id read_env ch in
	       let k = read_kind read_env ch in (x,k)) ch in
      let cons = read_vector
	  (fun ch -> 
	    let x = read_id read_env ch in
	    let k = read_kind read_env ch in
	    let c = read_int_con read_env ch in (x,k,c)) ch in 
      let vals = read_vector
	  (fun ch -> 
	    let x = read_id read_env ch in
	    let c = read_con read_env ch in (x,c)) ch in 
      Int_data
	(filename,
	 { int_abbrevs = abbrevs;
	   int_kindabbrevs = kindabbrevs;
	   int_cons = cons;
	   int_vals = vals })
  | d -> parse_error "tal_int" d ch

let read_in_psi ch = 
   let read_env = read_read_env ch in 
   read_list 
      (fun ch -> 
	 let x = read_id read_env ch in
	 let k = read_kind read_env ch in
	 let c = read_int_con read_env ch in (x,k,c)) ch

let read_in_tal_int_type ch = 
   let read_env = read_read_env ch in 
   let it_cons_arr = read_vector
      (fun ch -> 
	let x = read_id read_env ch in
	(* Printf.printf "con_id: %s\n" (Identifier.id_to_string x); *)
	let k = read_kind read_env ch in
	let c = read_int_con read_env ch in (x,k,c)) ch in 
   let it_vals_arr = read_vector
       (fun ch -> 
	 let x = read_id read_env ch in
	 (* Printf.printf "val_id: %s\n" (Identifier.id_to_string x); *)
	 let c = read_con read_env ch in (x,c)) ch in 
   { it_cons = Array.to_list it_cons_arr; 
     it_vals = Array.to_list it_vals_arr }

let read_tal_info read_internals ch = 

  dbg_print "opening .to file and reading environment\n";

  let read_env = read_read_env ch in
  match_c ch 'I';
  let imports = read_vector (read_in_tal_int read_env) ch in
(*
  if debug then 
    (Printf.printf "Imports: ";
     for i = 0 to (Array.length imports) - 1 do
       Printf.printf "%s " imports.(i)
     done);
*)
  match_c ch 'E';
  let exports = read_vector (read_in_tal_int read_env) ch in
(*
  if debug then
    (Printf.printf "\nExports: ";
     for i = 0 to (Array.length exports) - 1 do
       Printf.printf "%s " imports.(i)
     done);
*)
  if read_internals then
    begin
      match_c ch 'A';
      let abbrevs = 
	read_vector 
	  (fun ch -> 
	    let x = read_id read_env ch in
	    let c = read_con read_env ch in (x,c)) ch in
      match_c ch 'K';
      let kindabbrevs = 
	 read_vector
	    (fun ch ->
	       let x = read_id read_env ch in
	       let k = read_kind read_env ch in (x,k)) ch in

      if debug then 
	(Printf.printf "\nAbbreviations: \n";
	 Printf.printf "--------------\n";
	 for i = 0 to (Array.length abbrevs) - 1 do 
	   let (x,c) = abbrevs.(i) in
	   id_prn Format.std_formatter x;
	   Format.print_string " = ";
	   Talpp.print_con Format.std_formatter Talpp.std_options c;
	   Format.print_newline();
	 done);

      match_c ch 'C';
      let con_blocks = 
	read_vector 
	  (fun ch -> 
	    let x = read_id read_env ch in
	    let k = read_kind read_env ch in
	    let c = read_con read_env ch in (x,k,c)) ch in

      if debug then
	(Printf.printf "\nCon definitions: \n";
	 Printf.printf "----------------------\n";
	 for i = 0 to (Array.length con_blocks) - 1 do 
	   let (x,k,c) = con_blocks.(i) in
	   id_prn Format.std_formatter x;
	   Format.print_string ":";
	   Talpp.print_kind Format.std_formatter Talpp.std_options k;
	   Format.print_string " = ";
	   Talpp.print_con Format.std_formatter Talpp.std_options c;
	   Format.print_newline();
	   flush stdout;
	   flush stderr;
	 done);

      match_c ch 'c';
      let code_annots = read_code_annots read_env ch in
      match_c ch 'd';
      let data_annots = read_data_annots read_env ch in
      match_c ch 't';
      let template_annots = read_template_annots read_env ch in
      { ti_imports = imports;
	ti_exports = exports;
	ti_kindabbrevs = kindabbrevs;
	ti_abbrevs = abbrevs;
	ti_con_blocks  = con_blocks;
	ti_code_annots = code_annots;
	ti_data_annots = data_annots;
	ti_template_annots = template_annots;
      } 
    end
  else (* read_internals *)
    { ti_imports = imports;
      ti_exports = exports;
      ti_kindabbrevs = [||];
      ti_abbrevs = [||];
      ti_con_blocks = [||];
      ti_code_annots = [||];
      ti_data_annots = [||];
      ti_template_annots = [||];
    } 
;;

end 

      
module Chan =  TalbininFunc ( Pervasives)
module Str  = TalbininFunc (Stringchan)
