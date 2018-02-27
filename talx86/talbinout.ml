(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* talbinout.ml 
 * 
 * Provides routines for emitting various TAL constructs in a binary 
 * format.  This file is linked intimately with talbin and talbinin.
 *)

open Numtypes;;
open Identifier;;
open Tal;;
open Talbin;;

let fail s = 
  (Printf.eprintf "talbinout: %s\n" s; flush stderr; raise Gcdfec.Exit)
;;

let print_stats = ref false;; 

module type talbinout = sig
   type out_channel
   val emit_tal_pre_mod : out_channel -> tal_pre_mod -> unit;;
   (* emit a single con ... its inverse is Talbinin.read_in_con *)
   val emit_out_con : out_channel -> con -> unit;;
   val emit_out_kind : out_channel -> kind -> unit ;;
   val emit_out_label : out_channel -> identifier -> unit;;
   val emit_out_psi : out_channel -> int_con list -> unit;;
   val emit_out_tal_int_type : out_channel -> tal_int_type -> unit;;
end 

module TalbinoutFunc (X: sig
   type out_channel 
   val output_char : out_channel -> char -> unit
   val output_string : out_channel -> string -> unit 
   val pos_out : out_channel -> int
   val output_buffer : out_channel -> Buffer.t -> unit
end ) : talbinout with type out_channel = X.out_channel = 
struct 

type out_channel = X.out_channel
open X

let outputs outchar outstring s =
  (* make sure there are no null characters in the string *)
  let rec aux s i l =
    if i = l then true
    else
      if s.[i] = '\000' then false
      else aux s (i+1) l in
  if aux s 0 (String.length s) then
    (outstring s;
     outchar '\000')
  else
    fail "output_string_with_term: string arg contains null characters"
  
let output_string_with_term ch s =
  outputs (output_char ch) (output_string ch) s

let emit_c = Buffer.add_char;;
let emit_s buf s = outputs (Buffer.add_char buf) (Buffer.add_string buf) s

let topbyte i = 
  if (i < 0) then
    (((i asr 24) land 0xff) lor 0x80)
  else 
    ((i lsr 24) land 0xff)
;;

let emit_big_int buf i = 
  emit_c buf (Char.chr(i land 0xff));
  emit_c buf (Char.chr((i lsr 8) land 0xff));
  emit_c buf (Char.chr((i lsr 16) land 0xff));
  emit_c buf (Char.chr(topbyte i))
;;

(* special case for small ints *)
let emit_int buf i = 
  if i >= 0 & i < 255 then 
    emit_c buf (Char.chr(i))
  else 
    (emit_c buf (Char.chr(0xff));
     emit_big_int buf i)
;;

let emit_int32 buf i = emit_int buf (int32_to_int i)
;;

(* this packs a 15-bit integer into a byte when possible.
 * if low bit is 1 then there's only 7 bits of info.  
 * if low bit is 0 then there's 15 bits of info.
 *
 * if we need more than 15 bits because the tables get too large,
 * then we'll need to either use the encoding below or something
 * more general.  
 *
 * The tables got too large. So now emit-short emits however many bytes 
 * it needs to to get the job one.
 * If the low order bit of a byte is 1 then it is the last byte.
 *)
let short_max = 32767;;

(* NB: The following code drops one of 16 bits.  Hence short_max = 2^15 not 2^16 *)
let emit_short buf i = 
  let rec aux i =
    (* Each time around the loop emit 7 lowest order bits. *)
    let j = i lsl 1 in
    if j<=254 then
      emit_c buf (Char.chr(j lor 1))
    else
      (emit_c buf (Char.chr(j land 0xff));
       aux (i lsr 7))
  in
  aux i
;;

(*
  if i < 0 or i > short_max then fail "emit_short: bad arg" else ();
  let i = i lsl 1 in
  if i <= 254 then
    emit_c buf (Char.chr(i lor 1))
  else 
    begin
      emit_c buf (Char.chr(i land 0xff));
      emit_c buf (Char.chr((i lsr 8) land 0xff))
    end
;;

*)
(*
let short_max = 65535;;
let emit_short buf i = 
  emit_c buf (Char.chr(i land 0xff));
  emit_c buf (Char.chr((i lsr 8) land 0xff))
;;
*)

(* an emit_table is used to build a (flat) table of unique strings for some
 * class of values.  The strings are not separated when they are emitted --
 * it is assumed that the separation is contained in the strings and they
 * can be parsed as appropriate.  The entries dictionary 
 * maps the string to the table index.
 *)
type emit_table = 
   { mutable entries: (string,int) Dict.dict; 
     mutable next: int
   }

let new_emit_table () = { entries = Dict.empty compare; next = 0};;

(* returns the string entries in the table as a vector v, with v[i]
 * containing the string that maps to i in the entries dictionary.
 *)
let emit_table_to_vector et = 
  let v = Array.create et.next "" in
  Dict.app_dict (fun s i -> v.(i) <- s) et.entries;
  v
;;

(* returns an index for a given string in an emit_table -- inserts
 * the string if it is not already present.
 *)
let emit_table_lookup et s = 
  try Dict.lookup et.entries s 
  with Dict.Absent ->
    let pos = et.next in
    begin
      et.entries <- Dict.insert et.entries s pos;
      et.next <- pos + 1;  
      pos
    end
;;

type emit_env = 
   { label_map           : identifier -> bool; (* false means no coercion *)
      id_table            : emit_table;
      con_table           : emit_table; 
      kind_table          : emit_table;
      coercion_list_table : emit_table;
    } 

let new_emit_env f = 
  { label_map = f;
    id_table = new_emit_table();
    con_table = new_emit_table();
    kind_table = new_emit_table();
    coercion_list_table = new_emit_table();
  } 
  
(* dump an emit_env to a channel *)    
let output_emit_env ch env = 
  let id_v = emit_table_to_vector env.id_table in
  let con_v = emit_table_to_vector env.con_table in
  let kind_v = emit_table_to_vector env.kind_table in 
  let cl_v = emit_table_to_vector env.coercion_list_table in
  let output_len ar = 
    let x = Array.length ar in
    let buf = Buffer.create 2 in
    emit_short buf x; output_buffer ch buf in
  let pos_id = pos_out ch in 
  (* emit identifiers -- zero terminate them *)
  output_len id_v;
  for i = 0 to (Array.length id_v) - 1 do
    output_string_with_term ch id_v.(i)
  done;
  let pos_kind = pos_out ch in 
  (* emit kinds -- no need to zero terminate them as they'll
   * be parsed back in *)
  output_len kind_v;
  (* Printf.printf "con table: \n"; *)
  for i = 0 to (Array.length kind_v) - 1 do
    (* Printf.printf "%d: %s\n" i (String.escaped(con_v.(i)));  *)
    output_string ch kind_v.(i) 
  done;

  let pos_con = pos_out ch in 
  (* emit constructors -- no need to zero terminate them as they'll
   * be parsed back in *)
  output_len con_v;
  (* Printf.printf "con table: \n"; *)
  for i = 0 to (Array.length con_v) - 1 do
    (* Printf.printf "%d: %s\n" i (String.escaped(con_v.(i)));  *)
    output_string ch con_v.(i) 
  done;

  let pos_coerce = pos_out ch in 
  (* emit coercion lists -- again, no need to zero terminate them as they'll
   * be parsed back in *)
  output_len cl_v;
  for i = 0 to (Array.length cl_v) - 1 do
    output_string ch cl_v.(i)
  done;
  let pos_final = pos_out ch in
  if (!print_stats) then
    begin
      Printf.printf "id array           : %d bytes, %d entries\n" (pos_kind - pos_id) (Array.length id_v);
      Printf.printf "kind array         : %d bytes, %d entries\n" (pos_con - pos_kind) (Array.length kind_v);
      Printf.printf "con array          : %d bytes, %d entries\n" (pos_coerce - pos_con) (Array.length con_v);
      Printf.printf "coercion array     : %d bytes, %d entries\n" (pos_final - pos_coerce) (Array.length cl_v);
    end
  else ()
;;

(* emit a list of elements into buf using emit_elt.  zero-terminate
 * the list.  So this requires that each element in the list not start
 * with zero so that we may parse back in.
 *)
let rec emit_list emit_elt buf lis = 
  let len = List.length lis in
  emit_short buf len;
  List.iter emit_elt  lis
;;

(* emit a list of arbitrary elements into buf.  Puts the length first
 * so that we can have elements which may start with zero.  
 *)
let emit_len_list emit_elt buf lis = 
  let i = List.length lis in
  if (i > short_max) then fail "emit_len_list:  list too long";
  emit_short buf i;
  List.iter (fun x -> emit_elt x) lis
;;

(* emit an optional value:  None = zero, Some = 1 followed by value.
 *)
let emit_opt emit_elt buf opt = 
  match opt with
    None -> emit_c buf '\000'
  | Some x -> emit_c buf '\001'; emit_elt x
;;

(* emit a dictionary -- just converts to a list of pairs and emits
 * with emit_len_list, allowing the domain to start with 000.
 *)
let emit_dict emit_dom emit_cod buf d = 
  let elts = Dict.fold_dict (fun d c elts -> (d,c)::elts) d [] in
  emit_len_list (fun (d,c) -> emit_dom d; emit_cod c) buf elts
;;

(* emit a vector -- puts the length first so that we can start with zero *)
let emit_vector emit_elt buf v = 
  if (Array.length v) > short_max then fail "emit_vector:  too long";
  emit_short buf (Array.length v);
  for i = 0 to (Array.length v) - 1 do
    emit_elt v.(i)
  done
;;

(* Emit the offset of the id's string into buf.  If the string isn't
 * in the id table of the emit_env, then enter it. *)
let emit_id emit_env buf id = 
  let i = emit_table_lookup emit_env.id_table (id_to_string id) in
  if i > short_max then fail "emit_id:  index too large";
  emit_short buf i
;;

(* emit a scale into a buf -- this is used inline in places like 
 * con so we should be sure these characters do not conflict. *)
let emit_scale buf s = 
  match s with
    Byte1 -> emit_c buf '1'
  | Byte2 -> emit_c buf '2'
  | Byte4 -> emit_c buf '4'
  | Byte8 -> emit_c buf '8'
;;

(* emit a register *)
let emit_reg buf r = 
  match r with
    Eax -> emit_c buf 'A'
  | Ebx -> emit_c buf 'B'
  | Ecx -> emit_c buf 'C'
  | Edx -> emit_c buf 'D'
  | Esi -> emit_c buf 's'
  | Edi -> emit_c buf 'd'
  | Ebp -> emit_c buf 'b'
  | Esp -> emit_c buf 'S'
  | Virt _ -> fail "emit_reg: virtual register"
;;

(* emit a kind into a buf *)
let rec emit_kind emit_env outer_buf k = 
   let buf = Buffer.create 8 in 
   (match k.rkind with
      Kbyte s -> emit_scale buf s
    | Ktype -> emit_c buf 'T'
    | Kmemi i -> emit_c buf 'm'; emit_int32 buf i
    | Kmem -> emit_c buf 'M'
    | Kstack -> emit_c buf 'S'
    | Kint -> emit_c buf 'I'
    | Kbool -> emit_c buf 'B'
    | Karrow(k1,k2) -> emit_c buf '>'; emit_kind emit_env buf k1; emit_kind emit_env buf k2
    | Kprod ks -> emit_c buf '*'; emit_list (emit_kind emit_env buf) buf ks
(* LX *)
    | Ksum ks -> emit_c buf '+'; emit_list (emit_kind emit_env buf) buf ks
    | Kvar i -> emit_c buf 'V'; emit_id emit_env buf i
    | Kmu (schema,i) -> emit_c buf 'U'; emit_id emit_env buf i;
	 emit_len_list (fun (i,k) -> emit_id emit_env buf i; emit_kind emit_env buf k) buf
	    schema
(* end LX *)	  
  | Kname -> emit_c buf 'N'
  | Kcap -> emit_c buf 'C'
  | Kms -> emit_c buf 'G');
   let s = Buffer.contents buf in 
   let i = emit_table_lookup emit_env.kind_table s in 
   if i > short_max then fail "emit_kind: index too large";
   emit_short outer_buf i
;;

(* emit a primcon into a buf -- note that characters were chosen so as 
 * not to conflict with emit_scale or emit_con.  *)
let emit_primcon buf p = 
  match p with
    PCbytes s -> emit_scale buf s
  | PCfloat32 -> emit_c buf '3'
  | PCfloat64 -> emit_c buf '6'
  | PCjunk i  -> emit_c buf 'J'; emit_int32 buf i
  | PCjunkbytes s -> 
      emit_c buf 
	(match s with
	| Byte1 -> 'X'
	| Byte2 -> 'O'
	| Byte4 -> 'G'
	| Byte8 -> 'H')
  | PCint i   -> emit_c buf 'I'; emit_int32 buf i
  | PCtrue    -> emit_c buf 'T'
  | PCfalse   -> emit_c buf 'f'
;;

(* emit a variance into buf. *)
let emit_variance buf v = 
  match v with
    Read -> emit_c buf 'R'
  | Write -> emit_c buf 'W'
  | ReadWrite -> emit_c buf 'B'
;;

(* emit a logical operator into buf. *)
let emit_log buf log = 
  match log with
    Cadd -> emit_c buf '+'
  | Csub -> emit_c buf '-'
  | Cmuls -> emit_c buf '*'
  | Cmulu -> emit_c buf 'u'
  | Cand -> emit_c buf '&'
  | Cor -> emit_c buf '|'
  | Cimp -> emit_c buf '>'
  | Ciff -> emit_c buf '='
  | Cnot -> emit_c buf '!'
  | Clts -> emit_c buf '<'
  | Cltu -> emit_c buf 'A'
  | Cltes -> emit_c buf 'B'
  | Clteu -> emit_c buf 'C'
;;

(* emit alias info into buf. *)
let emit_alias_info buf ai = 
  match ai with
    Unique -> emit_c buf 'U'
  | MayAlias -> emit_c buf 'M'
;;

(* Creates the string for the con c and emits the con table entry 
 * number into outer_buf, adding the con's string to the emit_env
 * if not already present.
 *)
let rec emit_con emit_env outer_buf c = 
  let buf = Buffer.create 8 in
  let rec emit_vkc (v,k,c) = 
    (emit_id emit_env buf v; emit_kind emit_env buf k; emc c)
  and emit_ccinfo cc = 
    match cc with
      CCnoinfo -> emit_c buf 'N'
    | CCcmp(c1,c2) -> emit_c buf 'C'; emc c1; emc c2
    | CCtest(c1,c2) -> emit_c buf 'T'; emc c1; emc c2 
  and emit_ms ms = 
    (* it's important to compress the ms case as it occurs very often *)
    let (esp_opt,ebp_opt,regs) = 
      ms_fold_reg (fun r c (esp_opt,ebp_opt,rs) -> 
	match r with
	  Esp -> (Some c,ebp_opt,rs)
	| Ebp -> (esp_opt,Some c,rs)
	| _ -> (esp_opt,ebp_opt,(r,c)::rs)) ms (None,None,[]) in
    (* treat esp and ebp as special to save one byte -- they're almost always
     * defined regardless of the compiler. *)
    (match esp_opt, ebp_opt with
      Some c1,Some c2 -> emit_c buf '\000'; emc c1; emc c2 
    | Some c1,None    -> emit_c buf '\001'; emc c1
    | None,Some c2    -> emit_c buf '\002'; emc c2
    | None,None       -> emit_c buf '\003');
    List.iter (fun (r, c) -> emit_reg buf r; emc c) regs;
    (* NB: we stop parsing reg*con pairs when we see '\000'-'\002'. *)
    (match ms_get_cc ms with
      CCnoinfo -> emit_c buf '\000'
    | CCcmp(c1,c2) -> emit_c buf '\001'; emc c1; emc c2
    | CCtest(c1,c2) -> emit_c buf '\002'; emc c1; emc c2);
    emit_fpstack (ms_get_fpstack ms);
    emc (ms_get_cap ms) 
  and emit_fpstack fps =
    (* We need two bits per stack slot: full/empty/any
       The common case is that the stack is empty or that few are occupied
       So we emit two characters.  The first (full or any) and empty,
       the second set if full.  We only emit the second character if the 
       first one is non-zero.
       *)
    let rec aux i bm1 bm2 =
      let set bm = (bm lor (1 lsl i)) in
      if i < 0 then (bm1,bm2)
      else 
	match fpstack_get_fpreg fps i with 
	| FPfull  -> aux (i-1) (set bm1) (set bm2)
	| FPany   -> aux (i-1) (set bm1)      bm2
	| FPempty -> aux (i-1)      bm1       bm2
    in
    let (bm1,bm2) = aux 7 0 0 in
    emit_c buf (char_of_int bm1);
    if bm1 != 0 then emit_c buf (char_of_int bm2) else ()
  and emit_cons cs = emit_len_list emc buf cs 
  and emc c = emit_con emit_env buf c in
  (
    match c.rcon with
      Cvar x -> emit_c buf 'V'; emit_id emit_env buf x
    | Clam(x,k,c) -> emit_c buf '\\'; emit_vkc (x,k,c)
    | Capp(c1,c2) -> emit_c buf 'a'; emc c1; emc c2
    | Ctuple(cs) -> emit_c buf '('; emit_cons cs
    | Cproj(i,c) -> emit_c buf '#'; emit_short buf i; emc c
(* LX *)
    | Cinj (i,c,k) -> emit_c buf 'i'; emit_short buf i; emc c; emit_kind emit_env buf k
    | Ccase (c1,i,cs) -> emit_c buf ';'; emc c1; emit_id emit_env buf i;
	 emit_cons cs
    | Cfold (k,c) -> emit_c buf 'W' ; emit_kind emit_env buf k; emc c
    | Cpr (i,l) -> emit_c buf 'P'; emit_id emit_env buf i; 
	 emit_len_list (fun (j,a,k,f,k',c) -> 
	    emit_id emit_env buf j;
	    emit_id emit_env buf a;
	    emit_kind emit_env buf k;
	    emit_id  emit_env buf f;
	    emit_kind  emit_env buf k';
	    emit_con  emit_env buf c) buf l 
    | Cvoid k -> emit_c buf '0'; emit_kind emit_env buf k
(* end LX *)
    | Clab(x) -> emit_c buf 'L'; emit_id emit_env buf x
    | Cprim(p) -> emit_primcon buf p
    | Crec(vkcs) -> emit_c buf 'u'; emit_len_list emit_vkc buf vkcs
    | Cforall(v,k,c) -> emit_c buf 'A'; emit_vkc (v,k,c)
    | Cexist(v,k,c1,c2) -> 
	emit_c buf 'E'; emit_id emit_env buf v; emit_kind emit_env buf k; 
	emc c1; emc c2
    | Ccode c -> emit_c buf 'C'; emc c
    | Cms ms -> emit_c buf 'm'; emit_ms ms
    | Cmsjoin(c1,c2) -> emit_c buf '&'; emc c1; emc c2
    | Chptr(is,copt,cvopt) ->
	emit_c buf '!'; 
	emit_len_list (emit_int32 buf) buf is; 
	emit_opt emc buf copt;
	emit_opt (fun (c,v) -> emc c; emit_variance buf v) buf cvopt
    | Cfield(c,v) -> emit_c buf 'F'; emc c; emit_variance buf v
    | Cprod cs -> emit_c buf '*'; emit_cons cs
    | Csum cs -> emit_c buf '+'; emit_cons cs
    | Carray(c1,c2) -> emit_c buf 'v'; emc c1; emc c2
    | Csing(c) -> emit_c buf 'S'; emc c
    | Csptr(c) -> emit_c buf 's'; emc c
    | Cempty -> emit_c buf 'e'
    | Ccons(c1,c2) -> emit_c buf ':'; emc c1; emc c2
    | Cappend(c1,c2) -> emit_c buf '@'; emc c1; emc c2
    | Clog(log,cs) -> emit_c buf '~'; emit_log buf log; emit_cons cs
    | Cif(c1,c2) -> emit_c buf '?'; emc c1; emc c2
    | Cname c -> emit_c buf 'N'; emc c
    | Ccap d -> 
	emit_c buf 'c'; 
	emit_dict (emit_id emit_env buf)
	  (fun (ai,c) -> emit_alias_info buf ai; emc c) buf d
    | Cjoin cs -> emit_c buf 'j'; emit_cons cs
    | Ctagof c -> emit_c buf 't'; emc c
(* Cyclone *)
    | Ctmpl(c1,c2opt,ics1,ics2) -> 
	let emit_ic (i,c) = emit_id emit_env buf i; emc c in
	emit_c buf 'q';
	emc c1;
	emit_opt emc buf c2opt;
	emit_list emit_ic buf ics1;
	emit_list emit_ic buf ics2;
	()
    | Ctptr x -> emit_c buf 'p'; emit_id emit_env buf x
    | Ctrgn(c1,c2opt,rest) ->
	let emit_ic (i,c) = emit_id emit_env buf i; emc c in
	let emit_elt (x,ics1,ics2) =
	  emit_id emit_env buf x;
	  emit_list emit_ic buf ics1;
	  emit_list emit_ic buf ics2
	in
	emit_c buf 'r';
	emc c1;
	emit_opt emc buf c2opt;
	emit_list emit_elt buf rest
(* end Cyclone *)
    | Csubst(_,_) -> fail "emit_con: Csubst"
    | Cr (ci) -> emit_c buf 'R';
	 (match ci with 
	    RCon c -> emit_c buf 'c'; emc c 
	  | RKind k -> emit_c buf 'k'; emit_kind emit_env buf k
	  | RLabel l -> emit_c buf 'l'; emit_id emit_env buf l)
    | Ctypeof l -> emit_c buf 'y'; emit_id emit_env buf l);
  let s = Buffer.contents buf in 
  let i = emit_table_lookup emit_env.con_table s in
  if i > short_max then fail "emit_con: index too large";
  emit_short outer_buf i
;;

(* emit one of Dan's annotations into a buf -- not to be confused with
 * an instruction annotation (defined here.) *)
let emit_annotate emit_env buf a = 
  match a with
    Con c -> 
      emit_c buf 'C'; emit_con emit_env buf c
  | AReg r -> 
      emit_c buf 'R'; emit_reg buf r
  | StackTail(r,i) -> 
      emit_c buf 'T'; emit_reg buf r; emit_short buf i
  | StackSlice(r,i1,i2,c) -> 
      emit_c buf 'S'; emit_reg buf r; emit_short buf i1;
      emit_short buf i2; emit_con emit_env buf c

(* emit a coercion into a buf *)
let emit_coercion emit_env buf c = 
  match c with
    Pack(c1,c2)  -> emit_c buf 'P'; 
       emit_con emit_env buf c1; emit_con emit_env buf c2
  | Tapp(a)      -> emit_c buf 'A'; emit_annotate emit_env buf a
  | Roll(c)      -> emit_c buf 'R'; emit_con emit_env buf c
  | Unroll       -> emit_c buf 'U'
  | Tosum(c)     -> emit_c buf 'T'; emit_con emit_env buf c
  | Fromsum      -> emit_c buf 'F'
  | RollTosum(c) -> emit_c buf 'r'; emit_con emit_env buf c
  | Toarray(i,j,c) -> emit_c buf 'v'; 
      emit_int32 buf i; emit_int buf j; emit_con emit_env buf c
  | Slot(i,j)    -> emit_c buf 's'; emit_int32 buf i; emit_int32 buf j
  | Subsume(c)   -> emit_c buf '<'; emit_con emit_env buf c
  | Forgetname   -> emit_c buf 'f'
  | Prove        -> emit_c buf 'p'
(*  | VirtLabel(x) -> emit_c buf 'V'; emit_short buf x;  *)
  | VirtLabel(x) -> emit_c buf 'V'; emit_id emit_env buf x
;;

(* create a new string s which holds a list of coercions -- lookup the
 * the string in the emit_env.coercion_list_table and emit its offset
 * into outer_buf, adding the string if it's not already there. *)
let emit_coercions emit_env outer_buf coercions = 
  let buf = Buffer.create 16 in
  emit_list (emit_coercion emit_env buf) buf coercions;
  let s = Buffer.contents buf in
  let i = emit_table_lookup emit_env.coercion_list_table s in
  if i > short_max then fail "emit_coercions: index too large";
  emit_short outer_buf i
;;

let rec emit_mallocarg emit_env buf marg = 
  match marg with
    Mbytes s -> emit_scale buf s
  | Mprod mas -> 
      emit_c buf '*'; emit_list (emit_mallocarg emit_env buf) buf mas
  | Mbytearray(s,i) -> 
      emit_c buf 'b'; emit_scale buf s; emit_int32 buf i
;;

let emit_genop emit_env buf gop = 
  match gop with
    Immed i32 -> emit_c buf 'I'; emit_int32 buf i32
  | Reg r -> emit_c buf 'R'; emit_reg buf r
  | Addr x -> emit_c buf 'A'; emit_id emit_env buf x
  | Prjr ((r,c),i,sropt) ->
      emit_c buf 'P'; emit_reg buf r; emit_coercions emit_env buf c;
      emit_int32 buf i; 
      emit_opt (fun (s,r) -> emit_scale buf s; emit_reg buf r) buf sropt
  | Prjl ((x,c),i,sropt) ->
      emit_c buf 'p'; emit_id emit_env buf x; emit_coercions emit_env buf c;
      emit_int32 buf i; 
      emit_opt (fun (s,r) -> emit_scale buf s; emit_reg buf r) buf sropt
;;

(* emit the annotation into outer_buf.  It's possible to build a table
 * for these, but doesn't seem to be worth the trouble.
 *)
let emit_inst_annot emit_env buf annot = 
  let e_c = emit_c buf in
  let e_con = emit_con emit_env buf in
  let e_id = emit_id emit_env buf in
  let e_coercions = emit_coercions emit_env buf in
  let rec ema a = 
    match a with
      An_none i -> 
	if (i <= 0) or (i > 255) then fail ("An_none: "^string_of_int(i))
	else ();
	if i >= 1 & i <= 9 then 
	  e_c (Char.chr(i + (Char.code '0')))
	else (e_c 'N'; e_c (Char.chr i))
    | An_op1 cs -> e_c 'A'; e_coercions cs
    | An_op2 (cs1,cs2) -> 
	e_c 'B'; e_coercions cs1; 
	e_coercions cs2
    | An_op3 (cs1,cs2,cs3) -> 
	e_c 'C'; e_coercions cs1; 
	e_coercions cs2;
	e_coercions cs3;
    | An_op4(c1,c2,c3,c4) -> 
	e_c 'D'; 
	e_coercions c1;
	e_coercions c2;
	e_coercions c3;
	e_coercions c4
    | An_jcc(cs,annots) ->
	e_c 'j'; e_coercions cs; 
	emit_list ema buf annots
    | An_coerce(g,cs) ->
	e_c 'c'; emit_genop emit_env buf g;
	e_coercions cs
    | An_coercename(x,cs) ->
	e_c 'n'; e_id x; e_coercions cs
    | An_fallthru(cs) -> 
	e_c 'f'; emit_len_list (e_con) buf cs
    | An_malloc(x,margopt) ->
	e_c 'm'; e_id x; 
	emit_opt (emit_mallocarg emit_env buf) buf margopt
    | An_proof(lis) ->
	e_c 'p'; 
	emit_len_list 
	  (fun (x,cs) -> (e_id x;
			  emit_len_list (e_con) buf cs)) 
	  buf lis
    | An_unpacknomove(x,r,cs) ->
	e_c 'w'; e_id x; emit_reg buf r;
	e_coercions cs
    | An_unpackmove(x,c1,c2) ->
	e_c 'U'; e_id x; 
	e_coercions c1; e_coercions c2
    | An_sunpack(i,g) ->
	e_c 's'; e_id i; emit_genop emit_env buf g
    | An_nameobj(x,g) ->
	e_c 'x'; e_id x; emit_genop emit_env buf g
    | An_forgetunique(x) ->
	e_c 'F'; e_id x
    | An_removename(x) ->
	e_c 'R'; e_id x 
(* LX *)	    
    | An_vcase (i,c,id,(g,co)) -> e_c 'V' ; 
	 emit_int32 buf i ; 
	 e_con c; 
	 e_id id ; 
	 emit_genop emit_env buf g;
	 e_coercions co
    | An_letprod (is,c) -> e_c 'P' ; 
	 emit_len_list (e_id) buf is;
	 e_con c
    | An_letroll (i,c) -> e_c 'r' ; 
	 e_id i;
	 e_con c
(* end LX *)
(* Cyclone *)
    | An_cgstart   (i,c)         -> e_c 'a'; e_id i; e_con c 
    | An_cgdump    (i1,i2)       -> e_c 'k'; e_id i1; e_id i2
    | An_cghole    (i1,i2)       -> e_c 'l'; e_id i1; e_id i2
    | An_cgholejmp (i1,(i2,cs))  -> e_c 'h'; e_id i1; e_id i2; e_coercions cs
    | An_cgholejcc (i1,(i2,cs),annots)  -> 
	e_c 'i'; e_id i1; e_id i2; e_coercions cs; emit_list ema buf annots
    | An_cgfill    (i1,i2)       -> e_c 'o'; e_id i1; e_id i2
    | An_cgfilljmp (i1,i2,i3,i4) -> e_c 'u'; e_id i1; e_id i2; e_id i3; e_id i4
    | An_cgfilljcc (i1,i2,i3,i4) -> e_c 'z'; e_id i1; e_id i2; e_id i3; e_id i4
    | An_cgforget  (i1,i2)       -> e_c 'q'; e_id i1; e_id i2
    | An_cgend                   -> e_c 'Q'
(* end Cyclone *)
  in ema annot
;;

(* compress adjacent An_none annotations *)
let rec compress annots = 
  match annots with
    [] -> []
  | (An_none i)::(An_none j)::rest -> 
      if i < 254 then compress ((An_none (i+j))::rest)
      else (An_none i)::(compress ((An_none j)::rest))
  | (a::rest) -> a::(compress rest)
;;

let emit_inst_vector emit_env buf v = 
  let vlen = Array.length v in
  let annots = ref [] in
  for i = 0 to (vlen - 1) do
    annots := (inst_annot emit_env.label_map v.(i))::(!annots)
  done;
  let annots = compress(List.rev (!annots)) in
  List.iter (emit_inst_annot emit_env buf) annots;
;;

let emit_code_block emit_env buf (x,copt,v) = 
  (* we don't bother to emit the label as it should be apparent when
   * we zip up the instructions and annotations.
   * We emit the label type as if it's an "annotation" -- note that annotations
   * cannot begin with '\000', '\001', or 'd' so we can tell label where the
   * list of annotations end.  The '\000' or '\001' indicates a new label
   * whereas 'd' indicates the data section, thereby ending the code sectio.
   *)
  emit_opt (emit_con emit_env buf) buf copt;
  emit_inst_vector emit_env buf v
;;

let emit_template emit_env buf (templ_start,con,blocks) =
  (* The template labels are in the symbol table, so we don't need
     to emit them, although we must detect the end of a template somehow.
     emit_vector records the length of the vector, so we can detect the 
     number of templates, and within each template the number of blocks.
     *)
  let blocks_arr = Array.of_list blocks in
  emit_con emit_env buf con;
  emit_vector (emit_code_block emit_env buf) buf blocks_arr;
  emit_c buf 'Y';
  ()

let emit_data_annot emit_env buf d = 
  match d with
    An_dlabel c -> emit_c buf 'L'; emit_coercions emit_env buf c
  | An_dbytes (i) -> emit_c buf 'B'; emit_int buf i
  | An_d2bytes -> emit_c buf '2'
  | An_d4bytes c -> emit_c buf '4'; emit_coercions emit_env buf c
  | An_dfloat32 -> emit_c buf '3';
  | An_dfloat64 -> emit_c buf '6';
  | An_djunk -> emit_c buf 'J'
  | An_drep ri -> emit_c buf 'R';
       (match ri with 
	  An_con -> emit_c buf 'c'
     	| An_kind -> emit_c buf 'k'
     	| An_label ->emit_c buf 'l')
  | An_dup -> emit_c buf 'U'
  | An_ddown -> emit_c buf 'D'
;;

let emit_data_block emit_env buf (x,align,copt,(ds,c)) =
  (* FMS: If the alignment is 4 we emit nothing, otherwise we emit a marker
     and then an integer. We rely on emit_opt emitting either a \000 or a \001*)
  if align <>$ i32_4 then (emit_c buf 'A';
			   emit_int32 buf align);
  emit_opt (emit_con emit_env buf) buf copt;
  emit_coercions emit_env buf c;
  emit_list (emit_data_annot emit_env buf) buf (Talbin.data_annots ds)
;;

(* We want to do recursive kinds differently -- 
   so that we don't need to emit schemas on every use *)
let emit_abbrev emit_env buf c = 
(*   match c.rcon with 
      Cpr (i, l) -> 
	 emit_c buf 'P'; emit_id emit_env buf i; 
	 emit_len_list (fun (j,a,k,f,k',c) -> 
	    emit_id emit_env buf j;
	    emit_id emit_env buf a;
	    emit_kind emit_env buf k;
	    emit_id emit_env buf f;
	    emit_kind emit_env buf k';
	    emit_con emit_env buf c) buf l
    | _ -> *) emit_con emit_env buf c

let emit_kind_abbrev emit_env buf k = 
(*   match k.rkind with 
      Kmu (s,i) -> 
	 emit_c buf 'U'; emit_id emit_env buf i; emit_len_list 
	    (fun (i,k) -> emit_id emit_env buf i; emit_kind emit_env buf k) buf  s 
    | _ -> *) emit_kind emit_env buf k

let emit_out_con ch con = 
   let emit_env = new_emit_env (fun id -> false) in 
   let buf = Buffer.create 256 in
   emit_con emit_env buf con; 
   output_emit_env ch emit_env;
   output_buffer ch buf ;
   ()

let emit_out_kind ch kind = 
   let emit_env = new_emit_env (fun id -> false) in 
   let buf = Buffer.create 256 in
   emit_kind emit_env buf kind; 
   output_emit_env ch emit_env;
   output_buffer ch buf ;
   ()

let emit_out_label ch id = 
   let str = Identifier.id_to_string id in 
   output_string_with_term ch str

let emit_int_con_def emit_env buf d = 
   match d with 
      AbsCon -> emit_c buf 'A'
    | BoundCon c -> emit_c buf 'B'; emit_con emit_env buf c
    | ConcCon c -> emit_c buf 'C'; emit_con emit_env buf c

(* tal interface files *)
let emit_out_tal_int emit_env buf tali =
  match tali with
    Int_filename s -> 
      emit_c buf 'f'; 
      emit_s buf s
  | Int_data
      (filename,
       { int_abbrevs = abbrevs;
	 int_kindabbrevs = kindabbrevs;
	 int_cons = cons;
	 int_vals = vals }) ->
	   emit_c buf 'd';
	   emit_s buf filename;
	   emit_vector (fun (x,c) -> 
	     (emit_id emit_env buf x;
	      emit_abbrev emit_env buf c)) buf abbrevs;
	   emit_vector (fun (x,c) -> 
	     (emit_id emit_env buf x;
	      emit_kind_abbrev emit_env buf c)) buf kindabbrevs;
	   emit_vector (fun (x,k,c) -> 
	     (emit_id emit_env buf x;
	      emit_kind emit_env buf k;
	      emit_int_con_def emit_env buf c)) buf cons;
	   emit_vector (fun (x,c) -> 
	     (emit_id emit_env buf x;
	      emit_con emit_env buf c)) buf vals
;;

let emit_out_psi ch int_cons = 
   let emit_env = new_emit_env (fun id -> false) in 
   let buf = Buffer.create 256 in
   emit_list (fun (x,k,c) -> (emit_id emit_env buf x;
				emit_kind emit_env buf k;
				emit_int_con_def emit_env buf c)) buf int_cons;
   output_emit_env ch emit_env;
   output_buffer ch buf ;
   ()

let emit_out_tal_int_type ch it = 
   let emit_env = new_emit_env (fun id -> false) in 
   let buf = Buffer.create 256 in
   emit_vector (fun (x,k,c) -> 
     ((* Printf.printf "con_id: %s\n" (Identifier.id_to_string x); *)
      emit_id emit_env buf x;
      emit_kind emit_env buf k;
      emit_int_con_def emit_env buf c)) buf (Array.of_list it.it_cons);
   emit_vector (fun (x,c) -> 
     ((* Printf.printf "val_id: %s\n" (Identifier.id_to_string x); *)
      emit_id emit_env buf x;
      emit_con emit_env buf c)) buf (Array.of_list it.it_vals);
   output_emit_env ch emit_env;
   output_buffer ch buf ;
   ()

(* dump the type information for a tal module into a file *)
let emit_tal_pre_mod ch modl = 
  let imp = modl.pre_imp in
  let all_cbvs = 
    (* A list of all code block vectors (including those in templates). *)
    let cbvs = ref [] in
    for i = 0 to Array.length imp.templates - 1 do 
      let (_,_,bs) = imp.templates.(i) in    
      cbvs := (Array.of_list bs) :: !cbvs
    done;
    imp.code_blocks :: !cbvs in
  let emit_env = new_emit_env(Talbin.virtual_label_map all_cbvs) in
  let buf = Buffer.create 256 in
  (* imports *)
  emit_c buf 'I';  emit_vector (emit_out_tal_int emit_env buf) 
    buf modl.import_refs;
  let import_pos = Buffer.length buf in
  (* exports *)
  emit_c buf 'E';  emit_vector (emit_out_tal_int emit_env buf) 
    buf modl.export_refs; 
  let export_pos = Buffer.length buf in
  (* abbrevs *)
  emit_c buf 'A';  
  emit_vector (fun (x,c) -> (emit_id emit_env buf x;
			     emit_abbrev emit_env buf c)) buf imp.imp_abbrevs;
  let abbrev_pos = Buffer.length buf in
  (* kind abbrevs *)
  emit_c buf 'K';
  emit_vector (fun (x,c) -> (emit_id emit_env buf x;
			     emit_kind_abbrev emit_env buf c)) buf imp.imp_kindabbrevs;
  let kind_abbrev_pos = Buffer.length buf in

  (* con blocks *)
  emit_c buf 'C';
  emit_vector (fun (x,k,c) -> (emit_id emit_env buf x;
			       emit_kind emit_env buf k;
			       emit_con emit_env buf c)) buf imp.con_blocks;
  let con_pos = Buffer.length buf in

  (* code blocks *)
  emit_c buf 'c';
  emit_vector (emit_code_block emit_env buf) buf imp.code_blocks;
  let code_pos = Buffer.length buf in
  (* data blocks *)
  emit_c buf 'd';
  emit_vector (emit_data_block emit_env buf) buf imp.data_blocks;
  let data_pos = Buffer.length buf in
  (* templates *)
  emit_c buf 't';
  emit_vector (emit_template emit_env buf) buf imp.templates;
  let final_pos = Buffer.length buf in
  output_emit_env ch emit_env;
  output_buffer ch buf;
  if (!print_stats) then
    begin
      Printf.printf "imports            : %d bytes\n" import_pos;
      Printf.printf "exports            : %d bytes\n" 
	(export_pos - import_pos);
      Printf.printf "abbrevs            : %d bytes\n" 
	(abbrev_pos - export_pos);
       Printf.printf "kind abbrevs      : %d bytes\n" 
	(kind_abbrev_pos - export_pos);
      Printf.printf "conblocks          : %d bytes\n" 
	(con_pos - abbrev_pos);
      Printf.printf "codeblocks         : %d bytes\n" 
	(code_pos - con_pos);
      Printf.printf "datablocks         : %d bytes\n" 
	(data_pos - code_pos);
      Printf.printf "templates          : %d bytes\n"
	(final_pos - data_pos);
    end
  else ()
;;

end 

module Chan = TalbinoutFunc(struct
   type out_channel = Pervasives.out_channel
   let output_char = Pervasives.output_char
   let output_string = Pervasives.output_string
   let pos_out = Pervasives.pos_out
   let output_buffer = Buffer.output_buffer
end );;

module Buf = TalbinoutFunc (struct
   type out_channel = Buffer.t
   let output_char = Buffer.add_char
   let output_string = Buffer.add_string
   let pos_out = Buffer.length
   let output_buffer =  Buffer.add_buffer
end );;



  
