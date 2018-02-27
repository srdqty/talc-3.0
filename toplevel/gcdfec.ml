(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Gcdfec - Generic Compiler Front End Driver Control
 * Provides some common code for location tracking and error posting.
 *)

module L = Lineno

exception Exit;;

(*** Location tracking ***)

let source = ref "";;

type loc = int;;
type seg = {seg_s : loc; seg_e : loc};;

let seg_start s = s.seg_s;;
let loc_of_abs abs = abs;;

let seg_of_abs sabs eabs = {seg_s=loc_of_abs sabs; seg_e=loc_of_abs eabs};;

let seg_symbol () = 
  seg_of_abs (Parsing.symbol_start ()) (Parsing.symbol_end ())
;;

let seg_rhs n = seg_of_abs (Parsing.rhs_start n) (Parsing.rhs_end n);;

let string_of_loc loc =
  let pos = L.pos_of_abs !source loc in
  Printf.sprintf "%s(%d:%d)" pos.L.logical_file pos.L.line_no pos.L.col
;;

let string_of_seg { seg_s=s; seg_e=e } =
  let pos_s = L.pos_of_abs !source s in
  let pos_e = L.pos_of_abs !source e in
  if pos_s.L.logical_file = pos_e.L.logical_file
  then
    Printf.sprintf "%s (%d:%d-%d:%d)"
      pos_s.L.logical_file
      pos_s.L.line_no pos_s.L.col
      pos_e.L.line_no pos_e.L.col
  else 
    Printf.sprintf "%s(%d:%d)-%s(%d:%d)" 
      pos_s.L.logical_file pos_s.L.line_no pos_s.L.col 
      pos_e.L.logical_file pos_e.L.line_no pos_e.L.col
;;

let new_line lexbuf = ();;

(*** Errors ***)

type error_kind = EKlex | EKparse | EKelab;;
type error = {
    err_source : string;
    err_seg : seg;
    err_kind : error_kind;
    err_desc : string
  }
;;

let mk_err_lex l s =
  {err_source= !source; err_seg=l; err_kind=EKparse; err_desc=s}
;;
let mk_err_parse_symbol s =
  {err_source= !source; err_seg=seg_symbol (); err_kind=EKparse; err_desc=s}
;;
let mk_err_parse_rhs n s =
  {err_source= !source; err_seg=seg_rhs n; err_kind=EKparse; err_desc=s}
;;
let mk_err_elab l s =
  {err_source= !source; err_seg=l; err_kind=EKelab; err_desc=s}
;;

(*** Error Reporting ***)

(* If there are errors getting stuff from the source file then raise Nocontext
 * and just don't print the source context.
 *)
exception Nocontext;;

let trunc n s = 
  if (String.length s)<=n then
    s
  else
    let len = String.length s in
    let len_one = (n-3)/2 in
    let len_two = n-3-len_one in
    let sec_one = String.sub s 0 len_one
    and sec_two = String.sub s (len-len_two) len_two in
    sec_one ^ "..." ^ sec_two
;;

let line_length = 76;;

(* Produce a one line string that shows the peice of the source file containing
 * the error; return also the column numbers the error starts and finishes at.
 *)
let get_context { seg_s=abs_s; seg_e=abs_e } =
  (* Get the start and end lines from source file *)
  let (pos_s,pos_e) = 
    try
      (L.pos_of_abs !source abs_s,L.pos_of_abs !source abs_e)
    with _ -> raise Nocontext
  in
  let (sline,eline) = (pos_s.L.line,pos_e.L.line) in
  let (sl,el) = (pos_s.L.line_no,pos_e.L.line_no) in
  let (sc,ec) = (pos_s.L.col,pos_e.L.col) in
  (* Case 1: error is all on one line,
   * give third of line to before, error, and after
   *)
  if sl=el then
    let n = line_length / 3 in
    let sec_one = trunc n (String.sub sline 0 sc)
    and sec_two = trunc n (String.sub sline sc (ec-sc))
    and sec_three = trunc n (String.sub sline sc (String.length sline - ec)) in
    (sec_one^sec_two^sec_three,
     String.length sec_one,
     String.length sec_one + String.length sec_two)
  (* Case 2: error on multiple lines,
   *         give a fourth to start, start line error, end line error, end
   *)
  else
    let n = (line_length-3) / 4 in
    let sec_one = trunc n (String.sub sline 0 sc)
    and sec_two = trunc n (String.sub sline sc (String.length sline - sc))
    and sec_three = trunc n (String.sub eline 0 ec)
    and sec_four = trunc n (String.sub eline ec (String.length eline - ec)) in
    (sec_one^sec_two^".\\."^sec_three^sec_four,
     String.length sec_one,
     String.length sec_one+String.length sec_two+3+String.length sec_three)
;;

let error = ref false;;
let error_p () = !error;;
let print_context = ref false;;

exception Error of error;;

let post_error e =
  error := true;
  Printf.eprintf
    "%s: %s\n" (string_of_seg e.err_seg) e.err_desc;
  if !print_context then begin
    try
      let (c,sc,ec) = get_context e.err_seg in
      Printf.eprintf
 	"  %s\n  %s\n" c ((String.make sc ' ')^(String.make (ec-sc) '^'))
    with
      Nocontext -> ()
    | _ -> failwith "Gcdfec.print_context"
  end;
  flush stderr
;;

(*** Overall Control ***)

type ctxt = string;;

let reset_fe s = source := s; error := false;;

let get_ctxt () = !source;;
let set_ctxt s = (source := s; error := false);;

(* EOF: gcdfec.ml *)
