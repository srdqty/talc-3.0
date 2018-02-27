(**********************************************************************)
(* (c) Greg Morrisett, Frederick Smith                                *)
(*     January 1999, all rights reserved.                             *)
(**********************************************************************)

(* This file computes the line number, column offset, and file from
   an absolute file position and a file.

   We support line definitions as follows:
   # 3 "test.pop"  (gcc)
   or 
   #line 3 "test.pop" (visual studio)   
*)
{
type tokens = NEWLINE | LINEDEF | EOF 

type pos = { logical_file : string; 
	     line : string; 
	     line_no : int; 
	     col : int  }
} 
let newline = ('\010' | '\013' | "\013\010")

rule token = parse 
  "#" [^ '\010' '\013']* newline { LINEDEF }
| [^ '\010' '\013']* newline         { NEWLINE }
| [^ '\010' '\013']* eof             { EOF     }

{
  let parse_linedef line : (string * int) option =
    begin
      try
	let i = ref 0 in
      	while line.[!i] < '0' || line.[!i] > '9' do incr i done;
      	let number = 
 	  let rec aux idx = 
	    if line.[idx] >= '0' && line.[idx]<='9' then aux (idx+1) else idx
	  in
	  let (s,e) = (!i,aux !i) in
	  i:=e;
	  (int_of_string (String.sub line s (e-s)))
      	in
      	while line.[!i] <> '"' do incr i done;
	incr i;
      	let filename =
	  let rec aux idx = 
	    if line.[idx] <> '"' then aux (idx+1) else idx
	  in
	  let (s,e) = (!i,aux !i) in
	  i:=e+1;
	  if s>e then "" else String.sub line s (e-s)
      	in
      	Some (filename,number)
      with _ -> None
    end
           
let pos_of_abs filename abs =
   try
  if abs<0 then { logical_file = filename; line = ""; line_no = -1; col = 0; }
  else
    let ic = open_in_bin filename
    in
    try
      let lexbuf = Lexing.from_channel ic in
      let source_file = ref filename in
      let line = ref 1 in
      let rec parse () = 
      	let next = token lexbuf in
      	let eol = Lexing.lexeme_end lexbuf in
	let this_line = Lexing.lexeme lexbuf in
      	if eol > abs
      	then 
	  { logical_file = !source_file; 
	    line = this_line; 
	    line_no = !line;
	    col = (String.length this_line) - (eol - abs)
	  }
	else
      	  match next with
      	    EOF -> { logical_file = !source_file;
		     line = this_line; 
		     line_no = 9999999; (* Unexpected EOF *)
		     col = 0 }
      	  | NEWLINE -> (incr line; parse ())
      	  | LINEDEF -> 
	      begin
		let fno = parse_linedef (Lexing.lexeme lexbuf) in
	      	match fno with
	      	  None -> 
	      	    incr line; 
	      	    Printf.eprintf "Unknown directive: %s\n" this_line;
	      	    parse ()
	      	| Some (f,n) -> source_file := f; line := n; parse ()
	      end
      in
      let pos = parse () in
      close_in ic;
      pos
    with e -> close_in ic; raise e
   with Sys_error e ->
     prerr_string "Couldn't find source file.\n";
     { logical_file = filename; line = ""; line_no = -1; col = 0; }
}
