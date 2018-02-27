(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Stephanie Weirich,                  *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

{ 
open Numtypes;;
open Sparse;;

let err lexbuf s =
  let seg =
    Gcdfec.seg_of_abs
      (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) in
  Gcdfec.post_error (Gcdfec.mk_err_lex seg s)
;;

let keywords = Hashtbl.create 101;;
List.iter (fun (s,t) -> Hashtbl.add keywords s t)
      [	 "nil", NIL;
	 "begin",BEGIN;
         "lambda", LAMBDA;
         "set!", SET;
         "let", LET;
      "letrec", LETREC;
      "if", IF;
      "cond", COND;
      "else", ELSE;
      "and", AND;
      "or", OR;
      "define", DEFINE;
      "eq?", PTREQ;
(*      "equal?", STRUCTEQ; *)  (* library function *) 
      "not", NOT;
      "integer?", ISINT;
      "boolean?", ISBOOL;
      "null?", ISNIL;
(*      "nil?", ISNIL;
      "false?", ISNIL; *)
      "char?", ISCHAR;
      "pair?", ISPAIR;
      "procedure?", ISFN;
      "string?", ISSTRING;
      "input-port?", ISINDESC; (* in-desc *)
      "output-port?", ISOUTDESC; (* out-desc *)
      "cons", CONS;
      "car", CAR;
      "cdr", CDR;
      "set-car!", SETCAR;
      "set-cdr!", SETCDR;
      "list", LIST;
      "open-input-file", OPENIN;  (* open-in *)
      "open-output-file", OPENOUT; (* open-out *)
      "close-input-port", CLOSEIN; (* close-in *)
      "close-output-port", CLOSEOUT; (* close-out *)
      "current-input-port", CURRENTIN;
      "current-output-port", CURRENTOUT;
      "call-with-input-file", CALLWIN;
      "call-with-output-file", CALLWOUT;
      "with-input-from-file", WINFILE;
      "with-output-to-file", WOUTFILE;
      "eof-object?", ISEOF;
(*      "flush-out", FLUSHOUT; 
      	"std-in", STDIN;
      	"std-out", STDOUT;
      	"std-err", STDERR; *)
      "read-char", GETCHAR; (* get-char *)
      "write-char", PUTCHAR; (* write-char *)
      "peek-char", PEEKCHAR;
(*      "get-string", GETSTRING;
      	"put-string", PUTSTRING;  (* write and read should work *) *)
(* These are not needed as the originals work with just an 
   extra argument *)
(*      "fget-char", FGETCHAR;
      	"fput-char", FPUTCHAR;
      	"fpeek-char", FPEEKCHAR;
      	"fget-string", FGETSTRING;
      	"fput-string", FPUTSTRING;  *)
        "write",PRINT;  (* print *)
        "string", NEWSTRING; 
      	"string-ref", SUBS; (* subs *)
      	"string-set!", SETS; (* sets *)
      	"string-length", SIZES; (* sizes *)
      	"integer->char", CHR; (* chr *)
      	"char->integer", ORD  (* ord *)
    ] 
let process_id s =
  try Hashtbl.find keywords s with Not_found -> IDENT s;;

exception Unterminated_String;;
exception Bad_String_Character;;

let string_buffer = ref (String.create 100) ;;
let string_size = ref 100;;
let string_pos = ref 0;;
let store_string_char char =
  begin
    if !string_pos >= !string_size then
      let str = String.create(2 * (!string_size))
      in
         begin
	   String.blit !string_buffer 0 str 0 !string_size;
	   string_buffer := str;
	   string_size := 2 * !string_size
	 end
    else ();
    String.set !string_buffer !string_pos char;
    string_pos := 1 + !string_pos
  end
;;

let get_stored_string () = 
  let str = String.sub !string_buffer 0 !string_pos
  in (string_pos := 0; str)
;;

} 

rule token = parse
  [' ' '\t' '\011' '\012']+
                     { token lexbuf }
| '\010' | '\013' | "\013\010"
                     { Gcdfec.new_line lexbuf; token lexbuf }
| ";"                { comment lexbuf }
| '-'?['0'-'9']+         { INT (int32_of_string (Lexing.lexeme lexbuf)) }
| "\""               { string_pos := 0; string lexbuf;
		       STRING (get_stored_string()) }
| "'()"              { NIL }
| "#f"               { FALSE }
| "#t"               { TRUE }
| '+'                { PLUS }       
| '-'                { MINUS }
| '*'                { TIMES }        
| '/'                { DIV }
| '='                { INTEQ }
| '<'                { LESS }
| '>'                { GREATER }
| "<="               { LESSEQ }
| ">="               { GREATEREQ }
| "'"                { QUOTE }
| ['a'-'z' 'A'-'Z' '!' '$' '%' '&' '*' '/' ':' '<' '=' '>' '?' '^' '_' '~' ]
     ['a'-'z' 'A'-'Z' '0'-'9' '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '='
	 '>' '?' '^' '_' '~']*
                     { process_id (Lexing.lexeme lexbuf) }
| "..."              { process_id (Lexing.lexeme lexbuf) }
| '['                { LPAREN }
| ']'                { RPAREN }
| '('                { LPAREN }
| ')'                { RPAREN }
| eof                { EOF }
and comment = parse
  '\010' | '\013' | "\013\010"
                     { Gcdfec.new_line lexbuf; token lexbuf }
| eof                { EOF }
| _                  { comment lexbuf }
and string = parse
    "\""            { () }
  | '\\'('\010'|'\013'|"\013\010")
                    { Gcdfec.new_line lexbuf; string lexbuf }
  | "\\\t"          { string lexbuf }
  | "\\ "           { string lexbuf }
  | "\\\\"          { store_string_char '\\'; string lexbuf }
  | "\\n"           { store_string_char '\n'; string lexbuf }
  | "\\t"           { store_string_char '\t'; string lexbuf }
  | "\\\""          { store_string_char '\034'; string lexbuf }
  | [' '-'~']       { store_string_char (Lexing.lexeme_char lexbuf 0);
		      string lexbuf }
  | eof             { err lexbuf "unterminated string" }
  | _               { err lexbuf "bad string character"; string lexbuf }

(* EOF: slex.mll *)
