(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel, Dan Grossman      *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* newarray is no longer a keyword *)

(* Poplex - The popcorn lexer.
 *)

{
open Numtypes;;
open Popparse;;

module X = Poperr;;

let get_lexeme = Lexing.lexeme;;
let get_lexeme_char = Lexing.lexeme_char;;
let get_lexeme_start = Lexing.lexeme_start;;
let get_lexeme_end = Lexing.lexeme_end;;
let err e lb =
  let seg = Gcdfec.seg_of_abs (get_lexeme_start lb) (get_lexeme_end lb) in
  Gcdfec.post_error (X.mk_lex_error e seg)
;;

let get_location lb = 
  let seg = Gcdfec.seg_of_abs (get_lexeme_start lb) (get_lexeme_end lb) in
  Gcdfec.string_of_seg seg
;;

let runaway_start = ref 0;;
let runaway_err e lexbuf =
  let seg = Gcdfec.seg_of_abs !runaway_start (get_lexeme_start lexbuf) in
  Gcdfec.post_error (X.mk_lex_error e seg)
;;

let char_for_backslash c = 
  match c with
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let char_for_decimal_code lexbuf i = 
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr(c land 0xff)

let rw = Hashtbl.create 101;;
List.iter (fun (s,t) -> Hashtbl.add rw s t)
    [ "abstract",ABSTRACT;
      "abstype",ABSTYPE;
      "array",ARRAY;
      "bool",BOOL; 
      "break",BREAK; 
      "byte",BYTE;
      "case",CASE; 
      "catch",CATCH;
      "char",CHAR;
      "chr",CHR;
(* Cyclone *)
      "codegen",CODEGEN;
(* End Cyclone *)
      "const",CONST;
      "continue",CONTINUE; 
(* Cyclone *)
      "cut",CUT; 
(* End Cyclone *)
      "do",DO; 
      "default",DEFAULT; 
      "double",DOUBLE;
      "else",ELSE; 
      "exception",EXCEPTION;
      "exn",EXN;
      "exncon",EXNCON;
      "extern",EXTERN;
      "false",CONSTBOOLEAN false;
(* Cyclone *)
      "fill",FILL; 
(* End Cyclone *)
      "finally",FINALLY;
      "float",FLOAT;
      "for",FOR; 
      "fprintf",FPRINTF;
      "fun",FUN;
      "handle",HANDLE;
      "if",IF; 
      "int",INT; 
      "new",NEW; 
(*      "newarray",NEWARRAY; *)
      "null",NULL; 
      "open",OPEN;
      "ord",ORD;
      "prefix",PREFIX;
      "printf",PRINTF;
      "private",PRIVATE;
      "public",PUBLIC;
      "rdtsc", RDTSC;
      "raise",RAISE;
(* LR *)
      "rep",REPTYP;
      "repterm",REPTERM;
(* end LR *)
      "return",RETURN; 
      "signed",SIGNED;
      "size",SIZE;
      "short",SHORT;
(* End Cyclone *)
      "splice",SPLICE; 
(* End Cyclone *)
      "sprintf",SPRINTF;
      "static",STATIC; 
      "string",STRING;
      "struct",STRUCT; 
      "switch",SWITCH;
      "try",TRY;
      "true",CONSTBOOLEAN true; 
      "union",UNION;
      "unsigned",UNSIGNED;
      "void",VOID; 
      "while",WHILE;
      "with",WITH;
    ]
;;

let process_id s =
  try Hashtbl.find rw s with Not_found -> ID s
;;

let comment_depth = ref 0 ;;

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

let delete2char s =
  let len = String.length s in
  if len < 2 then
    failwith "poplex: impossible"
  else
    String.sub s 0 (len - 2)
}

let newline = ('\010' | '\013' | "\013\10")

rule token = parse
  ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']*
                                  { process_id (Lexing.lexeme lexbuf) }
| "__cdecl"                       { CDECL }
| "__stdcall"                     { STDCALL }
| "0x"['0'-'9''a'-'f''A'-'F']*    { CONSTINT
				      (int32_of_string (Lexing.lexeme lexbuf)) }
| "0o"['0'-'7']*                  { CONSTINT
				      (int32_of_string (Lexing.lexeme lexbuf)) }
| "0b"['0''1']*                  { CONSTINT
      				      (int32_of_string (Lexing.lexeme lexbuf)) }
| '.'['0'-'9']+                  { let s = Lexing.lexeme lexbuf in
                                   (String.set s 0 '0');
                                   TUPLEOFFSET (int_of_string s) } 
| ['0'-'9']+  
                                  { CONSTINT
				      (int32_of_string (Lexing.lexeme lexbuf)) }
| ['0'-'9']+"."['0'-'9']+(['E''e']['+''-']?['0'-'9']+)?"lf"
                                  { CONSTDOUBLE 
				      (dec_to_f64 
					 (delete2char (Lexing.lexeme lexbuf))) }
| ['0'-'9']+"."['0'-'9']+(['E''e']['+''-']?['0'-'9']+)?
                                  { CONSTDOUBLE 
				      (dec_to_f64 (Lexing.lexeme lexbuf)) }
| ['0'-'9']+"."['0'-'9']+(['E''e']['+''-']?['0'-'9']+)?['f''F']
                                  { CONSTFLOAT
				      (dec_to_f32 (Lexing.lexeme lexbuf)) }
| ['0'-'9']+("."['0'-'9']*)?(['E''e']['+''-']?['0'-'9']+)
                                  { CONSTDOUBLE 
				      (dec_to_f64 (Lexing.lexeme lexbuf)) }
| ['0'-'9']+("."['0'-'9']*)?(['E''e']['+''-']?['0'-'9']+)['f''F']
                                  { CONSTFLOAT
				      (dec_to_f32 (Lexing.lexeme lexbuf)) }
| "_"                             { UNDERSCORE }
| "("             		  { LPAREN }
| ")"             		  { RPAREN }
| "{"             		  { LBRACE }
| "}"             		  { RBRACE }
| "["             		  { LBRACKET }
| "]"             		  { RBRACKET }
| "+"             		  { PLUS }
| "-"             		  { MINUS }
| "*"             		  { TIMES }
| "/"             		  { DIV }
| "->"                            { ARROW }
| "=="            		  { EE }
| "!="            		  { NE }
| "="             		  { EQUALS }
| "~"                             { TILDE }
| "!"             		  { BANG }
| "?"             		  { QUESTION }
| ":"             		  { COLON }
| ";"             		  { SEMICOLON }
| "."             		  { DOT }
| ","             		  { COMMA }
| "<="            		  { LESSTHANEQ }
| ">="            		  { GREATERTHANEQ }
| "<"             		  { LESSTHAN }
| ">"             		  { GREATERTHAN }
| "++"            		  { PLUSPLUS }
| "--"            		  { MINUSMINUS }
| "+="            		  { PLUSEQUAL }
| "-="            		  { MINUSEQUAL }
| "*="            		  { TIMESEQUAL }
| "/="            		  { DIVEQUAL }
| "%="            		  { MODEQUAL }
| "|="            		  { PIPEEQUAL }
| "^="            		  { CARETEQUAL }
| "&="            		  { AMPEREQUAL }
| "<<="                           { LESSLESSEQ }
| ">>="                           { GREATERGREATEREQ }
| ">>>="                          { GREATERGREATERGREATEREQ }
| "&&"            		  { AMPERAMPER }
| "||"            		  { PIPEPIPE }  
| "&"                             { AMPER }
| "|"                             { PIPE }
| "<<"                            { LESSLESS }
| ">>"                            { GREATERGREATER }
| ">>>"                           { GREATERGREATERGREATER }
| "%"                             { PERCENT }
| "`"                             { BACKQUOTE }
| "^"                             { CARET }
| "@"                             { AT }
| "::"                            { COLONCOLON }
| newline                         { Gcdfec.new_line lexbuf; token lexbuf }
| "#" [^ '\010' '\013']*(newline) { token lexbuf  }
| [' ' '\009' '\011' '\012']+     { token lexbuf }
| "//"[^'\010' '\013']* newline   { Gcdfec.new_line lexbuf; token lexbuf }
| "/*"                            { comment_depth := 1; 
				    runaway_start := get_lexeme_start lexbuf; 
				    comment lexbuf; 
				    token lexbuf }
| "\""                            { string_pos := 0; 
				    runaway_start := get_lexeme_start lexbuf;
				    string lexbuf; 
				    CONSTSTRING(get_stored_string()) }
| "'" [^ '\\' '\''] "'"           
    { CONSTCHAR (Lexing.lexeme_char lexbuf 1) }                             
| "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
    { CONSTCHAR (char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
| "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { CONSTCHAR (char_for_decimal_code lexbuf 2) }
| eof                           { EOF }
| _ { err X.NonWhitespace lexbuf; token lexbuf }

and string = parse
    "\""            { () }
  | "\\" newline    { Gcdfec.new_line lexbuf; string lexbuf }
  | "\\\t"          { string lexbuf }
  | "\\ "           { string lexbuf }
  | "\\\\"          { store_string_char '\\'; string lexbuf }
  | newline         { Gcdfec.new_line lexbuf; store_string_char '\n';
		      string lexbuf }
  | "\\t"           { store_string_char '\t'; string lexbuf }
  | "\\n"           { store_string_char '\n'; string lexbuf }
  | "\\r"           { store_string_char '\r'; string lexbuf }
  | "\\\""          { store_string_char '\034'; string lexbuf }
  | [' '-'~']       { store_string_char (Lexing.lexeme_char lexbuf 0);
		      string lexbuf }
  | eof             { runaway_err X.RunawayString lexbuf }
  | _               { err (X.IllegalStringCharacter (get_lexeme lexbuf))
			lexbuf;
		      string lexbuf }
and comment = parse
   "/*"             { incr comment_depth; comment lexbuf }
 | "*/"             { decr comment_depth; 
		      if !comment_depth > 0 then (comment lexbuf)
		    }
 | newline          { Gcdfec.new_line lexbuf; comment lexbuf }
 | eof              { runaway_err X.RunawayComment lexbuf }
 | _                { comment lexbuf }

(* EOF: poplex.mll *)
