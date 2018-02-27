(*
A GML program is written using a subset of the printable ASCII
character set, plus the space, tab, return, linefeed and vertical tab
characters. 
*)

{
open Parser

(*
A subset of the identifiers are used as predefined operators, which
may not be rebound. A list of the operators can be found in the
appendix.
*)
(* XXX *)
}

(*
The space, tab, return, linefeed and vertical tab characters are
called whitespace.
*)
let whitespace = [' ' '\010' '\013' '\009' '\012']
let end_of_line = ('\010' | '\013' | "\013\10")

(*
Identifiers must start with an letter and can contain letters, digits,
dashes (`-'), and underscores (`_').
*)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identchar = letter | digit | ['-' '_']
(* ASCII printable characters *)
let printable = ['!'-'~']
(* All whitespaces but newline *)
let blank = [ '\009' ' ']
(*
Strings are written enclosed in double quotes and may contain
any printable character other than the double quote.
*)
let stringchar = ['!' '#'-'~'] | blank

rule token = parse
  | "include" { INCLUDE }
  | letter identchar*
      { IDENT (Lexing.lexeme lexbuf) }
(*
A binder is an identifier prefixed with a `/' character.
*)
  | ['-']? digit+
      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ['-']? digit+ ('.' digit+)? (['e' 'E']'-'?digit+)?
      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
(* The characters /, %, [, ], {, } are special characters. *)
  | '/' 
      { BIND }
  | '['
      { LBRACKET }
  | ']'
      { RBRACKET }
  | '{'
      { LBRACE }
  | '}'
      { RBRACE }
(*
Strings are written enclosed in double quotes and may contain
any printable character other than the double quote. There are no
escape sequences.
*)
  | end_of_line    { token lexbuf }
  | [' ' '\t']     { token lexbuf }
(*
Any occurrence of the character ``%'' not inside a string literal (see
below) starts a comment, which runs to the end of the current
line. Comments are treated as whitespace when tokenizing the input
file.
*)
  | '%' [^'\010' '\013']* end_of_line
      { token lexbuf }
  | '%' [^'\010' '\013']* eof 
      { EOF }
  | '"' [^'"']* '"'
      { let s = Lexing.lexeme lexbuf in
        STRING (String.sub s 1 (String.length s - 2)) }
  | eof
      { EOF }
  | _ { failwith ("Unexpected character = " ^ Lexing.lexeme lexbuf) }
