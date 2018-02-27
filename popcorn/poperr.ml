(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, and Frederick Smith                 *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Poperr - Popcorn's errors.
 *)

module Id = Identifier ;;

type id = Id.identifier ;;

(* To create a new kind of compiler error you have to modify the types in this
   file appropriately and update the various functions that produce strings. 
   Gcdfec.ml is responsible for outputing errors in a consistent manner,
   so do not format strings that you return with spaces or carriage returns.
*)

type lexerError =
    IllegalStringCharacter of string 
  | RunawayComment
  | RunawayString
  | InvalidHexNum
  | InvalidChar
  | NonWhitespace
;;

type parseError =
    Syntax of string
  | SwitchClausesDontMatch
  | ExternNoInit
  | ExternNoType
  | ParseUnimplemented 
  ;;

type tcError =
    Unimplemented of string
  | Impossible of string
  | TypeError of string

type tcWarn =
    WshadowVar of id
  ;;

type error =
    Eparse of parseError
  | Elexer of lexerError
  | Etypecheck of tcError
  | Wtypecheck of tcWarn
  ;;


type errorType =
    ETwarning of int
  | ETerror
  | ETpedantic
  | ETcomment
;;
 
let error_type e =
  match e with
     Eparse(_) -> ETerror
   | Elexer(_) -> ETerror
   | Etypecheck(_) -> ETerror
   | Wtypecheck(_) -> ETwarning(10)
;;

let error_message e =
  match e with
    Eparse(pe) ->
      (match pe with
	Syntax s -> s
      | SwitchClausesDontMatch -> "Switch clauses don't match."
      | ExternNoInit -> "External value declarations cannot have initializers"
      |	ExternNoType -> "External value declarations needs type"
      | ParseUnimplemented -> "Unimplemented") 
  | Elexer(le) -> 
      (match(le) with
	IllegalStringCharacter(s) ->
	  "Illegal string character in string: \"" ^ s ^ "\""
      | RunawayComment -> "Runaway comment."
      | RunawayString -> "Runaway string."
      | InvalidHexNum -> "Invalid hexadecimal number."
      | InvalidChar -> "Invalid character code."
      | NonWhitespace -> "Invalid whitespace. ")
  | Etypecheck (tce) ->
      (match tce with
	Unimplemented s -> ("Typechecking : " ^ s ^ " unimplemented.")
      | Impossible s -> ("Impossible: " ^ s)
      | TypeError s -> ("Type error: " ^ s))
  | Wtypecheck wtc ->
      (match wtc with
	WshadowVar i -> ("Variable " ^ (Id.id_to_string i) ^ " shadowed."))
;;

let is_verbose e =
  match e with
    _ -> true
;;

let error_level e =
  match(e) with
    Eparse(pe) -> 1
  | Elexer(le) -> 1
  | Etypecheck(tce) -> 1
  | _ -> 0
;;

let mk_lex_error e seg =
  Gcdfec.mk_err_lex seg (error_message (Elexer e))
;;

let mk_parse_error e =
  Gcdfec.mk_err_parse_symbol (error_message (Eparse e))
;;

(* EOF: poperr.ml *)
