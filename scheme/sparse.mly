/**********************************************************************
 * (c) Greg Morrisett, Neal Glew, Stephanie Weirich,                  *
 *     June 1998, all rights reserved.                                *
 **********************************************************************/

%{
open Sast;;

let err s =
  Gcdfec.post_error (Gcdfec.mk_err_parse_symbol s)
;;

let parse_error s = err s;;

let rec make_list xs = 
   match xs with 
      (x::rest) ->(Op(Cons, [x; make_list rest]))
    | [] -> Nil
;;

let rec make_if exps = 
  match exps with
    [] -> err "no clauses in cond"; raise Gcdfec.Exit
  | [(True,e)] -> e
  | [(_,e)] -> err "last clause of cond not default"; e
  | ((e1,e2)::r) -> If(e1,e2,make_if r)
;;

let split_it exps = 
  match exps with
    [] -> invalid_arg "Sparse.split_it"
  | e::es -> (e,es)
;;
%}  

%token <Numtypes.int32> INT
%token <string> IDENT
%token <string> STRING
%token NIL TRUE FALSE LAMBDA SET LET LETREC IF COND ELSE AND OR DEFINE PLUS MINUS
%token TIMES DIV INTEQ PTREQ STRUCTEQ NOT LESS LESSEQ GREATER GREATEREQ ISINT 
%token ISBOOL ISNIL ISCHAR ISPAIR ISFN ISSTRING ISINDESC ISOUTDESC CONS CAR CDR
%token SETCAR SETCDR LIST OPENIN OPENOUT CLOSEIN CLOSEOUT FLUSHOUT STDIN
%token STDOUT STDERR GETCHAR PUTCHAR PEEKCHAR GETSTRING PUTSTRING FGETCHAR
%token FPEEKCHAR FPUTCHAR FGETSTRING FPUTSTRING PRINT NEWSTRING SUBS SETS 
%token SIZES QUOTE LPAREN RPAREN EOF BEGIN CHR ORD
%token CURRENTIN CURRENTOUT CALLWIN CALLWOUT WINFILE WOUTFILE ISEOF
%type <Sast.prog> prog
%start prog 
%%

prog:
   deflist EOF  { $1  } 
;

deflist:
   def             { [ $1 ] }
|  def deflist     { $1  :: $2 }
;
def:
  LPAREN DEFINE IDENT exp RPAREN  { ($3,$4) }
| LPAREN DEFINE LPAREN IDENT vars RPAREN exp RPAREN { ($4, Lambda($5,$7)) }
;
exps: 
  exp      { [$1] }
| exp exps   { $1 :: $2 }
;

op:
  PLUS       {Plus}
| MINUS      {Minus}
| TIMES      {Times}
| DIV        {Div}
| INTEQ      {Inteq}
| PTREQ      {Ptreq}
| STRUCTEQ   {Structeq}
| NOT        {Not}
| LESS       {Less}
| GREATER    {Greater}
| LESSEQ     {Lesseq}
| GREATEREQ  {Greatereq}
| ISINT      {Isint}
| ISBOOL     {Isbool}
| ISNIL      {Isnil}
| ISPAIR     {Ispair}
| ISFN       {Isfn}
| ISCHAR     {Ischar}
| ISSTRING   {Isstring}
| ISINDESC   {Isindesc}
| ISOUTDESC  {Isoutdesc}
| CONS       {Cons}
| CAR        {Car}
| CDR        {Cdr}
| SETCAR     {Setcar}
| SETCDR     {Setcdr}
| OPENIN     {Openin}
| OPENOUT    {Openout}
| CLOSEIN    {Closein}
| CLOSEOUT   {Closeout}
| FLUSHOUT   {Flushout}
| GETCHAR    {Getchar}
| GETSTRING  {Getstring}
| PUTCHAR    {Putchar}
| PUTSTRING  {Putstring}
| FGETCHAR   {Fgetchar}
| FGETSTRING {Fgetstring}
| FPUTCHAR   {Fputchar}
| FPUTSTRING {Fputstring}
| PRINT      {Print}
| NEWSTRING  {Newstring}
| SIZES      {Sizes}
| SUBS       {Subs}
| SETS       {Sets}
| CHR        {Chr}
| ORD        {Ord}
| CURRENTIN  {Currentin}
| CURRENTOUT {Currentout}
| CALLWIN    {Callwin}
| CALLWOUT   {Callwout}
| WINFILE    {Winfile}
| WOUTFILE   {Woutfile}
| ISEOF      {Iseof}

;
exp:
  INT                                { Int ($1) } 
| STRING                             { String ($1) }
| NIL                                { Nil }
| TRUE                               { True }
| FALSE                              { False }
| IDENT                              { Var($1) }
| STDIN                              { Stdin }
| STDOUT                             { Stdout }
| STDERR                             { Stderr }
| lambda                             { Lambda($1) }
| LPAREN LIST exps RPAREN            { make_list $3 }
| QUOTE LPAREN RPAREN                { make_list [] }
| QUOTE LPAREN exps RPAREN           { make_list $3 }
| LPAREN op exps RPAREN              { Op($2,$3) }
| LPAREN LET LPAREN binds RPAREN exp RPAREN { Let($4,$6) }
| LPAREN LETREC LPAREN lambdabinds RPAREN exp RPAREN { Letrec($4,$6) }
| LPAREN IF exp exp exp RPAREN       { If ( $3, $4, $5 ) } 
| LPAREN COND exppairs RPAREN    { make_if $3 }
| LPAREN AND exp exp RPAREN          { And ( $3, $4 ) } 
| LPAREN OR  exp exp RPAREN          { Or ( $3, $4 ) }
| LPAREN exps RPAREN                 { let (h,t) = split_it($2) in App(h,t) }
| LPAREN BEGIN exps RPAREN           { Seq($3) }
| LPAREN RPAREN                      { Nil }
;
lambda:
  LPAREN LAMBDA LPAREN vars RPAREN exps RPAREN { ($4,Seq($6)) }
;
vars:
          { [] }
| IDENT vars { $1 :: $2 }
;
binds:
          { [] }
| LPAREN IDENT exp RPAREN binds  { ($2,$3) :: $5 }
;
exppairs:
  LPAREN ELSE exp RPAREN         { [(True,$3)] }
| LPAREN exp exp RPAREN exppairs { ($2,$3)::$5 }
;
lambdabinds:
         { [] }
| LPAREN IDENT lambda RPAREN lambdabinds { ($2,$3)::$5 }
;
