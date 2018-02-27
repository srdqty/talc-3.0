/**********************************************************************
 * (c) Greg Morrisett, Neal Glew, Chris Hawblitzel, Dan Grossman      *
 *     June 1998, all rights reserved.                                *
 **********************************************************************/

%{

(* Type variables vs. named types is no disambiguated in the type-checker, so
   we never create a VarType or a NamedType at parse time.  We always create
   a MutableTyp(UnresolvedTyId ...).
 *)

(* Does unary minus inneficiently -- should be trivial update to abstract
   syntax
 *)

open Numtypes;;
open Popsyntax;;

module SS = 
  Smallset.Make(struct type t=string let compare = compare end)
module X = Poperr;;

type type_modifier =
    ModArray of exp option
  | ModParams of convention * string list * (string option * typ) list

type pid_or_builtin = 
    Pid of string
  | Array_id
(* LR *)
  | Rep_id
(* end LR *)
(* exncons *)
  | Exncon_id
(* end exncons *)

type exp_or_fun = 
    Init_exp of exp option ref
  | Init_block of stmt

let locus () = Gcdfec.seg_symbol ();;
let err e = (Gcdfec.post_error (X.mk_parse_error e))
let parse_error s = err (X.Syntax s)
let abort s = (parse_error s; raise Gcdfec.Exit)
let debug s = Format.print_string s; Format.print_newline ()

let name_or_builtin pa ps = 
  match pa,ps with
    Pid n,ps -> MutableTyp(ref (UnresolvedTyId(n,ps)))
  | Array_id,[p] -> ArrayType(p,None)
  | Array_id,_ -> abort "expecting one type argument for array"
(* LR *)
  | Rep_id, [p] -> RepType(p)
  | Rep_id,_ -> abort "expecting one type argument for rep"
(* end LR *)
  | Exncon_id, [p] -> ExnconType(p)
  | Exncon_id, _ -> abort "expecting one type argument for exncon"

let rec make_type t ms =
  match ms with
    [] -> t
  | ModArray(eopt)::tl -> make_type (ArrayType(t,eopt)) tl
  | (ModParams (c,tvs,tps)):: tl ->
      let args = List.map (fun (_,tp) -> tp) tps in
      make_type (FnType(c,tvs,t,args)) tl

(* Take a list of type_modifiers, and a return_type
 * Returns complete return_type, args, and tyvars
 *
 * The modifiers come in outermost to innermost order.
 * The function declaration is the last element of the list.
 * The ones outside that go with the return type.
 *)
let make_func_decl ms ret_typ =
  let (arg_mod,ret_mods) =
    let rec aux ms=
      match ms with
	[] -> abort "Illformed declaration."
      |	[hd] -> (hd,[])
      | hd::tl -> let (a',ms') = aux tl in (a',hd::ms')
    in
    aux ms
  in
  let (c,tyvars,args) =
    match arg_mod with
      ModArray _ -> abort "Illformed declaration."
    | ModParams(c,ts,ps) ->
	let proc_arg (so,t) =
	  match so with
	    None -> abort "Variables in function declarations require names."	
	  | Some v -> (v,t)
	in
	let args = List.map proc_arg ps in
	(c,ts,args)
  in
  begin 
    (c,tyvars, make_type ret_typ ret_mods, args)
  end

type switch_clause = 
    AnyClause
  | IntClauses of (int32 * stmt) list
  | CharClauses of (char * stmt) list
  | UnionClauses of (field_name * Popsyntax.pattern * stmt) list;;

let add_int_clause i s (scs,d) = 
  match scs with
    IntClauses cs -> (IntClauses ((i,s)::cs),d)
  | AnyClause -> (IntClauses [(i,s)],d)
  | _ -> err X.SwitchClausesDontMatch; (scs,d)
;;

let add_char_clause c s (scs,d) = 
  match scs with
    CharClauses cs -> (CharClauses ((c,s)::cs),d)
  | AnyClause -> (CharClauses [(c,s)],d)
  | _ -> err X.SwitchClausesDontMatch; (scs,d)
;;

let add_union_clause f pat s (scs,d) = 
  match scs with
    UnionClauses cs -> (UnionClauses ((f,pat,s)::cs),d)
  | AnyClause -> (UnionClauses [(f,pat,s)],d)
  | _ -> err X.SwitchClausesDontMatch; (scs,d)
;;

let convert_union_clause (f,p,s) =
  { arm_field = f; arm_pat = p; arm_body = s };;
let convert_union_clauses = List.map convert_union_clause;;

let make_exp re = { exp_typ = None; raw_exp = re; exp_loc = locus();
                    exp_un_after = mt_varset; };;
let make_stmt rs = { raw_stmt = rs; stmt_loc = locus ();
                     un_after = mt_varset; un_before = mt_varset; } ;;
let make_binop lft oper rgt = make_exp (Primop(oper, [lft; rgt]));;
 
let make_extern_val vtel =
  begin
    let rec aux vtel a = 
      match vtel with
	[] -> List.rev a
      |	(v,t,e) :: tl ->
	  begin match !e,t with 
	    Some _,_ -> err X.ExternNoInit
          | _,Evar _ -> err X.ExternNoType
	  | None,_ -> ()
	  end;
	  aux tl ((v,t) :: a)
    in
    aux vtel []
  end

let make_var_decl vd stmt =
  (List.fold_right (fun (x,t,eopt) s -> make_stmt(Decl(x,t,eopt,s))) vd stmt)

let make_global_decl scope (x,t,eopt) =
  (GlobalDecl(scope,x,t,eopt),locus())
;;

(********************************************************************)
(* compilation of printf                                            *)
(********************************************************************)
type printf_desc = 
    Printf_literal of string | Printf_s_int | Printf_u_int | Printf_x_int
  | Printf_f_float | Printf_g_double | Printf_char | Printf_string
;;
(* explode a string into a list of chars *)
let explode (s : string) : char list = 
  let rec expl (i : int) (lis : char list) = 
    if i >= 0 then expl (i-1) ((String.get s i)::lis) else lis
  in expl ((String.length s) - 1) []
;;
(* implode a list of chars into a string *)
let implode chars = 
  let s2 = String.create (List.length chars) in
  let rec loop i chars = 
    match chars with 
      [] -> s2 
    | c::rest -> (String.set s2 i c; loop (i+1) rest) in
  loop 0 chars
;;

(* make expression and statement utilities *)
let make_seq s1 s2 = make_stmt (Seq(s1,s2));;
let skip_stmt() = make_stmt Skip;;
(* return an expression that calls some function whose name is f *)
let make_call_exp f args = 
   make_exp(FunCall(make_exp (Var f),ref None,args)) 
;;
(* return a statement that calls some function whose name is f *)
let make_call_stmt f args = make_stmt (Exp (make_call_exp f args))
;;
(* return a statement that calls fprint_int(fexp,intexp). *)
let print_int_stmt fexp iexp = 
   make_call_stmt "fprint_int" [fexp;iexp]
;;
(* return a statement that calls fprint_uint(fexp,intexp). *)
let print_uint_stmt fexp iexp = 
   make_call_stmt "fprint_uint" [fexp;iexp]
;;
let print_float_stmt fexp iexp =
  make_call_stmt "fprint_float" [fexp;iexp]
;;
let print_double_stmt fexp iexp =
  make_call_stmt "fprint_double" [fexp;iexp]
;;
(* return a statement that calls fprint_uint(fexp,intexp). *)
let print_hex_stmt fexp iexp = 
   make_call_stmt (add_prefix "Core" "fprint_hex") [fexp;iexp]
;;
(* return a statement that calls fprint_char(fexp,charexp). *)
let print_char_stmt fexp cexp = 
   make_call_stmt "fprint_char" [fexp;cexp]
;;
(* return a statement that calls fprint_string(fexp,strexp). *)
let print_string_stmt fexp strexp = 
   make_call_stmt "fprint_string" [fexp;strexp]
;;
(* Break a string s into a list of printf_descs.  For example,
 * "foo %d bar %c" would become the list
 * [Printf_literal "foo "; Printf_int; Printf_literal " bar "; 
 *  Printf_char].  *)
let printf_descriptors s =
   (* chars is the list of characters we're parsing.  string_chars are
    * the non-formatting characters seen thus far (between two formatting
    * characters), and descs is the reversed list of descriptors seen
    * thus far. *)
  let rec loop chars string_chars descs = 
    match chars with
      ('%'::'%'::rest) -> loop rest ('%'::string_chars) descs
    | ('%'::c::rest) ->
        let descs = 
          (match string_chars with [] -> descs | cs -> 
            (Printf_literal (implode (List.rev cs)))::descs) in
        (match c with
          'd' -> loop rest [] (Printf_s_int::descs)
        | 'u' -> loop rest [] (Printf_u_int::descs)
        | 'x' -> loop rest [] (Printf_x_int::descs)
        | 's' -> loop rest [] (Printf_string::descs)
        | 'c' -> loop rest [] (Printf_char::descs)
	| 'f' -> loop rest [] (Printf_f_float :: descs)
	| 'g' -> loop rest [] (Printf_g_double :: descs)
        | _ -> abort ("bad format character in printf string"))
    | c::rest -> loop rest (c::string_chars) descs
    | [] -> 
	let descs = 
	  (match string_chars with [] -> descs | cs ->
	    (Printf_literal (implode (List.rev cs)))::descs) in
	List.rev descs
  in loop (explode s) [] []

(* Given printf descriptor string, an expression fdexp that is the
 * file descriptor, and args which is a list of expressions to pass
 * to printf, generate the primitive statements needed to accomplish
 * the printf.  For instance, printf("foo: %d %s",i,g) generates
 * fprint_string(std_out,"foo: "); fprint_int(std_out,i); 
 * fprint_string(std_out,g).  
 *)
let expand_printf fdexp desc_string args = 
  let descs = printf_descriptors desc_string in
  let rec gen_stmts ds args = 
    match ds,args with
      [],[] -> []
    | (Printf_literal s)::ds,args ->
   	let sexp = make_exp (Const(String s)) in
   	let stmt = print_string_stmt fdexp sexp in
	stmt :: (gen_stmts ds args)
    | Printf_s_int :: ds, e :: args -> 
	let stmt = print_int_stmt fdexp e in
	stmt :: (gen_stmts ds args)
    | Printf_u_int :: ds, e :: args -> 
	let stmt = print_uint_stmt fdexp e in
	stmt :: (gen_stmts ds args)
    | Printf_x_int :: ds, e :: args -> 
	let stmt = print_hex_stmt fdexp e in
	stmt :: (gen_stmts ds args)
    | Printf_f_float :: ds, e :: args ->
	let stmt = print_float_stmt fdexp e in
	stmt :: (gen_stmts ds args)
    | Printf_g_double :: ds, e :: args ->
	let stmt = print_double_stmt fdexp e in
	stmt :: (gen_stmts ds args)
    | Printf_char :: ds, e :: args ->
	let stmt = print_char_stmt fdexp e in
	stmt :: (gen_stmts ds args)
    | Printf_string :: ds, e :: args ->
	let stmt = print_string_stmt fdexp e in
	stmt :: (gen_stmts ds args) 
    |  [], _ -> abort ("too many arguments to printf")
    |  _, [] -> abort ("not enough arguments to printf")
  in let stmts = gen_stmts (printf_descriptors desc_string) args in
  List.fold_right make_seq stmts (skip_stmt())
;;

let null_exp = make_exp(Const Null);;

(* similar, but for sprintf.  Here, we just append all of the strings
 * via String::strconcat_l. *)
let expand_sprintf desc_string args = 
  let descs = printf_descriptors desc_string in
  let cons_exp(hd,tl) = make_call_exp (add_prefix "List" "cons") [hd; tl] in
  let rec gen_exps ds args = 
  match ds,args with
    [],[] -> null_exp
  | (Printf_literal s)::ds,args ->
    let sexp = make_exp (Const(String s)) in
    cons_exp(sexp,gen_exps ds args)
  | Printf_s_int :: ds, e :: args -> 
    let sexp = make_call_exp (add_prefix "Core" "string_of_int") [e] in
    cons_exp(sexp,gen_exps ds args)
  | Printf_u_int :: ds, e :: args -> 
    let sexp = make_call_exp (add_prefix "Core" "string_of_uint") [e] in
    cons_exp(sexp,gen_exps ds args)
  | Printf_x_int :: ds, e :: args -> 
    let sexp = make_call_exp (add_prefix "Core" "hex_string_of_uint") [e] in
    cons_exp(sexp,gen_exps ds args)
  | Printf_f_float :: ds, e :: args ->
      let sexp = make_call_exp (add_prefix "Core" "string_of_float") [e] in
      cons_exp(sexp,gen_exps ds args)
  | Printf_g_double :: ds, e :: args ->
      let sexp = make_call_exp (add_prefix "Core" "string_of_double") [e] in
      cons_exp(sexp,gen_exps ds args)
  | Printf_char :: ds, e :: args ->
    let sexp = make_call_exp (add_prefix "Core" "string_of_char") [e] in
    cons_exp(sexp,gen_exps ds args)
  | Printf_string :: ds, e :: args ->
    cons_exp(e,gen_exps ds args)
  |  [], _ -> abort ("too many arguments to sprintf")
  |  _, [] -> abort ("not enought arguments to sprintf") in
  make_call_exp (add_prefix "String" "strconcat_l") [gen_exps descs args]
;;
%}

%token <string> ID
%token <Numtypes.int32> CONSTINT 
%token <int> TUPLEOFFSET
%token <bool> CONSTBOOLEAN
%token <string> CONSTSTRING
%token <char> CONSTCHAR
%token EOF

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET LESSTHAN
%token GREATERTHAN LESSTHANEQ GREATERTHANEQ PLUSPLUS MINUSMINUS
%token PLUSEQUAL MINUSEQUAL TIMESEQUAL DIVEQUAL AMPERAMPER BACKQUOTE
%token PIPEPIPE EQUALS EE NE PLUS MINUS TIMES DIV BANG QUESTION
%token COLON SEMICOLON DOT COMMA AMPER PIPE LESSLESS GREATERGREATER PERCENT
%token AT ABSTYPE WITH ARRAY
%token RDTSC CDECL STDCALL

%token AMPEREQUAL PIPEEQUAL LESSLESSEQ GREATERGREATEREQ MODEQUAL TILDE 
%token GREATERGREATERGREATER GREATERGREATERGREATEREQ CARET CARETEQUAL

%token ARROW EXCEPTION EXN HANDLE RAISE TRY CATCH FINALLY FUN UNDERSCORE

%token PREFIX COLONCOLON OPEN
%token BOOL BREAK CASE CHAR CHR CONST CONTINUE DEFAULT ELSE EXTERN FOR IF 
%token BYTE SHORT INT NEW NEWARRAY NULL ORD RETURN SIGNED SIZE STATIC 
%token STRING STRUCT SWITCH UNION UNSIGNED VOID WHILE
%token DO PRIVATE PUBLIC ABSTRACT PRINTF FPRINTF SPRINTF
/* Cyclone */
%token CODEGEN CUT SPLICE FILL
/* End Cyclone */
/* Dynamic Loader */
%token DLSYM
/* End Dynamic Loader */
/* LR */
%token REPTYP REPTERM
/* End LR */
%token EXNCON

/* Floating Point */
%token <Numtypes.f32> CONSTFLOAT
%token <Numtypes.f64> CONSTDOUBLE
%token FLOAT DOUBLE

%right PLUSPLUS MINUSMINUS TILDE BANG
%left  TIMES DIV PERCENT
%left  PLUS MINUS
%left  LESSLESS GREATERGREATER GREATERGREATERGREATER
%left  LESSTHAN LESSTHANEQ GRWATERTHAN GREATERTHANEQ
%left  EE NE
%left  AMPER
%left  CARET
%left  PIPE
%left  AMPERAMPER
%left  PIPEPIPE
%right EQUALS PLUSEQUAL MINUSEQUAL TIMESEQUAL DIVEQUAL MODEQUAL 
%right AMPEREQUAL PIPEEQUAL CARETEQUAL
%right LESSLESSEQ GREATERGREATEREQ GREATERGREATERGREATEREQ

%type <Popsyntax.top_decl list> top

%start top

%%

top:
    top_decls EOF           { List.rev($1) }
  ;

top_decls:
  | top_decl                        { $1                             }
  | PREFIX pid SEMICOLON top_decls  { [(PrefixDecl($2,$4),locus ())] }
  | OPEN pid SEMICOLON top_decls    { [(OpenDecl($2,$4),locus ())]   }
  | top_decl top_decls              { $1 @ $2                        }
  ;

top_decl:  
  | func_decl               { [$1] }
  | struct_decl             { [$1] }
  | union_decl              { [$1] }
  | abs_decl                { [$1] }
  | exn_decl                { [$1] }
  | prefix_decl             { [$1] }
  | open_decl               { [$1] }
  | extern                  { $1   }
  | global_var_decl         { $1   }
  ;

prefix_decl:
  | PREFIX pid LBRACE top_decls RBRACE  { (PrefixDecl ($2,$4),locus ()) }
  ;

open_decl:
  | OPEN pid LBRACE top_decls RBRACE    { (OpenDecl ($2,$4),locus ()) }
  ;

func_decl:
  | static_opt prim_type pid type_mods block 
      { let (c,tyvars,ret_typ,args) = make_func_decl $4 $2 in
      let arg_typs = List.map snd args in
      (FunDecl { fn_static   = $1;
		 fn_convention = c;
		 fn_name     = $3;
		 fn_tyvars   = tyvars;
		 fn_ret_type = ret_typ;
		 fn_args     = args;
		 fn_body     = $5
	       }, locus ())
      } 
  ;

struct_decl:
  | scope_opt question_opt STRUCT type_params  pid LBRACE struct_field_decls RBRACE
                       { (StructDecl { st_scope = $1;
				       st_name = $5;
				       st_tyvars = $4;
				       st_possibly_null = $2;
				       st_fields = $7}, locus()) }
  ;

union_decl:
  | scope_opt question_opt UNION type_params pid LBRACE union_field_decls RBRACE
                       {   (UnionDecl { un_scope = $1;
				      	un_name = $5;
					un_tyvars = $4;
				      	un_possibly_null = $2;
				      	un_fields = $7}, locus ()) }
  ;

abs_decl:
  | scope_opt ABSTYPE type_params pid etype_params EQUALS param_decl SEMICOLON
      { (AbsDecl { abs_scope = $1;
		   abs_name = $4;
		   abs_all_tyvars = $3;
		   abs_exist_tyvars = $5;
		   abs_defn = snd($7) }, locus ()) }
  ;

exn_decl:
  | scope_opt EXCEPTION pid LPAREN param_decl RPAREN SEMICOLON
      { (ExceptionDecl($3,$1,snd $5),locus()) }
  | scope_opt EXCEPTION pid SEMICOLON
      { (ExceptionDecl($3,$1,VoidType),locus()) }
  ;
	  
global_var_decl:
  | static_opt prim_type init_list SEMICOLON
      { let scope = if $1 then Public else Static in
      (List.map (make_global_decl scope) 
	 (let t = $2 in let il = $3 in
	 List.map (fun (x,b,e) -> (x,make_type t b,e)) il))
      }
  ; 

type_params:
  | /* empty */         { [] }
  | LESSTHAN tyvar_list { $2 }
  ;

tyvar_list:
  | id GREATERTHAN  { [$1] }
  | id COMMA tyvar_list { $1 :: $3 }
  ;

etype_params:
  | LBRACKET etyvar_list  { $2 }
  ;

etyvar_list:
  | id RBRACKET { [$1] }
  | id COMMA etyvar_list { $1 :: $3 }
  ;

type_mods:
  | /* empty */                             { [] }
  | type_mods LBRACKET RBRACKET { ModArray(None)::($1) }
  | type_mods LBRACKET exp RBRACKET{ ModArray(Some($3))::($1)}
  | type_mods convention type_params LPAREN RPAREN      { (ModParams($2,$3,[]))::$1 }
  | type_mods convention type_params LPAREN param_list RPAREN { (ModParams($2,$3,$5))::$1 }
  ;

param_list:
    param_decl                    { [($1)] }
  | param_decl COMMA param_list   { ($1)::($3) }
  ;

param_decl:
    prim_type id type_mods  { (Some($2), make_type ($1) ($3)) }
  | prim_type type_mods     { (None, make_type ($1) ($2)) }
  ;

named_param_list:
    named_param_decl                          { [($1)] }
  | named_param_decl COMMA named_param_list   { ($1)::($3) }
  ;

named_param_decl:
    prim_type id type_mods  { ($2, make_type ($1) ($3)) }
  ;

num_type:
  | INT    { B4 }
  | SHORT  { B2 }
  | BYTE   { B1 }
  ;

signed_type:
  | SIGNED num_type    { IntType (true ,$2) }
  | UNSIGNED num_type  { IntType (false,$2) }
  | num_type           { IntType (true ,$1) }
  ;

times_or_percent:
  | TIMES              { ReadWrite }
  | PERCENT            { ReadOnly }
  ;

prim_type:
  | VOID                           { VoidType                             }
  | signed_type                    { $1                                   }
  | BOOL                           { BooleanType                          }
  | CHAR                           { CharType                             }
  | STRING                         { StringType                           }
  | FLOAT                          { FloatType                            }
  | DOUBLE                         { DoubleType                           }
  | times_or_percent LPAREN RPAREN  
      { TupleType ($1,[]) }
  | times_or_percent LPAREN param_list RPAREN 
      { TupleType ($1,List.map snd $3) }
  | LESSTHAN param_list GREATERTHAN pid_or_builtin 
      { name_or_builtin $4 (List.map snd $2) }
  | LESSLESS param_list GREATERTHAN pid_or_builtin GREATERTHAN 
      pid_or_builtin
      { name_or_builtin $6 [name_or_builtin $4 (List.map snd $2)] }
  | LESSLESS param_list GREATERTHAN pid_or_builtin COMMA param_list 
      GREATERTHAN pid_or_builtin
      { name_or_builtin $8 
	  ((name_or_builtin $4 (List.map snd $2))::List.map snd $6) }
  | EXN                            { ExnType                    }
  | pid  { MutableTyp(ref (UnresolvedTyId($1,[]))) }
  ;

prim_type_or_wild:
    prim_type  { $1 }
  | UNDERSCORE { Evar(Any,ref None) }
;

pid_or_builtin:
    pid    { Pid($1) }
  | ARRAY  { Array_id }
/* LR */
  | REPTYP { Rep_id }
/* end LR */
  | EXNCON { Exncon_id }
;

extern:
    EXTERN var_decl
      { (List.map (fun (v,t) -> (ExternVal(v,t),locus())) 
	   (make_extern_val $2)) }
  | EXTERN pid question_opt type_params SEMICOLON             
      { [ExternType ($2,$4,$3),locus()] }
  ;
static_opt:
                        { true }
  | STATIC              { false }
  ;

scope_opt:
    STATIC            { Static   }
  | ABSTRACT          { Abstract }
  | EXTERN            { Extern   }
  | PUBLIC	      { Public   }
  |                   { Public   }
  ;

question_opt:
                       { false }
  | QUESTION           { true }
  ;

constopt:
          { ReadWrite }
  | CONST { ReadOnly }
  ;

iddecl:
    prim_type ibl  { let t = $1 in 
                   let idbs : (string * type_modifier list) list = $2 in
                   List.map(fun (x,b) -> (x,make_type t b)) idbs }
  ;

ibl:
    id type_mods             { [($1,$2)] }
  | id type_mods COMMA ibl   { ($1,$2)::$4 }
  ;

struct_field_decls:
                                          { [] }
  | struct_field_decl struct_field_decls  { $1 @ $2 }
  ;

struct_field_decl:
    constopt iddecl SEMICOLON   { let rw = $1 in 
				  List.map (fun (x,t) -> (x,rw,t)) $2 }
  ;

union_field_decls:
                                        { [] }
  | union_field_decl union_field_decls  { $1 @ $2 }
  ;

union_field_decl:
    iddecl SEMICOLON   { $1 }
  ;

block:
    LBRACE stmts RBRACE  { $2 }
  ;

stmts:
    stmt  { $1 }
  | var_decl stmts { make_var_decl $1 $2 }
  | id COLON stmts { make_stmt(Label($1,$3)) }
  | stmt stmts { make_stmt (Seq($1,$2)) }
  ;

var_decl:
    prim_type_or_wild local_init_list SEMICOLON
    { let t = $1 in let il = $2 in
      List.map 
      (fun (x,b,e_or_f) ->
	match e_or_f with
	  Init_exp(e) -> (x,make_type t b,e)
	| Init_block(s) -> 
	    let (c,tyvars,ret_typ,args) = make_func_decl b t in
	    let arg_typs = List.map snd args in
	    let e = make_exp (Fun { fn_static = true;
				    fn_convention = c;
				    fn_name = x;
				    fn_tyvars = tyvars;
				    fn_ret_type = ret_typ;
				    fn_args = args;
				    fn_body = s }) in
	    (x,make_type t b,ref(Some e))) il }
  ; 

local_init_list:
    local_init_var                        { [$1] }
  | local_init_var COMMA local_init_list  { $1 :: $3 }
  ;

local_init_var:
    pid type_mods             { ($1,$2,Init_exp(ref None)) }
  | pid type_mods EQUALS comma_free_exp  { ($1,$2,Init_exp(ref (Some $4))) }
  | pid type_mods block       { ($1,$2,Init_block($3)) }
  ;

init_list:
    init_var                        { [$1] }
  | init_var COMMA init_list  { $1 :: $3 }
  ;

init_var:
    pid type_mods             { ($1,$2,ref None) }
  | pid type_mods EQUALS comma_free_exp  { ($1,$2,ref (Some $4)) }
  ;

stmt:
    stmt_no_trailing              { $1 }
  | IF LPAREN exp RPAREN stmt
      { make_stmt (IfThenElse ($3, $5, make_stmt Skip)) }
  | IF LPAREN exp RPAREN stmt_no_short_if ELSE stmt   
      { make_stmt (IfThenElse ($3, $5, $7)) }
  | WHILE LPAREN exp RPAREN stmt  { make_stmt (While ($3, $5)) }
  | for_stmt { $1 }
/* Cyclone */
  | CUT stmt { make_stmt (Cut($2)) }
  | SPLICE stmt { make_stmt (Splice($2)) }
/* End Cyclone */
  | TRY stmt HANDLE pid stmt { make_stmt (TryHandle($2,$4,$5)) }
  | TRY stmt CATCH LBRACE switch_clauses RBRACE FINALLY stmt
      { let (cs,d) = $5 in
        match cs with
	  UnionClauses cs -> 
            make_stmt (TryCatchFinally($2,convert_union_clauses cs,d,Some $8))
	| _ -> abort "case is not an exception pattern within catch"
      }	
  | TRY stmt_no_short_if FINALLY stmt
      { make_stmt (TryCatchFinally($2,[],None,Some $4)) }
  | WITH id etype_params EQUALS exp DO stmt 
      { make_stmt (With($2,ref None,$3,$5,$7)) }
  ;

stmt_no_short_if:
    stmt_no_trailing    { $1 }
  | IF LPAREN exp RPAREN stmt_no_short_if ELSE stmt_no_short_if   
      { make_stmt (IfThenElse ($3, $5, $7)) }
  | WHILE LPAREN exp RPAREN stmt_no_short_if  { make_stmt (While ($3, $5)) }
  | for_stmt_no_short_if { $1 }
/* Cyclone */
  | CUT stmt_no_short_if { make_stmt (Cut($2)) }
  | SPLICE stmt_no_short_if { make_stmt (Splice($2)) }
/* End Cyclone */
  | TRY stmt HANDLE pid stmt_no_short_if { make_stmt (TryHandle($2,$4,$5)) }
  | TRY stmt CATCH LBRACE switch_clauses RBRACE FINALLY stmt_no_short_if
      { let (cs,d) = $5 in
        match cs with
	  UnionClauses cs -> 
            make_stmt (TryCatchFinally($2,convert_union_clauses cs,d,Some $8))
	| _ -> abort "case is not an exception pattern within catch"
      }	
  | TRY stmt FINALLY stmt_no_short_if
      { make_stmt (TryCatchFinally($2,[],None,Some $4)) }
  | WITH id etype_params EQUALS exp DO stmt_no_short_if 
      { make_stmt (With($2,ref None,$3,$5,$7)) }
  ;

for_stmt:
  | FOR LPAREN exp_opt SEMICOLON bool_exp_opt SEMICOLON exp_opt RPAREN stmt
      { make_stmt (For ($3,$5,$7,$9)) }
  | FOR LPAREN var_decl bool_exp_opt SEMICOLON exp_opt RPAREN stmt
      { make_var_decl $3 (make_stmt (For (make_exp Nop,$4,$6,$8))) }
  ;

for_stmt_no_short_if:
  | FOR LPAREN exp_opt SEMICOLON bool_exp_opt SEMICOLON exp_opt RPAREN stmt_no_short_if
      { make_stmt (For ($3,$5,$7,$9)) }
  | FOR LPAREN var_decl bool_exp_opt SEMICOLON exp_opt RPAREN stmt_no_short_if
      { make_var_decl $3 (make_stmt (For (make_exp Nop,$4,$6,$8))) }
  ;

exp_opt:
  |     { make_exp Nop }
  | exp { $1           }
  ;
bool_exp_opt:
  |     { make_exp (Const (Bool true)) }
  | exp { $1                           }
  ;
switch_clauses:
                          { (AnyClause,None) }
  | DEFAULT COLON stmts   { (AnyClause,Some $3) }
  | CASE pid LPAREN prim_pat RPAREN COLON stmts switch_clauses 
      { add_union_clause $2 (Prim_pat $4) $7 $8 }
  | CASE pid TIMES LPAREN RPAREN COLON stmts switch_clauses
      { add_union_clause $2 (Tuple_pat []) $7 $8 }
  | CASE pid TIMES LPAREN prim_pat_list RPAREN COLON stmts switch_clauses
      { add_union_clause $2 (Tuple_pat $5) $8 $9 }
  | CASE pid COLON stmts switch_clauses 
      { add_union_clause $2 No_pat $4 $5 }
  | CASE CONSTINT COLON stmts switch_clauses
      { add_int_clause $2 $4 $5 }
  | CASE MINUS CONSTINT COLON stmts switch_clauses
      { add_int_clause (i32_0 -$ $3) $5 $6 }
  | CASE CONSTCHAR COLON stmts switch_clauses
      { add_char_clause $2 $4 $5 }
  ;

stmt_no_trailing:
    block                 { $1 }
  | RETURN SEMICOLON      { make_stmt (Return None) }
  | RETURN exp SEMICOLON  { make_stmt (Return (Some $2)) }
  | BREAK id_opt SEMICOLON       { make_stmt  (Break    $2) }
  | CONTINUE id_opt SEMICOLON    { make_stmt  (Continue $2) }
  | PRINTF LPAREN const_string RPAREN SEMICOLON
      { expand_printf (make_exp(Var "tal_stdout")) $3 [] }
  | PRINTF LPAREN const_string COMMA exps RPAREN SEMICOLON
      { expand_printf (make_exp(Var "tal_stdout")) $3 $5 }
  | FPRINTF LPAREN pid COMMA const_string RPAREN SEMICOLON
      { expand_printf (make_exp (Var $3)) $5 [] }
  | FPRINTF LPAREN pid COMMA const_string COMMA exps RPAREN SEMICOLON
      { expand_printf (make_exp (Var $3)) $5 $7 }
  | exp SEMICOLON         { make_stmt (Exp $1) }
  | SEMICOLON             { make_stmt  Skip }
  | SWITCH exp LBRACE switch_clauses RBRACE  
      { let (cs,d) = $4 in
        let must_init d =
	  match d with
	    Some s -> s
	  | None -> err (X.Syntax "switch requires default"); make_stmt Skip 
	in
        let rs = match cs with
	  IntClauses cs -> IntSwitch($2,cs,must_init d)
	| CharClauses cs -> CharSwitch($2,cs,must_init d)
        | UnionClauses cs -> UnionSwitch($2,List.map convert_union_clause cs,d)
	| AnyClause ->
	    err (X.Syntax "No non-default clauses in switch");
	    raise Parsing.Parse_error
	in
	make_stmt rs
      }	
  | DO stmt WHILE LPAREN exp RPAREN SEMICOLON    { make_stmt (Do($2,$5)) }
  | TRY stmt CATCH LBRACE switch_clauses RBRACE 
      { let (cs,d) = $5 in
        match cs with
	  UnionClauses cs -> 
            make_stmt (TryCatchFinally($2,convert_union_clauses cs,d,None))
	| _ -> abort "case is not an exception pattern within catch"
      }	
  | RDTSC exp COLON exp SEMICOLON
      { make_stmt (Rdtsc ($2, $4)) }
  ;

exp1:
    LPAREN exp RPAREN        { $2 }
  | pid                      { make_exp (Var $1) }
  | CONSTINT                 { make_exp (Const(Int $1)) }
  | CONSTBOOLEAN             { make_exp (Const(Bool $1)) }
  | const_string              { make_exp (Const(String $1)) }
  | CONSTCHAR                { make_exp (Const(Char $1)) }
  | CONSTFLOAT               { make_exp (Const(Float $1)) }
  | CONSTDOUBLE              { make_exp (Const(Double $1)) }
  | LBRACE COLON prim_type type_mods RBRACE
      { make_exp (ConstArray ([],Some(make_type $3 $4))) }
  | LBRACE exps RBRACE   { make_exp (ConstArray ($2,None)) }
  | NULL                     { make_exp (Const(Null)) }
  | SIZE LPAREN exp RPAREN   { make_exp (Primop(Size,[$3])) }
  | ORD LPAREN exp RPAREN    { make_exp (Primop(Ord,[$3])) }
  | CHR LPAREN exp RPAREN    { make_exp (Primop(Chr,[$3])) }
  | RAISE pid LPAREN RPAREN  { make_exp (Raise(make_exp (NewExn ($2,None)))) }
  | RAISE pid LPAREN exp RPAREN {make_exp (Raise(make_exp (NewExn($2,Some $4))))}
  | RAISE LPAREN exp RPAREN { make_exp (Raise $3) }
/* Cyclone */
  | CODEGEN LPAREN prim_type pid convention LPAREN named_param_list RPAREN type_mods block RPAREN
      { make_exp (Codegen { fn_static = false;
			    fn_convention = $5;
			    fn_name = $4;
			    fn_tyvars = [];
			    fn_ret_type = make_type $3 $9;
			    fn_args = $7;
			    fn_body = $10 }) }
  | CODEGEN LPAREN prim_type pid convention LPAREN RPAREN type_mods block RPAREN
      { make_exp (Codegen { fn_static = false;
			    fn_convention = $5;
			    fn_name = $4;
			    fn_tyvars = [];
			    fn_ret_type = make_type $3 $8;
			    fn_args = [];
			    fn_body = $9 }) }
  | FILL LPAREN exp RPAREN   { make_exp (Fill $3) }
/* End Cyclone */
  | FUN prim_type prim_pat type_mods block
      { let name = (match $3 with Var_pat(x,_) -> x | Wild_pat _ -> "_")
        in let (c,tyvars,ret_typ,args) = make_func_decl $4 $2 in
        let arg_typs = List.map snd args in
        make_exp (Fun { fn_static = true;
			fn_convention = c;
                        fn_name = name;
                        fn_tyvars = tyvars;
                        fn_ret_type = ret_typ;
                        fn_args = args;
                        fn_body = $5})
      }	
  | SPRINTF LPAREN const_string RPAREN { make_exp (Const(String $3)) }
  | SPRINTF LPAREN const_string COMMA exps RPAREN
      { expand_sprintf $3 $5 }
  | exp1 LPAREN RPAREN             { make_exp (FunCall ($1, ref None, []))   }
  | exp1 LPAREN exps RPAREN    { make_exp (FunCall ($1, ref None, $3))   }
  | exp1 AT LESSTHAN param_list GREATERTHAN           
                                   { make_exp (TypInst ($1,List.map snd $4)) }
  | exp1 AT LESSLESS param_list GREATERTHAN pid_or_builtin GREATERTHAN
      { make_exp (TypInst($1,[name_or_builtin $6 (List.map snd $4)])) }
  | exp1 AT LESSLESS param_list GREATERTHAN pid_or_builtin COMMA param_list GREATERTHAN
      { make_exp (TypInst($1,(name_or_builtin $6 (List.map snd $4))::(List.map snd $8))) }
/* LR */
  | REPTERM /* should always be part of type application */
      { make_exp RepTerm }
/* end LR */
  | exp1 LBRACKET exp RBRACKET     { make_exp (Subscript($1,$3))           }
  | exp1 DOT id                    { make_exp (StructMember ($1, $3))      }
  | exp1 TUPLEOFFSET              { make_exp (TupleMember($1,$2)) }
  ;

new_kw:
  | NEW   { () }
  | CARET { () }
  ;

exp2:
  | exp1                                { $1 }
  | new_kw pid LPAREN RPAREN               
      { make_exp (NewStruct ($2,ref None,[])) }
  | new_kw pid LPAREN exps RPAREN      
      { make_exp (NewStruct ($2,ref None,List.map (fun e -> None,e) $4)) }
  | new_kw pid LBRACE RBRACE
      { make_exp (NewStruct ($2,ref None,[])) }
  | new_kw pid LBRACE labelled_exps RBRACE
      { make_exp (NewStruct ($2,ref None,$4)) }
  | new_kw pid DOT id LPAREN exp RPAREN    
      { make_exp (NewUnion($2,ref None,$4,Some $6)) }
  | new_kw pid DOT id                      
      { make_exp (NewUnion($2,ref None,$4,None)) }
  | new_kw DOT id LPAREN exp RPAREN
      { make_exp (NewUnion("",ref None,$3,Some $5)) }
  | new_kw DOT id                      
      { make_exp (NewUnion("",ref None,$3,None)) }
  | new_kw LPAREN RPAREN                  { make_exp (NewTuple([])) }
  | new_kw LPAREN exps RPAREN         { make_exp (NewTuple($3)) }
  ;

exp3:
    exp2              { $1 }

  | exp3 PLUSPLUS      { 
    make_exp(Primop(Minus,
		    [make_exp(AssignOp($1,Some Plus, 
				       make_exp(Const(Int i32_1))));
		      make_exp(Const(Int i32_1))]))}
  | exp3 MINUSMINUS      { 
    make_exp(Primop(Plus,
		    [make_exp(AssignOp($1,Some Minus,
				       make_exp(Const(Int i32_1))));
		     make_exp(Const(Int i32_1))]))}
  | MINUSMINUS exp3 { make_exp(AssignOp($2,Some Minus,
					make_exp(Const(Int i32_1)))) }
  | PLUSPLUS exp3   { make_exp(AssignOp($2,Some Plus, 
					make_exp(Const(Int i32_1)))) }
  | PLUS exp3       { $2 }
      /* make this negate but requires change to abstract syntax! */
  | MINUS exp3      
      { match $2.raw_exp with
      	Const(Int i) -> make_exp (Const(Int (i32_0 -$ i)))
      |	Const(Float f) -> make_exp (Const(Float (neg_f32 f)))
      |	Const(Double d) -> make_exp (Const(Double (neg_f64 d)))
      |	_ -> make_exp(Primop(Minus, [make_exp (Const(Int i32_0)); $2])) 
      }
  | TILDE exp3      { make_exp(Primop(Bitnot,[$2])) }
  | BANG exp3         { make_exp (Primop(Not,[$2])) }
  | AMPER exp2	      { make_exp (Primop(AddrOf,[$2])) }
  ;

exp35:
  | exp3     { $1 }
  | LPAREN COLON prim_type type_mods RPAREN exp35 
             { make_exp(Cast (make_type $3 $4,$6)) }
  ;

exp4:
    exp35             { $1 }
  | exp4 exp4op exp35 { make_binop $1 $2 $3 }
  ;
exp5:
    exp4             { $1 }
  | exp5 exp5op exp4 { make_binop $1 $2 $3 }
  ;
exp6:
    exp5             { $1 }
  | exp6 exp6op exp5 { make_binop $1 $2 $3 }
  ;
exp7:
    exp6              { $1 }
  | exp7 exp7op exp6  { make_binop $1 $2 $3 }
  ;
exp8:
    exp7              { $1 }
  | exp8 exp8op exp7  { make_binop $1 $2 $3 }
  ;
exp9:
    exp8              { $1 }
  | exp9 exp9op exp8  { make_binop $1 $2 $3 }
  ;
exp10:
    exp9              { $1 }
  | exp10 exp10op exp9  { make_binop $1 $2 $3 }
  ; 
exp11:
    exp10              { $1 }
  | exp11 exp11op exp10  { make_binop $1 $2 $3 }
  ;

exp4op:
    TIMES                        {Times}
  | DIV                          {Div}
  | PERCENT                      {Mod}
  ;
exp5op:
    PLUS                         {Plus}
  | MINUS                        {Minus}
  ;
exp6op:
    GREATERGREATER               {Bitarshift}
  | LESSLESS                     {Bitlshift}
  | GREATERGREATERGREATER        {Bitlrshift}
  ;
exp7op:
    LESSTHAN                     {Lt}
  | GREATERTHAN                  {Gt}
  | LESSTHANEQ                   {Lte}
  | GREATERTHANEQ                {Gte}
  ;
exp8op:
    EE                           {Eq}
  | NE                           {Neq}
  ;
exp9op:
    AMPER                        {Bitand}
  ;
exp10op:
    CARET                        {Bitxor}
  ;
exp11op:
    PIPE                         {Bitor}
  ;

exp12: 
    exp11             { $1 }
  | exp12 AMPERAMPER exp11    
       { make_exp (Conditional($1,$3,make_exp(Const(Bool false)))) }
  ;
exp13:
    exp12            { $1 }
  | exp13 PIPEPIPE exp12
       { make_exp (Conditional($1,make_exp(Const(Bool true)),$3)) }
  ;
exp14:
    exp13                           { $1 }
  | exp13 QUESTION exp COLON exp14  { make_exp (Conditional ($1, $3, $5)) }
  ;

exp15:
    exp14                { $1 }
  | exp14 exp15op exp15  { make_exp(AssignOp($1,Some $2,$3)) }
  | exp14 EQUALS exp15   { make_exp(AssignOp($1,None   ,$3)) }
  ;

exp15op:
  | PLUSEQUAL               {Plus}
  | MINUSEQUAL              {Minus}
  | TIMESEQUAL              {Times}
  | DIVEQUAL                {Div}
  | MODEQUAL                {Mod}
  | AMPEREQUAL              {Bitand}
  | PIPEEQUAL               {Bitor}
  | CARETEQUAL              {Bitxor}
  | LESSLESSEQ              {Bitlshift}
  | GREATERGREATEREQ        {Bitarshift}
  | GREATERGREATERGREATEREQ {Bitlrshift}
  ;

comma_free_exp:
  | exp15 { $1 }
  ;

exps:
  | comma_free_exp  { [$1] }
  | comma_free_exp COMMA exps { $1 :: $3 }
  ;
 
exp:
  | comma_free_exp { $1 }
  | comma_free_exp COMMA exps { make_exp (SeqExp ($1 :: $3)) }
  ;

labelled_exps:
  | id EQUALS comma_free_exp { [(Some $1,$3)] }
  | id EQUALS comma_free_exp COMMA labelled_exps { (Some $1,$3)::$5 }
  ;

prim_pat_list:
  | prim_pat   { [$1] }
  | prim_pat COMMA prim_pat_list { $1::$3 }
;

prim_pat:
  | id { Var_pat($1,ref VoidType) }
  | UNDERSCORE { Wild_pat(ref VoidType) }
;

convention:
  |         { default_convention }
  | STDCALL { Stdcall }
  | CDECL   { Cdecl}
;

id_opt:
  |    { None    }
  | id { Some $1 }
  ;

id: 
  | ID { $1 }
  ;

pid: 
  | ID { $1 }
  | ID COLONCOLON pid { add_prefix $1 $3 }
  ;
  
const_string:
  | CONSTSTRING { $1 }
  | CONSTSTRING const_string { $1 ^ $2 }
  ;
