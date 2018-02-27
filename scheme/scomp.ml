(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Stephanie Weirich, Dan Grossman     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Nil and False are now distinct *)

open Numtypes;;
open Identifier;;
open Tal;;
open Sast;;
open Sil;;

exception Unimplemented of string;;
exception Error of string;;
let unimp s = raise (Unimplemented s);;
let die s = raise (Error s);;

let tapp c = Tapp (Con c) (* Dan: for annotation hack *)

let string_of_op op = 
  begin
    match op with
      (* arithmatic ops *)
      Plus -> "+" 
    | Minus -> "-" 
    | Times -> "*" 
    | Div -> "/" 
      (* comparisons *)
    | Inteq -> "=" 
    | Ptreq -> "eq?"
    | Structeq -> "equal?"
    | Not -> "not" 
    | Less -> "<" 
    | Greater -> ">" 
    | Lesseq -> "<="
    | Greatereq -> ">=" 
      (* type predicates *)
    | Isint -> "int?" 
    | Isbool -> "bool?" 
    | Isnil -> "nil?"
    | Ispair -> "pair?" 
    | Isfn -> "procedure?" 
    | Ischar -> "char?"
    | Isstring -> "string?"
    | Isindesc -> "input-port?"
    | Isoutdesc -> "output-port?"
      (* lists *)
    | Cons -> "cons" 
    | Car -> "car"
    | Cdr -> "cdr" 
    | Setcar -> "set-car!" 
    | Setcdr -> "set-cdr!"
      (* I/O *)
    | Openin -> "open-input-file"
    | Openout -> "open-output-file"
    | Closein -> "close-input-port"
    | Closeout -> "close-output-port"
    | Flushout -> "flush-out"
    | Getchar -> "read-char"
    | Peekchar -> "peek-char"
    | Getstring -> "get-string"
    | Putchar -> "put-char"
    | Putstring -> "put-string"
    | Fgetchar -> "fget-char"
    | Fpeekchar -> "fpeek-char"
    | Fgetstring -> "fget-string"
    | Fputchar -> "fput-char"
    | Fputstring -> "fput-string"
    | Print -> "write"
    | Currentin -> "current-input-port"
    | Currentout -> "current-output-port"
    | Callwin -> "call-with-input-file"
    | Callwout -> "call-with-output-file"
    | Winfile -> "with-input-from-file"
    | Woutfile -> "with-output-to-file"
    | Iseof -> "eof-object?"
      (* String ops *)
    | Newstring -> "newstring" 
    | Sizes -> "string-length" 
    | Subs -> "string-ref"
    | Sets -> "string-set!"
      (* Char ops *)
    | Chr -> "integer->char"
    | Ord -> "char->integer"
  end;;

(* various error labels to jump to when coercions don't work out *)
let exit_label = id_of_string "_tal_exit";;
let openin_label = id_of_string "_scopen_in";;
let openout_label = id_of_string "_scopen_out";;
let closein_label = id_of_string "_scclose_in";;
let closeout_label = id_of_string "_scclose_out";;
let flushout_label = id_of_string "_scflush_out";;
let fgetchar_label = id_of_string "_scfgetchar";;
let fpeekchar_label = id_of_string "_scfpeekchar";;
let fputchar_label = id_of_string "_scfputchar";;
let fgetstring_label = id_of_string "_scfgetstring";;
let fputstring_label = id_of_string "_scfputstring";;
let getchar_label = id_of_string "_scgetchar";;
let peekchar_label = id_of_string "_scpeekchar";;
let putchar_label = id_of_string "_scputchar";;
let getstring_label = id_of_string "_scgetstring";;
let putstring_label = id_of_string "_scputstring";;
let get_stdin_label = id_of_string "_scgetstdin";;
let get_stdout_label = id_of_string "_scgetstdout";;
let get_stderr_label = id_of_string "_scgetstderr";;
let current_in_port_label = id_of_string "_currentInPort";;
let current_out_port_label = id_of_string "_currentOutPort";;
let push_in_port_label = id_of_string "_pushInPort";;
let push_out_port_label = id_of_string "_pushOutPort";;
let pop_in_port_label = id_of_string "_popInPort";;
let pop_out_port_label = id_of_string "_popOutPort";;
let newstring_label = id_of_string "_scnewstring";;
let newstringchar_label = id_of_string "_scnewstringchar";;
let print_label = id_of_string "_scprint";;
let not_int_label = id_of_string "_notInt";;
let not_fn_label  = id_of_string "_notFn";;
let not_pair_label = id_of_string "_notPair";;
let not_char_label = id_of_string "_notChar";;
let not_indesc_label = id_of_string "_notIndesc";;
let not_outdesc_label = id_of_string "_notOutdesc";;
let not_string_label = id_of_string "_notString";;

let tal_main = id_of_string "_tal_main";;
let tal_main_con = ccode_l [(Esp,csptr cempty)];;

(********************************************************************)
(* type definitions                                                 *)
(********************************************************************)
let raw_int_c = cbyte4;;
let raw_bool_c = chptr [i32_0;i32_1] None None;;
let array_size_var = id_of_string "size";;
let raw_string_c = carray_s array_size_var (cfield (pcbytes Byte1) ReadWrite);;
let gen_arity f =
  let rec gen i = if i > max_args then [] else (f i)::(gen (i+1))
  in gen 0;;
(* Intuitively, we're using the following datatype definition:
 *  datatype dyn = False | True | Int of int | Pair of (dyn,dyn) | 
 *     Fn0 of () -> dyn | Fn1 of (dyn) -> dyn | Fn2 of (dyn,dyn) -> dyn |
 *     ... | Fnmax_args of (dyn,...,dyn) -> dyn
 *
 *  Where (dyn1,...,dynn) -> dyn is short-hand for:
 *   Exists 'a:K4byte.
 *   *[All 'r:Kstack.{Esp:sptr {Eax:dyn,Esp:'r}::r,Ebx:*['a,dyn1,...,dynn]},'a]
 *
 * Hence:
 *  False = Tag 0  (rolled and injected properly)
 *  True =  Tag 1
 *  Int of int = *[Tag(0),int]
 *  Pair of (dyn,dyn) = *[Tag(1),dyn,dyn]
 *  Fn0 of () -> dyn = *[Tag(2),()->dyn]
 *  Fn1 of (dyn)->dyn = *[Tag(3),(dyn)->dyn]
 *  Fn2 of (dyn,dyn)->dyn = *[Tag(4),(dyn,dyn)->dyn]
 *  ...
 *  Fnk of (dyn,...,dyn)->dyn = *[Tag(k+2),(dyn,...,dyn)->dyn]
 *
 * We must generate a bunch of type abbreviations to get this to work
 * out.  We generate the following definitions:
 *
 * *** NG - This is no longer correct.
 *
 * code type for closures that abstracts the argument (ebx) 
 *   let c = fn D:K4 => fn a:K4 => All r:KS.{Esp:sptr {Eax:D,Esp:r}::r, Ebx:a}
 * code types (of varying arity) that abstracts the environment 
 *   let c0 = fn D:K4 => fn e:K4 => c D *[e]
 *   let c1 = fn D:K4 => fn e:K4 => c D *[e,D]
 *   let c2 = fn D:K4 => fn e:K4 => c D *[e,D,D]
 *      ...
 *   let ck = fn D:K4 => fn e:K4 => c D *[e,D,...,D]
 * closure type that abstracts the code 
 *   let cl = fn c:K4->K4 => fn D:K4 => Exist env:K4.*[c D env,env]
 * abstract closures of varying arity
 *   let cl0 = cl c0
 *   let cl1 = cl c1
 *     ...
 *   let clk = cl ck
 * the function that generates the universal type D 
 *   let aD = 
 *     fn D:K4 => +[S(0),S(1),S(0)*[B4^4],S(1)*[D,D],
 *                  S(2)*[cl0 D],S(3)*[cl1 D],S(4)*[cl2 D],...S(k+2)*[clk D]]
 * the universal type D 
 *   let D = rec(D:K4=>aD D)
 * the unrolled universal type D 
 *   let uD = aD D
 * now apply the code and closure abstractions to D shadowing abstractions:
 *   let c0 = c0 D
 *   let c1 = c1 D
 *     ...
 *   let ck = ck D
 *   let cl0 = cl0 D
 *   let cl1 = cl1 D
 *    ...
 *   let clk = clk D
 *)
(* tags *)
let int_tag = i32_0;;
let pair_tag = i32_1;;
let string_tag = i32_2;;
let char_tag = i32_3;;
let indesc_tag = i32_4;;
let outdesc_tag = int_to_int32 5;;
let first_closure_tag = int_to_int32 6
let closure_tag arity = 
  if arity >= 0 & arity <= max_args then
    (int_to_int32 arity) +$ first_closure_tag
  else
    bug ("closure_tag with "^(string_of_int arity)^" args");;
(* type variables *)
let dyn_l = id_of_string "D";;
let env_v = id_of_string "env";;  
let stack_v = id_of_string "r";;
let code_vs = gen_arity (fun i -> id_of_string ("c"^(string_of_int i)));;
(*let code_abs_v = id_of_string "c";;  *)
(*let closure_abs_v = id_of_string "cls";;*)
(*let closure_vs = gen_arity (fun i -> id_of_string ("cl"^(string_of_int i)));;*)
(*let abs_dyn_v = id_of_string "aD";;*)
(*let udyn_v = id_of_string "uD";;*)
(* type variables as constructors *)
let lvar = cvar
let dyn_c = clab dyn_l;;
let env_c = cvar env_v;;
let stack_c = cvar stack_v;;
(*let code_abs_c = cvar code_abs_v*)
let code_cs = List.map clab code_vs;;
(*let closure_abs_c = cvar closure_abs_v*)
let closure_cs =
  let aux v =
    cexist
      env_v
      k4byte
      (cprod_b [ cfield (capp (clab v) env_c) Read; cfield env_c Read ]) in
  List.map aux code_vs;;
(*let abs_dyn_c = cvar abs_dyn_v;;*)
(*let udyn_c = cvar udyn_v;;*)
(* see scheme.tali for definitions of variables *)

(********************************************************************)
(* compilation environment                                          *)
(********************************************************************)
(* As we're compiling, we keep track of three things:
 *  (a) the TAL type of the environment which is always held in
 *         register Ebx
 *  (b) the TAL type of the stack which is always held in Esp
 *  (c) a map from Scheme variables to paths off of the environment
 *        register.  Paths consist of a "depth" and an "offset".
 *  (d) the depth of the environment, this is so we can unroll the top level
 *      environment.
 *)
type env = { env_type : con;
	     var_map  : (string,int * int32) Dict.dict;
	     depth : int;
	     stack_type : con
	   } 

let empty_env = 
  { env_type = defcon(Cprod []);
    var_map = Dict.empty compare;
    depth = 0;
    stack_type = stack_c
  } 

let env_to_con regs env =
  let esp_c = csptr env.stack_type in
  let ebx_c = env.env_type in
  cforall stack_v Kstack (ccode_l ([(Esp,esp_c);(Ebx,ebx_c)]@regs))
;;

(********************************************************************)
(* code generation utilities                                        *)
(********************************************************************)
(* list of code blocks (in reverse order) *)
let code_blocks : code_block list ref = ref [];;
(* an optional variable for giving lambdas a good name *)
let current_var : var option ref = ref None;;
let set_current_var x = current_var := (Some x);;
let clean x =
  let x = String.copy x in
  let rec loop i = 
    if i < String.length x then
      let c = String.get x i in
      let j = Char.code c in
      let k = 
	if (j >= Char.code 'a' & j <= Char.code 'z') or
	   (j >= Char.code 'A' & j <= Char.code 'Z') then j
	else if (j = Char.code '-') then Char.code '_'
	else Char.code 'X'
      in (String.set x i (Char.chr k); loop (i+1))
    else ()
  in (loop 0; x)
let lambda_label() = 
  match !current_var with
    None -> id_new "f"
  | Some x -> id_new (clean x)
(* current label whose instructions are in current_instrs *)
let current_label : (identifier * con) ref = ref (tal_main,tal_main_con);;
(* instructions kept in reverse order *)
let current_instrs : instruction list ref = ref [];;

let reset_generator () =
  code_blocks := [];
  current_var := None;
  current_label := (tal_main,tal_main_con);
  current_instrs := [];
  ()
;;

(* emit one instruction *)
let emit i = current_instrs := i::(!current_instrs);;
let print_comments = ref true;;
let comment s = if (!print_comments) then emit(Comment("\t"^s)) else ();;

let negate_cc cc = 
  match cc with
    Tal.Eq -> Tal.NotEq
  | Tal.NotEq -> Tal.Eq
  | Tal.GreaterEq -> Tal.Less
  | Tal.Greater -> Tal.LessEq
  | Tal.LessEq -> Tal.Greater
  | Tal.Less -> Tal.GreaterEq
  | _ -> bug "optimizer found unexpected condition code"

let rec optimize is =
  match is with
    [] -> []
  | ((Mov(Reg Edx,(Reg Eax,[])))::
     (Mov(Reg Eax,(Immed i,[Tosum bc])))::
     (Cmp(Reg Edx,Reg Ecx))::
     (Setcc(cc,Reg Eax))::
     (Mov(Reg Eax,(Reg Eax,[RollTosum dc])))::
     (Mov(Reg Eax,(Reg Eax,[Unroll])))::
     (Btagi(Eax,j,falselab,cond))::rest) when i=$i32_0 & j=$i32_0 ->
       optimize((Cmp(Reg Eax,Reg Ecx))::(Jcc (negate_cc cc,falselab))::rest)
  | (Mov(Reg Eax,cgop))::(Push(Reg Eax,[]))::rest ->
      optimize((Push cgop)::rest)
  | (Mov(Reg Eax,(gop,c1)))::(Mov(Reg Eax,(Reg Eax,c2)))::rest ->
      optimize((Mov(Reg Eax,(gop,c2 @ c1)))::rest)
  | ((Mov(Reg Eax,(gop1,c))) as i)::(((Mov(gop2,(Reg Eax,[])))::rest) as is) ->
      (match gop1 with
	Prjr _ -> i::(optimize is)
      |	Prjl _ -> i::(optimize is)
      |	_ -> optimize((Mov(gop2,(gop1,c)))::rest))
  | (ArithBin (Add,Reg Esp,Immed i1))::
    (ArithBin (Add,Reg Esp,Immed i2))::is ->
      optimize ((ArithBin (Add,Reg Esp,Immed (i1+$i2)))::is)
  | i::rest -> i::(optimize rest)

let flush_code() =
  let instrs = Array.of_list (optimize(List.rev (!current_instrs))) in
  let (cl,clc) = !current_label in
  begin
    code_blocks := (cl,Some clc,instrs)::(!code_blocks);
    current_instrs := []
  end
(* emit a label -- this packages up the current label and list of
 * instructions as a code block, pushes that code block on the
 * list of code blocks, and starts a new instruction list. *)
let emit_lab env l regs = 
  let l_con = env_to_con regs env in
  begin
    flush_code();
    current_label := (l,l_con)
  end
(* functions yet to be generated *)
let functions : (env * identifier * ((var * tipe) list) * exp) list ref =
  ref []
let push_code env lab args e = functions := (env,lab,args,e) :: (!functions)
(* strings yet to be generated *)
let strings : (identifier * string) list ref = ref []
let push_string s =
  let l = id_new "s" in begin strings := (l,s)::(!strings); l end
let get_strings() = 
  let get_string (l,s) =
    let len = int_to_int32 (String.length s) in
    (l,None,([ D4bytes (len,[]); Dup; Dbytes(Raw, s) ],
	     [ Pack (pcint len, raw_string_c);
	       Toarray (i32_4,0,cfield (pcbytes Byte1) ReadWrite) ])) in
  List.map get_string !strings
;;

let init_field c offset = 
  match c.rcon with
    Chptr ([],Some {rcon=Cprod cs},_) ->
      let rec inf cs offset = 
	match cs,offset with
	  {rcon=Cfield (c,Uninit)}::cs,i when i=$i32_0 ->
	    (cfield c ReadWrite)::cs
	| c::cs,i when i>$i32_0 -> c::(inf cs (i-$i32_1))
	| _,_ -> die "init_field: mismatch" in
      cprod_b (inf cs offset)
  | _ -> die "init_field: not a product"

let initenv env v offset =
  let var_map = Dict.insert env.var_map v (0,offset) in
  let env_type = init_field env.env_type offset in
  { env_type = env_type;
    var_map = var_map;
    depth = env.depth;
    stack_type = env.stack_type
  } 
    
(* push a coerced genop of type c on the stack and return an
 * updated compilation environment.
 *)
let push env cgop c =
  begin
    emit (Push cgop);
    { env_type = env.env_type;
      var_map = env.var_map;
      depth = env.depth;
      stack_type = ccons c env.stack_type
    } 
  end
(* pop a value from the stack into genop and return an updated
 * compilation environment.
 *)
let pop env gop =
  begin
    emit (Pop gop);
    { env_type = env.env_type;
      var_map = env.var_map;
      depth = env.depth;
      stack_type = 
        match env.stack_type.rcon with
	  Ccons (_,c) -> c
	| _ -> bug "pop" }
  end
(* assuming the object environment is in Ebx, move the frame at depth 
 * d into Eax in order to lookup a variable
 *)
let get_frame env i =
  let rec next_frame i = 
    if i = 0 then () else
    begin
      emit (Mov(Reg Eax,(Prjr((Eax,[]),i32_0),[])));
      next_frame(i-1)
    end in
  begin
    if i = 0 then
      emit (Mov(Reg Eax,(Reg Ebx,[])))
    else 
      begin
	emit (Mov(Reg Eax,(Prjr((Ebx,[]),i32_0),[])));
    	next_frame(i-1)
      end;
    if i=env.depth then
      emit (Tal.Coerce (Eax,[Unroll]))
  end

(* coercion to take a tagged value to the dynamic type *)
let dyn_coercion = [RollTosum dyn_c];;
(* standard coercion for a branch (within a function) *)
let branch_coercion = [tapp stack_c]
(* coercion for branch to error label *)
let err_coercion env = [tapp env.stack_type]
(* coercion for a call *)
let call_coercion env = [tapp env.stack_type]

(********************************************************************)
(* code generation                                                  *)
(********************************************************************)
(* Invariants:
 *   1. each expression returns a value of type dyn_c in Eax.
 *   2. the current environment is in Ebx.
 *   3. the environment is an n-tuple frame (n >= 1) where the 
 *        first component is a pointer to the previous lexically-enclosing
 *        frame and the rest of the components are the variables defined
 *        at that depth.
 *   4. env contains:
 *         (a) the current type of the stack
 *         (b) a map from variables to (depth * offset) where the depth
 *               is the depth of the frame (0 = current frame) and offset 
 *               is the word offset within the frame.
 *         (c) the type of the current frame
 *)       
exception DoesTailCall

(* code for doing an external function call *)
let do_call0 env lab =
  begin
    emit(Push(Reg Ebx,[]));  (* save env *)
    emit(Call(Addr lab,[tapp(ccons env.env_type env.stack_type)]));
    emit(Pop(Reg Ebx))       (* throw away argument *)
  end

let do_call1 env lab =
  begin
    emit(Push(Reg Ebx,[]));  (* save environment *)
    emit(Push(Reg Eax,[]));  (* push argument *)
    emit(Call(Addr lab,[tapp(ccons env.env_type env.stack_type)]));
    emit(Pop(Reg Ebx));      (* throw away argument *)
    emit(Pop(Reg Ebx))       (* restore environment *)
  end

let do_call2 env lab =
  begin
    emit(Pop(Reg Ecx));   (* shuffle argument 1 into reg *)
    emit(Push(Reg Ebx,[]));  (* save environment *)
    emit(Push(Reg Eax,[]));  (* push argument 2 *)
    emit(Push(Reg Ecx,[]));  (* push argument 1 *)
    emit(Call(Addr lab,[tapp(ccons env.env_type env.stack_type)]));
    emit(Pop(Reg Ebx));      (* throw away argument 1 *)
    emit(Pop(Reg Ebx));      (* throw away argument 2 *)
    emit(Pop(Reg Ebx))       (* restore environment *)
  end

(* code for building a boxed and tagged object - kills eax *)
let make_obj tag genop c =
  begin
    emit (Mov (Reg Ebp,(genop,[])));
    emit (Push (Reg Ebx,[]));
    emit (Malloc(i32_8,malloc_prod [csing (pcint tag);c],None));
    emit (Pop (Reg Ebx));
    emit (Mov(Prjr((Eax,[]),i32_0),(Immed tag,[])));        (* put in tag *)
    emit (Mov(Prjr((Eax,[]),i32_4),(Reg Ebp,[])));        (* put in value *)
    emit (Tal.Coerce (Eax,dyn_coercion))              (* coerce to dyn_c *)
  end

(* code for checking (and refining) types *)
let check_tag env tag error_label =
  begin
    emit (Mov(Reg Eax,(Reg Eax,[Unroll])));
    emit (Btagi(Eax,min_pointer_integer,(error_label,err_coercion env),
		Tal.Below));
    emit (Btagvar(Eax,i32_0,tag,(error_label,err_coercion env),Tal.NotEq))
  end
let check_tag_extract env tag error_label = 
  begin
    check_tag env tag error_label;
    emit (Mov(Reg Eax,(Prjr((Eax,[Fromsum]),i32_4),[])))
  end
let check_pair env = 
  begin
    check_tag env pair_tag not_pair_label;
    emit (Mov(Reg Eax,(Reg Eax,[Fromsum])))
  end
let check_int env = check_tag_extract env int_tag not_int_label
let check_string env = check_tag_extract env string_tag not_string_label
let check_char env = check_tag_extract env char_tag not_char_label
let check_indesc env = check_tag_extract env indesc_tag not_indesc_label
let check_outdesc env = check_tag_extract env outdesc_tag not_outdesc_label
let check_fn arity env = 
  check_tag env (closure_tag arity) not_fn_label

let rec check_tag_bool env tag tagstring e =
  let false_lab = id_new ("not_"^tagstring) in
  begin
    comp_exp false env e;
    comment("is Eax a (tagged & boxed) "^tagstring^"?");
    emit (Mov(Reg Ecx,(Reg Eax,[Unroll])));
    emit (Mov(Reg Eax,(Immed i32_0,dyn_coercion)));
    emit (Btagi(Ecx,min_pointer_integer,(false_lab,branch_coercion),
		Tal.Below));
    emit (Btagvar(Ecx,i32_0,tag,(false_lab,branch_coercion),Tal.NotEq));
    comment("Eax is a (tagged & boxed) "^tagstring);
    emit (Mov(Reg Eax,(Immed i32_1,dyn_coercion)));
    emit (Fallthru[stack_c]);
    emit_lab env false_lab [(Eax,dyn_c)]
  end
and comp_exps env exps =
  match exps with
    [] -> env
  | (e::rest) -> 
      begin
	comp_exp false env e;
	comp_exps (push env (Reg Eax,[]) dyn_c) rest
      end
and comp_and_set_exps offset env exps vars =
  match (exps,vars) with
    [],[] -> env
  | (e::rest),(v::vrest) ->
      begin
	comp_exp false env e;
	emit (Mov(Prjr((Ebx,[]),offset*$i32_4),(Reg Eax,[])));
	let env = initenv env v offset in
	comp_and_set_exps (offset+$i32_1) env rest vrest
      end
  | _,_ -> die "comp_and_set_exps"
and comp_exp tailcall env (exp,tipe) =
  match exp with
    Int i -> emit (Mov(Reg Eax,(Immed i,[])))
  | String s -> 
      begin
      	let l = push_string s in
      	emit (Mov(Reg Eax,(Addr l,[])));
      end
  | Char c -> emit (Mov(Reg Eax,(Immed(int_to_int32 (Char.code c)),[])))
  | Nil -> emit   (Mov(Reg Eax,(Immed i32_2,dyn_coercion)))
  | True -> emit  (Mov(Reg Eax,(Immed i32_1,dyn_coercion)))
  | False -> emit (Mov(Reg Eax,(Immed i32_0,dyn_coercion)))
  | Stdin -> 
      begin
	do_call0 env get_stdin_label
      end
  | Stdout -> 
      begin
	do_call0 env get_stdout_label
      end
  | Stderr -> 
      begin
	do_call0 env get_stderr_label
      end
  | Var x ->
      begin
	comment("lookup variable "^x);
	try 
	  let (depth,offset) = Dict.lookup env.var_map x in
	  if depth != 0 or env.depth=0 then
	    begin
	      get_frame env depth;  (* Eax points to correct frame *)
              (* get variable *)
	      emit (Mov(Reg Eax,(Prjr((Eax,[]),offset*$i32_4),[]))) 
	    end
	  else emit (Mov(Reg Eax,(Prjr((Ebx,[]),offset*$i32_4),[]))) 
	with Dict.Absent -> die (x ^ ": unbound variable")
      end
  | Set(x,e) ->
      begin
	comment("set variable "^x);
	try
	  let (depth,offset) = Dict.lookup env.var_map x in
	  comp_exp false env e;   (* Eax contains value of e *)
	  if depth != 0 or env.depth=0 then
	    begin
	      emit (Mov(Reg Ecx,(Reg Eax,[]))); (* save it in Ecx *)
	      get_frame env depth;  (* Eax points to correct frame *)
              (* set variable *)
	      emit (Mov(Prjr((Eax,[]),offset*$i32_4),(Reg Ecx,[]))); 
	      comp_exp false env (False,D_t)
	    end
	  else emit (Mov(Prjr((Ebx,[]),offset*$i32_4),(Reg Eax,[])))
	with Dict.Absent -> die (x ^ ": (set) unbound variable")
      end
  | Lambda(args,e) ->
      (* if we have too many arguments, break it into multiple lambdas *)
      let num_args = List.length args in
      if num_args > max_args then
	bug "too many arguments for Lambda in scomp"
      else
      begin
	let lab = lambda_label() in
	let tag = closure_tag num_args in
	let env_c = env.env_type in
	let code_c = capp (List.nth code_cs num_args) env_c in
	let closure_c = List.nth closure_cs num_args in
	begin
	  push_code env lab args e;    (* generate the code later *)
	  comment("*** begin build closure***");
	  comment("build raw closure");
	  emit(Push (Reg Ebx,[]));
	  emit(Malloc(i32_8,malloc_prod [code_c; env_c],None));
	  comment("store code");
	  emit(Mov(Prjr((Eax,[]),i32_0),(Addr lab,[Roll code_c])));
	  comment("store env");
	  emit(Pop (Reg Ebx));
	  emit(Mov(Prjr((Eax,[]),i32_4),(Reg Ebx,[])));
	  emit(Tal.Coerce (Eax,[Pack(env_c,closure_c)]));
	  comment("***end build closure***")
	end
      end
  | App(e,es) ->
      (* if we have too many arguments, break it into multiple applications *)
      let num_args = List.length es in
      if num_args > max_args then
	bug "Too many arguments in Application"
      else 
      begin
	let cs = env_c::(List.map (fun _ -> dyn_c) es) in
	let tag = closure_tag num_args in
	let num_args = int_to_int32 num_args in
	let rec copy_args i =
	  if (i =$ i32_0) then () else
	  begin
	    emit (Pop(Reg Ecx));
	    emit (Mov(Prjr((Eax,[]),i*$i32_4),(Reg Ecx,[])));
	    copy_args (i-$i32_1);
	  end in
	let env' = comp_exps env es in                    (* args on stack *)
        comp_exp false env' e;                            (* closure in eax *)
	comment("***begin apply closure***");
	emit (Unpack(env_v,Ebp,(Prjr((Eax,[Fromsum]),i32_4),[])));
         (* unpack closure *)
	comment("build new frame for closure");
	emit (Push (Reg Ebx,[]));
         (* allocate frame *)
	emit (Malloc(i32_4+$(num_args*$i32_4),malloc_prod cs,None));
	emit (Pop (Reg Ebx));
	comment("link in closure's environment");
	emit (Mov(Reg Ecx,(Prjr((Ebp,[]),i32_4),[])));  (* get closure's env *)
	emit (Mov(Prjr((Eax,[]),i32_0),(Reg Ecx,[])));  (* save in frame *)
	comment("move arguments from stack into frame");
	copy_args num_args;                            (* save args in frame *)
	if tailcall then
	  begin
	    comment("install closure's environment");
	    emit (Mov(Reg Ebx,(Reg Eax,[])));
	    comment("get code of closure");
	    emit (Mov(Reg Eax,(Prjr((Ebp,[]),i32_0),[])));
	    comment("tail call");
	    emit (Jmp(Reg Eax,[tapp (stack_c);Unroll]));
	    raise DoesTailCall
	  end
	else 
	  begin
	    comment("save current environment");
	    let env' = push env (Reg Ebx,[]) env.env_type in
	    comment("install closure's environment");
	    emit (Mov(Reg Ebx,(Reg Eax,[])));              
	    comment("call closure -- result in Eax");
	    emit (Mov(Reg Eax,(Prjr((Ebp,[]),i32_0),[])));
	    emit (Call(Reg Eax,(call_coercion env')@[Unroll]));
	    comment("restore current environment");
	    emit (Pop(Reg Ebx));                           
	    comment("***end apply closure***")
	  end
      end
  | Op (op,es) -> comp_op env op es 
  | Let(binds,e) ->
      let (vars,es) = List.split binds in
      let num_vars = int_to_int32 (List.length vars) in
      let field_tys = List.map (fun _ -> cfield dyn_c Uninit) vars in
      let env_ty = cprod_b ((cfield env.env_type Read)::field_tys) in
      let malloc_arg =
 	malloc_prod (env.env_type::(List.map (fun _ -> dyn_c) field_tys)) in
      let vmap = Dict.map_dict (fun (d,o) -> (d+1,o)) env.var_map in
      let env =
 	{ env_type=env_ty; var_map=vmap; depth=env.depth+1;
	  stack_type=env.stack_type } in
      comment("build let frame");
      emit(Mov (Reg Ebp,(Reg Ebx,[])));
      emit(Malloc(i32_4 *$ num_vars +$ i32_4,malloc_arg,None));
      emit(Mov(Prjr((Eax,[]),i32_0),(Reg Ebp,[])));
      emit(Mov(Reg Ebx,(Reg Eax,[])));
      comment("initialize let-bound variables");
      let env' = comp_and_set_exps i32_1 env es vars in
      comment("body of the let");
      comp_exp tailcall env' e;
      emit(Mov(Reg Ebx,(Prjr((Ebx,[]),i32_0),[])))
	(* DJG: Nil is no longer false *)
(*  | If((Op(Isnil,[e1]),_),e2,e3) -> comp_exp tailcall env (If(e1,e3,e2),tipe)*)
  | If((Op(Not,[e1]),_),e2,e3) -> comp_exp tailcall env (If(e1,e3,e2),tipe)
  | If(e1,e2,e3) ->
      begin
	let false_lab = id_new "ifFalse" in
	let end_lab = id_new "ifEnd" in
	comp_exp false env e1;
	comment("if null/false goto "^(id_to_string false_lab));
	emit (Mov(Reg Eax,(Reg Eax,[Unroll])));
	emit (Btagi(Eax,i32_0,(false_lab,branch_coercion),Tal.Eq));
	comment("true branch");
	let e2_returns = 
	  try 
	    comp_exp tailcall env e2;
	    if tailcall then
	      (emit (Retn None); false)
	    else 
	      (emit (Jmp(Addr end_lab,branch_coercion)); true)
	  with DoesTailCall -> false in  (* omit Jmp if e2 is a tail call *)
	comment("false branch");
	emit_lab env false_lab [];
	let e3_returns = 
	  try 
	    comp_exp tailcall env e3;
	    if tailcall then
	      (emit (Retn None); false)
	    else 
	      (emit (Fallthru[stack_c]); true)
	  with DoesTailCall -> false in
	if e2_returns or e3_returns then
	  begin
	    emit_lab env end_lab [(Eax,dyn_c)];
	    comment("end if")
	  end
	else raise DoesTailCall
      end
  | Seq es -> 
      let rec loop es =
	match es with
	  [] -> die "empty sequence of expressions"
	| [e] -> comp_exp tailcall env e
	| e1::rest -> (comp_exp false env e1; loop rest)
      in loop es
  | Coerce(c,e) ->
      begin
	comp_exp false env e;
	match c with
	  Int2D  -> make_obj int_tag (Reg Eax) raw_int_c
	| String2D -> make_obj string_tag (Reg Eax) raw_string_c
	| Char2D -> make_obj char_tag (Reg Eax) raw_int_c
	| Indesc2D -> make_obj indesc_tag (Reg Eax) raw_int_c
	| Outdesc2D -> make_obj outdesc_tag (Reg Eax) raw_int_c
	| Pair2D -> emit(Mov(Reg Eax,(Reg Eax,dyn_coercion)))
	| Fn2D i -> make_obj (closure_tag i) (Reg Eax) (List.nth closure_cs i)
	| D2Int -> check_int env
	| D2String -> check_string env
	| D2Char -> check_char env
	| D2Indesc -> check_indesc env
	| D2Outdesc -> check_outdesc env
	| D2Pair -> check_pair env
	| D2Fn i -> check_fn i env
      end
and comp_op env op es =
  match (op,es) with
    (Plus,[e1;e2]) -> comp_arith e1 e2 env Add
  | (Minus,[e1;e2]) -> comp_arith e1 e2 env Sub
  | (Times,[e1;e2]) -> comp_arith e1 e2 env Imul2
  | (Div,[e1;e2]) -> 
      begin
	comp_exp false env e1;
	let env' = push env (Reg Eax,[]) raw_int_c in
	comp_exp false env' e2;
	emit (Pop (Reg Ecx));
	emit (Xchg (Reg Eax,Ecx));
	emit (Conv Cdq);
	emit (ArithMD (Tal.Div, Reg Ecx))
      end
  | (Inteq,[e1;e2]) -> comp_int_compare e1 e2 env Tal.Eq
  | (Ptreq,[e1;e2]) -> comp_ptr_compare e1 e2 env
  | (Structeq,[e1;e2]) -> unimp "Structeq unimplemented"
  | (Not,[e]) -> comp_exp false env (If(e,(False,D_t),(True,D_t)),D_t)
  | (Less,[e1;e2]) -> comp_int_compare e1 e2 env Tal.Less
  | (Greater,[e1;e2]) -> comp_int_compare e1 e2 env Tal.Greater
  | (Lesseq,[e1;e2]) -> comp_int_compare e1 e2 env Tal.LessEq
  | (Greatereq,[e1;e2]) -> comp_int_compare e1 e2 env Tal.GreaterEq
  | (Isint,[e]) -> check_tag_bool env int_tag "int" e
  | (Isbool,[e]) -> 
      let false_lab = id_new "not_bool" in
      begin
	comp_exp false env e;
	comment "is Eax a bool?";
	emit (Mov(Reg Ecx,(Reg Eax,[Unroll])));
	emit (Mov(Reg Eax,(Immed i32_0,dyn_coercion)));
	emit (Btagi(Ecx,i32_2,(false_lab,branch_coercion),Tal.Below));
	comment "Eax is a bool";
	emit (Mov(Reg Eax,(Immed i32_1,dyn_coercion)));
	emit (Fallthru[stack_c]);
	emit_lab env false_lab [(Eax,dyn_c)]
      end
  | (Isnil,[e]) -> (* DJG : Nil is no longer false *)
      let false_lab = id_new "not_null" in
      begin
	comp_exp false env e;
	comment "is Eax null/false?";
	emit (Mov(Reg Ecx,(Reg Eax,[Unroll])));
	emit (Mov(Reg Eax,(Immed i32_0,dyn_coercion)));
	emit (Btagi(Ecx,i32_2,(false_lab,branch_coercion),Tal.NotEq));
	comment "Eax is a null/false";
	emit (Mov(Reg Eax,(Immed i32_1,dyn_coercion)));
	emit (Fallthru[stack_c]);
	emit_lab env false_lab [(Eax,dyn_c)]
      end
  | (Ispair,[e]) -> check_tag_bool env pair_tag "pair" e
  | (Isfn,[e]) -> 
      let no_lab = id_new "not_fn" in
      begin
	comp_exp false env e;
	comment "is Eax a function?";
	emit (Mov(Reg Ecx,(Reg Eax,[Unroll])));
	emit (Mov(Reg Eax,(Immed i32_0,dyn_coercion)));
	emit (Btagi(Ecx,i32_255,(no_lab,branch_coercion),Tal.Below));
	emit (Btagvar(Ecx,i32_0,closure_tag 0,
		      (no_lab,branch_coercion),Tal.Below));
	comment "Eax is a function";
	emit (Mov(Reg Eax,(Immed i32_1,dyn_coercion)));
	emit (Fallthru[stack_c]);
	emit_lab env no_lab [(Eax,dyn_c)]
      end
  | (Ischar,[e]) -> check_tag_bool env char_tag "char" e
  | (Isstring,[e]) -> check_tag_bool env string_tag "string" e
  | (Isindesc,[e]) -> check_tag_bool env indesc_tag "indesc" e
  | (Isoutdesc,[e]) -> check_tag_bool env outdesc_tag "outdesc" e
  | (Cons,[e1;e2]) ->
      begin
	comp_exp false env e1;
	let env = push env (Reg Eax,[]) dyn_c in
	comp_exp false env e2;
	emit(Mov(Reg Ebp,(Reg Eax,[])));
	comment("build cons cell from Eax and top-of-stack");
	emit(Push(Reg Ebx,[]));
	let twelve = int_to_int32 12 in
	emit(Malloc(twelve,malloc_prod [csing (pcint pair_tag);dyn_c;dyn_c],
		    None));
	emit(Pop(Reg Ebx));
	emit(Mov(Prjr((Eax,[]),i32_0),(Immed pair_tag,[])));
	emit(Mov(Prjr((Eax,[]),i32_8),(Reg Ebp,[])));
	emit(Pop(Reg Ebp));
	emit(Mov(Prjr((Eax,[]),i32_4),(Reg Ebp,[])))
      end
  | (Car,[e]) ->
      begin
	comp_exp false env e;
	comment("get car");
	emit(Mov(Reg Eax,(Prjr((Eax,[]),i32_4),[])));
      end
  | (Cdr,[e]) ->
      begin
	comp_exp false env e;
	comment("get cdr");
	emit(Mov(Reg Eax,(Prjr((Eax,[]),i32_8),[])));
      end
  | (Setcar,[e1;e2]) -> 
      begin
	comp_exp false env e2;
	let env' = push env (Reg Eax,[]) dyn_c in
	comp_exp false env' e1;
	comment("setcar");
	emit(Pop(Reg Ecx));
	emit(Mov(Prjr((Eax,[]),i32_4),(Reg Ecx,[])));
	comp_exp false env (False,D_t)  (* return False *)
      end	
  | (Setcdr,[e1;e2]) -> 
      begin
	comp_exp false env e2;
	let env' = push env (Reg Eax,[]) dyn_c in
	comp_exp false env' e1;
	comment("setcdr");
	emit(Pop(Reg Ecx));
	emit(Mov(Prjr((Eax,[]),i32_8),(Reg Ecx,[])));
	comp_exp false env (False,D_t)  (* return False *)
      end	
  | (Openin,[e]) ->
      begin
	comment("call openin");
	comp_exp false env e;
	do_call1 env openin_label;
      end
  | (Openout,[e]) ->
      begin
	comment("call openout");
	comp_exp false env e;
	do_call1 env openout_label;
      end
  | (Closein,[e]) ->
      begin
	comment("call closein");
	comp_exp false env e;
	do_call1 env closein_label;
	comp_exp false env (False,D_t)
      end
  | (Closeout,[e]) ->
      begin
	comment("call closeout");
	comp_exp false env e;
	do_call1 env closeout_label;
	comp_exp false env (False,D_t)
      end
  | (Flushout,[e]) -> 
      begin
	comment("call flushout");
	comp_exp false env e;
	do_call1 env flushout_label;
	comp_exp false env (False,D_t)
      end
  | (Getchar,[]) -> do_call0 env getchar_label
  | (Peekchar,[]) -> do_call0 env peekchar_label
  | (Getstring,[e]) -> 
      begin
	comp_exp false env e;
	do_call1 env getstring_label
      end
  | (Putchar,[e]) -> 
      begin
	comp_exp false env e;
	do_call1 env putchar_label;
	comp_exp false env (False,D_t)
      end
  | (Putstring,[e]) -> 
      begin
	comp_exp false env e;
	do_call1 env putstring_label;
	comp_exp false env (False,D_t)
      end
  | (Getchar,[e]) -> 
      begin
	comment("call fgetchar");
	comp_exp false env e;
	do_call1 env fgetchar_label
      end
  | (Peekchar,[e]) ->
      begin
	comment("call fpeekchar");
	comp_exp false env e;
	do_call1 env fpeekchar_label
      end
  | (Fgetstring,[e1;e2]) ->
      begin
	comment("call fgetstring");
	comp_exp false env e1;
	let env' = push env (Reg Eax,[]) raw_int_c in
	comp_exp false env' e2;
	do_call2 env fgetstring_label
      end
  | (Putchar,[e1;e2]) ->
      begin
	comment("call fputchar");
	comp_exp false env e1;
	let env' = push env (Reg Eax,[]) raw_int_c in
	comp_exp false env' e2;
	do_call2 env fputchar_label;
	comp_exp false env (False,D_t)
      end
  | (Fputstring,[e1;e2]) ->
      begin
	comment("call fgetstring");
	comp_exp false env e1;
	let env' = push env (Reg Eax,[]) raw_int_c in
	comp_exp false env' e2;
	do_call2 env fputstring_label;
	comp_exp false env (False,D_t)
      end
  | (Print,[e]) ->
      begin
	comp_exp false env e;
	do_call1 env print_label;
	comp_exp false env (False,D_t)
      end
  | (Newstring,[e]) ->
      begin
	comment("newstring");
	comp_exp false env e;
	do_call1 env newstring_label
      end
  | (Newstring, [e1; e2]) ->
       begin
	  comment("newstring");
	  comp_exp false env e1;
	  check_int env;
	  let env' = push env (Reg Eax, []) raw_int_c in
	  comp_exp false env' e2;
	  check_char env';
	  do_call2 env' newstringchar_label
       end
	  
  | (Sizes,[e]) ->
      begin
	comment("string length");
	comp_exp false env e;
	emit(Unpack (array_size_var,Eax,(Reg Eax,[])));
	emit(Mov (Reg Eax,(Prjr ((Eax,[]),i32_0),[])));
	emit(Tal.Coerce (Eax,[Subsume cbyte4]))
      end
  | (Subs,[e1;e2]) ->
      begin
	comment("string ref");
	comp_exp false env e1;
	let env' = push env (Reg Eax,[]) raw_string_c in
	comp_exp false env' e2;
	emit(Pop(Reg Ecx));
	emit(Unpack (array_size_var,Ecx,(Reg Ecx,[])));
	emit(Asub(Eax,(Prjr ((Ecx,[]),i32_4)),i32_1,Eax,
		  (Prjr ((Ecx,[]),i32_0))))
      end
  | (Sets,[e1;e2;e3]) ->
      begin
	comment("string ref");
	comp_exp false env e1;
	let env' = push env (Reg Eax,[]) raw_string_c in
	comp_exp false env' e2;
	let env'' = push env (Reg Eax,[]) raw_int_c in
	comp_exp false env'' e3;
	emit(Pop(Reg Ecx));
	emit(Pop(Reg Edx));
	emit(Unpack (array_size_var,Edx,(Reg Edx,[])));
	emit(Aupd((Prjr ((Edx,[]),i32_4)),i32_1,Ecx,Eax,
		  (Prjr ((Edx,[]),i32_0))));
	comp_exp false env (False,D_t)
      end
  | (Chr,[e]) ->
      begin
	comp_exp false env e;
	comment("chr -- take low 8 bits only");
	emit(ArithBin(Tal.And,Reg Eax,Immed i32_255))
      end
  | (Ord,[e]) ->
      begin
	comp_exp false env e;
	(* DJG *)
        comment("ord!");
	emit (Mov(Reg Ecx, (Immed i32_0,[])));
	emit (Movpart(true, Reg Ecx, RPe, Reg Eax, RPl));
	emit (Mov(Reg Eax, (Reg Ecx,[])))
	(* coment("ord unimplemented");
	emit(Mov(Reg Eax,(Immed i32_0,[]))) *)
      end

  | (Currentin, []) ->
      begin
	 do_call0 env current_in_port_label;
	 make_obj indesc_tag (Reg Eax) raw_int_c
      end
  | (Currentout, []) ->
      begin
	do_call0 env current_out_port_label;
	make_obj  outdesc_tag (Reg Eax) raw_int_c
      end

  | (Callwin, [e1;e2]) ->
       comp_exp false env (App (e2, [ (Op (Openin, [e1]), Indesc_t)]),D_t )
  | (Callwout, [e1;e2]) ->
       comp_exp false env (App (e2, [ (Op (Openout, [e1]), Outdesc_t)]), D_t)
  | (Winfile, [str; thunk]) ->       
       begin
	  comp_exp false env str;
	  check_string env;
	  do_call1 env push_in_port_label;
	  comp_exp false env (App (thunk, []), D_t);
	  let env' = push env (Reg Eax, []) dyn_c in
	  do_call1 env' pop_in_port_label;
	  pop env (Reg Eax);
	  ()
       end
  | (Woutfile,[str;thunk]) ->
       begin
	  comp_exp false env str;
	  check_string env;
	  do_call1 env push_out_port_label;
	  comp_exp false env (App (thunk, []), D_t);
	  let env' = push env (Reg Eax, []) dyn_c in
	  do_call1 env' pop_out_port_label;
	  pop env (Reg Eax);
	  ()
       end
  | _ -> die ("wrong number of arguments for "^(string_of_op op))
and comp_arith e1 e2 env aop = 
  begin
    comp_exp false env e2;
    let env = push env (Reg Eax,[]) raw_int_c in
    comp_exp false env e1;
    let env = pop env (Reg Ecx) in
    emit(ArithBin(aop,Reg Eax,Reg Ecx))
  end
and comp_int_compare e1 e2 env cc = 
  begin
    comp_exp false env e2;
    let env = push env (Reg Eax,[]) raw_int_c in
    comp_exp false env e1;
    let env = pop env (Reg Ecx) in
    emit (Mov(Reg Edx,(Reg Eax,[])));
    emit (Mov(Reg Eax,(Immed i32_0,[Tosum raw_bool_c])));
    emit (Cmp(Reg Edx,Reg Ecx));
    emit (Setcc(cc,Reg Eax));
    emit (Mov(Reg Eax,(Reg Eax,dyn_coercion)));
  end
and comp_ptr_compare e1 e2 env =
  begin
    comp_exp false env e1;
    let env = push env (Reg Eax,[]) dyn_c in
    comp_exp false env e2;
    let env = pop env (Reg Ecx) in
    emit (Mov(Reg Edx,(Reg Eax,[])));
    emit (Mov(Reg Eax,(Immed i32_0,[Tosum raw_bool_c])));
    emit (Cmp(Reg Edx,Reg Ecx));
    emit (Setcc(Tal.Eq,Reg Eax));
    emit (Mov(Reg Eax,(Reg Eax,dyn_coercion)))
  end
(* convert an integer genop to a Scheme integer -- i.e., allocate a
 * pair, put the tag for integers in the first component and the
 * integer in the second component, and then coerce the pair to the
 * universal type. Assumes the genop is not in Ecx. *)

(* when we get data working at the TAL level, we can just declare the
 * global defines as top-level labels -- we'll have to treat them
 * specially in the environment however...
 *)
let comp_defines defs = 
  let vars = List.map fst defs in
  let num_vars = int_to_int32 (List.length vars) in
  let field_tys = List.map (fun _ -> cfield dyn_c ReadWrite) vars in
  let env_ty_def = cprod_b field_tys in
  let env_ty_label = id_new "defty" in
  let env_ty = clab env_ty_label in
  let malloc_arg = malloc_prod (List.map (fun _ -> dyn_c) field_tys) in
  let rec init_env count = 
    if (count =$ num_vars) then () else
    begin
      emit (Mov(Prjr((Eax,[]),i32_4*$count),(Reg Ebp,[])));
      init_env (count +$ i32_1)
    end in
  let add_var (d,c) x = (Dict.insert d x (0,c),c+$i32_1) in
  let env = 
    { env_type = env_ty;
      var_map = fst(List.fold_left add_var (Dict.empty compare,i32_0) vars);
      depth = 0;
      stack_type = cempty } in
  begin
    emit (Mov(Reg Ebp,(Immed i32_0,dyn_coercion)));
    emit (Malloc((i32_4 *$ num_vars), malloc_arg, None));
    init_env i32_0;
    emit (Mov(Reg Ebx,(Reg Eax,[Roll env_ty])));
    List.iter 
      (fun (x,e) -> 
      	(set_current_var x; comp_exp false env (Set(x,e),D_t))) defs;
    emit (Push(Immed i32_0,[]));
    emit (Jmp(Addr exit_label,[tapp cempty]));
    (env_ty_label,k4byte,env_ty_def)
  end

let comp_function (env,label,args,body) =
  let argfs = List.map (fun _ -> cfield dyn_c ReadWrite) args in
  let new_env_type = cprod_b ((cfield env.env_type Read)::argfs) in
  let vm = Dict.map_dict (fun (d,o) -> (d+1,o)) env.var_map in
  let (new_var_map,_) =
    List.fold_left
      (fun (vm,o) (x,_) -> (Dict.insert vm x (0,o),o+$i32_1))
      (vm,i32_1)
      args in
  let new_stack_type =
    ccons (ccode_l [(Eax,dyn_c); (Esp,csptr stack_c)]) stack_c in
  let new_env = { env_type = new_env_type; var_map = new_var_map;
		  depth = env.depth+1; stack_type = new_stack_type } in
  let label_c = 
    cforall stack_v Kstack
      (ccode_l [(Esp,csptr new_stack_type);(Ebx,new_env_type)]) in
  begin
    current_label := (label,label_c);
    (try 
      comp_exp true new_env body;
      emit (Retn None)
    with DoesTailCall -> ());
    flush_code();
  end

let rec comp_functions() =
  match !functions with
    [] -> ()
  | (f::rest) -> (functions := rest; comp_function f; comp_functions());;

let code_gen defs = 
  reset_generator ();
  let defines_con_blk = comp_defines defs in
  flush_code();
  comp_functions();
  let code_blocks = Array.of_list (List.rev (!code_blocks))
  in { imports = [|"tal.tali";"stdlib.tali";"scheme.tali"|];
       exports = [|"tal_prog.tali"|];
       imp_abbrevs = [||];
       con_blocks = [|defines_con_blk|];
       code_blocks = code_blocks;
       data_blocks = Array.of_list (get_strings())
(* Cyclone *)
         ;
       templates=[||]
(* End Cyclone *)
     } 
;;

(* EOF: scomp.ml *)
