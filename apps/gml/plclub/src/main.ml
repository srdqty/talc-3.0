
(* This works great as long as there are no loops in the includes *)
let parse ch_in =
  let (includes,p) = Parser.program Lexer.token (Lexing.from_channel ch_in) in
  let rec loop includes p =
    match includes with
      [] -> p
    | hd :: tl ->
	try 
	  let ch = open_in_bin hd in
	  let (is',p') = Parser.program Lexer.token (Lexing.from_channel ch) in
	  close_in ch;
	  loop (is' @ tl) (p' @ p)
	with e -> (Printf.eprintf "Failed to open file %s.\n" hd;
			   raise e)
  in
  loop includes p
;;

let _ =
  let args = Array.to_list Sys.argv in
  let rec last l = 
    match l with [] -> raise Not_found | [hd] -> hd | hd :: tl -> last tl in
  (match args with
    [_;"-b";_] -> Render.optimize_scene := false
  | _ -> ());
  let fname = last args in
  let ch_in = open_in fname in
  let p = parse ch_in in
  let _ = close_in ch_in in
(*  if List.mem "-compile" (Array.to_list Sys.argv) then
    let chan = open_out "compiled.ml" in
    let str = Compile.f p in
    let _ = output_string chan str in
    let _ = close_out chan in
    if Unix.WEXITED 0 = Unix.system
	("cd support && " ^
	 "ocamlopt.opt -o ../compiled -unsafe " ^
	 "ppm.cmx math.cmx program.cmx matrix.cmx render.cmx " ^
	 "../compiled.ml && " ^
	 "cd .. && " ^
	 "./compiled " ^
	 if List.mem "-all" (Array.to_list Sys.argv)
	 then "-all"
	 else "")
    then ()
    else
      (prerr_string
	 ("compile failed: falling back\n" ^
	  "(if this message appears too often, there might be a bug in the run script)\n");
       Eval.f p (* fallback *))
  else
*)
    Eval.f p
