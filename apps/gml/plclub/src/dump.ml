open Program

let rec program o p =
  Format.fprintf o "@[";
  begin match p with
    [] -> ()
  | t::r ->
      token o t;
      List.iter (fun t -> Format.fprintf o "@ "; token o t) r
  end;
  Format.fprintf o "@]"

and token o t =
  match t with
    Fun' f ->
      Format.fprintf o "@[<2>{@ %a@ }@]" program f
  | Arr' a ->
      Format.fprintf o "@[<2>[@ %a@ ]@]" program a
  | Ident' i ->
      Format.fprintf o "v%d" i
  | Binder' ->
      Format.fprintf o "/"
(*
  | Int' i ->
      Format.fprintf o "%d" i
  | Float' f ->
      Format.fprintf o "%f" f
  | String' s ->
      Format.fprintf o "\"%s\"" s
*)
  | Val' v ->
      value o v
  | _ ->
      try
        Format.fprintf o "%s" (name t)
      with Not_found ->
        failwith "Could not print token"

and value o v =
  match v with
    VInt i ->
      Format.fprintf o "%d" i
  | VFloat f ->
      Format.fprintf o "%f" f
  | VBool b ->
      Format.fprintf o "%b" b
  | VStr s ->
      Format.fprintf o "\"%s\"" s
  | VClos _ ->
      Format.fprintf o "<fun>"
  | VFun _ ->
      Format.fprintf o "<fun>"
  | VArr a ->
      Format.fprintf o "@[<2>[@ ";
      for i = 0 to Array.length a - 1 do
        value o a.(i);
        Format.fprintf o "@ "
      done;
      Format.fprintf o "]@]";
  | VPoint (x, y, z) ->
      Format.fprintf o "<point %a %a %a>" value x value y value z
(* XXX Print the objects and lights? *)
  | VObj _ ->
      Format.fprintf o "<obj>"
  | VLight _ ->
      Format.fprintf o "<light>"
  | VPtLight _ ->
      Format.fprintf o "<pointlight>"
  | VStLight _ ->
      Format.fprintf o "<spotlight>"

let stack o st =
  Format.fprintf o "@[";
  let st = List.rev st in
  begin match st with
    [] -> ()
  | v::r ->
      value o v;
      List.iter (fun v -> Format.fprintf o "@ "; value o v) r
  end;
  Format.fprintf o "@]"

let f env st p =
  Format.eprintf "@[<2>Program:@ %a@]@." program p;
  Format.eprintf "@[<2>Stack:@ %a@]@." stack st
