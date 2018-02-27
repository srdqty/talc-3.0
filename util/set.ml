
type 'a set = ('a list) * ('a -> 'a -> int)

let empty       f              = ([],f)
let singleton   f a            = ([a],f)
let cardinality (s,_)          = List.length s
let member      (s,f)        a = List.exists (fun x -> (f a x) = 0) s
let insert      ((s,f) as d) a = if member d a then d else (a::s,f)

let union (s1,f) d2 = 
  let rec aux elts d2 =
    match elts with
      []      -> d2
    | hd::tl  -> aux tl (insert d2 hd) in
  aux s1 d2

let delete ((s,f) as d) a = (* scan twice to promote sharing *)
  if member d a 
  then 
    let rec aux s =
      match s with
	[]      -> failwith "set_list: impossible"
      |	hd::tl  -> if (f hd a) = 0 then tl else hd::(aux tl) in
    (aux s,f)
  else d

let elements = fst

let is_empty (s,_)   = match s with [] -> true | _ -> false

let app  f   (s,b)    = ignore (List.iter (fun a -> ignore (f a)) s)
let fold  f   (s,_) b = failwith "fold unimplemented"
let intersect d1 d2   = failwith "intersect unimplemented"
let from_list f l     = failwith "from_list unimplemented"
let subset d1 d2      = failwith "subset unimplemented"
let diff d1 d2        = failwith "diff unimplemented"
let equals d1 d2      = failwith "equals unimplemented"
let choose d1         = failwith "choose unimplemented"
let print f format d1 = failwith "print unimplmented"

