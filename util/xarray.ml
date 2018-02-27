(**********************************************************************)
(* (c) Greg Morrisett, Steve Zdancewic                                *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* xarray.ml *)

type 'a xarray = {
    mutable elts : 'a array;
    index : int ref
  } 

let length x = !(x.index)

let get x i =
  if (i < (length x)) then 
    x.elts.(i) else raise (Invalid_argument "Xarray.get")

let set x i a =
  if (i < (length x)) then x.elts.(i) <- a 
  else raise (Invalid_argument "Xarray.set")

let create len a = {
  elts = Array.create len a;
  index = ref 0
} 

let app f x =
  for i = 0 to (length x)-1 do begin
    f (get x i);
  end done

let add x a = try
  let len = length x in
  if (Array.length x.elts = 0) then begin
    x.elts <- Array.create 1 a;
    x.index := 1
  end
  else if len < (Array.length x.elts) then begin
    x.elts.(len) <- a;
    incr x.index
  end else
    let new_elts = Array.create ((Array.length x.elts) * 2) x.elts.(0) in begin
      Array.blit x.elts 0 new_elts 0 len;
      x.elts <- new_elts;
      x.elts.(len) <- a;
      incr x.index
    end
with Invalid_argument _ -> failwith "Xarray.app..."

let to_array x = Array.sub x.elts 0 (length x)

let from_array a =
  if Array.length a = 0 then
    {elts = a;
     index = ref 0}
  else
    let elts = Array.create (Array.length a) a.(0) in 
    let index = Array.length a in begin
      Array.blit a 0 elts 0 index;
      {elts = elts;
       index = ref index}
    end

let append x1 x2 =
  let a1 = to_array x1 in
  let a2 = to_array x2 in
  let a = Array.append a1 a2 in
  from_array a

let map f x =
  let a = to_array x in
  from_array (Array.map f a)

(* EOF: xarray.ml *)
