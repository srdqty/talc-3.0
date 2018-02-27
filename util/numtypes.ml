(**********************************************************************)
(* (c) Neal Glew, David Walker                                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Numtypes - provides a number of machine numeric types and ops on them.
 *)

(* Warning: Floating point support requires a custom link to the file float.c *)

type int8 = int;;
type int16 = int;;
type int32 = int;;

let i32_0 = 0;;
let i32_1 = 1;;
let i32_2 = 2;;
let i32_3 = 3;;
let i32_4 = 4;;
let i32_8 = 8;;
let i32_16 = 16;;
let i32_32 = 32;;
let i32_255 = 255;;

let int_to_int32 i = i;;
let int32_to_int i = i;;
let int32_of_string s = int_of_string s;;

let string_of_int8 i = string_of_int i;;
let string_of_int16 i = string_of_int i;;
let string_of_int32 i = string_of_int i;;

let int16_to_int32 i = i;;
let int32_to_int8 i = i;;
let int32_to_int16 i = i;;

let int8_to_byte i = Char.chr (i land 255);;
let int32_to_1byte i = Char.chr (i land 255);;
let int32_to_2bytes i = (Char.chr (i land 255),Char.chr ((i lsr 8) land 255));;
let int32_to_4bytes i =
  let b4 =
    if (i < 0) then
      ((i asr 24) land 0xFF) lor 0x80
    else
      (i lsr 24) land 0xFF in
  (Char.chr (i land 255),
   Char.chr ((i lsr 8) land 255),
   Char.chr ((i lsr 16) land 255),
   Char.chr b4)
;;

let (~-$) = (~-);;
let (+$) = (+);;
let (-$) = (-);;
let ( *$ ) = ( * );;
let (/$) = (/);;
let mod32 = (mod);;
let (=$) = (=);;
let (<>$) = (<>);;
let (<$) = (<);;
let (<=$) = (<=);;
let (>$) = (>);;
let (>=$) = (>=);;
let lnot32 i = lnot i;;
let land32 i1 i2 = i1 land i2;;
let lor32 i1 i2 = i1 lor i2;;
let lxor32 i1 i2 = i1 lxor i2;;
let lsl32 i1 i2 = i1 lsl i2;;
let lsr32 i1 i2 = i1 lsr i2;;
let asr32 i1 i2 = i1 asr i2;;

let abs32 i = abs i
let min32 i j = min i j
let max32 i j = max i j
let max_int32 = max_int
let min_int32 = min_int
let pow32 i j =
  if j <$ 0 then invalid_arg "pow32: in32 exponent must not be negative"
  else
    let result = ref 1 in
    for k = 1 to j do
      result := !result *$ i
    done;
    !result

(* integers ordered: 0 < 1 < .... < maxint < minint < ... < -1 *)
let unsigned_lt32 i j = 
  if i >=$ 0 then
    if j >=$ 0 then i <$ j
    else true
  else (* i <$ 0 *)
    if j <$ 0 then i <$ j
    else false
      
let unsigned_lte32 i j =
  if i >=$ 0 then
    if j >=$ 0 then i <=$ j
    else true
  else (* i <$ 0 *)
    if j <$ 0 then i <=$ j
    else false

(* operators with overflow checks. Raise Invalid_arg on overflow *)
let add32_over i j =
  let res = i +$ j in 
  if i >$ i32_0 && j >$ i32_0 then
    if res <=$ i32_0 then invalid_arg "add32 overflow"
    else res
  else if i <$ i32_0 && j <$ i32_0 then
    if res >=$ i32_0 then invalid_arg "add32 overflow"
    else res
  else
    res

let sub32_over i j = 
  try
    add32_over i (0 -$ j)
  with Invalid_argument _ -> invalid_arg "sub32 overflow"

let square_root = int_to_int32 32768 (* 2^15 *)
let two_pow_14 = int_to_int32 16384
let two_pow_29 = square_root *$ two_pow_14
let signed_mul32_over i j =
  let res = i *$ j in
  let i',j' = abs32 i,abs32 j in
  if i' <$ square_root && j' <$ square_root then res
  else
    let i',j' = (min32 i' j', max32 i' j') in
    if i' <=$ i32_4 && j' <=$ two_pow_29 then res
    else if i' <=$ i32_1 && j' <=$ max_int32 then res
    else invalid_arg "signed mul32 overflow"

(* laziness: unsigned multiply has the same range as signed multiply *)
let unsigned_mul32_over i j =
  if i >$ i32_0 && j >$ i32_0 then 
    try signed_mul32_over i j
    with Invalid_argument "signed mul32 overflow" -> 
      invalid_arg "unsigned mul32 overflow"
  else invalid_arg "unsigned mul32 overflow"


(* floating point numbers:
 * conversions between 3 formats are supported:
 * 1. dec: string with format [+|-]digits.digits[(E|e)[+|-]digits] 
 *      - decimal digits; exponent is base 10
 * 2. bytes: int list where ints are in range 0..255
 *      - least significant byte is 0
 *      - See Intel Developer's Manual, Volume 1, sections 7-2, 7-4
 *      - each int is 8 binary digits
 *      - float32: 1bit sign, 8bit exp, 23bit precision, exp bias = 127
 *      - float64: 1bit sign, 11bit exp, 52bit precision, exp bias = 1023 
 *      - the exponent bias is added to each binary exponent (hence negative
 *        exponents will be represented as positive numbers)
 * 3. hex: string of hexidecimal digits representing bytes 
 *      - most significant byte = 0 (printing order)
 *)

type f32 = string
type f64 = string

(* External functions implemented in C. *)
external dec_to_float : string -> float = "dec_to_float"
(* Need float_to_dec so we get C printing, not OCaml printing. *)
external float_to_dec : float -> string = "float_to_dec"
external float_to_f32 :  float ->   f32 = "float_to_f32"(*+/-INF if out of range.*)
external float_to_f64 :  float ->   f64 = "float_to_f64"
external f32_to_float :    f32 -> float = "f32_to_float"
external f64_to_float :    f64 -> float = "f64_to_float"

let hex_to_char (lo:char) (hi:char) =
  let to_int x =
    match x with
      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
	(int_of_char x) - (int_of_char '0')
    | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' ->
	(int_of_char x) - (int_of_char 'A') + 10
    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' ->
	(int_of_char x) - (int_of_char 'a') + 10
    | _ -> invalid_arg "cannot convert this hex character to an int."
  in
  let v = ((to_int hi) lsl 4) lor (to_int lo) in
  char_of_int v

let put_hex (s:string) (i:int) (x:char) =
  let dx = int_of_char x in
  let hx = (dx lsr 4) land 0xF in
  let lx =  dx        land 0xF in
  let to_hex x =
    if x>=0 && x<16 then
      if x<10 then char_of_int(x + int_of_char '0')
      else char_of_int( (x-10) + int_of_char 'A')
    else invalid_arg "to_hex: argument < 0 or > 15" in
  s.[i  ] <- to_hex hx;
  s.[i+1] <- to_hex lx;
  ()

let f32_to_hex f32 =
  let hex = String.create 8 in 
  for i = 0 to 3 do
    put_hex hex (2 * (3-i)) f32.[i]
  done;
  hex

let f64_to_hex f64 =
  let hex = String.create 16 in 
  for i = 0 to 7 do
    put_hex hex (2 * (7-i)) f64.[i]
  done;
  hex

let hex_to_f32 hex =
  let f32 = String.create 4 in
  for i = 0 to 3 do
    let hi = hex.[(3 - i)*2] in
    let lo = hex.[(3 - i)*2 + 1] in
    f32.[i] <- (hex_to_char lo hi)
  done;
  f32

let hex_to_f64 hex =
  let f64 = String.create 8 in
  for i = 0 to 7 do
    let hi = hex.[(7 - i)*2] in
    let lo = hex.[(7 - i)*2 + 1] in
    f64.[i] <- (hex_to_char lo hi)
  done;
  f64

let f32_to_bytes f32 = f32
let f64_to_bytes f64 = f64

let bytes_to_f32 b = 
  if String.length b = 4 then b else 
  invalid_arg "bytes_to_f32: argument should have length 4."
let bytes_to_f64 b =
  if String.length b = 8 then b else
  invalid_arg "byte_to_f64: argument should have length 8."

let f32_to_dec f32 = float_to_dec(f32_to_float f32)
let f64_to_dec f64 = float_to_dec(f64_to_float f64)
let dec_to_f32 dec = float_to_f32(dec_to_float dec)
let dec_to_f64 dec = float_to_f64(dec_to_float dec)

let f32_to_f64 f32 = float_to_f64 (f32_to_float f32);;
let f64_to_f32 f64 = float_to_f32 (f64_to_float f64);;

let neg_f32 (f32:f32) = 
  let res = String.copy f32 in (* make a copy of the string! *)
  let b3 = int_of_char res.[3] in
  let b3 = b3 lxor (1 lsl 7) in (* toggle the sign bit. *)
  res.[3] <- char_of_int b3;
  res

let neg_f64 (f64:f64) = 
  let res = String.copy f64 in (* make a copy of the string! *)
  let b7 = int_of_char res.[7] in
  let b7 = b7 lxor (1 lsl 7) in (* toggle the sign bit. *)
  res.[7] <- char_of_int b7;
  res

(* FMS: I am leaving the code for is_nan_fXX here in case we need it
   but it has not been checked for correctness!!!! 
   That is why it is not in the interface. *)

(* An f32 is a NAN if 
   the exponent is 0xFF (8 bits) (most significant bits)
   the frac is non_zero (23 bits)
   format of f32 is sign bit : exponent : fraction
*)
let is_nan_f32 f32 = 
  let x3 = (int_of_char f32.[3]) in 
  let x2 = (int_of_char f32.[2]) in 
  let x1 = (int_of_char f32.[1]) in
  let x0 = (int_of_char f32.[0]) in
  (* 0x7F = 0111 1111 *)
  let exp = ((x3 land 0x7F) lsl 1) lor (x2 lsr 7) in
  let frac = (((x2 land 0x7F) lsl 16) lor 
	      (x1 lsl 8) lor
	      x0) in
  (exp = 0xFF && frac != 0)

(* An f64 is a NAN if 
   the exponent is 0xFFE (11 bits)
   the fraction is non_zero (52 bits)
*)
let is_nan_f64 f64 = 
  let x7 = (int_of_char f64.[7]) in 
  let x6 = (int_of_char f64.[6]) in 
  let x5 = (int_of_char f64.[5]) in 
  let x4 = (int_of_char f64.[4]) in 
  let x3 = (int_of_char f64.[3]) in 
  let x2 = (int_of_char f64.[2]) in 
  let x1 = (int_of_char f64.[1]) in
  let x0 = (int_of_char f64.[0]) in
  let exp = ((x7 land 0x7F) lsl 1) lor (x6 lsr 4) in
  let frac1 = ((((x6 land 0x0F)) lsl 24) lor 
	       (x5 lsl 16) lor
	       (x4 lsl  8) lor
	       x3) in
  let frac2 = (x2 lsl 16) lor (x1 lsl 8) lor x0 in
  (exp = 0xFF && (frac1 != 0 || frac2 != 0))

let zero_f32 = float_to_f32 0.0
let zero_f64 = float_to_f64 0.0
let one_f32 = float_to_f32 1.0
let one_f64 = float_to_f64 1.0

let neginf_f32 = float_to_f32 (-1.0 /. 0.0)
let posinf_f32 = float_to_f32 ( 1.0 /. 0.0)
let neginf_f64 = float_to_f64 (-1.0 /. 0.0)
let posinf_f64 = float_to_f64 ( 1.0 /. 0.0)

let is_zero_f32 f32 = (f32 = zero_f32)
let is_zero_f64 f64 = (f64 = zero_f64)
let is_one_f32 f32 = (f32 = one_f32)
let is_one_f64 f64 = (f64 = one_f64)

(*
let hex_to_int c =
  match c with
    '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' | 'a' -> 10
  | 'B' | 'b' -> 11
  | 'C' | 'c' -> 12
  | 'D' | 'd' -> 13
  | 'E' | 'e' -> 14
  | 'F' | 'f' -> 15
  | _ -> invalid_arg "hex_to_int: not hex digit"

let rec hex_to_ints s i l =
  if i < l then
    let d1 = hex_to_int s.[i]   in
    let d2 = hex_to_int s.[i+1] in
    ((d1 lsl 4) lor d2)::hex_to_ints s (i+2) l
  else
    []

let hexformat_to_float32 s = 
  if String.length s = 8 then hex_to_ints s 0 8
  else invalid_arg "hexformat_to_float32: wrong size"
  
let hexformat_to_float64 s =
  if String.length s = 16 then hex_to_ints s 0 16
  else invalid_arg "hexformat_to_float64: wrong size"

let int_to_hex i =
  match i with
    0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'A'
  | 11 -> 'B'
  | 12 -> 'C'
  | 13 -> 'D'
  | 14 -> 'E'
  | 15 -> 'F'
  | _ -> invalid_arg "int_to_hex: bad digit"

let ints_to_hexs is =
  let l = List.length is in
  let s = String.create (2*l) in
  let rec aux is index =
    match is with
      [] -> ()
    | i::is -> 
      	let h1 = int_to_hex ((i lsr 4) land 0xF) in
      	let h2 = int_to_hex (i land 0xF)         in
      	s.[index]   <- h1;
	s.[index+1] <- h2;
	aux is (index+2) in
  aux is 0;
  s

let float32_to_hexformat f32 = ints_to_hexs f32
let float64_to_hexformat f64 = ints_to_hexs f64

(* bytes = lists of integers in the range 0..255 *)
let bytes_to_float32 bytes =
  if List.length bytes = 4 then bytes
  else invalid_arg "bytes_to_float32: wrong number of bytes"

let bytes_to_float64 bytes =
  if List.length bytes = 8 then bytes
  else invalid_arg "bytes_to_float64: wrong number of bytes"

let float32_to_bytes f32 = f32
let float64_to_bytes f64 = f64

(* convert a string containing the machine representation of a float to the
 * float32/float64 representation *)

(* s.[0] is the least significant byte and should go at the end therefore.
   s.[String.length(s)-1] is the most significant bit and should go at
   the beginning. *)

let c_to_f32 s =
  let result = ref [] in
  for i = 0 to 3 do
    result := (Char.code (s.[i])) :: !result 
  done;
  !result

let c_to_f64 s =
  let result = ref [] in
  for i = 0 to 7 do
    result := (Char.code (s.[i])) :: !result 
  done;
  !result

let f32_to_c f32 =
  let result = String.create 4 in
  let rec aux f32 i =
    if i >=0 then
      match f32 with
	[] -> invalid_arg "f32_to_c: float32 ill-formed"
      |	hd::rest -> result.[i] <- (Char.chr hd); aux rest (i-1)
    else
      () in
  aux f32 3;
  result

let f64_to_c f64 =
  let result = String.create 8 in
  let rec aux f64 i =
    if i >= 0 then
      match f64 with
	[] -> invalid_arg "f64_to_c: float64 ill-formed"
      |	hd::rest -> result.[i] <- (Char.chr hd); aux rest (i-1)
    else
      () in
  aux f64 7;
  result


let decformat_to_float32 s = c_to_f32 (d_to_h32 s)
let decformat_to_float64 s = c_to_f64 (d_to_h64 s)

let zero_f32 = 0::0::0::0::[]
let zero_f64 = 0::0::0::0::0::0::0::0::[]
let one_f32  = 63::128::0::0::[]              (* 0011 1111 1000 0000 *)
let one_f64  = 63::240::0::0::0::0::0::0::[]  (* 0011 1111 1111 0000 0000 0000 0000 0000 *)
let posinf_f32 = 127::128::0::0::[] (* 0111 1111 1000 0000 0000 0000 0000 0000*)
let neginf_f32 = 255::128::0::0::[] (* 1111 1111 1000 0000 .... *)
let posinf_f64 = 127::240::0::0::0::0::0::0::[](* 0111 1111 1111 0000 .... *)
let neginf_f64 = 255::240::0::0::0::0::0::0::[](* 1111 1111 1111 0000 .... *)

let is_zero_f32 f32 = f32 = zero_f32
let is_zero_f64 f64 = f64 = zero_f64
let is_one_f32  f32 = f32 = one_f32 
let is_one_f64  f64 = f64 = one_f64  

(* Floating Point Negation *)
external negate_f32 : string -> string = "negate_f32"
external negate_f64 : string -> string = "negate_f64"
 
let negf32 f32 = c_to_f32 (negate_f32 (f32_to_c f32))
let negf64 f64 = c_to_f64 (negate_f64 (f64_to_c f64))

*)
(* EOF: numtypes.ml *)



