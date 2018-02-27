(**********************************************************************)
(* (c) Neal Glew, David Walker                                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Numtypes - provides a number of machine numeric types and ops on them.
 *)

type int8;;
type int16;;
type int32;;

(* Constants *)

val i32_0 : int32;;
val i32_1 : int32;;
val i32_2 : int32;;
val i32_3 : int32;;
val i32_4 : int32;;
val i32_8 : int32;;
val i32_16 : int32;;
val i32_32 : int32;;
val i32_255 : int32;;

(* Conversions *)

val int_to_int32 : int -> int32;;
val int32_to_int : int32 -> int;;
val int32_of_string : string -> int32;;

val string_of_int8 : int8 -> string;;
val string_of_int16 : int16 -> string;;
val string_of_int32 : int32 -> string;;

val int16_to_int32 : int16 -> int32;;
val int32_to_int8 : int32 -> int8;;
val int32_to_int16 : int32 -> int16;;

val int8_to_byte : int8 -> char;;
val int32_to_1byte : int32 -> char;;
val int32_to_2bytes : int32 -> char*char;;
val int32_to_4bytes : int32 -> char*char*char*char;; (* lsb...msb *)

(* int32 ops *)

val (~-$) : int32 -> int32;;
val (+$) : int32 -> int32 -> int32;;
val (-$) : int32 -> int32 -> int32;;
val ( *$ ) : int32 -> int32 -> int32;;
val (/$) : int32 -> int32 -> int32;;
val mod32 : int32 -> int32 -> int32;;
val (=$) : int32 -> int32 -> bool;;
val (<>$) : int32 -> int32 -> bool;;
val (<$) : int32 -> int32 -> bool;;
val (<=$) : int32 -> int32 -> bool;;
val (>$) : int32 -> int32 -> bool;;
val (>=$) : int32 -> int32 -> bool;;
val lnot32 : int32 -> int32;;
val land32 : int32 -> int32 -> int32;;
val lor32 : int32 -> int32 -> int32;;
val lxor32 : int32 -> int32 -> int32;;
val lsl32 : int32 -> int32 -> int32;;
val lsr32 : int32 -> int32 -> int32;;
val asr32 : int32 -> int32 -> int32;;

val abs32 : int32 -> int32
val max32 : int32 -> int32 -> int32
val min32 : int32 -> int32 -> int32
val max_int32 : int32
val min_int32 : int32
(* i1^i2 where i2 must not be negative *)
val pow32 : int32 -> int32 -> int32

(* integers ordered: 0 < 1 < .... < maxint < minint < ... < -1 *)
val unsigned_lt32 : int32 -> int32 -> bool
val unsigned_lte32 : int32 -> int32 -> bool
(* raise Invalid_arg if overflow check fails *)
(* overflow check for multiplication is conservative *)
val add32_over : int32 -> int32 -> int32
val sub32_over : int32 -> int32 -> int32
val signed_mul32_over : int32 -> int32 -> int32
val unsigned_mul32_over : int32 -> int32 -> int32

(* floating point numbers:
 * conversions between 3 formats are supported:
 * 1. dec: string with format [+|-]digits.digits[(E|e)[+|-]digits] 
 *      - decimal digits; exponent is base 10
 * 2. bytes: string 
 *      - least significant byte is 0 !!! (in memory order)
 *      - See Intel Developer's Manual, Volume 1, sections 7-2, 7-4
 *      - float32: 1bit sign, 8bit exp, 23bit precision, exp bias = 127
 *      - float64: 1bit sign, 11bit exp, 52bit precision, exp bias = 1023 
 *      - the exponent bias is added to each binary exponent (hence negative
 *        exponents will be represented as positive numbers)
 * 3. hex: string of hexidecimal digits representing bytes
 *      - most significant byte is 0  (in printing order)
 *)
type f32
type f64

val f32_to_hex   : f32 -> string
val f64_to_hex   : f64 -> string
val hex_to_f32   : string -> f32 (* string is "FFB00000" *)
val hex_to_f64   : string -> f64

val f32_to_bytes : f32 -> string
val f64_to_bytes : f64 -> string
val bytes_to_f32 : string -> f32 (* string is binary. *)
val bytes_to_f64 : string -> f64

val f32_to_dec   : f32 -> string
val f64_to_dec   : f64 -> string
val dec_to_f32   : string -> f32 (* string is "1e6" or "1.054" ... *)
val dec_to_f64   : string -> f64

val f32_to_float : f32 -> float  (* OCaml floats are 64 bit. *)
val f64_to_float : f64 -> float 
val float_to_f32 : float -> f32  (* C pops to infinity if out of range. *)
val float_to_f64 : float -> f64

val f32_to_f64 : f32 -> f64
val f64_to_f32 : f64 -> f32

val neg_f32 : f32 -> f32 
val neg_f64 : f64 -> f64

val zero_f32 : f32
val zero_f64 : f64
val one_f32  : f32
val one_f64  : f64

val neginf_f32 : f32
val posinf_f32 : f32
val neginf_f64 : f64
val posinf_f64 : f64

val is_zero_f32 : f32 -> bool
val is_zero_f64 : f64 -> bool
val is_one_f32  : f32 -> bool
val is_one_f64  : f64 -> bool

(* EOF: numtypes.mli *)

