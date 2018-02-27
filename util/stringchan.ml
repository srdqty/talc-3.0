(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Stringchan
 * The string_chan ADT, which manages writing to a string buffer.
 *)

(* topbyte -- PRIVATE
   Returns the top byte of a 4-byte integer, preserving the sign bit. *)

let topbyte n =
   if (n < 0) then
      (((n asr 24) land 0xFF) lor 0x80)
   else
      ((n lsr 24) land 0xFF)
;;

type string_chan = {
   mutable soc_data : string;
   mutable soc_mark : int
 }
;;

let create len = {
   soc_data = (String.create len);
   soc_mark = 0
 }
;;

let from_string str = {
   soc_data = str;
   soc_mark = 0
 }
;;

let to_string soc = 
  soc.soc_data
;;

let get_mark soc =
   soc.soc_mark
;;

let set_mark soc newmark =
   if (newmark <= (String.length soc.soc_data)) then
      soc.soc_mark <- newmark
   else
      (invalid_arg ("Stringchan.set_mark: Mark set to "
		      ^ (string_of_int newmark) ^ ", string length is "
		      ^ (string_of_int (String.length soc.soc_data))))
;;

(* output *)

let put_char soc c =
  soc.soc_data.[soc.soc_mark] <- c;
  soc.soc_mark <- soc.soc_mark + 1
;;

let put_byte soc n = 
   (soc.soc_data.[soc.soc_mark] <- (Char.chr (n land 0xFF)));
   soc.soc_mark <- soc.soc_mark + 1
;;

let put_2bytes soc n = 
   (put_byte soc n);
   (put_byte soc (n lsr 8))
;;
     
let put_4bytes soc n = 
   (put_byte soc n);
   (put_byte soc (n lsr 8));
   (put_byte soc (n lsr 16));
   (put_byte soc (topbyte n))
;;
 
let put_string_len soc str offset len = 
   (String.blit str offset soc.soc_data soc.soc_mark len);
   soc.soc_mark <- soc.soc_mark + len
;;

let put_string soc str =
   (put_string_len soc str 0 (String.length str))
;;

let put_extend soc str offset len =
   (put_string_len soc str offset len);
   if len > (String.length str) then
      for extra = (String.length str) to len do
	 (put_byte soc 0)
      done
;;

(* peek
   Returns the value currently at the mark, without moving the mark. *)

let peek_byte soc =
   (Char.code soc.soc_data.[soc.soc_mark])
;;

let peek_2bytes soc =
   let byte2 = (Char.code soc.soc_data.[soc.soc_mark]) in
   let byte1 = (Char.code soc.soc_data.[(soc.soc_mark + 1)]) in
   ((byte1 lsl 8) lor byte2)
;;

let peek_4bytes soc =
   let byte4 = (Char.code soc.soc_data.[soc.soc_mark]) in
   let byte3 = (Char.code soc.soc_data.[(soc.soc_mark + 1)]) in
   let byte2 = (Char.code soc.soc_data.[(soc.soc_mark + 2)]) in
   let byte1 = (Char.code soc.soc_data.[(soc.soc_mark + 3)]) in
   ((byte1 lsl 24) lor (byte2 lsl 16) lor (byte3 lsl 8) lor byte4)
;;

(* input 
   returns value at mark, and moves the mark
   *)

type in_channel = string_chan
let input_char soc =
   try 
      let c = String.get soc.soc_data soc.soc_mark in
      soc.soc_mark <- soc.soc_mark + 1;
      c       
   with Invalid_argument _ -> 
      raise End_of_file 

let input_byte soc = 
   let c = input_char soc in 
   Char.code c
let pos_in = get_mark
let seek_in = set_mark
let open_in_bin = from_string
let close_in soc = ()

let really_input sc buff ofs len = 
   try 
      String.blit sc.soc_data sc.soc_mark buff ofs len ;
      sc.soc_mark <- sc.soc_mark + len
   with Invalid_argument _ ->
      if sc.soc_mark + len >= String.length sc.soc_data
      then raise End_of_file
      else if ofs+len >= String.length buff then 
	 raise (Invalid_argument "really_input")
      else failwith "BUG in really_input"


(* output *)
type out_channel = string_chan
let output_char = put_char
let output_string = put_string
let pos_out = set_mark
let output_buffer sc buf = put_string sc (Buffer.contents buf)
   
(* EOF: stringchan.ml *)
