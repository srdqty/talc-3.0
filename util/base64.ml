(**********************************************************************)
(* base64.ml                                                          *)
(* Michael Hicks                                                      *)
(*                                                                    *)
(* Conversions from and to base 64 encoding, as described in the      *)
(* Internet Engineering Task Force RFC-1521.  Based on a C            *)
(* implementation.                                                    *)
(*                                                                    *)
(**********************************************************************)

let base64 =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let pad64 = '='

let base64string_of_string src ofs srclength =
  let target = String.create ((srclength / 3 + 1) * 4) in
  let input = "XXX" in
  let output = "XXXX" in
  
  try 
    let rec doBase64 sofs tofs bytes_left =
      if bytes_left <= 2 then (sofs,tofs,bytes_left)
      else
	(input.[0] <- src.[sofs];
	 input.[1] <- src.[sofs+1];
	 input.[2] <- src.[sofs+2];
	 
	 output.[0] <- Char.chr ((Char.code input.[0]) lsr 2);
	 output.[1] <- Char.chr ((((Char.code input.[0]) land 0x03) lsl 4) + 
				   ((Char.code input.[1]) lsr 4));
	 output.[2] <- Char.chr ((((Char.code input.[1]) land 0x0f) lsl 2) + 
				   ((Char.code input.[2]) lsr 6));
	 output.[3] <- Char.chr ((Char.code input.[2]) land 0x3f);
(*
        Assert(output[0] < 64);
        Assert(output[1] < 64);
        Assert(output[2] < 64);
        Assert(output[3] < 64);
*)
	 target.[tofs] <- base64.[Char.code (output.[0])];
	 target.[tofs+1] <- base64.[Char.code (output.[1])];
	 target.[tofs+2] <- base64.[Char.code (output.[2])];
	 target.[tofs+3] <- base64.[Char.code (output.[3])];
	 doBase64 (sofs+3) (tofs+4) (bytes_left - 3)) in
    
    let doPadding sofs tofs bytes_left =
      if 0 <> bytes_left then
        (* Get what's left. *)
	(input.[0] <- '\000';
	 input.[1] <- '\000';
	 input.[2] <- '\000';
	 for i = 0 to (bytes_left-1) do
	   input.[i] <- src.[i+sofs]
	 done;
	 output.[0] <- Char.chr ((Char.code input.[0]) lsr 2);
	 output.[1] <- Char.chr ((((Char.code input.[0]) land 0x03) lsl 4) + 
				   ((Char.code input.[1]) lsr 4));
	 output.[2] <- Char.chr ((((Char.code input.[1]) land 0x0f) lsl 2) + 
				   ((Char.code input.[2]) lsr 6));
(*
       Assert(output.[0] < 64);
       Assert(output.[1] < 64);
       Assert(output.[2] < 64);
*)
	 target.[tofs] <- base64.[(Char.code output.[0])];
	 target.[tofs+1] <- base64.[(Char.code output.[1])];
	 if bytes_left = 1 then
           target.[tofs+2] <- pad64
	 else
	   target.[tofs+2] <- base64.[(Char.code output.[2])];
	 target.[tofs+3] <- pad64
         (* Assert(tofs+4 = (String.length target)) *) )
      else ()
    in
    let (sofs,tofs,bytes_left) = doBase64 ofs 0 srclength in
    let _ = doPadding sofs tofs bytes_left in
    target
  with Invalid_argument _ ->
    (Printf.eprintf "Oops -- out of bounds array access in Base64\n";
     raise Exit)
