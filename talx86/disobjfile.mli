(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* dasmobjfile.mli
 * 
 * Provides interface for reading a generic object file.  Intention is
 * to support both Coff and Elf in a generic way.  This interface is
 * used by disasm.ml to disassemble a given object file.
 *)
open Utilities;;
open Identifier;;

exception No_Code_Section;;
exception No_Data_Section;;
exception No_Cyclone_Section;;
exception End_of_Section;;

type objfile = 
    { obj_close : unit -> unit;
      (* move seek pointer to first byte of .text -- raise No_Code_Section
       * if .text cannot be found. *)
      obj_seek_code : unit -> unit; 
      (* move seek pointer to first byte of .data -- raise No_Data_Section
       * if .data cannot be found. *)
      obj_seek_data : unit -> unit; 
      (* move seek pointer to first byte of .cyc -- raise No_Cyclone_Section
       * if .cyc cannot be found. *)
      obj_seek_cyclone : unit -> unit;
      (* return any labels associated with the current seek position + i *)
      obj_get_labels : int -> identifier list;
      (* if id is at some address as other labels, return all of them,
	 else raise Dict.Absent *)
      obj_get_shared_labels: unit -> (identifier,identifier list) Dict.dict;
      (* return a byte associated with the current seek position -- one
       * should call obj_get_labels() on instruction boundaries to see 
       * if there is a label associated with the byte. *)
      obj_get_byte : unit -> int;
      (* align the current address to an n-byte boundary *)
      obj_align : int -> unit;
      (* return a label if there is a relocation for the current position *)
      obj_get_reloc : unit -> identifier option;
      (* return a position as a section number and virtual address *)
      obj_get_pos : unit -> int * int;
      (* return the section number + offset of a label, or raise Dict.absent *)
      obj_label_offset : (identifier,int * int) Dict.dict
    } 

(* result only has ids s.t. size of co-domain > 1 *)
val offset_dict_to_virtlabels : 
    (int,identifier list)Dict.dict -> (identifier,identifier list)Dict.dict
