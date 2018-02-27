#ifndef TALOUT_H
#define TALOUT_H

#include "core.h"
#include "list.h"
#include "arg.h"

prefix Talout {
open Talout   {

extern union bintool   { void MS, TALC, GNU; }
extern union binformat { void COFF, ELF; }

extern string    object_file_suffix; 
extern string    library_suffix;     
extern string    default_executable; 
extern bintool   asm_bintool;        
extern bintool   link_bintool;       
extern binformat objformat;          

extern string runtime();
extern void set_runtime(string);
extern <string>List::list includes;
extern void add_include(string);

extern Arg::bool_ref verbose_sys_commands;
extern bool verify        (string);
extern bool asm           (string, <string>Core::Opt);
extern bool verify_and_asm(string, <string>Core::Opt);
extern bool link          (<string>List::list, string);

}}
#endif
