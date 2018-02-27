#ifndef GCD_H
#define GCD_H

#include "list.h"

prefix Gcd {
open   Gcd {
open   List {

extern void set_elab_only();
extern void set_generate_tal_only();
extern void set_do_link();
extern void set_no_link();

extern bool code_generate_p();
extern bool assemble_p();
extern bool link_p();

extern void set_file_types(<*(string, bool f(string,string,string))>list);

extern void compile_file(string);

extern void add_object_file(string);
extern void add_library    (string);
extern void set_output_name(string);
extern bool compile_object_file(string,string,string);
extern bool compile_library    (string,string,string);

extern void set_tooldesc(string);

extern <*(string,Arg::spec,string)>list             std_options   ();
extern <*(string,bool f(string,string,string))>list std_file_types();


extern int driver(string toolname, <*(string,Arg::spec,string)>list options,
	   <*(string, bool f(string,string,string))>list file_types,
	   bool middle(), bool do_link(<string>list,<string>list,string));


}}}

#endif
