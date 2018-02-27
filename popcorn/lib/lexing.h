
#ifndef LEXING_H
#define LEXING_H

#include "core.h"

prefix Lexing {
open Lexing   {
open Core     {

extern exception Error(string);

extern struct <a>lexbuf { /* use 'a for state that refill_buff might need */
 
  void refill_buff(<a>lexbuf);
  a  refill_state;

  string lex_buffer;
  int    lex_buffer_len;
  int    lex_abs_pos;
  int    lex_start_pos;
  int    lex_curr_pos;
  int    lex_last_pos;
  int    lex_last_action;
  bool   lex_eof_reached;
}
extern struct <b>function_lexbuf_state { 
// instantiation for using function to read
  int read_fun(string,int,b);
  b read_fun_state;
}

extern struct lex_tables { 
// for space this should probably be shorts, but who cares
  int lex_base   [];
  int lex_backtrk[];
  int lex_default[];
  int lex_trans  [];
  int lex_check  [];
}


extern < <b>function_lexbuf_state>lexbuf
  from_function<b>(int read_fun(string,int,b), b);

extern < <FILE>function_lexbuf_state>lexbuf from_file(FILE);
extern <bool>lexbuf from_string(string);

extern string lexeme      <a>(<a>lexbuf);
extern char   lexeme_char <a>(<a>lexbuf, int);
extern int    lexeme_start<a>(<a>lexbuf);
extern int    lexeme_end  <a>(<a>lexbuf);

extern int lex_engine<a>(lex_tables,int,<a>lexbuf);

}}}

#endif
