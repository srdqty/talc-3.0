#ifndef CHAR_H
#define CHAR_H 
prefix Char {
open   Char {

extern bool is_upper(char c);
extern bool is_lower(char c);
extern bool is_num(char c);
extern bool is_xnum(char c);
extern bool is_space(char c);
extern bool is_alpha(char c);
extern bool is_printable(char c);

extern char to_upper(char c);
extern char to_lower(char c);

}}

#endif
