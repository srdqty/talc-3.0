#ifndef STRING_H
#define STRING_H

#include "list.h"

prefix String {
open   String {
open   List   {

extern string strconcat_l(<string>list);
extern string str_sepstr(<string>list,string);

extern exception Substring(string);
extern string substring(string,int,int); //first is start, second is num to copy

  // replace last with second at end of first
  // raise InvalidArg if second is not end of first.
extern string replace_suffix(string,string,string);

extern exception Strncmp;  // raised when strncmp is given a negative n
// compare s1 and s2 lexicographically up to n characters.  
// returns -1 if s1 < s2, 0 if s1 == s2, and +1 if s1 > s2.
extern int strncmp(string s1,string s2,int n);
// same as strncmp but on substrings
extern int strncmp_subs(string s1,int ofs1,string s2,int ofs2,int n);
// same as strncmp_subs but case-insensitive
extern int strncasecmp_subs(string s1,int ofs1,string s2,int ofs2,int n);
// explode a string into a list of characters
extern <char>list explode(string s);
// implode a list of characters into a string
extern string implode(<char>list c);
}}}

#endif
