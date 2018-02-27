#ifndef SCANF_H
#define SCANF_H

extern void eat_whitespace(string s, *(int) ofs_ret);
extern bool read_int(string s, *(int) ofs_ret, *(int) int_ret);
extern bool read_alphastring(string s, *(int) ofs_ret, 
			     string string_ret, int string_ret_ofs);
extern bool eat_pattern(string s, *(int) ofs_ret, string pat);


#endif
