#ifndef C_STRING_H
#define C_STRING_H

extern FILE?;

extern int    C_strlen(string s);
extern int    C_strnlen(string s, int ofs);
extern void   C_strcat(string dest,string src);
extern void   C_strncat(string dest,string src,int src_ofs);
extern string expand(string s, int sz); /* like realloc */
extern string realloc_str(string str, int sz); /* conditional realloc */
extern void   C_strcpy(string dest,string src);
extern void   C_strncpy(string dest,int dest_ofs,
			string src,int src_ofs,int len);
extern string C_strdup(string src);
extern int    C_strchr(string s, int ofs, char c);
extern int    C_strrchr(string s, int ofs, char c);
extern int    C_strncmp(string s1, int ofs1, string s2, int ofs2, int len);
extern int    C_strncasecmp(string s1, int ofs1, string s2, int ofs2, int len);
extern int    C_strcmp(string s1, int ofs1, string s2, int ofs2);
extern int    C_strpbrk(string s, int ofs, string accept);
extern int    C_strspn(string s, int ofs, string accept);
extern void   C_fprint_string(FILE f, string s, int ofs);
extern void   C_fprint_raw_string(FILE f, string s, int ofs);

#endif
