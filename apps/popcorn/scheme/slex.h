#ifndef SLEX_H
#define SLEX_H

// don't prefix these -- it confuses bison

// bison already defines this and we don't allow duplicates now
// extern int yylex(); 
extern int yyline;

prefix Slex {
open   Slex {

extern void slex_init(); // MUST BE CALLED PRIOR TO SCANNING!!!
extern exception SlexError(string);

}}
#endif
