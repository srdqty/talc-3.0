#ifndef PP_H
#define PP_H

#include "core.h"
#include "list.h"

prefix PP {
open   PP {
open List{
extern Ppstate;
extern Out;
extern Doc;
extern string string_of_doc(Doc d, int w);
extern void file_of_doc(Doc d, int w, FILE f);
extern *(string,<*(int,int,int,string)>list) string_and_links(Doc d, int w);
extern Doc text(string s);
extern Doc nil_doc();
extern Doc blank_doc();
extern Doc hyperlink(string shrt, string full);
extern Doc line_doc();
extern Doc nest(int k, Doc d);
extern Doc concat(Doc d1, Doc d2);
extern Doc concats(<Doc>list doclist);
extern Doc doc_union(Doc d, Doc d2);
extern Doc oline_doc();
extern Doc tab(Doc d);
extern Doc ppseq<a>(Doc pp(a), string sep, <a>list l0);
extern Doc seq(string sep, <Doc>list l0);
extern Doc ppseql<a>(Doc pp(a), string sep, <a>list l0);
extern Doc seql(string sep, <Doc>list l0);
}}}
#endif
