#ifndef MISC_H
#define MISC_H

#include "planbase.h"

open List;

extern string make_node(string, int);

/* routing table */
extern void read_rout_table_file(string_file);
extern void read_interface_file(string_file);

/* Shortest Path First */
extern <*(string, string)> list do_spf(string, <*(string, string)> list);


/* eventually goes in popcorn/lib/list.pop */
/* extern <a> list list_del<a>(a, <a> list); */

#endif
