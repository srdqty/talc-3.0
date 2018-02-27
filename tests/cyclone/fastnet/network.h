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

/* functions over global variables */
extern void print_pland_hosts_ports(<*(string, int)> list lst);
extern <*(string, int)> list pland_hosts_ports();
extern <*(string, int)> list pland_neighbors_ports();
extern string rout_table_lookup(string, <*(string, string)> list);
extern bool eval_dest(*(string, int));

#endif
