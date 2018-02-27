#ifndef MISC_H
#define MISC_H

#include "tal_sockets.h"
#include "list.h"

/* pland globals */
extern <*(string, string, string, int, string, int)>
                           List::list pland_interface_info;
extern <*(string, string)> List::list pland_rtable;
extern <*(FILE, string)>   List::list pland_interfaces;
extern <string> List::list pland_neighbors;
extern <*(string, string)> List::list pland_spf_edges;
extern string              received_host;
extern FILE                logfile;
extern int                 current_rb;

extern <*(string, string)> List::list
  do_spf(string, <*(string, string)> List::list);
extern string make_node(string, int);

extern int                MAX_LST_LEN;
extern int                MAX_IDENT_LEN;
extern int                MAX_PLANPROG;

/* routing table */
extern void read_rout_table_file(string_file);
extern void read_interface_file(string_file);
extern string rout_table_lookup(string, <*(string, string)> List::list);

/* names, ports, etc. */
extern *(string, int) parse_hostname(string);
extern *(string, int) parse_hostnames(<string> List::list);
extern void print_pland_hosts_ports(<*(string, int)> List::list lst);
extern <*(string, int)> List::list pland_hosts_ports();
extern <*(string, int)> List::list pland_neighbors_ports();

/* error exceptions */
extern exception InternalError(string);
extern exception PlanExcept(string);
extern exception ResourceBound(string);

extern void print_Error(string);
extern void print_Internal(string);
extern bool eval_dest(*(string, int));

/* eventually goes in popcorn/lib/list.pop */
extern <a> List::list list_del<a>(a, <a> List::list);

/* string_files */
extern struct string_file {
  int index;
  string file;
}

#endif
