#ifndef PLANBASE_H
#define PLANBASE_H

#include <list.h>
open List;

/* max values */
#define MAX_LST_LEN   (255)
#define MAX_IDENT_LEN (255)
#define MAX_PLANPROG  (20000)

/* string_files */
extern struct string_file {
  int index;
  string file;
}

/* Error exceptions */
extern exception InternalError(string);
extern exception PlanExcept(string);
extern exception ResourceBound(string);

/* pland globals */
extern <*(string, string, string, int, string, int)>
                           list pland_interface_info;
extern <*(string, string)> list pland_rtable;
extern <*(FILE, string)>   list pland_interfaces;
extern <string> list       pland_neighbors;
extern <*(string, string)> list pland_spf_edges;
extern string              received_host;
extern FILE                logfile;
extern int                 current_rb;

#endif
