#ifndef SOCKET_TYPES_H
#define SOCKET_TYPES_H

#include "tal_sockets.h"

/*****************************************************************************
 * Misc.
 ****************************************************************************/

struct string_file {
  int index;
  string file;
}

/*****************************************************************************
 * Internet addresses (is this how abstract data types work???)
 ****************************************************************************/

extern inet_addr;
extern inet_addr INET_ADDR_ANY;

/* sfuns.pop */
extern void sock_send( file_descr,  string);
extern string sock_read(file_descr);

#endif
