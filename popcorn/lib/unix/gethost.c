#include "unixsupport.h"
#include "socketaddr.h"
#ifdef __linux__
#include <netdb.h>
#endif

/*
struct host_entry {
  string h_name;
  string h_aliases[];
  socket_domain h_addrtype;
  int h_addr_list[];
}
*/

struct pop_host_entry {
  string h_name;
  array h_aliases;
  int h_addrtype;
  array h_addr_list;
};

static int copy_inet_addr(int inet_addr) { return inet_addr; }

static struct pop_host_entry *alloc_host_entry(struct hostent *entry) {
  struct pop_host_entry *pop_entry;

  pop_entry = xalloc(sizeof(struct pop_host_entry));
  pop_entry->h_name = Cstring_to_string(entry->h_name);
  pop_entry->h_aliases = copy_popstring_array(entry->h_aliases);
  pop_entry->h_addrtype = 
    entry->h_addrtype == PF_UNIX ? UNIX_ADDR : INET_ADDR;
#ifdef h_addr /* abbreviation for first element of array */
  pop_entry->h_addr_list = copy_poparray(copy_inet_addr,entry->h_addr_list);
#else /* h_addr part of hostent struct */
  { int addr[1];
    addr[0] = entry->h_addr;
    pop_entry->h_addr_list = copy_poparray(copy_inet_addr,addr);
  }
#endif  
  return pop_entry;
}

struct pop_host_entry *unix_gethostbyname(string hostname) {
  struct hostent *entry;
  assert(hostname->chars[hostname->size] == '\0');
  entry = gethostbyname(hostname->chars);
  if (entry == (struct hostent *) NULL) 
    unix_error(__FILE__,__LINE__,"gethostbyname");
  return alloc_host_entry(entry);
}

struct pop_host_entry *unix_gethostbyaddr(int addr) {
  struct hostent *entry;
  entry = gethostbyaddr(&addr, 4, AF_INET);
  if (entry == (struct hostent *) NULL) 
    unix_error(__FILE__,__LINE__,"gethostbyaddr");
  return alloc_host_entry(entry);
}
