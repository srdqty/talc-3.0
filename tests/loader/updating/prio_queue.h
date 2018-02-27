#include "list.h"

prefix Queue {
  open Queue {

extern prio_queue<a>;

extern exception Queue_empty;
extern exception Queue_full;

extern <a>prio_queue create<a>(int max_sz);

extern void enqueue<a>(<a>prio_queue q, a elem, int prio);
extern a dequeue<a>(<a>prio_queue q);
extern int length<a>(<a>prio_queue q);
extern int max_size<a>(<a>prio_queue q);

}}
