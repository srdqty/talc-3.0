#include "list.h"

prefix Queue {
  open Queue {

extern queue<a>;

extern exception Queue_empty;
extern exception Queue_full;

extern <a>queue create<a>(int max_sz);

extern void enqueue<a>(<a>queue q, a elem);
extern a dequeue<a>(<a>queue q);
extern a peek<a>(<a>queue q);
extern int length<a>(<a>queue q);
extern int max_size<a>(<a>queue q);

}}
