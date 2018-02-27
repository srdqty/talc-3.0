#ifndef EVAL_H
#define EVAL_H

extern value     eval_pop_pkt(environ, pop_pkt);
extern <value> List::list eval_explist(<exp> List::list, environ);

#endif
