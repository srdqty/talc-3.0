#ifndef EVAL_H
#define EVAL_H

extern value     eval_pop_pkt(<Env::environ> list, pop_pkt);
extern <value> list eval_explist(<exp> list, <Env::environ> list);

#endif
