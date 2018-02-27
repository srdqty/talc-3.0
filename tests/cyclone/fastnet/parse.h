#ifndef PARSE_H
#define PARSE_H

open List;

/* PLAN programs */
extern pop_pkt deflist_to_pop_pkt(<def> list);
extern string  deflist_to_str(<def> list);
extern string  exp_to_str(exp);
extern exp     get_fexp(string_file);
extern void    print_exp(exp);
extern void    print_fundef(fundef);
extern void    print_value(value);
extern string  pop_pkt_to_str(pop_pkt);
extern pop_pkt str_to_pop_pkt(string_file);
extern string  value_to_str(value);
extern string  valuelist_to_str(<value> list, string);

/* hostnames */
extern *(string, int) parse_hostname(string);
extern *(string, int) parse_hostnames(<string> list);

#endif
