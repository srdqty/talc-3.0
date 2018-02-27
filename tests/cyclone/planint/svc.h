#ifndef SVC_H
#define SVC_H

extern environ Svc::toplevel_env();
extern environ Svc::top_environ;
extern value   Svc::eval_svc(string, <value> List::list, pop_pkt);
extern void    Svc::init_services();


#endif
