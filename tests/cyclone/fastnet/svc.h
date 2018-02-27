#ifndef SVC_H
#define SVC_H

extern <Env::environ> list Svc::toplevel_env();
extern <Env::environ> list Svc::top_environ;

extern value   Svc::eval_svc(string, <value> list, pop_pkt);
extern void    Svc::init_services();


extern void    Svc::print_service_env();

extern <*(string, value (<value> list vl, pop_pkt ppkt))> list
  Svc::svc_env;

#endif
