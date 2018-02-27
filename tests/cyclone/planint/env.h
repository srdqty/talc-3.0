#ifndef ENV_H
#define ENV_H

/*
extern union binding {
  value  Varbind;
  string Exnbind;
  *(<*(string, tipe)> List::list, tipe, exp, environ) Funbind;
  value Svcbind (<value> List::list vl, pop_pkt ppkt);
}
*/

extern union binding {
  value  Varbind;
  string Exnbind;
  *(<*(string, tipe)> List::list, tipe, exp, environ) Funbind;
  string Svcbind;
}

extern ?struct environ {
  string id;
  binding bind;
  environ next;
}

extern environ Env::add(string, binding, environ);
extern environ Env::add_actuals(<*(string, tipe)> List::list,
                                <value> List::list, environ);
extern binding Env::lookup(string, environ);
extern void    Env::print(environ env);
extern void    Env::print_service_env();

extern <*(string, value (<value> List::list vl, pop_pkt ppkt))> List::list Env::service_env;

#endif
