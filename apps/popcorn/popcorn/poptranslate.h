#ifndef POPTRANSLATE_H
#define POPTRANSLATE_H

#include "list.h"

#include "popil.h"
#include "popsyntax.h"
#include "poptypeenv.h"

prefix Poptranslate {

extern Popil::cf_file trans_file(Poptypeenv::global_env, 
				 <Popsyntax::top_decl>List::list);

}

#endif
