#ifndef SCOMP_H
#define SCOMP_H

#include "tal.h"
#include "list.h"
#include "sil.h"
#include "sast.h"

prefix Scomp {

extern bool print_comments;
extern bool peephole_optimize;

extern Tal::tal_imp code_gen(<*(string,Sil::ilexp)>List::list defs);

extern exception undefined_var(Sast::var);

}

#endif
