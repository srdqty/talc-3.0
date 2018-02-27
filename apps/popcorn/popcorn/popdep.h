#ifndef POPDEP_H
#define POPDEP_H

#include "list.h"

prefix Popdep {

// given source name, target name, directory include list, find all the
// reachable #include files and return an appropriate make target
extern string generate_depend(string,string,<string>List::list);

}

#endif
