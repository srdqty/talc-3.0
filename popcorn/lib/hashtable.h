#ifndef HASHTABLE_H
#define HASHTABLE_H

prefix Hashtable {
open   Hashtable {

extern table<a,b>;

extern <a,b>table create<a,b>(int sz, int cmp(a,a), int hash(a));
extern void       insert<a,b>(<a,b>table t, a key, b val);
extern b          lookup<a,b>(<a,b>table t, a key);   // raises Not_found

// remove only most recent binding.  Silently return if none.
// does a destructive list update since this data structure is not functional
extern void remove<a,b>(<a,b>table t, a key);

extern int hash_string(string s);
extern void iter<a,b>(void f(a,b), <a,b>table t);

// debugging
extern 
void print_table_map<a,b>(<a,b>table t, void prn_key(a), void prn_val(b));

}}

// from stdlib.c

extern int hash<a>(a x);
extern int ptr_compare<a>(a x1, a x2);

#endif

