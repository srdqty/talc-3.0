#ifndef SPLAY_H
#define SPLAY_H

#include "core.h"
#include "list.h"

prefix Splay {
open   Splay {

extern union <a,b>tree {
  void         Leaf;
  <a,b>noderef Node;
}

extern struct <a,b>noderef {
  <a,b>node v;
}

extern struct <a,b>node {
  a key;
  b data;
  <a,b>tree left;
  <a,b>tree right;
}

extern bool splay<a,b>(int f(a,a), a, <a,b>tree);

}}
#endif
