#ifndef __GMLSYNTAX_H
#define __GMLSYNTAX_H

#include "top.h"
#include "core.h"
#include "list.h"
#include "dict.h"
#include "point.h"
#include "affine.h"

#define id string
#define list List::list
#define env <Gmlsyntax::value>list

#define print_id print_string

prefix Gmlsyntax {
open Gmlsyntax;
/* Primitives taking one float argument. */
extern union prim_fp1 {
  void Acos;
  void Asin;
  void Clampf;
  void Cos;
  void Floor;
  void Frac;
  void Sin;
  void Sqrt;
}

extern union prim_fp2 {
  void Addf;
  void Divf;
  void Eqf;
  void Lessf;
  void Mulf;
  void Subf;
}

extern union prim_i2 {
  void Addi;
  void Divi;
  void Eqi;
  void Lessi;
  void Modi;
  void Muli;
  void Subi;
}

extern union prim_obj {
  void Cone;
  void Cube;
  void Cylinder;
  void Plane;
  void Sphere;
}

extern union prim_obj1 {
  void Rotatex;
  void Rotatey;
  void Rotatez;
  void Uscale;
}

extern union prim_obj2 {
  void Difference;
  void Intersect;
  void Union;
}

extern union prim_point {
  void Getx;
  void Gety;
  void Getz;
}

extern union prim {
  void Apply;
  void Get;
  void If;
  void Length;
  void Light;
  void Negi;
  void Negf;
  void Point;
  void Pointlight;
  void Print;
  void Real;
  void Render;
  void Scale;
  void Spotlight;
  void Translate;
  prim_fp1   Fp1;
  prim_fp2   Fp2;
  prim_i2    I2;
  prim_obj   Obj;
  prim_obj1  Obj1;
  prim_obj2  Obj2;
  prim_point Prim_Point;
}

// Syntax from the parser.
 extern union syn {
  id Sid;
  id Sbind;
  value Spush;
  <syn>list Sfun;
  <syn>list Sarray;
  prim Sprim;
 }

 // After we convert to de-bruijn indices.
extern union token {
  int Tid;
  void Tbinder;
  value Tpush;
  <token>list Tfun;
  <token>list Tarray;
  prim Tprim;
  void ArrayDone; // Reads from the top of the stack down until it finds a 
  // marker into a new array and binds that on the stack.
}

extern union value {
  void Marker; // Used to mark the start of an array on the stack.
  bool Vbool;
  int  Vint;
  FP   Vfp;
  string Vstring;
  *(<value>list,<token>list) Vclos;
  *(<value>list,void f(<value>list)) Vclos_rtcg;
  value Varray[];
  Point::point Vpoint;
  light Vlight;
  obj Vobj;
}


extern union solid {
  void Sphere;
  void Cube;
  void Cylinder;
  void Cone;
  void Plane;
}

extern struct surf {
  color s_color;
  FP s_kdiff;
  FP s_kspec;
  FP s_phong;
}

extern struct base {
  value o_clos;
  solid o_solid;
}

extern union obj {
  base Base;
  *(obj,obj) Union;
  *(obj,obj) Inter;
  *(obj,obj) Diff;
  *(Affine::t,obj) Trans;
}

extern struct ldir {
  Point::point ld_dir;
  color  ld_color;
}

extern struct lpoint {
  Point::point lp_pos;
  color lp_color;
}

extern struct lspot {
  Point::point ls_pos;
  Point::point ls_at;
  color  ls_color;
  degree ls_cutoff;
  FP     ls_exp;
}

extern union light {
  ldir Ldir;
  lpoint Lpoint;
  lspot Lspot;
}

/* Environments must be functional! */
extern env empty_env();
extern env extend(env,value);
extern value lookup(env,int);

extern string prim_to_string(prim);

extern void print_syn(syn);
extern void print_token(token);
extern void print_value(value);
extern void print_env(env);

extern void print_machine(env,<value>list,<token>list);

extern <token>list syn_to_token(<syn>list,<id>list);
}

#endif
