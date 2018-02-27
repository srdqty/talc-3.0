#ifndef POPUNIFY_H
#define POPUNIFY_H
// Popunify:  provides unification, coercion, and type-well-formedness
// checks for Poptype.

#include "list.h"
#include "popsyntax.h"
#include "gcdfec.h"
#include "poptypeenv.h"
#define location Gcdfec::seg

prefix Popunify {
open Popunify {

open Popsyntax {
open List {
open Poptypeenv {

// returns true when the type constructor n is an ? type
extern bool possibly_null(global_env genv,var n);
// Check that a type is well-formed with respect to a global environment
// and a list of potentially free type variables.
extern void check_valid_type(location loc,
			     global_env g,<var>list tyvars,typ t);
// check a list of types
extern void check_valid_types(location loc,
			      global_env g,<var>list tyvars,<typ>list ts);
// check that the types in a function declaration are well-formed
extern typ check_fundecl_valid_type(global_env g,location loc,fndecl fd);
// instantiate a type t to yield a new type -- the instantiation is
// an association list mapping type variables to types.
extern typ subst(<*(var,typ)>list inst,typ t);
// used to generate a fresh instantiation of polymorphic things.
extern *(var,typ) gen_type_var(var v);
// compresses out any Evars that have been constrained 
extern typ compress(typ t);
// unify two types
extern bool unify(global_env g,typ t1,typ t2);
// rewrite a cast expression e so that it conforms with IL invariants.
extern void rewrite_cast(exp e);
// insert a cast expression
extern void coerce(exp e,typ t);
// true if t is a "coercable" type (e.g., numeric or character)
extern bool coercable(typ t);

// unify the types of two expressions if possible.
// If not, attempt to coerce one to the other --
// we assume the expressions already have types on them.
extern bool unify_coerce2(global_env g,exp e1,exp e2);

// Perform integral promotions only.
extern bool unify_coerce_int(global_env g, exp e);

// Given a target type and a typed expression, coerce e to the target type if
// possible.
extern bool unify_coerce1(global_env g, exp e, typ t);

// returns an expression for a default initializer for a type t (if any) 
extern exp default_initializer(global_env g,typ t,location loc);

// this type must be fully defined, and those it uses must be mentioned
extern void destruct_struct(global_env g, type_name n);
extern void destruct_union (global_env g, type_name n);
extern void destruct_abstype(global_env g,type_name n);
extern void destruct_exn   (global_env g, string    n);
extern void mention_types(global_env g, typ t);

}}}}}
#endif
