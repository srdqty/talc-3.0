#ifndef EVEXP_H
#define EVEXP_H

#include "core.h"
#include "absyn.h"
#include "list.h"
#include "gcdfec.h"

open Core;
open Absyn;
open List;
open Gcdfec;

prefix Evexp {
open Evexp {

extern unsigned int eval_const_uint_exp(exp e);
extern void exp_err(seg sg,string msg);


}}
#endif
