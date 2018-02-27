#ifndef TCEXP_H
#define TCEXP_H

#include "absyn.h"
#include "gcdfec.h"
#include "synpp.h"
#include "tcenv.h"
#include "tcutil.h"
#include "evexp.h"

prefix Tcexp{
open Tcexp{

open Core;
open List;
open Gcdfec;
open Absyn;
open Synpp;
open Tcenv;
open Tcutil;
open Evexp;

extern synth tcExp(tenv, <typ>Opt, exp);
extern bool is_const_exp(tenv, exp);

}}
#endif
