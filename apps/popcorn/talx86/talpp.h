#ifndef TALPP_H
#define TALPP_H

#include "core.h"
#include "id.h"
#include "xarray.h"
#include "tal.h"

prefix Talpp {
open   Talpp {
open   Tal {

extern FILE output_file;

extern void pp_comment(string);
extern void pp_code_block (code_block cb);
extern void pp_code_block_x(Id::id i, <con>Core::Opt t, 
			    <instruction>Xarray::xarray insts);
extern void pp_tal_before_code(string name, tal_imp m);
extern void pp_tal_after_code (tal_imp m);
extern void pp_tal_imp(string, tal_imp);
extern void pp_tal_int(string, tal_int);

// Cyclone +
// When streaming a template use these to start and end the template.
// Body of template is printed as usual with pp_code_block_x
extern void pp_template_start(Id::id start_id, con tipe);
extern void pp_template_end();
// Cyclone -

extern exception Unimplemented(string);

extern void pp_con(con);
extern void pp_reg(reg r);
}}}

#endif
