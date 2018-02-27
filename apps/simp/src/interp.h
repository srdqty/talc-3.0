#ifndef __INTERP_H
#define __INTERP_H

#include "core.h"

prefix Interp {
open Interp;
extern union action {
  void PrintRegs;
  int  PrintGp;
  int  PrintFp;
  void  PrintPc;
  *(ptr,int) DumpMem;
  *(ptr,int) DumpStrMem;
  *(ptr,int) Disassemble; // Disassemble n instruction starting at ptr.
  int DisassemblePc;
  int  RunN;
  int  RunVerboseN;
  void Run;
  void Exit;
  void Nop;
  string Error;
  string SetOutputFile;
}

extern action get_command(FILE f);

}
#endif
