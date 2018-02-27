#include "opcodes.h"

/* Instruction type */

struct inst
{
  int opcode;
  int val;
};

/* loads some values into memory, then rotates them around */
struct inst cprog1[]=
{
  {PUSH, 0},    {PUSH, 0},  {ST, EMPTY},
  {PUSH, 111},  {PUSH, 1},  {ST, EMPTY},
  {PUSH, 222},  {PUSH, 2},  {ST, EMPTY},
  {PUSH, 333},  {PUSH, 3},  {ST, EMPTY},

  {PUSH, 1},  {LD, EMPTY},  {PUSH, 0},  {ST, EMPTY},
  {PUSH, 2},  {LD, EMPTY},  {PUSH, 1},  {ST, EMPTY},
  {PUSH, 3},  {LD, EMPTY},  {PUSH, 2},  {ST, EMPTY},
  {PUSH, 0},  {LD, EMPTY},  {PUSH, 3},  {ST, EMPTY},

  {PUSH, 1},  {LD, EMPTY},  {PUSH, 0},  {ST, EMPTY},
  {PUSH, 2},  {LD, EMPTY},  {PUSH, 1},  {ST, EMPTY},
  {PUSH, 3},  {LD, EMPTY},  {PUSH, 2},  {ST, EMPTY},
  {PUSH, 0},  {LD, EMPTY},  {PUSH, 3},  {ST, EMPTY},

  {PUSH, 1},  {LD, EMPTY},  {PUSH, 0},  {ST, EMPTY},
  {PUSH, 2},  {LD, EMPTY},  {PUSH, 1},  {ST, EMPTY},
  {PUSH, 3},  {LD, EMPTY},  {PUSH, 2},  {ST, EMPTY},
  {PUSH, 0},  {LD, EMPTY},  {PUSH, 3},  {ST, EMPTY},

  {END, EMPTY}
};

/* arithmetic operations */
struct inst cprog2[]=
{
  {PUSH, 1234},

  {PUSH, 5678},    {ADD, EMPTY},
  {PUSH, 4},       {MUL, EMPTY},
  {PUSH, 3},       {MUL, EMPTY},
  {PUSH, 21},      {MUL, EMPTY},
  {PUSH, 1740590}, {SUB, EMPTY},

  {PUSH, 5678},    {ADD, EMPTY},
  {PUSH, 4},       {MUL, EMPTY},
  {PUSH, 3},       {MUL, EMPTY},
  {PUSH, 21},      {MUL, EMPTY},
  {PUSH, 1740590}, {SUB, EMPTY},

  {PUSH, 5678},    {ADD, EMPTY},
  {PUSH, 4},       {MUL, EMPTY},
  {PUSH, 3},       {MUL, EMPTY},
  {PUSH, 21},      {MUL, EMPTY},
  {PUSH, 1740590}, {SUB, EMPTY},

  {PUSH, 5678},    {ADD, EMPTY},
  {PUSH, 4},       {MUL, EMPTY},
  {PUSH, 3},       {MUL, EMPTY},
  {PUSH, 21},      {MUL, EMPTY},
  {PUSH, 1740590}, {SUB, EMPTY},

  {PUSH, 5678},    {ADD, EMPTY},
  {PUSH, 4},       {MUL, EMPTY},
  {PUSH, 3},       {MUL, EMPTY},
  {PUSH, 21},      {MUL, EMPTY},
  {PUSH, 1740590}, {SUB, EMPTY},

  {PUSH, 5678},    {ADD, EMPTY},
  {PUSH, 4},       {MUL, EMPTY},
  {PUSH, 3},       {MUL, EMPTY},
  {PUSH, 21},      {MUL, EMPTY},
  {PUSH, 1740590}, {SUB, EMPTY},

  {PUSH, 3},  {ST  ,  EMPTY},

  {END, EMPTY}
};

/* triply nested loop */
struct inst cprog3[]=
{
           {PUSH, 0},  {PUSH, 0},  {ST, EMPTY},

           {PUSH, 2}, {PUSH, 1}, {ST, EMPTY},
  /* 6 */  
           {PUSH, 1}, {LD, EMPTY}, 
           {PUSH, 0}, {CMP, EMPTY},
           {JEQ, 54},

           {PUSH, 2}, {PUSH, 2}, {ST, EMPTY},
  /* 14 */  
           {PUSH, 2}, {LD, EMPTY}, 
           {PUSH, 0}, {CMP, EMPTY},
           {JEQ, 47},

           {PUSH, 2}, {PUSH, 3}, {ST, EMPTY},
  /* 22 */ 
           {PUSH, 3}, {LD, EMPTY},
           {PUSH, 0}, {CMP, EMPTY},
           {JEQ, 40},

           {PUSH, 0}, {LD, EMPTY},
           {PUSH, 1}, {ADD, EMPTY},
           {PUSH, 0}, {ST, EMPTY},

	   {PUSH, 3}, {LD, EMPTY}, {PUSH, 1}, {SUB, EMPTY}, {PUSH, 3}, {ST, EMPTY},

           {JMP, 22},
  /* 40 */ {PUSH, 2}, {LD, EMPTY}, {PUSH, 1}, {SUB, EMPTY}, {PUSH, 2}, {ST, EMPTY},
           {JMP, 14},
  /* 47 */ {PUSH, 1}, {LD, EMPTY}, {PUSH, 1}, {SUB, EMPTY}, {PUSH, 1}, {ST, EMPTY},
           {JMP, 6},
  /* 54 */ {END, EMPTY}
};

/* three sequential loops */
struct inst cprog4[]=
{
           {PUSH, 0},  {PUSH, 0},  {ST, EMPTY},

           {PUSH, 5}, {PUSH, 1}, {ST, EMPTY},
  /* 6 */ 
           {PUSH, 1}, {LD, EMPTY},
           {PUSH, 0}, {CMP, EMPTY},
           {JEQ, 24},

           {PUSH, 0}, {LD, EMPTY},
           {PUSH, 1}, {ADD, EMPTY},
           {PUSH, 0}, {ST, EMPTY},

	   {PUSH, 1}, {LD, EMPTY}, {PUSH, 1}, {SUB, EMPTY}, {PUSH, 1}, {ST, EMPTY},
           {JMP, 6},

  /* 24 */ {PUSH, 5}, {PUSH, 2}, {ST, EMPTY},
  /* 27 */ 
           {PUSH, 2}, {LD, EMPTY},
           {PUSH, 0}, {CMP, EMPTY},
           {JEQ, 45},

           {PUSH, 0}, {LD, EMPTY},
           {PUSH, 1}, {ADD, EMPTY},
           {PUSH, 0}, {ST, EMPTY},

	   {PUSH, 2}, {LD, EMPTY}, {PUSH, 1}, {SUB, EMPTY}, {PUSH, 2}, {ST, EMPTY},
           {JMP, 27},

  /* 45 */ {PUSH, 5}, {PUSH, 3}, {ST, EMPTY},
  /* 48 */ 
           {PUSH, 3}, {LD, EMPTY},
           {PUSH, 0}, {CMP, EMPTY},
           {JEQ, 66},

           {PUSH, 0}, {LD, EMPTY},
           {PUSH, 1}, {ADD, EMPTY},
           {PUSH, 0}, {ST, EMPTY},

	   {PUSH, 3}, {LD, EMPTY}, {PUSH, 1}, {SUB, EMPTY}, {PUSH, 3}, {ST, EMPTY},
           {JMP, 48},

  /* 66 */ {END, EMPTY}
};

/* factorial */
struct inst cprog5[]=
{ 
  {PUSH, 1}, 
  {PUSH, 0},
  {ST, EMPTY}, /* accum = 1 */

  {PUSH, 1},  /* BEGIN: */
  {LD, EMPTY},
  {PUSH, 0},
  {CMP, EMPTY},
  {JEQ, 23}, /* if(n == 0) then goto FIN */

  {PUSH, 0},
  {LD, EMPTY},
  {PUSH, 1},
  {LD, EMPTY},
  {MUL, EMPTY},
  {PUSH, 0},
  {ST, EMPTY}, /* accum = accum * n */

  {PUSH, 1},
  {LD, EMPTY},
  {PUSH, 1},
  {SUB, EMPTY},
  {PUSH, 1},
  {ST, EMPTY}, /* n = n - 1 */
  {JMP, 3}, /* goto BEGIN */

  /* {PUSH, 1},  FIN: (Useless instruction just for the end) */
  {END, EMPTY}
}; 
