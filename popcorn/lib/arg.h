
#ifndef ARG_H
#define ARG_H

#include "core.h"
#include "list.h"

prefix Arg  {
open   Arg  {
open   List {

extern exception Error;

extern struct bool_ref { bool v; }

extern union spec {
  void Unit();          // Call the function with unit argument     
  bool_ref Set;         // Set the reference to true                
  bool_ref Clear;       // Set the reference to false               
  void String(string);  // Call the function with a string argument 
  void Int(int);        // Call the function with an int argument   
  void Rest(string);    // Stop interpreting keywords and call the
                           // function with each remaining argument 
}

extern exception Bad(string);

extern void usage (<*(string,spec,string)>list, string);

extern int current;

extern void parse(<*(string,spec,string)>list, void anonfun(string),string);

}}}

#endif
