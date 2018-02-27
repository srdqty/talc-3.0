#ifndef LINENO_H
#define LINENO_H

prefix Lineno {
open   Lineno {

extern struct pos {
  string logical_file;
  string line;
  int    line_no;
  int    col;
}
extern pos pos_of_abs(string,int);

}}
#endif
