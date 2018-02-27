#ifndef __PPM_H
#define __PPM_H

prefix Ppm {
open Ppm;

extern pixmap;

extern pixmap init(int width, int height);
extern void dump(string filename, pixmap p);
extern void load(string filename, pixmap p);

extern int width(pixmap p);
extern int height(pixmap p);

extern void setp(pixmap p, int i, int j, int r, int g, int b); 

}

#endif
