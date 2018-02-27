#ifndef __RENDER_H
#define __RENDER_H

#include "top.h"
#include "gmlsyntax.h"


prefix Render{
open Render;

 extern bool optimize_scene;
 
 extern void render(string filename, 
		    int width, 
		    int height, 
		    FP fov, 
		    int depth, 
		    Gmlsyntax::obj scene, 
		    Gmlsyntax::light lights[], 
		    Point::point a);
}
#endif
