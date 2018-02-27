1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.8 0.0 point /darkgreen
0.0 0.0 1.0 point /blue
1.0 1.0 1.0 point /white
0.5 0.5 0.5 point /grey
1.0 0.0 1.0 point /magenta
1.0 1.0 0.25 point /yellow
0.0 0.0 0.0 point /black
0.5 0.5 0.1 point /brown
0.3 0.2 0.05 point /darkbrown

{ /u /v /face
  white
  0.95
  0.05
  1.0
} plane
/field

{ /u /v /face
  darkbrown 1.0 0.0 1.0 } cube
0.3 uscale
-0.15 -0.15 -0.15 translate
45.0 rotatez
/spoke

{ /u /v /face
  grey 1.0 0.0 1.0 } cylinder
0.5 0.2 0.5 scale
-90.0 rotatex
spoke 0.0 0.0 -0.2 translate
union
0.0 0.45 0.0 translate
/wheel

{ /u /v /face
  darkbrown
  1.0
  0.0
  1.0
} cube
/simple_cube

simple_cube 2.0 3.5 2.0 scale
/base1
simple_cube 2.0 2.7 2.0 scale
/base2
simple_cube 1.5 1.0 2.0 scale
/base3

base1
base2 
-2.0 0.0 0.0 translate union
base3
-3.5 0.0 0.0 translate union
wheel 
1.0 0.0 0.0 translate union
wheel
-1.0 0.0 0.0 translate union
wheel
1.0 0.0 2.0 translate union
wheel
-1.0 0.0 2.0 translate union
/base

{/u /v /face
 grey
 1.0
 0.0
 1.0
} cylinder
1.0 6.0 1.0 scale
/cyl1

{/u /v /face
 black
 1.0
 0.0
 1.0
} cylinder
1.0 6.0 1.0 scale
/cyl2

{/u /v /face
 grey
 1.0 
 0.0
 1.0
} sphere
/barrel_end

{/u /v /face
 grey
 1.0
 0.0
 1.0} cylinder
0.3 uscale 
0.5 0.0 0.0 translate
/fuse

cyl1
cyl2 0.9 1.1 0.9 scale
difference
-90.0 rotatez
barrel_end union
fuse 
0.0 1.0 0.0 translate union
/barrel

field
0.0 -6.5 0.0 translate
base
barrel
-2.0 3.5 1.0 translate
union
/cannon

cannon
45.0 rotatey
0.0 -6.5 2.0 translate
union
0.0 2.0 5.0 translate 
/scene

1.0 -1.0 1.0 point %% position
0.4 0.4 0.4 point   %% intensity
light
/sun

0.6 0.6 0.6 point %% Ambient
[ sun ]  %% Lights
scene
2   %% Depth
90.0  %% fov
200 %% width (pixels)
200 %% height (pixels)
"cannon.ppm" %% filename
render
