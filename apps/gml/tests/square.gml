1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue
1.0 1.0 1.0 point /white
1.0 0.0 1.0 point /magenta
1.0 1.0 0.25 point /yellow

[ [ blue white blue ]
  [ white blue white ]
  [ blue white blue ]
] / texture

[ red white green blue magenta yellow ] /texture2

{ /v /u /face
   u 0.25 lessf { red } { blue } if
   0.5 %% kd
   0.5 %% ks
   1.0 %% n
} sphere
/sphere1

{ /v /u /face
   white
   0.2
   0.8
   2.0
} plane
/plane1

{ /v /u /face 
   { 
     clampf 3.0 mulf floor /i
     i 3 eqi { 2 } { i } if
   } /toIntCoord
   texture u toIntCoord apply get v toIntCoord apply get
   1.0
   0.0
   1.0
} sphere
/cube1

{ /v /u /face 
   { 
     clampf 3.0 mulf floor /i
     i 3 eqi { 2 } { i } if
   } /toIntCoord
   texture u toIntCoord apply get v toIntCoord apply get
   1.0
   0.0
   1.0
} cylinder
/cyl1

{ /v /u /face 
   { 
     clampf 3.0 mulf floor /i
     i 3 eqi { 2 } { i } if
   } /toIntCoord
   texture u toIntCoord apply get v toIntCoord apply get
   0.2
   0.8
   2.0
} cone
/cone1


{ /v /u /face
  texture2 face get
  1.0
  0.0
  1.0
} cube
/cube2

%% My light
-200.0
-1.0
2.0
point
0.75
0.75
0.75
point
pointlight
/light1

0.0
0.5
-200.0
point
0.5
0.5
0.5
point
pointlight
/light2

1.0 0.5 0.0 point % position
0.0 0.0 1.0 point % at
0.8 0.8 0.8 point % color
15.0              % cutoff
1.0               % exponent
spotlight
/light3

 cube1
 45.0 rotatex
 45.0 rotatey
 0.0 0.0 2.0 translate
 cube2
 45.0 rotatex
 0.5 0.0 2.0 translate
 difference
sphere1
45.0 rotatex
45.0 rotatey
45.0 rotatez
-1.0 0.0 2.0 translate
union
-45.0 rotatez
0.0 -1.0 0.0 translate
sphere1
0.0 0.0 3.0 translate
/scene1

cube1
60.0 rotatex
45.0 rotatey
-1.5 0.0 4.0 translate
sphere1
0.0 0.0 4.5 translate
union
plane1
0.0 -0.5 0.0 translate
union
/scene2

cone1
-45.0 rotatex
-45.0 rotatez
0.0 0.10 2.0 translate
plane1
0.0 -0.5 0.0 translate
union
/scene3

cone1
%%-45.0 rotatex
%%-45.0 rotatez
0.0 0.0 1.0 translate
/scene4

cube1
45.0 rotatey
-60.0 rotatex
0.0 0.0 0.0 translate
/scene5
 
0.8 0.8 0.8 point %% Ambient
[ ]  %% Lights
scene5
2   %% Depth
90.0  %% fov
320 %% width (pixels)
200 %% height (pixels)
"square.ppm" %% filename
render

