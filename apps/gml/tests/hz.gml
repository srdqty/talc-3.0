1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue
1.0 1.0 1.0 point /white
1.0 0.0 1.0 point /magenta
1.0 1.0 0.25 point /yellow

{ /v /u /face
  red
  0.2
  0.8
  30.0
} sphere
/sphere1

{ /v /u /face
  green
  0.8 
  0.2
  1.0
} cube
/cube1

{ /v /u /face
  white
  0.05
  0.95
  1.5
} plane
/plane1

{ /v /u /face
  1.0 u v mulf 0.25 point
  0.8
  0.2
  2.0
} cylinder
/cylinder1

{ /x x x  } /dup 

{ /v /u /face
  u 0.5 mulf 1.0 v subf lessf 
	{ 1.0 v subf u 0.75 mulf lessf { white } { blue } if} { blue } if
  0.8
  0.2
  2.0
} cone
/cone1

sphere1
0.5 uscale
-2.5 0.0 -0.5 translate
cube1
-0.5 -0.5 -0.5 translate
-1.0 0.0 0.0 translate
union
cylinder1
0.0 -0.5 0.0 translate
0.5 1.0 0.5 scale
0.5 0.0 0.0 translate
union
cone1
0.0 -0.5 0.0 translate 
0.5 1.0 0.5 scale
2.0 0.0 -0.5 translate
union
30.0 rotatex
0.0 0.0 3.0 translate
plane1
0.0 -0.5 0.0 translate
union
/scene1

0.0 10.0 1.0 point %% position
1.0 1.0 1.0 point   %% intensity
pointlight
/light1

0.5 0.5 0.5 point %% Ambient
[ light1 ]  %% Lights
scene1
2   %% Depth
90.0  %% fov
320 %% width (pixels)
200 %% height (pixels)
"hz.ppm" %% filename
render
