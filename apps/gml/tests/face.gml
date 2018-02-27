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

{ /p1 /p2
  p1 getx p2 getx addf
  p1 gety p2 gety addf
  p1 getz p2 getz addf
  point } /addp

{ /u /v /face
  white
  0.95
  0.05
  1.0
} plane
/field

{ /u /v /face
  white
  1.0
  0.0
  1.0
} sphere
1.5 uscale
/ball1

{ /u /v /face
  white
  1.0
  0.0
  1.0
} sphere
2.0 uscale
/ball2

{ /u /v /face
  yellow
  1.0
  0.0
  1.0
} cylinder
0.05 1.7 0.05 scale
/twig

twig  20.0 rotatez
twig  10.0 rotatez union
twig   0.0 rotatez union
twig -10.0 rotatez union
twig -20.0 rotatez union
/broom_head

{ /u /v /face
  brown
  1.0 
  0.0
  1.0
} cylinder
0.1 5.5 0.1 scale
broom_head
0.0 5.5 0.0 translate
union
/broomstick

{ /u /v /face
  grey
  0.2
  0.8
  1.0
} sphere
0.12 uscale
/button

{ /u /v /face
  white
  1.0 
  0.0
  1.0
} sphere
1.1 1.3 1.0 scale
/head

{ /u /v /face
  red
  1.0 
  0.0
  1.0
} sphere
0.2 uscale
/nose

{ /u /v /face
  black
  1.0
  0.0
  1.0
} sphere
0.2 uscale
/eye

{ /u /v /face
  darkgreen
  0.2 
  0.8
  1.0
} sphere
0.1 uscale
/tooth

0.4
/mouth_width 
0.2
/mouth_height

%% Tooth placement
%% Given an angle, and a center for the mouth returns a tooth.
{ /theta  %% angle -- float
  /center %% center of mouth -- point 
  theta sin mouth_width mulf  %% x placement 
  theta cos mouth_height mulf %% y placement 
  0.0  %% z placement?
  point
  center
  addp apply
  /offset
  tooth
  offset getx 
  offset gety
  offset getz  translate
} /put_tooth

0.0 0.0 0.0 point
/mouth_center

mouth_center 90.0 put_tooth apply
mouth_center 145.0 put_tooth apply
union
mouth_center 180.0 put_tooth apply
union
mouth_center 215.0 put_tooth apply
union
mouth_center 270.0 put_tooth apply
union
/mouth

head
nose
0.0 -0.2 -1.0 translate 
union

eye
-0.4 0.1 -0.8 translate
eye
0.4 0.1 -0.8 translate
union

union

mouth
0.0 -0.45 -0.9 translate
union

ball1
0.0 -2.5 0.0 translate
union

button
0.0 -2.0 -1.4 translate
union

button
0.0 -2.5 -1.5 translate
union

ball2
0.0 -5.0 0.0 translate
union

broomstick
20.0 rotatez
-5.0 rotatex
-0.5 -6.0 -0.8 translate
union

field
0.0 -6.5 0.0 translate
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
"face.ppm" %% filename
render
