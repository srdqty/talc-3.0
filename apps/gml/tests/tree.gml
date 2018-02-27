include "point.gmi"

1.0 0.0 0.0 point /red

{ /n /x 
  n 0 lessi 
  { 1.0 }
  { 
   { /self /n /x 
      n 0 eqi { 1.0 } { x n 1 subi self self apply x mulf } if } /loop
   x n loop loop apply } 
  if
} /pow

6 /tree_depth

%% gen_twig takes thickness, depth and length and returns an object
%% and returns an object
%% theta is rotation about the z-axis, alpha rotation about the y-axis
{ /gen_twig
30.0 /delta_theta
90.0 /delta_alpha
0.66 /twig_scale %% less than 1.0 

360.0 delta_alpha divf floor /num_twigs

{ /self /d /twig /pos /dir
  %% create twig, leave on stack
  dir p2polar apply /alpha /theta /r
  twig_scale d 1 addi pow apply 0.05 mulf /thickness
  twig_scale d 1 addi pow apply /len
  thickness d len gen_twig apply
  theta rotatez
  alpha rotatey
  pos getx pos gety pos getz translate
  %% Iteration
  d 1 addi /d
  d tree_depth eqi { }
  {
   { /self2 /i
    i num_twigs eqi {}
    { delta_theta delta_alpha i real mulf 0.0 1.0 0.0 point rotp apply /dirt
      theta alpha dirt rotp apply /dir2
      i real num_twigs real divf 0.5 mulf 0.5 addf /sc
      len sc mulf theta alpha polar2p apply pos addp apply /pos2
      dir2 pos2 i d self self apply union 
      i 1 addi self2 self2 apply
    } if } /twig_loop
    0 twig_loop twig_loop apply
  }
  if
} /loop
 0.0 1.0 0.0 point %% dir
 0.0 0.0 0.0 point %% pos
 num_twigs         %% twig
 0                 %% depth
 loop
 loop apply
} /gen_tree

{ /len /d /thickness
  d tree_depth 1 subi eqi
  {
    { red 1.0 1.0 1.0 } sphere len 3.0 divf uscale }
  {
    d real tree_depth real divf /ratio
    1.0 ratio subf 0.6 mulf /rc
    ratio 0.75 mulf 0.25 addf /gc
    ratio 0.1 mulf /bc
    { rc gc bc point 
      1.0 0.0 1.0 } cylinder
    thickness len thickness scale 
  }
  if
} /new_twig

new_twig gen_tree apply /the_tree

{ /t /a
  a 360.0 divf t 360.0 divf 0.2 point /c 
  { c 1.0 0.0 1.0 } cylinder 0.05 1.0 0.05 scale 
  t rotatez
  a rotatey
  1.0 t a polar2p apply /p2
  { c 1.0 0.0 1.0 } sphere 0.1 uscale
  p2 getx p2 gety p2 getz translate
  union
} /gen_lollipop

{ /x x 0.0 lessf { x negf } { x } if } /absf

{ /p
  p getx absf apply p gety absf apply p getz absf apply point /c 
  p p2polar apply /a /t /r
  { c 1.0 0.0 1.0 } cylinder 0.05 1.0 0.05 scale 
  t rotatez
  a rotatey
  p /p2
  { c 1.0 0.0 1.0 } sphere 0.1 uscale
  p2 getx p2 gety p2 getz translate
  union
} /gen_lollipop2

{ /t /a
  a 360.0 divf t 360.0 divf 0.2 point /c 
  { c 1.0 0.0 1.0 } cylinder 0.05 1.0 0.05 scale 
  t rotatez
  a rotatey
  t a 0.0 1.0 0.0 point rotp apply /p2
  { c 1.0 0.0 1.0 } sphere 0.1 uscale
  p2 getx p2 gety p2 getz translate
  union
} /gen_lollipop3

0.0 0.0 gen_lollipop apply
  0.0  90.0 gen_lollipop apply union
  0.0  30.0 gen_lollipop apply union
  0.0 180.0 gen_lollipop apply union
  0.0 270.0 gen_lollipop apply union
180.0  45.0 gen_lollipop apply union
 -90.0 90.0 gen_lollipop apply union
 -35.0 -22.0 gen_lollipop apply union
 15.0 rotatey
 -15.0 rotatex
/lollipops

2.0 sqrt 2.0 divf /tmp
1.0 3.0 sqrt divf /tmp2

0.0 1.0 0.0 point gen_lollipop2 apply
%%0.0 -1.0 0.0 point gen_lollipop2 apply union
%%1.0 0.0 0.0 point gen_lollipop2 apply union
-1.0 0.0 0.0 point gen_lollipop2 apply union
%%0.0 0.0 1.0 point gen_lollipop2 apply union
0.0 0.0 -1.0 point gen_lollipop2 apply union
%%tmp tmp 0.0 point gen_lollipop2 apply union
tmp tmp negf 0.0 point gen_lollipop2 apply union
%%tmp negf tmp negf 0.0 point gen_lollipop2 apply union
%%tmp negf tmp 0.0 point gen_lollipop2 apply union
tmp 0.0 tmp point gen_lollipop2 apply union
%%tmp negf 0.0 tmp negf point gen_lollipop2 apply union
%%tmp 0.0 tmp negf point gen_lollipop2 apply union
%%tmp 0.0 tmp point gen_lollipop2 apply union
%%tmp2 tmp2 tmp2 point gen_lollipop2 apply union
/lollipops2

180.0 90.0 gen_lollipop3 apply
0.0 90.0 gen_lollipop3 apply union
45.0 90.0 gen_lollipop3 apply union
%%270.0 40.0 gen_lollipop3 apply union
%%  40.0  90.0 gen_lollipop3 apply union
%%  40.0  30.0 gen_lollipop3 apply union
%%  40.0 180.0 gen_lollipop3 apply union
%% 180.0 40.0 gen_lollipop3 apply union
%%180.0  45.0 gen_lollipop3 apply union
%% -90.0 90.0 gen_lollipop3 apply union
%% -35.0 -22.0 gen_lollipop3 apply union
%%15.0 rotatey
-25.0 rotatex
/lollipops3

the_tree
2.0 uscale
15.0 rotatey
%%lollipops2
0.0 -1.5 1.5 translate
/scene


0.0 0.0 -2.0 point %% position
0.5 0.5 0.5 point   %% intensity
pointlight
/light1

1.0 1.0 1.0 point
[ light1 ]
scene 
10 
90.0
320
200
"tree.ppm"
render
