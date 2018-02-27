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
120.0 /delta_alpha
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
    { red 1.0 0.0 0.0 } sphere 0.0 1.0 0.0 translate len 5.0 divf uscale }
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


the_tree
2.0 uscale
15.0 rotatey
0.0 -1.5 1.5 translate
/scene


0.0 5.0 -1.0 point %% position
0.5 1.0 0.8 point   %% intensity
pointlight
/light1

0.5 0.5 0.5 point
[ light1 ]
scene 
10 
50.0
600
768
"tree.ppm"
render
