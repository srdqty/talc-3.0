%% Time warp.

1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue
1.0 1.0 1.0 point /white
1.0 0.0 1.0 point /magenta
1.0 1.0 0.25 point /yellow
0.0 0.0 0.0 point /black
0.5 0.5 0.5 point /grey
0.0 0.0 0.5 point /darkblue
0.2 0.2 0.2 point /darkgrey

%% Roof

{/v /u /face white 1.0 1.0 1.0 } sphere /roof_base

roof_base 
roof_base 0.95 uscale 
difference
15.0 uscale
/roof

%% Floor

{ /v /u /face darkgrey 1.0 1.0 2.0 } plane 
/my_floor

%% Housing

{ /v /u /face grey 1.0 1.0 1.0 } /metallic

metallic cube -0.5 -0.5 -0.5 translate 2.3 2.3 0.1 scale /base_cube
base_cube 45.0 rotatez
base_cube
intersect
0.0 0.0 -1.9 translate
/frame

{ /v /u /face face 1 eqi {darkgrey} {black} if  1.0 1.0 1.0 } cone 1.05 2.0 1.05 scale -90.0 rotatex /housing

housing frame union /housing

%% Swirl

30 /streak_arc %% in degrees
[
 { white 1.0 1.0 1.0 }
 { black 1.0 0.0 1.0 }
 { red   1.0 1.0 1.0 }
] /streaks

{ /v /u /face
  face 1 eqi 
  { blue 1.0 0.0 1.0   } %% True
  { streaks 360.0 u v addf mulf floor streak_arc divi streaks length modi get apply }
  if 
} cone
1.0 2.05 1.0 scale
-90.0 rotatex
/swirl

%% Stairs

5 /stairs_num
1.0 stairs_num real divf /stair_height

{ /v /u /face  face 0 eqi {black}{grey} if 1.0 0.2 2.0 } cylinder /stair_base

{ /i 
  stair_height i real mulf  /y
  stairs_num i subi real -1.0 stairs_num real divf mulf /z
  stairs_num i subi real 2.0 stairs_num real mulf divf 0.5 addf /w 
  stair_base
  w stair_height w scale
  0.0 y z translate
} /stair_i
  
{ /self /i
  i 1 eqi
  { 1 stair_i apply }
  { i stair_i apply i 1 subi self self apply union }
  if
} /stair_gen

stairs_num stair_gen stair_gen apply 
1.0 0.3 1.0 scale
/stairs

%% Observer

{ darkblue 1.0 1.0 1.0 } sphere 0.25 uscale /observer


%% Housed swirl

housing swirl difference
0.0 0.0 1.8 translate
/housed_swirl

%% The button

{ /v /u /face black 1.0 1.0 1.0 } cube
-0.5 -0.5 -0.5 translate
2.0 0.05 2.0 scale
/button_base

{ /v /u /face red 1.0 0.0 2.0 } cylinder 
0.75 0.2 0.75 scale
/button

button button_base union
0.0 0.0 1.0 translate 
0.5 1.0 0.5 scale 
-30.0 rotatex
-45.0 rotatey
/button

%% The scene

housed_swirl
observer -0.25 0.25 0.0 translate
union
0.0 1.2 0.0 translate
stairs
union
my_floor
union
button
-1.5 0.0 -3.0 translate
union
roof union
0.0 -1.0 0.0 translate
50.0 rotatey
1.0 0.0 2.5 translate
/scene

0.0 0.0 -2.0 point %% position
0.5 0.5 0.5 point   %% intensity
pointlight
/light1

-5.0 0.0 1.0 point %% pos 
-5.0 0.5 2.0 point %% at
green %% color
10.0 %% cutoff
1.0 %% exp
spotlight /light2

0.1 0.1 0.1 point %% Ambient
[ light1 light2 ]  %% Lights
scene
10   %% Depth
90.0  %% fov
320 %% width (pixels)
200 %% height (pixels)
"warp.ppm" %% filename
render
