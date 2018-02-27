1.0 1.0 1.0 point /white

{ /v /u /face
  white
  1.0
  0.0
  1.0
} sphere
0.5 uscale
/ball

ball
0.0 0.0 2.0 translate 
/scene

0.0 0.0 1.0 point %% position
1.0 1.0 1.0 point   %% intensity
light
/sun

1.0 -1.0 1.0 point %% position
0.8 0.8 0.8 point   %% intensity
light
/light1

0.0 0.75 2.0 point %% position
0.8 0.8 0.8 point   %% intensity
pointlight
/light2

0.0 0.75 2.0 point % position
0.0 0.0 2.0 point % at
1.0 1.0 1.0 point % color
50.0              % cutoff
2.0               % exponent
spotlight
/light3

0.0 0.0 0.0 point %% Ambient
[ sun ]  %% Lights
scene
2   %% Depth
90.0  %% fov
100 %% width (pixels)
100 %% height (pixels)
"test.ppm" %% filename
render


