1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue
1.0 1.0 1.0 point /white
1.0 0.0 1.0 point /magenta
1.0 1.0 0.25 point /yellow

{ /v /u /face
  red
  0.5
  0.5
  30.0
} sphere
/sphere1

{ /v /u /face
  white
  0.1
  0.9
  1.5
} plane
/plane1

plane1
0.0 -0.5 0.0 translate
sphere1
0.0 0.0 2.0 translate
union
/scene

0.0 -1.0 0.0 point %% position
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

0.25 0.25 0.25 point %% Ambient
[ light2 ]  %% Lights
scene
2   %% Depth
90.0  %% fov
320 %% width (pixels)
200 %% height (pixels)
"light.ppm" %% filename
render
