1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green

{ /v /u /face
  red
  0.8 
  0.2
  1.0
} cube
/cube1

{ /v /u /face
  green
  0.8 
  0.2
  1.0
} cube
/cube2


cube1
cube2
45.0 rotatez
union
0.0 0.0 3.0 translate
/scene

1.0 1.0 1.0 point %% Ambient
[ ]  %% Lights
scene
2   %% Depth
90.0  %% fov
320 %% width (pixels)
200 %% height (pixels)
"rot.ppm" %% filename
render
