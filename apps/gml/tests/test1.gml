1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green

{ /v /u /face
  red
  0.5
  0.5
  1.0
} sphere
/sphere1

{ /v /u /face
  green
  0.05
  0.95
  1.0
} plane
/plane1


sphere1
0.0 0.0 0.0 translate
/scene1

0.0 2.0 2.0 point %% position
0.5 0.5 0.5 point   %% intensity
light
/light1

0.75 0.75 0.75 point %% Ambient
[light1]  %% Lights
scene1
2   %% Depth
90.0  %% fov
320 %% width (pixels)
200 %% height (pixels)
"test1.ppm" %% filename
render
