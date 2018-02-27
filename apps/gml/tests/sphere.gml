1.0 0.0 0.0 point /red

{ /v /u /face 
   red
   1.0
   0.0
   1.0
} sphere
/sphere1

1.0 -1.0 1.0 point %% position
0.4 0.4 0.4 point   %% intensity
light
/sun

sphere1
0.5 uscale
0.0 0.0 1.0 translate
/scene

0.5 0.5 0.5 point %% Ambient
[ sun ]  %% Lights
scene
2          %% Depth
90.0       %% fov
320        %% width (pixels)
200        %% height (pixels)
"sphere.ppm" %% filename
render
