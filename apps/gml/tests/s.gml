% This example is a sphere subtracted from a cube with
% a pointlight in the center of the sphere.
%

0.2 0.6 0.2 point /c1
0.6 0.2 0.2 point /c2

{ /v /u /face
  c1 0.7 1.0 10.0
} cube
-0.5 -0.5 -0.5 translate
1.5 uscale

{ /v /u /face
  c2 0.5 1.0 10.0
} sphere 
difference
0.0 0.0 2.0 translate /scene


1.0 -1.0 1.0 point
0.5 0.5 0.5 point light /dl
0.0 0.0 2.0 point
0.8 0.8 0.80 point pointlight /pl

0.6 0.6 0.6 point
[ dl pl ]
scene
1
60.0
320 200
"s.ppm"
render

