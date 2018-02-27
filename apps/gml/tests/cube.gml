0.0 0.0 1.0 point /blue
1.0 1.0 1.0 point /white

[ [ blue white blue ]
  [ white blue white ]
  [ blue white blue ]
] / texture

{ /v /u /face 
   blue
   1.0
   0.0
   1.0
} cube
/cube1

cube1
45.0 rotatey
45.0 rotatez
0.0 0.0 3.0 translate
/scene


1.0 1.0 1.0 point
[ ]
scene
2
90.0
320 
200
"cube.ppm"
render
