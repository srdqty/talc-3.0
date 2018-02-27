%% mandel.gml

511 /num_colors
0.33001 /left
0.007 /width
0.41001 /top
0.007 /height

0.0 0.0 0.0 point /black
0.2 0.2 0.2 point /grey

{ {false} {true} if} /not
{ { } { /x false } if } /and

[
 { /c   c 0.2 0.2 point }
 { /c 0.2   c 0.2 point }
 { /c 0.2 0.2   c point }
 { /c   c   c   c point }
] /color_ranges

{ /c
 c real num_colors real divf /b
 b 4.0 mulf /rangef rangef floor /range
 range 4 eqi 
 { grey }
 { rangef range real subf /c c sqrt color_ranges range get apply } if
} /makeColor

{ /cr /ci
  { /self /count /zr /zi /r2 /i2
    r2 i2 addf 4.0 lessf 
    count 0 eqi not apply
    and apply
    { zr zi mulf 2.0 mulf ci addf /zi
      r2 i2 subf cr addf /zr
      zr zr mulf /r2
      zi zi mulf /i2
      count 1 subi /count
      i2 r2 zi zr count self self apply }
   { num_colors count subi } 
   if
  } /f
  0.0 0.0 0.0 0.0 num_colors f f apply
} /mandel

{ /v /u /face
  u width mulf left addf /x
  v height mulf top addf /y 
  x y mandel apply makeColor apply
  1.0 0.0 1.0 } sphere 2.0 uscale /mandel_sphere

{ /v /u /face 0.15 0.15 0.55 point 1.0 0.0 1.0 } plane
0.0 -1.0 0.0 translate
mandel_sphere
union
0.0 0.0 2.5 translate
/scene

0.0 0.0 -2.0 point %% position
0.5 0.5 0.5 point   %% intensity
pointlight
/light1

1.0 1.0 1.0 point %% Ambient
[ light1 ]  %% Lights
scene
10   %% Depth
90.0  %% fov
320 %% width (pixels)
200 %% height (pixels)
"mandel.ppm" %% filename
render




