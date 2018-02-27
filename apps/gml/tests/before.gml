{ /v /u /face			  % bind arguments
  0.8 0.2 v point		  % surface color
  1.0 0.2 1.0			  % kd ks n
} sphere /s

{ /v /u /face			  % bind arguments
  v 0.8 0.2 point		  % surface color
  1.0 0.2 1.0			  % kd ks n
} sphere /s2

{ /v /u /face			  % bind arguments
  0.0 0.9 v point		  % surface color
  1.0 0.2 1.0			  % kd ks n
} cube /c

{ /v /u /face
  1.0 1.0 1.0 point
  1.0 0.0 1.0
} plane /p

%s 3.0 uscale 0.0 0.0  10.0 translate
%c 40.0 uscale -20.0 -41.0 0.0 translate
%union /scene

  s 2.0 uscale 0.0 0.0  6.0 translate
  s2 5.0 uscale 0.0 0.0 10.0 translate
union
/scene

				% directional light
1.0 -1.0 0.0 point		  % direction
1.0  1.0 1.0 point light /l	  % directional light

				% render
0.4 0.4 0.4 point		  % ambient light
[ l ]				  % lights
scene				  % scene to render
3				  % tracing depth
90.0				  % field of view
320 240				  % image wid and height
"before.ppm"			  % output file
render
