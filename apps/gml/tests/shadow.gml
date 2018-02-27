
1.0 0.0 0.0 point /red
0.0 0.0 1.0 point /red

{ /v /u /face			  % bind arguments
  red		                  % surface color
  1.0 0.2 1.0			  % kd ks n
} sphere /s

s 0.0 0.0 10.0 translate /scene		                  

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
"shadow.ppm"			  % output file
render
