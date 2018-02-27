include "texture.gmi"

%% Test scene.

yellow stripet apply 6.0 scalet apply -0.5 0.0 shiftt apply /stripe1
orange stripet apply 3.0 scalet apply -0.1 0.0 shiftt apply /stripe2


lightblue baset apply
stripe1 -0.2 0.0 shiftt apply
stripe1 flipt apply 0.0 0.1 shiftt apply
stripe2 -0.2 0.0 shiftt apply
stripe2 flipt apply 0.0 -0.5 shiftt apply mixt apply mixt apply mixt apply
layert apply /plaid_base

plaid_base 
0.5 0.5 shiftt apply
2.0 swirlt apply 
-0.5 -0.5 shiftt apply 
%% 0.25 0.25 tilet apply
0.01 blurt apply
/plaid

%%% Sample scene

{ /v /u /face 
  u v plaid apply
  1.0 0.0 1.0 } sphere 
%%-0.5 -0.5 -0.5 translate
%%2.0 uscale 
/cube1

cube1 
0.0 0.0 2.5 translate
/scene

0.0 0.0 -2.0 point %% position
0.5 0.5 0.5 point   %% intensity
pointlight
/light1

1.0 1.0 1.0 point
[ light1 ]
scene 
10 
90.0
320
200
"texture.ppm"
render
