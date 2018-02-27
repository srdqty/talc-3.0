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
/sphere1

{ /v /u /face 
  0.2 0.5 0.2 point
  1.0 1.0 5.0 } plane 
0.0 -1.0 0.0 translate
/plane1

sphere1 
0.0 0.0 3.0 translate
plane1
union
/scene

0.0 4.0 1.0 point %% position
0.7 0.7 0.7 point   %% intensity
pointlight
/light1

0.3 0.3 0.3 point
[ light1 ]
scene 
10 
50.0
600
600
"texture.ppm"
render
