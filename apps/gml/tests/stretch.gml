%
% stretch.gml
%

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

{ /u /v /face
  red 1.0 0.0 1.0 } sphere
/s1

s1
1.0 1.0 10.0 scale
0.0 0.0 10.0 translate
/scene

1.0 1.0 1.0 point	% ambient
[] 		        % lights
scene			% object
3			% depth
90.0			% fov
640 400			% wid ht
"stretch.ppm"		% output file
render
