% Tier-3; simple matte plane with a spotlight
%

{ /x } /pop             % pop a stack item
{ /x x x } /dup         % duplicate a stack item

0.0  0.0  0.0  point /black
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  1.0  1.0  point /white
1.0  0.0  1.0  point /magenta
1.0  1.0  0.25 point /yellow

{ /n /f                                 % apply a closure to the values 0..(n-1)
  { /self /i
    n i eqi
    { }
    { i f apply i 1 addi self self apply
    } if
  } /loop
  0 loop loop apply
} /repeat

% ... <color> matte  ==>  <surface>
{ /color
  { /x /x /x
    color 1.0 0.0 1.0
  }
} /matte

white matte apply plane
0.0 -1.0 0.0 translate
/scene

0.0 2.0 2.0 point       % pos
0.0 1.0 2.0 point       % at; (points down)
magenta                 % color
20.0                    % cutoff
10.0                    % exp
spotlight /sl

0.2 0.2 0.2 point       % ambient
[sl]                    % lights
scene                   % object
1                       % depth
90.0                    % fov
320 200                 % wid ht
"tier3.ppm"             % output file
render

