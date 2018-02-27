% Tier-1; Two planes in a vee with a sphere above
%

{ /x } /pop             % pop a stack item
{ /x x x } /dup         % duplicate a stack item

1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue

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
  { 3 pop repeat        % discard face, u, v
    color 1.0 0.0 1.0
  }
} /matte


  green matte apply plane
  15.0 rotatez

  blue matte apply plane
  -15.0 rotatez

union
0.0 -1.0 0.0 translate

  red matte apply sphere
  0.0 1.0 5.0 translate

union
/scene

1.0 1.0 1.0 point       % ambient
[]                      % lights
scene                   % object
1                       % depth
90.0                    % fov
320 200                 % wid ht
"t1-003.ppm"            % output file
render

