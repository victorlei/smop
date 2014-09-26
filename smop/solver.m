function mv = solver(ai,af,w)
rand(1,2,3);
%
% Copyright 2004 The MathWorks, Inc.

nBlocks = max(ai(:));
[m,n] = size(ai);

% Make increment tables
% N=1, E=2, S=3, W=4
I = [0  1  0 -1];
J = [1  0 -1  0];

a = ai;
mv = [];

while ~isequal(af,a)

    % Pick a random block
    bid = ceil(rand()*nBlocks);
    [i,j] = find(a==bid);

    % Move it in a random direction
    r = ceil(rand()*4);
    ni = i + I(r);
    nj = j + J(r);

    % Is it a legal move? Check edges
    if (ni<1) || (ni>m) || (nj<1) || (nj>n)
        continue
    end

    % Is it a legal move? Check for collision
    if a(ni,nj)>0
        continue
    end

    % Check distance
    % Get the target location
    [ti,tj] = find(af==bid);
    d = (ti-i)^2 + (tj-j)^2;
    dn = (ti-ni)^2 + (tj-nj)^2;
    % Have we moved closer to the target location?
    if (d<dn) && (rand()>0.05)
        continue
    end

    % Move the block
    a(ni,nj) = bid;
    a(i,j) = 0;

    % Record the move
    mv(end+1,[1 2]) = [bid r];
    end
end

function r = rand(varargin)
    global s1 s2 s3
    if nargin != 0
        r=0;
        s1=varargin{1};
        s2=varargin{2};
        s3=varargin{3};
    else
        [r,s1,s2,s3] = r8_random(s1,s2,s3);
    end
end

