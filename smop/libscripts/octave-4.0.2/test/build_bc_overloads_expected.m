% this script is intended to be Matlab compatible
% first, run the script
%
%   ./build_bc_overloads_tests.sh overloads_only
%
% to generate the overloaded functions.
%
ex.double = 1;
ex.single = single (1);
ex.logical = true;
ex.char = 'char';
ex.int8  = int8 (1);
ex.int16 = int16 (1);
ex.int32 = int32 (1);
ex.int64 = int64 (1);
ex.uint8  = uint8 (1);
ex.uint16 = uint16 (1);
ex.uint32 = uint32 (1);
ex.uint64 = uint64 (1);
ex.cell = {};
ex.struct = struct ();
ex.function_handle = @numel;

f = fieldnames (ex);
n = numel (f);

fid = fopen ('bc_overloads_expected','w');
fid
for i = 1:n
  for j = 1:n
    s = tbcover (ex.(f{i}), ex.(f{j}));
    fprintf (fid, '%s %s %s\n', f{i}, f{j}, s);
  end
end
fclose (fid)
