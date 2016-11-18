## Copyright (C) 2015 Mike Miller
## Copyright (C) 2005-2015 Michael Zeising
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{y} =} wavread (@var{filename})
## @deftypefnx {Function File} {[@var{y}, @var{fs}, @var{nbits}] =} wavread (@var{filename})
## @deftypefnx {Function File} {[@dots{}] =} wavread (@var{filename}, @var{n})
## @deftypefnx {Function File} {[@dots{}] =} wavread (@var{filename}, [@var{n1} @var{n2}])
## @deftypefnx {Function File} {[@dots{}] =} wavread (@dots{}, @var{datatype})
## @deftypefnx {Function File} {@var{sz} =} wavread (@var{filename}, "size")
## @deftypefnx {Function File} {[@var{n_samp}, @var{n_chan}] =} wavread (@var{filename}, "size")
## Read the audio signal @var{y} from the RIFF/WAVE sound file @var{filename}.
##
## If the file contains multichannel data, then @var{y} is a matrix with the
## channels represented as columns.
##
## If @var{n} is specified, only the first @var{n} samples of the file are
## returned.  If [@var{n1} @var{n2}] is specified, only the range of samples
## from @var{n1} to @var{n2} is returned.  A value of @code{Inf} can be used
## to represent the total number of samples in the file.
##
## If the option @qcode{"size"} is given, then the size of the audio signal
## is returned instead of the data.  The size is returned in a row vector of
## the form [@var{samples} @var{channels}].  If there are two output arguments,
## the number of samples is assigned to the first and the number of channels
## is assigned to the second.
##
## The optional return value @var{fs} is the sample rate of the audio file in
## Hz.  The optional return value @var{nbits} is the number of bits per sample
## as encoded in the file.
##
## @seealso{audioread, audiowrite, wavwrite}
## @end deftypefn

function [y, fs, nbits] = wavread (filename, varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! ischar (filename))
    error ("wavread: FILENAME must be a character string");
  endif

  datatype = "double";
  samples = [1, Inf];
  do_file_size = false;

  if (nargin == 3)
    samples = varargin{1};
    datatype = varargin{2};
  elseif (nargin == 2)
    if (strcmp (varargin{1}, "size"))
      do_file_size = true;
    elseif (ischar (varargin{1}))
      datatype = varargin{1};
    else
      samples = varargin{1};
    endif
  endif

  if (isscalar (samples))
    samples = [1, samples];
  endif

  if (! (isrow (samples) && numel (samples) == 2 && all (samples > 0)
         && all (fix (samples) == samples)))
    error ("wavread: SAMPLES must be a 1- or 2-element integer row vector");
  endif

  if (! (ischar (datatype) && any (strcmp (datatype, {"double", "native"}))))
    error ('wavread: DATATYPE must be either "double" or "native"');
  endif

  info = audioinfo (filename);

  if (do_file_size)
    if (nargout > 1)
      [y, fs] = deal (info.TotalSamples, info.NumChannels);
    else
      y = [info.TotalSamples, info.NumChannels];
    endif
  else
    [y, fs] = audioread (filename, samples, datatype);
    nbits = info.BitsPerSample;
  endif

endfunction


## Functional tests for wavread/wavwrite pair are in wavwrite.m.

## Test input validation
%!error wavread ()
%!error wavread (1)
%!error wavread ("foo.wav", 2, 3, 4)
%!error wavread ("foo.wav", "foo")
%!error wavread ("foo.wav", -1)
%!error wavread ("foo.wav", [1, Inf], "foo");

