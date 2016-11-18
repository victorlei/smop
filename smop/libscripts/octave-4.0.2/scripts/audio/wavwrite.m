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
## @deftypefn  {Function File} {} wavwrite (@var{y}, @var{filename})
## @deftypefnx {Function File} {} wavwrite (@var{y}, @var{fs}, @var{filename})
## @deftypefnx {Function File} {} wavwrite (@var{y}, @var{fs}, @var{nbits}, @var{filename})
## Write the audio signal @var{y} to the RIFF/WAVE sound file @var{filename}.
##
## If @var{y} is a matrix, the columns represent multiple audio channels.
##
## The optional argument @var{fs} specifies the sample rate of the audio signal
## in Hz.
##
## The optional argument @var{nbits} specifies the number of bits per sample
## to write to @var{filename}.
##
## The default sample rate is 8000 Hz and the default bit depth is 16 bits
## per sample.
##
## @seealso{audiowrite, audioread, wavread}
## @end deftypefn

function wavwrite (y, varargin)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  ## Defaults.
  fs = 8000;
  nbits = 16;

  filename = varargin{end};
  if (nargin > 2)
    fs = varargin{1};
    if (nargin > 3)
      nbits = varargin{2};
    endif
  endif

  ## calculate filesize
  [n, channels] = size (y);

  ## allow y to be a row vector
  if (n == 1)
    y = y(:);
    n = channels;
    channels = 1;
  endif

  ## test arguments
  if (channels < 1)
    error ("wavwrite: Y must have at least one column");
  endif

  if (channels > 0x7FFF)
    error ("wavwrite: Y must have no more than 32767 columns");
  endif

  if (! (isscalar (fs) && (fs > 0)))
    error ("wavwrite: sample rate FS must be a positive number");
  endif

  if (! isscalar (nbits) || isempty (find (nbits == [8, 16, 24, 32])))
    error ("wavwrite: bit depth NBITS must be 8, 16, 24, or 32");
  endif

  audiowrite (filename, y, fs, "BitsPerSample", nbits);

endfunction


%!shared fname
%! fname = [tempname() ".wav"];

%!testif HAVE_SNDFILE
%! A = [-1:0.1:1; -1:0.1:1]';
%! unwind_protect
%!   wavwrite (A, fname);
%!   [B, samples_per_sec, bits_per_sample] = wavread (fname);
%!   assert (B, A, 2^-14);
%!   assert (samples_per_sec, 8000);
%!   assert (bits_per_sample, 16);
%! unwind_protect_cleanup
%!   unlink (fname);
%! end_unwind_protect

%!testif HAVE_SNDFILE
%! A = [-1:0.1:1; -1:0.1:1]';
%! unwind_protect
%!   wavwrite (A, 4000, fname);
%!   [B, samples_per_sec, bits_per_sample] = wavread (fname);
%!   assert (B, A, 2^-14);
%!   assert (samples_per_sec, 4000);
%!   assert (bits_per_sample, 16);
%! unwind_protect_cleanup
%!   unlink (fname);
%! end_unwind_protect

%!testif HAVE_SNDFILE
%! A = [-1:0.1:1; -1:0.1:1]';
%! unwind_protect
%!   wavwrite (A, 4000, 8, fname);
%!   [B, samples_per_sec, bits_per_sample] = wavread (fname);
%!   assert (B, A, 2^-6);
%!   assert (samples_per_sec, 4000);
%!   assert (bits_per_sample, 8);
%! unwind_protect_cleanup
%!   unlink (fname);
%! end_unwind_protect

%!testif HAVE_SNDFILE
%! A = [-2:2]';
%! unwind_protect
%!   wavwrite (A, fname);
%!   B = wavread (fname);
%!   B *= 32768;
%!   assert (B, [-32767 -32767 0 32767 32767]');
%! unwind_protect_cleanup
%!   unlink (fname);
%! end_unwind_protect

%!testif HAVE_SNDFILE
%! A = [-1:0.1:1];
%! unwind_protect
%!   wavwrite (A, fname);
%!   [B, samples_per_sec, bits_per_sample] = wavread (fname);
%!   assert (B, A', 2^-14);
%!   assert (samples_per_sec, 8000);
%!   assert (bits_per_sample, 16);
%! unwind_protect_cleanup
%!   unlink (fname);
%! end_unwind_protect

%!testif HAVE_SNDFILE
%! A = [-1:0.1:1; -1:0.1:1]';
%! unwind_protect
%!   wavwrite (A, fname);
%!   B = wavread (fname, 15);
%!   assert (B, A(1:15,:), 2^-14);
%!   wavwrite (A, fname);
%!   B = wavread (fname, [10, 20]);
%!   assert (B, A(10:20,:), 2^-14);
%! unwind_protect_cleanup
%!   unlink (fname);
%! end_unwind_protect

%!testif HAVE_SNDFILE
%! A = [-1:0.1:1; -1:0.1:1]';
%! unwind_protect
%!   wavwrite (A, fname);
%!   [nsamp, nchan] = wavread (fname, "size");
%!   assert (nsamp, 21);
%!   assert (nchan, 2);
%! unwind_protect_cleanup
%!   unlink (fname);
%! end_unwind_protect

## Test input validation
%!error wavwrite ()
%!error wavwrite (1)
%!error wavwrite (1,2,3,4,5)
%!error wavwrite ([], "foo.wav");

