## Copyright (C) 2013-2015 Vytautas Jančauskas
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
## @deftypefn  {Function File} {@var{recorder} =} audiorecorder ()
## @deftypefnx {Function File} {@var{recorder} =} audiorecorder (@var{fs}, @var{nbits}, @var{channels})
## @deftypefnx {Function File} {@var{recorder} =} audiorecorder (@var{fs}, @var{nbits}, @var{channels}, @var{id})
## Create an audiorecorder object recording 8 bit mono audio at 8000 Hz
## sample rate.
##
## The optional arguments @var{fs}, @var{nbits}, @var{channels}, and @var{id}
## specify the sample rate, bit depth, number of channels and recording
## device id, respectively.  Device IDs may be found using the audiodevinfo
## function.
## @end deftypefn

## FIXME: callbacks don't work properly, apparently because portaudio
## will execute the callbacks in a separate thread, and calling Octave
## functions in a separate thread which is likely to cause trouble with
## all of Octave's global data...
##
## @deftypefnx {Function File} {@var{recorder} =} audiorecorder (@var{function}, @dots{})
##
## Given a function handle, use that function to process the audio.

function recorder = audiorecorder (varargin)

  if (nargin > 5)
    print_usage ();
  endif

  if (nargin > 0 && ischar (varargin{1}))
    varargin{1} = str2func (varargin{1});
  endif

  recorder.recorder = __recorder_audiorecorder__ (varargin{:});
  recorder = class (recorder, "audiorecorder");

endfunction


%!demo
%! recorder = audiorecorder (44100, 16, 2);
%! record (recorder, 1);
%! sleep (2);
%! player1 = audioplayer (recorder);
%! player2 = getplayer (recorder);
%! play (player1);
%! sleep (2);
%! play (player2);
%! sleep (2);
%! stop (player1);
%! stop (player2);

## Tests of audiorecorder must not actually record anything.

%!testif HAVE_PORTAUDIO
%! recorder = audiorecorder (44100, 16, 2);
%! data = getaudiodata (recorder, "int16");
%! assert (strcmp (class (data), "int16"));
%! data = getaudiodata (recorder, "int8");
%! assert (strcmp (class (data), "int8"));
%! data = getaudiodata (recorder, "uint8");
%! assert (strcmp (class (data), "uint8"));
%! assert (size (data)(1), recorder.TotalSamples);
%! assert (size (data)(2), 2);

%!testif HAVE_PORTAUDIO
%! recorder = audiorecorder;
%! set (recorder, {"SampleRate", "Tag", "UserData"}, {8000, "tag", [1, 2; 3, 4]});
%! assert (recorder.SampleRate, 8000);
%! assert (recorder.Tag, "tag");
%! assert (recorder.UserData, [1, 2; 3, 4]);

%!testif HAVE_PORTAUDIO
%! recorder = audiorecorder;
%! settable = set (recorder);
%! settable.SampleRate = 8000;
%! settable.Tag = "tag";
%! settable.UserData = [1, 2; 3, 4];
%! set (recorder, settable);
%! assert (recorder.SampleRate, 8000);
%! assert (recorder.Tag, "tag");
%! assert (recorder.UserData, [1, 2; 3, 4]);

%!testif HAVE_PORTAUDIO
%! recorder = audiorecorder;
%! recorder.SampleRate = 8000;
%! recorder.Tag = "tag";
%! recorder.UserData = [1, 2; 3, 4];
%! properties = get (recorder, {"SampleRate", "Tag", "UserData"});
%! assert (properties, {8000, "tag", [1, 2; 3, 4]});

#%!function status = callback_record (sound)
#%!  fid = fopen ("record.txt", "at");
#%!  for index = 1:rows(sound)
#%!    fprintf (fid, "%.4f, %.4f\n", sound(index, 1), sound(index, 2));
#%!  endfor
#%!  fclose (fid);
#%!  status = 0;
#%!endfunction

#%!testif HAVE_PORTAUDIO
#%! recorder = audiorecorder (@callback_record, 44100);
#%! unlink ("record.txt")
#%! record (recorder);
#%! sleep (2);
#%! stop (recorder);
#%! s = stat ("record.txt");
#%! assert (s.size > 0);

#%!testif HAVE_PORTAUDIO
#%! recorder = audiorecorder (@callback_record, 44100);
#%! unlink ("record.txt")
#%! record (recorder);
#%! sleep (2);
#%! stop (recorder);
#%! s = stat ("record.txt");
#%! assert (s.size > 0);
