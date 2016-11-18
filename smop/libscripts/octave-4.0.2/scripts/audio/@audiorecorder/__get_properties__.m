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
## @deftypefn {Function File} {@var{properties} =} __get_properties__ (@var{recorder})
## Return a struct containing all named properties of the recorder object
## @var{recorder}.
## @end deftypefn

function props = __get_properties__ (recorder)

  if (nargin != 1)
    print_usage ();
  endif

  if (__recorder_isrecording__ (struct (recorder).recorder))
    running = "on";
  else
    running = "off";
  endif

  props = struct ("BitsPerSample",
                  __recorder_get_nbits__ (struct (recorder).recorder),

                  "CurrentSample",
                  __recorder_get_sample_number__ (struct (recorder).recorder),

                  "DeviceID",
                  __recorder_get_id__ (struct (recorder).recorder),

                  "NumberOfChannels",
                  __recorder_get_channels__ (struct (recorder).recorder),

                  "Running",
                  running,

                  "SampleRate",
                  __recorder_get_fs__ (struct (recorder).recorder),

                  "TotalSamples",
                  __recorder_get_total_samples__ (struct (recorder).recorder),

                  "Tag",
                  __recorder_get_tag__ (struct (recorder).recorder),

                  "Type",
                  "audiorecorder",

                  "UserData",
                  __recorder_get_userdata__ (struct (recorder).recorder));

endfunction
