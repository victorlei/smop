/*

Copyright (C) 2002-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_octave_h)
#define octave_octave_h 1

#ifdef  __cplusplus
extern "C" {
#endif

extern OCTINTERP_API int octave_main (int argc, char **argv, int embedded);

extern OCTINTERP_API void octave_process_command_line (int argc, char **argv);

extern OCTINTERP_API void
octave_initialize_interpreter (int argc, char **argv, int embedded);

extern OCTINTERP_API int octave_execute_interpreter (void);

extern OCTINTERP_API int octave_cmdline_argc;
extern OCTINTERP_API char **octave_cmdline_argv;
extern OCTINTERP_API int octave_embedded;

extern OCTINTERP_API int octave_starting_gui (void);
extern OCTINTERP_API int octave_fork_gui (void);

#ifdef  __cplusplus
}
#endif

#endif
