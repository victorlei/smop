/*

Copyright (C) 2010, 2013 Martin Hepperle

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// A class to hold a TeX character code -> Unicode translation pair.

package org.octave;

public class TeXcode
{
  protected String tex;
  protected char ucode;

  public TeXcode (String t, char u)
  {
    tex = t;
    ucode = u;
  }
}
