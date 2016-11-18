/*

Copyright (C) 2007, 2013 Michael Goffioul

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

package org.octave;

public class OctaveReference
{
  private int ID;

  public OctaveReference (int ID)
  {
    this.ID = ID;
  }

  private native static void doFinalize (int ID);

  protected void finalize () throws Throwable
  {
    doFinalize (this.ID);
  }

  public String toString ()
  {
    return ("<octave reference " + this.ID + ">");
  }

  public int getID ()
  {
    return this.ID;
  }

  public Object invoke (Object[] args)
  {
    //System.out.println ("OctaveReference::invoke");
    Octave.doInvoke (this.ID, args);
    return null;
  }

  public synchronized Object invokeAndWait (Object[] args)
  {
    //System.out.println ("OctaveReference::invokeandWait");
    Octave.invokeAndWait (this, args);
    return null;
  }
}
