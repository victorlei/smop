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

import java.util.*;

public class Octave
{
  private static Object notifyObject = null;
  private static Object[] args = null;
  private static LinkedList invokeList = new LinkedList ();
  private static LinkedList waitList = new LinkedList ();

  public native static boolean call (String name, Object[] argin, Object[] argout);
  public native static void doInvoke (int ID, Object[] args);
  public native static void doEvalString (String cmd);
  public native static boolean needThreadedInvokation ();

  public static void checkPendingAction ()
  {
    if (notifyObject != null)
      {
        synchronized (notifyObject)
          {
            if (notifyObject instanceof OctaveReference)
              doInvoke (((OctaveReference)notifyObject).getID (), args);
            else if (notifyObject instanceof String)
              doEvalString ((String)notifyObject);
            notifyObject.notifyAll ();
          }
        notifyObject = null;
        args = null;
      }

    Object obj;
    Object[] objArgs;

    while (true)
      {
        obj = null;
        objArgs = null;

        synchronized (invokeList)
          {
            if (invokeList.size () > 0)
              {
                obj = invokeList.remove ();
                if (obj instanceof OctaveReference)
                  objArgs = (Object[])invokeList.remove ();
              }
          }

        if (obj != null)
          {
            if (obj instanceof Runnable)
              ((Runnable)obj).run ();
            else if (obj instanceof OctaveReference)
              doInvoke (((OctaveReference)obj).getID (), objArgs);
            else if (obj instanceof String)
              doEvalString ((String)obj);
          }
        else
          break;
      }
    /*
      synchronized (invokeList)
      {
      while (invokeList.size () > 0)
      {
      Object obj = invokeList.remove ();
      if (obj instanceof Runnable)
      ((Runnable)obj).run ();
      if (obj instanceof OctaveReference)
      {
      Object[] objArgs = (Object[])invokeList.remove ();
      doInvoke (((OctaveReference)obj).getID (), objArgs);
      }
      else if (obj instanceof String)
      doEvalString ((String)obj);
      }
      }
    */
  }

  private static void checkWaitState ()
  {
    if (waitList.size () > 0)
      {
        Object wObj = waitList.getFirst ();
        synchronized (wObj)
          {
            wObj.notifyAll ();
          }
      }
  }

  public static void invokeAndWait (OctaveReference ref, Object[] invokeArgs)
  {
    if (needThreadedInvokation ())
      {
        synchronized (ref)
          {
            notifyObject = ref;
            args = invokeArgs;
            try { checkWaitState (); ref.wait (); }
            catch (InterruptedException e) {}
          }
      }
    else
      doInvoke (ref.getID (), invokeArgs);
  }

  public static void evalAndWait (String cmd)
  {
    if (needThreadedInvokation ())
      {
        synchronized (cmd)
          {
            notifyObject = cmd;
            args = null;
            try { checkWaitState (); cmd.wait (); }
            catch (InterruptedException e) {}
          }
      }
    else
      doEvalString (cmd);
  }

  public static void invokeLater (Runnable r)
  {
    if (needThreadedInvokation ())
      synchronized (invokeList)
        {
          invokeList.add (r);
          checkWaitState ();
        }
    else
      r.run ();
  }

  public static void invokeLater (OctaveReference ref, Object[] invokeArgs)
  {
    if (needThreadedInvokation ())
      synchronized (invokeList)
        {
          invokeList.add (ref);
          invokeList.add (invokeArgs);
          checkWaitState ();
        }
    else
      doInvoke (ref.getID (), invokeArgs);
  }

  public static void evalLater (String cmd)
  {
    if (needThreadedInvokation ())
      synchronized (invokeList)
        {
          invokeList.add (cmd);
          checkWaitState ();
        }
    else
      doEvalString (cmd);
  }

  public static void waitFor (Object wObj)
  {
    waitList.add (0, wObj);
    synchronized (wObj)
      {
        while (waitList.size () > 0 && waitList.getFirst () == wObj)
          {
            try { wObj.wait (); }
            catch (InterruptedException e) {}
            checkPendingAction ();
          }
      }
  }

  public static void endWaitFor (Object obj)
  {
    boolean isCurrentWaitObject = (waitList.size () > 0 && waitList.getFirst () == obj);

    waitList.remove (obj);
    if (needThreadedInvokation () && isCurrentWaitObject)
      synchronized (obj)
        {
          obj.notifyAll ();
        }
  }

  public static Object do_test (String name, Object arg0) throws Exception
  {
    Object[] argin = new Object[] { arg0 };
    Object[] argout = new Object[1];
    if (call (name, argin, argout))
      return argout[0];
    throw new Exception ("octave call failed");
  }
}
