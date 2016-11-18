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

package org.octave;

import java.awt.event.*;
import javax.swing.*;

/**
 * <p>Implements button click actions for dialog box functions</p>
 *
 * <p>Copyright (c) 2010 Martin Hepperle</p>
 *
 * @author Martin Hepperle
 * @version 1.0
 */
public class DlgListener
  extends WindowAdapter implements ActionListener, KeyListener
{
  // the parent frame of the dialog
  JDialogBox m_Parent;

  public DlgListener (JDialogBox d)
  {
    m_Parent = d;
  }


  //
  // --- extension of the WindowAdapter class ---
  //
  /**
   * Called when the user clicks the close button on the window frame.
   *
   * @param e WindowEvent
   */
  public void windowClosing (WindowEvent e)
  {
    m_Parent.closeDialog (JDialogBox.CLOSE_CANCEL);
  }

  public void windowOpened(WindowEvent e)
  {
    m_Parent.setFocus();
  }


  //
  // --- implementation of the ActionListener interface ---
  //
  /**
   * Called when the user clicks a button in the dialog.
   * Closes the dialog when either a button with an
   * action command OK, CANCEL or NO is pressed.
   *
   * @param e ActionEvent
   */
  public void actionPerformed (ActionEvent e)
  {
    if (e.getActionCommand ().equals ("OK"))
      {
        m_Parent.closeDialog (JDialogBox.CLOSE_OK);
      }
    else if (e.getActionCommand ().equals ("CANCEL"))
      {
        m_Parent.closeDialog (JDialogBox.CLOSE_CANCEL);
      }
    else if (e.getActionCommand ().equals ("NO"))
      {
        m_Parent.closeDialog (JDialogBox.CLOSE_NO);
      }
    else if (e.getActionCommand ().equals ("SELALL"))
      {
        m_Parent.SelectAll ();
      }
  }


  //
  // --- implementation of the KeyListener interface ---
  //
  /**
   * Closes the dialog when the ENTER or ESCAPE keys are released.
   *
   * @param e KeyEvent
   */
  public void keyTyped (KeyEvent e)
  {
    if (e.getKeyCode () == KeyEvent.VK_ESCAPE)
      {
        m_Parent.closeDialog (JDialogBox.CLOSE_CANCEL);
      }
  }


  /**
   * @param e KeyEvent
   */
  public void keyPressed (KeyEvent e)
  {
    if (e.getSource ().getClass ().equals (JTextArea.class))
      {
        JTextArea ta = (JTextArea) e.getSource ();
        if (e.getKeyCode () == KeyEvent.VK_ENTER)
          {
            char c[] = ta.getText ().toCharArray ();
            int nLines = 1;
            for (int i = 0; i < c.length; i++)
              {
                if (c[i] == '\n')
                  {
                    nLines++;
                  }
              }

            if (nLines >= ta.getRows ())
              {
                e.consume ();
              }
          }
        else if (e.getKeyCode () == KeyEvent.VK_TAB)
          {
            e.consume ();

            if ((e.getModifiersEx () & KeyEvent.SHIFT_DOWN_MASK) ==
                KeyEvent.SHIFT_DOWN_MASK)
              {
                ta.transferFocusBackward();
              }
            else
              {
                ta.transferFocus ();
              }
          }
      }
  }


  /**
   * @param e KeyEvent
   */
  public void keyReleased (KeyEvent e)
  {
  }
}
