/*

Copyright (C) 2014-2015 Colin Foster
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

import java.net.*;
import java.util.*;

import java.awt.*;
import javax.swing.*;

/**
 * <p>Implementation of various dialog box functions</p>
 *
 * <p>The following functions are provided for being called via
 * Octave script files:</p>
 * <ul>
 * <li>errordlg</li>
 * <li>helpdlg</li>
 * <li>inputdlg</li>
 * </ul>
 *
 * <p>Copyright (c) 2010 Martin Hepperle</p>
 *
 * @author Martin Hepperle
 * @version 1.0
 */
public class JDialogBox
{
  public static final int CLOSE_OK = 1;
  public static final int CLOSE_NO = 2;
  public static final int CLOSE_CANCEL = 3;

  // dialog type
  public static int FLAG_LABEL = 1;
  public static int FLAG_TEXT = 2;
  public static int FLAG_INPUT = 4;
  public static int FLAG_LIST = 8;
  // icon selection
  public static int FLAG_QUESTION = 32;
  public static int FLAG_ERROR = 64;
  public static int FLAG_WARNING = 128;
  public static int FLAG_INFORMATION = 256;

  public static int DLG_QUEST = FLAG_QUESTION | FLAG_TEXT;
  public static int DLG_INPUT = FLAG_QUESTION | FLAG_INPUT;
  public static int DLG_LIST = FLAG_QUESTION | FLAG_LIST;

  private final static String m_OK = "OK";
  private final static String m_Yes = "Yes";
  private final static String m_No = "No";
  private final static String m_Cancel = "Cancel";

  private JButton m_ButtonNO;
  private JButton m_ButtonOK;
  private JButton m_ButtonCANCEL;
  private JButton m_Focus;
  private JTextArea m_TextField[];
  private JList m_List;
  private JFrame m_ParentFrame;
  private int m_CloseMethod;

  // ------------------------------------
  // implementation of listdlg function
  // ------------------------------------

  /**
   *
   * @param listcell String[] - a array of strings, one for each list entry
   * @param selmode String
   * @param sizecell Object[]
   * @param initialcell Object[]
   * @param name String
   * @param promptcell String[]
   * @param okstring String
   * @param cancelstring String
   * @return String[]
   */
  public static int[] listdlg (String[] listcell,
                               String selmode,
                               Object[] sizecell,
                               Object[] initialcell,
                               String name,
                               String[] promptcell,
                               String okstring,
                               String cancelstring)
  {
    JDialogBox d = new JDialogBox ();
    d.genericdlg (promptcell, listcell, name, selmode,
                  DLG_LIST,
                  sizecell, initialcell, okstring, null, cancelstring);
    return (d.getSelectedIndices ());
  }


  // ------------------------------------
  // implementation of inputdlg function
  // -------------------------------------

  /**
   * Implements a variation of the inputdlg function.
   *
   * @param prompt String[] - an array of text strings to be used as labels.
   * @param title String - a text string to be used to label the dialog caption.
   * @param RowsCols int - defines the width of the text fields in columns.
   * @param defaults Object[] - an array of text strings or numbers to be
   * placed into the text fields as default values.
   * @return String[] - an array of text strings containing the content of the
   * text fields.
   */
  public static String[] inputdlg (String[] prompt,
                                   String title,
                                   Object[] RowsCols,
                                   Object[] defaults)
  {
    JDialogBox d = new JDialogBox ();
    d.genericdlg (prompt, null, title, "on",
                  FLAG_INPUT | FLAG_QUESTION,
                  RowsCols, defaults, m_OK, null, m_Cancel);
    return (d.getInput ());
  }


  /**
   * Extract the current content from the text fields of an inputdlg.
   *
   * @return String[] - the text contained in the fields of an inputdlg.
   * null if the dialog was cancelled.
   */
  private String[] getInput ()
  {
    String s[] = null;

    if (m_CloseMethod == CLOSE_OK)
      {
        s = new String[m_TextField.length];

        for (int i = 0; i < s.length; i++)
          {
            s[i] = new String (m_TextField[i].getText ());
          }
      }

    return (s);
  }


  private String getResult ()
  {
    String s = null;

    if (m_CloseMethod == CLOSE_OK)
      {
        s = m_ButtonOK.getText ();
      }
    else if (m_CloseMethod == CLOSE_CANCEL)
      {
        s = m_ButtonCANCEL.getText ();
      }
    else
      {
        s = m_ButtonNO.getText ();
      }

    return (s);
  }


  /**
   * Extract the current content from the text fields of an inputdlg.
   *
   * @return String[] - the text contained in the fields of an inputdlg.
   * null if the dialog was cancelled.
   */
  private String[] getSelection ()
  {
    String s[] = null;

    if (m_CloseMethod == CLOSE_OK)
      {
        int selection[] = m_List.getSelectedIndices ();

        s = new String[selection.length];

        for (int i = 0; i < s.length; i++)
          {

            // s[i] = new String (Integer.toString(selection[i]));
            s[i] = (m_List.getSelectedValues ()[i]).toString ();
          }
      }

    return (s);
  }


  private int[] getSelectedIndices ()
  {
    int s[] = null;

    if (m_CloseMethod == CLOSE_OK)
      {
        s = m_List.getSelectedIndices ();
        for (int i = 0; i < s.length; i++)
          {

            // translate to 1 based indices
            s[i] = s[i] + 1;
          }
      }

    return (s);
  }


  public void SelectAll ()
  {
    if (null != m_List)
      {
        m_List.setSelectionInterval (0, m_List.getModel ().getSize () - 1);
      }
  }


  // -------------------------------------
  // implementation of helpdlg function
  // -------------------------------------

  /**
   * Implements a simple helpdlg with default text and caption. Not very useful.
   *
   * Octave > helpdlg('helpstring','title')
   *
   * Called via helpdlg.m.
   *
   * @param helpstring String - a message string to be presented to the user.
   * The string can have embedded newline (\n) characters to break the message
   * into multiple lines.
   * @param title String - a text string to be used to label the dialog caption.
   * @return int - always 1
   */
  public static int helpdlg (String helpstring, String title)
  {
    JDialogBox d = new JDialogBox ();
    String s[] = new String[1];
    s[0] = helpstring;
    return (d.genericdlg (s, null, title, "on",
                          FLAG_TEXT | FLAG_INFORMATION, null, null,
                          m_OK, null, m_Cancel));
  }


  // -------------------------------------
  // implementation of emptydlg function
  // -------------------------------------

  /**
   * Implements a simple helpdlg with default text and caption. Not very useful.
   *
   * Octave > emptydlg('messagestring','title')
   *
   * Called via dlgbox.m.
   *
   * @param messagestring String - a message string to be presented to the user.
   * The string can have embedded newline (\n) characters to break the message
   * into multiple lines.
   * @param title String - a text string to be used to label the dialog caption.
   * @return int - always 1
   */
  public static int emptydlg (String helpstring, String title)
  {
    JDialogBox d = new JDialogBox ();
    String s[] = new String[1];
    s[0] = helpstring;
    return (d.genericdlg (s, null, title, "on",
                          FLAG_TEXT, null, null,
                          m_OK, null, m_Cancel));
  }


  // -------------------------------------------
  // implementation of questdlg related functions
  // -------------------------------------------

  /**
   * Implements a simple questdlg with default text and caption. Not very useful.
   *
   * @param question String - the question to be presented
   * @param title String - the caption
   * @param options String[] - 'str1', 'str2', 'str3', 'default'
   * @return String - the caption of the button pressed by the user
   */
  public static String questdlg (String question,
                                 String title,
                                 String[] options)
  {
    JDialogBox d = new JDialogBox ();
    String s[] = new String[1];
    s[0] = question;
    d.genericdlg (s, options, title, "on",
                  DLG_QUEST, null, null,
                  options[0], options[1], options[2]);
    return (d.getResult ());
  }


  // -------------------------------------
  // implementation of errordlg function
  // -------------------------------------

  /**
   * Implements a simple errordlg with default text and caption. Not very useful.
   *
   * @param errorstring String - the error message to display.
   * @param dlgname String - the caption of the dialog box.
   * @return int - always 1
   */
  public static int errordlg (String errorstring, String dlgname)
  {
    JDialogBox d = new JDialogBox ();
    String s[] = new String[1];
    s[0] = errorstring;
    return (d.genericdlg (s, null, dlgname, "on", FLAG_TEXT | FLAG_ERROR,
                          null, null,
                          m_OK, null, m_Cancel));
  }


  // -------------------------------------------
  // implementation of warndlg related functions
  // -------------------------------------------

  /**
   * Implements a simple warndlg with default text and caption. Not very useful.
   *
   * Called via warndlg.m.
   *
   * @param errorstring String - the message to be presented to the user.
   * @param dlgname String - the caption of the dialog box.
   * @return int - always 1
   */
  public static int warndlg (String errorstring, String dlgname)
  {
    JDialogBox d = new JDialogBox ();
    String s[] = new String[1];
    s[0] = errorstring;
    return (d.genericdlg (s, null, dlgname, "on", FLAG_TEXT | FLAG_WARNING,
                          null, null,
                          m_OK, null, m_Cancel));
  }


  // -------------------------------------
  // generic dlg function
  // -------------------------------------
  /**
   * A generic dialog creation and display function.
   *
   * @param message String[]
   * @param list String[]
   * @param caption String
   * @param on String
   * @param flag int
   * @param RowsCols Object[]
   * @param defaults Object[]
   * @param okstring String
   * @param nostring String
   * @param cancelstring String
   * @return int
   */
  public int genericdlg (String message[],
                         String list[],
                         String caption,
                         String on,
                         int flag,
                         Object[] RowsCols,
                         Object[] defaults,
                         String okstring,
                         String nostring,
                         String cancelstring)
  {
    TeXtranslator theTranslator = new TeXtranslator ();
    setSystemLnF (true);

    caption = theTranslator.replace (caption);

    m_ButtonNO = null;
    m_ButtonOK = null;
    m_ButtonCANCEL = null;

    // create a modal dialog with an empty frame as its parent
    m_ParentFrame = new JFrame ();

    // --- trick to bring dialog to the front
    // In Windows, the dialog is not brought to the foreground, but hidden
    // behind the Octave window as long as the parent frame is not visible.
    // To avoid that the frame is visible, we move it outside of the screen.
    m_ParentFrame.setBounds (Toolkit.getDefaultToolkit ().getScreenSize ().
                             width + 100,
                             Toolkit.getDefaultToolkit ().getScreenSize ().
                             height + 100, 1, 1);
    m_ParentFrame.setVisible (true);
    //-- end of trick

    JDialog dlg;
    dlg = new JDialog (m_ParentFrame);
    dlg.setTitle (caption);

    dlg.setModal (true);
    dlg.setDefaultCloseOperation (JDialog.DISPOSE_ON_CLOSE);

    DlgListener theListener = new DlgListener (this);

    Container d = dlg.getContentPane ();
    d.setLayout (new BorderLayout (8, 8));

    // spacer
    d.add (new JLabel (" "), BorderLayout.NORTH);
    d.add (new JLabel ("  "), BorderLayout.EAST);

    JPanel p = new JPanel ();

    if (FLAG_LABEL == (FLAG_LABEL & flag))
      {
        // a single line label
        JLabel l = new JLabel (theTranslator.replace (message[0]));
        p.add (l);
      }
    else if (FLAG_TEXT == (FLAG_TEXT & flag))
      {
        String msg = theTranslator.replace (message[0]);
        // a multi-line text display for helpdlg
        StringTokenizer st = new StringTokenizer (msg, "\n");

        int nRows = (null == RowsCols) ? 1 :
          Integer.parseInt (RowsCols[0].toString ());
        nRows = Math.max (nRows, st.countTokens ());
        int nCols = Math.max (1, msg.length () / nRows);

        p.setLayout (new GridLayout (message.length, 1));
        JTextArea ta = new JTextArea (msg, nRows, nCols);
        ta.setEditable (false);
        ta.setFocusable (false);
        ta.setOpaque (false);
        // replace ugly monospaced font
        ta.setFont (p.getFont ());
        p.add (ta);
      }
    else if (FLAG_INPUT == (FLAG_INPUT & flag))
      {
        // a multi label/textfield entry dialog for inputdlg
        GridBagConstraints gbc = new GridBagConstraints ();
        gbc.insets.top = 4;
        gbc.insets.left = 8;
        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;

        p.setLayout (new GridBagLayout ());
        m_TextField = new JTextArea[message.length];

        // default values
        int nRows = 1;
        int nCols = 10;

        for (int i = 0; i < message.length; i++)
          {
            String msg = theTranslator.replace (message[i]);
            JLabel l = new JLabel (msg);
            l.setHorizontalAlignment (Label.LEFT);
            gbc.gridy = 2 * i;
            p.add (l, gbc);
            /**
             * @todo CHECK handling of RowsCols for inputdlg
             */
            if (RowsCols != null)
              {
                if (RowsCols.length == 2 * message.length)
                  {
                    nRows = Integer.parseInt (RowsCols[i].toString ());
                    nCols = Integer.parseInt (RowsCols[RowsCols.length / 2 +
                                                       i].toString ());
                  }
              }

            m_TextField[i] = new JTextArea ("", Math.max (nRows, 1), nCols);
            // avoid resizing
            m_TextField[i].setPreferredSize (new Dimension (Math.max (nRows,
                                                                      1), nCols));
            m_TextField[i].setAutoscrolls (false);
            m_TextField[i].setFont (p.getFont ());
            m_TextField[i].setBorder (new javax.swing.border.EtchedBorder ());
            m_TextField[i].addKeyListener (theListener);

            gbc.gridy = 2 * i + 1;
            p.add (m_TextField[i], gbc);
          }

        if (defaults != null)
          {
            if (defaults.length == message.length)
              {
                for (int i = 0; i < message.length; i++)
                  {
                    String def = theTranslator.replace (defaults[i].toString ());
                    m_TextField[i].setText (def);
                  }
              }
          }
      }
    else if (DLG_LIST == (DLG_LIST & flag))
      {
        GridBagConstraints gbc = new GridBagConstraints ();
        gbc.insets.top = 4;
        gbc.insets.left = 8;
        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;

        p.setLayout (new GridBagLayout ());

        for (int i = 0; i < message.length; i++)
          {
            // a single line label
            String msg = theTranslator.replace (message[i]);
            JLabel l = new JLabel (msg);
            gbc.gridy = i;
            p.add (l, gbc);
          }

        String lst[] = new String[list.length];

        for (int i = 0; i < list.length; i++)
          {
            lst[i] = theTranslator.replace (list[i]);
          }

        JScrollPane scrollPane = new JScrollPane();
        m_List = new JList (lst);
        scrollPane.setViewportView(m_List);


        // replace ugly monospaced font
        scrollPane.setFont (p.getFont ());

        scrollPane.setMinimumSize (
          new Dimension (
            Math.max (1, Integer.parseInt (RowsCols[0].toString ())),
            Math.max (1, Integer.parseInt (RowsCols[1].toString ()))));
        scrollPane.setPreferredSize (
          new Dimension (
            Math.max (1, Integer.parseInt (RowsCols[1].toString ())),
            Math.max (1, Integer.parseInt (RowsCols[0].toString ()))));
        scrollPane.setBorder (new javax.swing.border.EtchedBorder ());

        gbc.gridy = message.length;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        p.add (scrollPane, gbc);

        if (on.toLowerCase ().equals ("single"))
          {
            // single selection list
            m_List.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);

            m_List.setSelectedIndex (Integer.parseInt (
                                                       defaults[0].toString ()) - 1);
          }
        else
          {
            // multiple selection possible
            m_List.setSelectionMode (ListSelectionModel.
                                     MULTIPLE_INTERVAL_SELECTION);

            int selection[] = new int[defaults.length];
            for (int i = 0; i < defaults.length; i++)
              {
                selection[i] = Integer.parseInt (defaults[i].toString ()) - 1;
              }
            m_List.setSelectedIndices (selection);

            JButton b = new JButton ("Select All");
            b.setActionCommand ("SELALL");
            b.addActionListener (theListener);
            gbc.gridy = message.length + 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;
            p.add (b, gbc);
          }

      }

    // prepare icon, if any
    String sIconFile = null;
    String sIconResource = null;
    Icon theIcon = null;

    if (FLAG_ERROR == (FLAG_ERROR & flag))
      {
        sIconFile = "images/error.png";
        // Java for Windows
        sIconResource = "OptionPane.errorIcon";
        // Java for Linux does not offer these standard icons...
      }
    else if (FLAG_WARNING == (FLAG_WARNING & flag))
      {
        sIconFile = "images/warning.png";
        // Java for Windows
        sIconResource = "OptionPane.warningIcon";
        // Java for Linux does not offer these standard icons...
      }
    else if (FLAG_QUESTION == (FLAG_QUESTION & flag))
      {
        sIconFile = "images/question.png";
        // Java for Windows
        sIconResource = "OptionPane.questionIcon";
        // Java for Linux does not offer these standard icons...
      }
    else if (FLAG_INFORMATION == (FLAG_INFORMATION & flag))
      {
        sIconFile = "images/information.png";
        // Java for Windows
        sIconResource = "OptionPane.informationIcon";
        // Java for Linux does not offer these standard icons...
      }

    // first try to find the UIManager specific icon to fit look and feel
    // Note: the Windows XP look and feel offers 50 icons.
    if (sIconResource != null)
      {
        UIDefaults df = UIManager.getLookAndFeelDefaults ();
        theIcon = df.getIcon (sIconResource);
      }

    // fallback on bitmap image resource if icon was not found
    if (theIcon == null &&
        sIconFile != null)
      {
        URL theResource = JDialogBox.class.getResource (sIconFile);
        if (theResource != null)
          {
            theIcon = new ImageIcon (theResource);
          }
      }

    if (theIcon != null)
      {
        // dummy panel to provide space around icon
        JPanel pi = new JPanel (new GridLayout (1, 3));
        pi.add (new JLabel ());
        pi.add (new JLabel (theIcon));
        pi.add (new JLabel ());
        d.add (pi, BorderLayout.WEST);

        // use Octave icon if available. otherwise use dialog icon
        Icon theOctaveIcon = getOctaveIcon ();
        prepareFrameIcon (m_ParentFrame,
                          theOctaveIcon == null ? theIcon : theOctaveIcon);
      }

    d.add (p, BorderLayout.CENTER);

    // button bar (2 rows of 3 columns each

    p = new JPanel ();
    p.setLayout (new java.awt.GridLayout (2, 3));

    // spacer row
    p.add (new JLabel ());
    p.add (new JLabel ());
    p.add (new JLabel ());

    if (DLG_QUEST == (DLG_QUEST & flag))
      {
        // questdlg with empty option[2]: only two buttons
        if (nostring.length () < 1)
          {
            // spacer: left
            p.add (new JLabel ());
          }
      }
    else
      {
        // spacer: left
        p.add (new JLabel ());
      }

    m_ButtonOK = new JButton (theTranslator.replace (okstring));
    m_ButtonOK.setActionCommand ("OK");
    m_ButtonOK.addActionListener (theListener);
    m_Focus = m_ButtonOK;
    p.add (m_ButtonOK);

    if (DLG_QUEST == (DLG_QUEST & flag))
      {
        // questdlg with empty option[2]: only two buttons
        if (nostring.length () > 01)
          {
            // questdlg has three buttons
            m_ButtonNO = new JButton (theTranslator.replace (nostring));
            m_ButtonNO.setActionCommand ("NO");
            m_ButtonNO.addActionListener (theListener);
            p.add (m_ButtonNO);
            if (DLG_QUEST == (DLG_QUEST & flag))
              {
                // select default button
                if (list[3].equals (nostring))
                  {
                    m_Focus = m_ButtonNO;
                  }
              }
          }
      }

    if (DLG_INPUT == (DLG_INPUT & flag) ||
        DLG_LIST == (DLG_LIST & flag) ||
        DLG_QUEST == (DLG_QUEST & flag))
      {
        m_ButtonCANCEL = new JButton (theTranslator.replace (cancelstring));
        m_ButtonCANCEL.setActionCommand ("CANCEL");
        m_ButtonCANCEL.addActionListener (theListener);
        p.add (m_ButtonCANCEL);
        if (DLG_QUEST == (DLG_QUEST & flag))
          {
            // select default button
            if (list[3].equals (cancelstring))
              {
                m_Focus = m_ButtonCANCEL;
              }
          }
      }
    else
      {
        // spacer: right
        p.add (new JLabel ());
      }

    d.add (p, BorderLayout.SOUTH);
    dlg.pack ();

    dlg.addWindowListener (theListener);

    if (on.equals ("on"))
      {
        m_ParentFrame.setAlwaysOnTop (true);
      }

    // center dialog on screen
    Dimension dlgSize = dlg.getSize ();
    Dimension screenSize = Toolkit.getDefaultToolkit ().getScreenSize ();

    dlg.setLocation ((screenSize.width - dlgSize.width) / 2,
                     (screenSize.height - dlgSize.height) / 2);

    dlg.setVisible (true);
    dlg.requestFocus ();

    m_ParentFrame.setVisible (false);

    return (1);
  }


  /**
   *
   * @return Icon - null if icon was not found
   */
  private Icon getOctaveIcon ()
  {
    Icon theIcon = null;
    URL theResource = JDialogBox.class.getResource ("images/octave.png");
    if (theResource != null)
      {
        theIcon = new ImageIcon (theResource);
      }
    return theIcon;
  }


  /**
   * Replace the standard Java frame icon with an Octave Icon.
   *
   * @param theFrame Frame - the Frame to decorate
   * @param theIcon Icon - the icon to use if octave icon is not found.
   */
  private void prepareFrameIcon (Frame theFrame, Icon theIcon)
  {
    // prepare icon for upper left corner of Frame window
    // maybe there is a simpler way to achieve this
    int w = theIcon.getIconWidth ();
    int h = theIcon.getIconHeight ();
    // Frame must be made displayable by packing it for createImage() to succeed
    theFrame.pack ();
    Image theImage = theFrame.createImage (w, h);
    theIcon.paintIcon (theFrame, theImage.getGraphics (), 0, 0);
    theFrame.setIconImage (theImage);
  }


  /**
   * Select Look and Feel
   *
   * @param bSystemLnF boolean - if true, the current systesm Look&Feel is used,
   * otherwise the Swing/Metal cross platform Look&Feel is used.
   */
  private void setSystemLnF (boolean bSystemLnF)
  {
    try
      {
        if (bSystemLnF)
          {
            // switch from Swing LnF to local system LnF
            UIManager.setLookAndFeel (UIManager.
                                      getSystemLookAndFeelClassName ());
          }
        else
          {
            // use Swing LnF
            UIManager.setLookAndFeel (UIManager.
                                      getCrossPlatformLookAndFeelClassName ());
          }
      }
    catch (Exception exception)
      {
        exception.printStackTrace ();
      }
  }


  /**
   * Called when the dialog is closed. Allows for specific cleanup actions.
   *
   * @param closeMethod int - OctaveDialog.CLOSE_OK, OctaveDialog.CLOSE_CANCEL
   */
  public void closeDialog (int closeMethod)
  {
    m_CloseMethod = closeMethod;
    m_ParentFrame.dispose ();
  }


  public void setFocus ()
  {
    if (null != m_Focus)
      {
        m_Focus.requestFocus ();
        m_ParentFrame.getRootPane ().setDefaultButton (m_Focus);
        m_ParentFrame.setAlwaysOnTop (true);
      }
  }


  /**
   * Tests the dialogs
   *
   * @param args String[] - not used.
   */
  public static void main (String[] args)
  {
    TeXtranslator t = new TeXtranslator();

    if (false)
      {
        // find out key names of icon UI resources
        UIDefaults df = UIManager.getLookAndFeelDefaults ();

        for (Enumeration e = df.keys (); e.hasMoreElements ();)
          {
            String s = e.nextElement ().toString ();

            if (s.toLowerCase ().contains ("icon"))
              {
                System.out.println (s);
              }
          }
      }

    try
      {
        Class[] argTypes = new Class[1];
        argTypes[0] = String.class;

        java.lang.reflect.Constructor c = ClassHelper.findConstructor (java.lang.StringBuffer.class,
                                                                       argTypes);
        Object argValues[] = new Object[1];
        argValues[0] = new String("initial value");
        Object sb = c.newInstance(argValues);
        System.out.println(sb.toString());

        ClassHelper.invokeMethod(sb,"append",argValues,argTypes);
        System.out.println(sb.toString());

        argValues = new Object[2];
        argTypes = new Class[2];
        argTypes[0] =  Integer.class;
        argTypes[1] = String.class;
        argValues[0] = new Integer(0);
        argValues[1] = new String("inserted");

        ClassHelper.invokeMethod(sb,"insert",argValues,argTypes);
        System.out.println(sb.toString());
      }
    catch (Throwable e)
      {}

    if (true)
      {
        return;
      }

    helpdlg ("If you need help\nyou should ask for help\nif someone is around\notherwise you are on your own.",
             "Information");

    String[] options = new String[4];
    options[0] = "Yeah \\vartheta is too low";
    options[1] = "Maybe";
    options[2] = "Nay \\vartheta is too high";
    options[3] = "Maybe";

    System.out.println (questdlg ("Is it too cold?", "Temperature", options));

    // test variants of errordlg
    // does not affect layering of dialog
    errordlg ("Background error!", "Error");

    // test variants of helpdlg

    // test variants of inputdlg
    String prompt[] = new String[2];
    prompt[0] = "Question 1";
    prompt[1] = "Question 2";
    String defaults[] = new String[2];
    defaults[0] = "1.1";
    defaults[1] = "2.2";
    String title = "Enter values";

    Integer rc[] = new Integer[2 * 2];
    rc[0] = new Integer (1);
    rc[1] = new Integer (2);
    rc[2] = new Integer (10);
    rc[3] = new Integer (20);

    inputdlg (prompt, title, rc, defaults);

    String listcell[] = new String[4];
    listcell[0] = "a \\alpha";
    listcell[1] = "b \\beta";
    listcell[2] = "c \\gamma";
    listcell[3] = "d \\delta";

    Integer size[] = new Integer[2];
    size[0] = new Integer (80);
    size[1] = new Integer (100);

    Integer initial[] = new Integer[2];
    initial[0] = new Integer (4);
    initial[1] = new Integer (2);

    String promptcell[] = new String[2];
    promptcell[0] = "Select something";
    promptcell[1] = "(or even more than one thing)";

    int idx[] = listdlg (listcell,
                         "Multiple",
                         size,
                         initial,
                         "name",
                         promptcell,
                         "okstring",
                         "cancelstring");

    if (idx != null)
      {
        for (int i = 0; i < idx.length; i++)
          {
            System.out.println (idx[i]);
          }
      }
  }
}
