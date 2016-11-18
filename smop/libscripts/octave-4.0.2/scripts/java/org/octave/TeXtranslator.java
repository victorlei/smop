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

// A primitive TeX character translator.  Provides methods to translate
// a subset of TeX symbol strings into their aequivalent Unicode
// representation.
//
// Note that not all Unicode character sets contain all these characters.
// Suitable Windows fonts are e.g.:
//   - Lucida Sans Unicode
//   - Arial Unicode MS
//   - MS Mincho

package org.octave;

public class TeXtranslator
{
  private TeXcode m_texTable[] =
    {
      // lower case
      new TeXcode ("alpha", '\u03B1'),
      new TeXcode ("beta", '\u03B2'),
      new TeXcode ("gamma", '\u03B3'),
      new TeXcode ("delta", '\u03B4'),
      new TeXcode ("epsilon", '\u03B5'),
      new TeXcode ("zeta", '\u03B6'),
      new TeXcode ("eta", '\u03B7'),
      new TeXcode ("theta", '\u03B8'),
      new TeXcode ("vartheta", '\u03D1'),
      new TeXcode ("iota", '\u03B9'),
      new TeXcode ("kappa", '\u03BA'),
      new TeXcode ("lambda", '\u03BB'),
      new TeXcode ("mu", '\u03BC'),
      new TeXcode ("nu", '\u03BD'),
      new TeXcode ("xi", '\u03BE'),
      new TeXcode ("pi", '\u03C0'),
      new TeXcode ("rho", '\u03C1'),
      new TeXcode ("sigma", '\u03C3'),
      new TeXcode ("varsigma", '\u03C2'),
      new TeXcode ("tau", '\u03C4'),
      new TeXcode ("phi", '\u03C6'),
      new TeXcode ("chi", '\u03C7'),
      new TeXcode ("psi", '\u03C8'),
      new TeXcode ("omega", '\u03C9'),
      new TeXcode ("upsilon", '\u03C5'),
      // upper case
      new TeXcode ("Gamma", '\u0393'),
      new TeXcode ("Delta", '\u0394'),
      new TeXcode ("Theta", '\u0398'),
      new TeXcode ("Lambda", '\u039B'),
      new TeXcode ("Pi", '\u03A0'),
      new TeXcode ("Xi", '\u039E'),
      new TeXcode ("Sigma", '\u03A3'),
      new TeXcode ("Upsilon", '\u03A5'),
      new TeXcode ("Phi", '\u03A6'),
      new TeXcode ("Psi", '\u03A8'),
      new TeXcode ("Omega", '\u03A9'),
      // complex
      new TeXcode ("Im", '\u2111'),
      new TeXcode ("Re", '\u211c'),
      // special
      new TeXcode ("leq", '\u2264'),
      new TeXcode ("geq", '\u2265'),
      new TeXcode ("neq", '\u2260'),
      new TeXcode ("pm", '\u00B1'),
      new TeXcode ("infty", '\u221E'),
      new TeXcode ("partial", '\u2202'),
      new TeXcode ("approx", '\u2248'),
      new TeXcode ("circ", '\u2218'),
      new TeXcode ("bullet", '\u2022'),
      new TeXcode ("times", '\u00D7'),
      new TeXcode ("sim", '\u007E'),
      new TeXcode ("nabla", '\u2207'),
      new TeXcode ("ldots", '\u2026'),
      new TeXcode ("exists", '\u2203'),
      new TeXcode ("neg", '\u00AC'),
      new TeXcode ("aleph", '\u2135'),
      new TeXcode ("forall", '\u2200'),
      new TeXcode ("cong", '\u2245'),
      new TeXcode ("wp", '\u2118'),
      new TeXcode ("propto", '\u221D'),
      new TeXcode ("otimes", '\u2297'),
      new TeXcode ("oplus", '\u2295'),
      new TeXcode ("oslash", '\u2298'),
      new TeXcode ("cap", '\u2229'),
      new TeXcode ("cup", '\u222A'),
      new TeXcode ("ni", '\u220B'),
      new TeXcode ("in", '\u2208'),
      new TeXcode ("div", '\u00F7'),
      new TeXcode ("equiv", '\u2261'),
      new TeXcode ("int", '\u222B'),
      new TeXcode ("perp", '\u22A5'),
      new TeXcode ("wedge", '\u2227'),
      new TeXcode ("vee", '\u2228'),
      // sets
      new TeXcode ("supseteq", '\u2287'),
      new TeXcode ("supset", '\u2283'),
      new TeXcode ("subseteq", '\u2286'),
      new TeXcode ("subset", '\u2282'),
      // cards
      new TeXcode ("clubsuit", '\u2663'),
      new TeXcode ("spadesuit", '\u2660'),
      new TeXcode ("heartsuit", '\u2665'),
      new TeXcode ("diamondsuit", '\u2666'),
      new TeXcode ("copyright", '\u00A9'),
      // arrows
      new TeXcode ("leftarrow", '\u2190'),
      new TeXcode ("uparrow", '\u2191'),
      new TeXcode ("rightarrow", '\u2192'),
      new TeXcode ("downarrow", '\u2193'),
      new TeXcode ("leftrightarrow", '\u2194'),
      new TeXcode ("updownarrow", '\u2195'),
    };

   public TeXtranslator ()
   {
     /* DEBUG: output table to file
     try
       {
         java.io.PrintWriter pwTeX = new java.io.PrintWriter ("z:/tex.txt", "UTF-8");
         java.io.PrintWriter pwHTML = new java.io.PrintWriter ("z:/html.txt", "UTF-8");
         java.io.PrintWriter pwOctave = new java.io.PrintWriter ("z:/octave.txt", "UTF-8");
         pwOctave.print ("msgbox ( [");
         int i = 0;
         for (int k = 0; k < m_texTable.length; k++)
           {
             if (i++ == 0)
               {
                 pwTeX.print ("@item ");
                 pwHTML.print ("@item ");
                 pwOctave.print ("          '");
               }
             else
               {
                 pwTeX.print ("@tab ");
                 pwHTML.print ("@tab ");
                 pwOctave.print ("   ");
               }
             pwTeX.println ("\\" + m_texTable[k].tex);
             pwTeX.println ("@tab '@math{\\" + m_texTable[k].tex + "}'");
             pwHTML.println ("\\" + m_texTable[k].tex);
             pwHTML.println ("@tab '" + m_texTable[k].ucode + "'");
             pwOctave.print ("\\\\" + m_texTable[k].tex+" ");
             pwOctave.print (" = ''\\" + m_texTable[k].tex + " ''");
             if (i == 3)
               {
                 pwTeX.println ("@c ----------");
                 pwHTML.println ("@c ----------");
                 pwOctave.println ("', 10, ...");
                 i=0;
               }
             else
               {
                 pwTeX.println ("@tab");
                 pwHTML.println ("@tab");
                 pwOctave.print ("   ");
               }
           }
         pwOctave.print ("']);");
         pwTeX.close ();
         pwHTML.close ();
         pwOctave.close ();
       }
     catch (Exception e)
       {
         ;
       }
      /* */
   }


  /*
    NOT YET TRANSLATED
    o
    rfloor
    lceil
    lfloor
    cdot
    prime
    0
    rceil
    surd
    mid
    varpi
    langle
    rangle
  */

  public String replace (String s)
  {
    StringBuffer sb = new StringBuffer (s);
    // append trailing blank
    sb.append (' ');

    int i = 0;
    do
      {
        // 26 08 2010 MH szatt search at index i
        i = sb.indexOf ("\\", i);
        if (i > -1)
          {
            int j = sb.indexOf (" ", i);
            if (j > i)
              {
                String token = sb.substring (i + 1, j);

                for (int k = 0; k < m_texTable.length; k++)
                  {
                    if (m_texTable[k].tex.equals (token))
                      {
                        sb.replace (i, j + 1,
                                    Character.toString (m_texTable[k].ucode));
                        break;
                      }
                  }
                if (sb.charAt (i) == '\\')
                  {
                    // backslash sztill there: not found
                    if (sb.charAt (i + 1) == 'n')
                      {
                        // newline
                        sb.replace (i, i + 2, "\n");
                      }
                    else if (sb.charAt (i + 1) == '\\')
                      {
                        // backslash
                        sb.replace (i, i + 2, "\\");
                      }
                  }

                // 26 08 2010 MH
                // advance i to avoid deadlock in case of incorrect escape
                // sequences like \\\\alpha (double backslash) or
                // \\bogus (unknown escape sequence)
                i++;
              }
          }
      }
    while (i > -1);
    // finall: remove trailing blank
    return (sb.substring (0, sb.length () - 1).toString ());
  }
}
