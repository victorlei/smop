/*

Copyright (C) 2011-2015 Michael Goffioul

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QMap>
#include <Qt>

#include "KeyMap.h"

namespace QtHandles
{

namespace KeyMap
{

std::string
qKeyToKeyString (int key)
{
  static QMap<int, std::string> keyMapper;

  if (keyMapper.isEmpty ())
    {
      keyMapper[Qt::Key_Escape] = "escape";
      keyMapper[Qt::Key_Tab] = "tab";
      keyMapper[Qt::Key_Backtab] = "backtab";
      keyMapper[Qt::Key_Backspace] = "backspace";
      keyMapper[Qt::Key_Return] = "return";
      keyMapper[Qt::Key_Enter] = "enter";
      keyMapper[Qt::Key_Insert] = "insert";
      keyMapper[Qt::Key_Delete] = "delete";
      keyMapper[Qt::Key_Pause] = "pause";
      keyMapper[Qt::Key_Print] = "print";
      keyMapper[Qt::Key_SysReq] = "sysreq";
      keyMapper[Qt::Key_Clear] = "clear";
      keyMapper[Qt::Key_Home] = "home";
      keyMapper[Qt::Key_End] = "end";
      keyMapper[Qt::Key_Left] = "left";
      keyMapper[Qt::Key_Up] = "up";
      keyMapper[Qt::Key_Right] = "right";
      keyMapper[Qt::Key_Down] = "down";
      keyMapper[Qt::Key_PageUp] = "pageup";
      keyMapper[Qt::Key_PageDown] = "pagedown";
      keyMapper[Qt::Key_Shift] = "shift";
      keyMapper[Qt::Key_Control] = "control";
      keyMapper[Qt::Key_Meta] = "meta";
      keyMapper[Qt::Key_Alt] = "alt";
      keyMapper[Qt::Key_CapsLock] = "capslock";
      keyMapper[Qt::Key_NumLock] = "numlock";
      keyMapper[Qt::Key_ScrollLock] = "scrolllock";
      keyMapper[Qt::Key_F1] = "f1";
      keyMapper[Qt::Key_F2] = "f2";
      keyMapper[Qt::Key_F3] = "f3";
      keyMapper[Qt::Key_F4] = "f4";
      keyMapper[Qt::Key_F5] = "f5";
      keyMapper[Qt::Key_F6] = "f6";
      keyMapper[Qt::Key_F7] = "f7";
      keyMapper[Qt::Key_F8] = "f8";
      keyMapper[Qt::Key_F9] = "f9";
      keyMapper[Qt::Key_F10] = "f10";
      keyMapper[Qt::Key_F11] = "f11";
      keyMapper[Qt::Key_F12] = "f12";
      keyMapper[Qt::Key_F13] = "f13";
      keyMapper[Qt::Key_F14] = "f14";
      keyMapper[Qt::Key_F15] = "f15";
      keyMapper[Qt::Key_F16] = "f16";
      keyMapper[Qt::Key_F17] = "f17";
      keyMapper[Qt::Key_F18] = "f18";
      keyMapper[Qt::Key_F19] = "f19";
      keyMapper[Qt::Key_F20] = "f20";
      keyMapper[Qt::Key_F21] = "f21";
      keyMapper[Qt::Key_F22] = "f22";
      keyMapper[Qt::Key_F23] = "f23";
      keyMapper[Qt::Key_F24] = "f24";
      keyMapper[Qt::Key_F25] = "f25";
      keyMapper[Qt::Key_F26] = "f26";
      keyMapper[Qt::Key_F27] = "f27";
      keyMapper[Qt::Key_F28] = "f28";
      keyMapper[Qt::Key_F29] = "f29";
      keyMapper[Qt::Key_F30] = "f30";
      keyMapper[Qt::Key_F31] = "f31";
      keyMapper[Qt::Key_F32] = "f32";
      keyMapper[Qt::Key_F33] = "f33";
      keyMapper[Qt::Key_F34] = "f34";
      keyMapper[Qt::Key_F35] = "f35";
      keyMapper[Qt::Key_Super_L] = "super_l";
      keyMapper[Qt::Key_Super_R] = "super_r";
      keyMapper[Qt::Key_Menu] = "menu";
      keyMapper[Qt::Key_Hyper_L] = "hyper_l";
      keyMapper[Qt::Key_Hyper_R] = "hyper_r";
      keyMapper[Qt::Key_Help] = "help";
      keyMapper[Qt::Key_Direction_L] = "direction_l";
      keyMapper[Qt::Key_Direction_R] = "direction_r";
      keyMapper[Qt::Key_Space] = "space";
      keyMapper[Qt::Key_Any] = "any";
      keyMapper[Qt::Key_Exclam] = "exclam";
      keyMapper[Qt::Key_QuoteDbl] = "quotedbl";
      keyMapper[Qt::Key_NumberSign] = "numbersign";
      keyMapper[Qt::Key_Dollar] = "dollar";
      keyMapper[Qt::Key_Percent] = "percent";
      keyMapper[Qt::Key_Ampersand] = "ampersand";
      keyMapper[Qt::Key_Apostrophe] = "apostrophe";
      keyMapper[Qt::Key_ParenLeft] = "parenleft";
      keyMapper[Qt::Key_ParenRight] = "parenright";
      keyMapper[Qt::Key_Asterisk] = "asterisk";
      keyMapper[Qt::Key_Plus] = "plus";
      keyMapper[Qt::Key_Comma] = "comma";
      keyMapper[Qt::Key_Minus] = "minus";
      keyMapper[Qt::Key_Period] = "period";
      keyMapper[Qt::Key_Slash] = "slash";
      keyMapper[Qt::Key_0] = "0";
      keyMapper[Qt::Key_1] = "1";
      keyMapper[Qt::Key_2] = "2";
      keyMapper[Qt::Key_3] = "3";
      keyMapper[Qt::Key_4] = "4";
      keyMapper[Qt::Key_5] = "5";
      keyMapper[Qt::Key_6] = "6";
      keyMapper[Qt::Key_7] = "7";
      keyMapper[Qt::Key_8] = "8";
      keyMapper[Qt::Key_9] = "9";
      keyMapper[Qt::Key_Colon] = "colon";
      keyMapper[Qt::Key_Semicolon] = "semicolon";
      keyMapper[Qt::Key_Less] = "less";
      keyMapper[Qt::Key_Equal] = "equal";
      keyMapper[Qt::Key_Greater] = "greater";
      keyMapper[Qt::Key_Question] = "question";
      keyMapper[Qt::Key_At] = "at";
      keyMapper[Qt::Key_A] = "a";
      keyMapper[Qt::Key_B] = "b";
      keyMapper[Qt::Key_C] = "c";
      keyMapper[Qt::Key_D] = "d";
      keyMapper[Qt::Key_E] = "e";
      keyMapper[Qt::Key_F] = "f";
      keyMapper[Qt::Key_G] = "g";
      keyMapper[Qt::Key_H] = "h";
      keyMapper[Qt::Key_I] = "i";
      keyMapper[Qt::Key_J] = "j";
      keyMapper[Qt::Key_K] = "k";
      keyMapper[Qt::Key_L] = "l";
      keyMapper[Qt::Key_M] = "m";
      keyMapper[Qt::Key_N] = "n";
      keyMapper[Qt::Key_O] = "o";
      keyMapper[Qt::Key_P] = "p";
      keyMapper[Qt::Key_Q] = "q";
      keyMapper[Qt::Key_R] = "r";
      keyMapper[Qt::Key_S] = "s";
      keyMapper[Qt::Key_T] = "t";
      keyMapper[Qt::Key_U] = "u";
      keyMapper[Qt::Key_V] = "v";
      keyMapper[Qt::Key_W] = "w";
      keyMapper[Qt::Key_X] = "x";
      keyMapper[Qt::Key_Y] = "y";
      keyMapper[Qt::Key_Z] = "z";
      keyMapper[Qt::Key_BracketLeft] = "bracketleft";
      keyMapper[Qt::Key_Backslash] = "backslash";
      keyMapper[Qt::Key_BracketRight] = "bracketright";
      keyMapper[Qt::Key_AsciiCircum] = "asciicircum";
      keyMapper[Qt::Key_Underscore] = "underscore";
      keyMapper[Qt::Key_QuoteLeft] = "quoteleft";
      keyMapper[Qt::Key_BraceLeft] = "braceleft";
      keyMapper[Qt::Key_Bar] = "bar";
      keyMapper[Qt::Key_BraceRight] = "braceright";
      keyMapper[Qt::Key_AsciiTilde] = "asciitilde";

      keyMapper[Qt::Key_nobreakspace] = "nobreakspace";
      keyMapper[Qt::Key_exclamdown] = "exclamdown";
      keyMapper[Qt::Key_cent] = "cent";
      keyMapper[Qt::Key_sterling] = "sterling";
      keyMapper[Qt::Key_currency] = "currency";
      keyMapper[Qt::Key_yen] = "yen";
      keyMapper[Qt::Key_brokenbar] = "brokenbar";
      keyMapper[Qt::Key_section] = "section";
      keyMapper[Qt::Key_diaeresis] = "diaeresis";
      keyMapper[Qt::Key_copyright] = "copyright";
      keyMapper[Qt::Key_ordfeminine] = "ordfeminine";
      keyMapper[Qt::Key_guillemotleft] = "guillemotleft";
      keyMapper[Qt::Key_notsign] = "notsign";
      keyMapper[Qt::Key_hyphen] = "hyphen";
      keyMapper[Qt::Key_registered] = "registered";
      keyMapper[Qt::Key_macron] = "macron";
      keyMapper[Qt::Key_degree] = "degree";
      keyMapper[Qt::Key_plusminus] = "plusminus";
      keyMapper[Qt::Key_twosuperior] = "twosuperior";
      keyMapper[Qt::Key_threesuperior] = "threesuperior";
      keyMapper[Qt::Key_acute] = "acute";
      keyMapper[Qt::Key_mu] = "mu";
      keyMapper[Qt::Key_paragraph] = "paragraph";
      keyMapper[Qt::Key_periodcentered] = "periodcentered";
      keyMapper[Qt::Key_cedilla] = "cedilla";
      keyMapper[Qt::Key_onesuperior] = "onesuperior";
      keyMapper[Qt::Key_masculine] = "masculine";
      keyMapper[Qt::Key_guillemotright] = "guillemotright";
      keyMapper[Qt::Key_onequarter] = "onequarter";
      keyMapper[Qt::Key_onehalf] = "onehalf";
      keyMapper[Qt::Key_threequarters] = "threequarters";
      keyMapper[Qt::Key_questiondown] = "questiondown";
      keyMapper[Qt::Key_Agrave] = "agrave";
      keyMapper[Qt::Key_Aacute] = "aacute";
      keyMapper[Qt::Key_Acircumflex] = "acircumflex";
      keyMapper[Qt::Key_Atilde] = "atilde";
      keyMapper[Qt::Key_Adiaeresis] = "adiaeresis";
      keyMapper[Qt::Key_Aring] = "aring";
      keyMapper[Qt::Key_AE] = "ae";
      keyMapper[Qt::Key_Ccedilla] = "ccedilla";
      keyMapper[Qt::Key_Egrave] = "egrave";
      keyMapper[Qt::Key_Eacute] = "eacute";
      keyMapper[Qt::Key_Ecircumflex] = "ecircumflex";
      keyMapper[Qt::Key_Ediaeresis] = "ediaeresis";
      keyMapper[Qt::Key_Igrave] = "igrave";
      keyMapper[Qt::Key_Iacute] = "iacute";
      keyMapper[Qt::Key_Icircumflex] = "icircumflex";
      keyMapper[Qt::Key_Idiaeresis] = "idiaeresis";
      keyMapper[Qt::Key_ETH] = "eth";
      keyMapper[Qt::Key_Ntilde] = "ntilde";
      keyMapper[Qt::Key_Ograve] = "ograve";
      keyMapper[Qt::Key_Oacute] = "oacute";
      keyMapper[Qt::Key_Ocircumflex] = "ocircumflex";
      keyMapper[Qt::Key_Otilde] = "otilde";
      keyMapper[Qt::Key_Odiaeresis] = "odiaeresis";
      keyMapper[Qt::Key_multiply] = "multiply";
      keyMapper[Qt::Key_Ooblique] = "ooblique";
      keyMapper[Qt::Key_Ugrave] = "ugrave";
      keyMapper[Qt::Key_Uacute] = "uacute";
      keyMapper[Qt::Key_Ucircumflex] = "ucircumflex";
      keyMapper[Qt::Key_Udiaeresis] = "udiaeresis";
      keyMapper[Qt::Key_Yacute] = "yacute";
      keyMapper[Qt::Key_THORN] = "thorn";
      keyMapper[Qt::Key_ssharp] = "ssharp";
      keyMapper[Qt::Key_division] = "division";
      keyMapper[Qt::Key_ydiaeresis] = "ydiaeresis";
    }

  return keyMapper.value (key, std::string ("<unknown key>"));
}

}; //namespace KeyMap

}; // namespace QtHandles
