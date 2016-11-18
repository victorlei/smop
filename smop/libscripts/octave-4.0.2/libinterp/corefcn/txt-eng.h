/*

Copyright (C) 2009-2015 Michael Goffioul

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

#if ! defined (txt_eng_h)
#define txt_eng_h 1

#include <memory>
#include <string>

#include "base-list.h"
#include "caseless-str.h"
#include "dMatrix.h"

class text_element;
class text_element_string;
class text_element_symbol;
class text_element_list;
class text_element_subscript;
class text_element_superscript;
class text_element_combined;
class text_element_fontname;
class text_element_fontsize;
class text_element_fontstyle;
class text_element_color;

class text_processor;

class
OCTINTERP_API
text_element
{
public:
  text_element (void) { }

  virtual ~text_element (void) { }

  virtual void accept (text_processor& p) = 0;

private:
  text_element (const text_element&);
};

class
OCTINTERP_API
text_element_string : public text_element
{
public:
  text_element_string (const std::string& s = "")
    : text_element (), str (s) { }

  ~text_element_string (void) { }

  std::string string_value (void) const { return str; }

  void accept (text_processor& p);

private:
  std::string str;

private:
  text_element_string (const text_element_string &);
};

class
OCTINTERP_API
text_element_symbol : public text_element
{
public:
  enum { invalid_code = 0xFFFFFFFFU };

public:
  text_element_symbol (int sym)
    : text_element (), symbol (sym) { }

  ~text_element_symbol (void) { }

  int get_symbol (void) const { return symbol; }

  uint32_t get_symbol_code (void) const;

  void accept (text_processor& p);

private:
  int symbol;
};

class
OCTINTERP_API
text_element_list
  : public text_element, public octave_base_list<text_element *>
{
public:
  text_element_list (void)
    : text_element (), octave_base_list<text_element*> () { }

  text_element_list (text_element* e)
    : text_element (), octave_base_list<text_element*> ()
  { push_back (e); }

  ~text_element_list (void)
  {
    while (! empty ())
      {
        iterator it = begin ();
        delete (*it);
        erase (it);
      }
  }

  void accept (text_processor& p);
};

class
OCTINTERP_API
text_element_subscript : public text_element
{
public:
  text_element_subscript (text_element* e)
    : text_element (), elem (e) { }

  text_element_subscript (char c)
    : text_element ()
  { elem = new text_element_string (std::string (1, c)); }

  ~text_element_subscript (void)
  { delete elem; }

  void accept (text_processor& p);

  text_element* get_element (void) { return elem; }

private:
  text_element* elem;

private:
  text_element_subscript (void);
};

class
OCTINTERP_API
text_element_superscript : public text_element
{
public:
  text_element_superscript (text_element* e)
    : text_element (), elem (e) { }

  text_element_superscript (char c)
    : text_element ()
  { elem = new text_element_string (std::string (1, c)); }

  ~text_element_superscript (void)
  { delete elem; }

  void accept (text_processor& p);

  text_element* get_element (void) { return elem; }

private:
  text_element* elem;

private:
  text_element_superscript (void);
};

class
OCTINTERP_API
text_element_combined : public text_element_list
{
public:
  text_element_combined (text_element* e)
    : text_element_list (e) { }

  text_element_combined (text_element* e1, text_element* e2)
    : text_element_list(e1)
  { push_back (e2); }

  void accept (text_processor& p);
};

class
OCTINTERP_API
text_element_fontstyle : public text_element
{
public:
  enum fontstyle
  {
    normal,
    bold,
    italic,
    oblique
  };

  text_element_fontstyle (fontstyle st)
    : text_element (), style (st) { }

  ~text_element_fontstyle (void) { }

  fontstyle get_fontstyle (void) const { return style; }

  void accept (text_processor& p);

private:
  fontstyle style;

private:
  text_element_fontstyle (void);
};

class
OCTINTERP_API
text_element_fontname : public text_element
{
public:
  text_element_fontname (const std::string& fname)
    : text_element (), name (fname) { }

  ~text_element_fontname (void) { }

  const std::string& get_fontname (void) const { return name; }

  void accept (text_processor& p);

private:
  std::string name;

private:
  text_element_fontname (void);
};

class
OCTINTERP_API
text_element_fontsize : public text_element
{
public:
  text_element_fontsize (double fsize)
    : text_element (), size (fsize) { }

  ~text_element_fontsize (void) { }

  double get_fontsize (void) const { return size; }

  void accept (text_processor& p);

private:
  double size;

private:
  text_element_fontsize (void);
};

class
OCTINTERP_API
text_element_color : public text_element
{
public:
  text_element_color (double r, double g, double b)
    : text_element (), rgb (1, 3, 0.0)
  {
    rgb(0) = r;
    rgb(1) = g;
    rgb(2) = b;
  }

  text_element_color (const std::string& cname)
    : text_element (), rgb (1, 3, 0.0)
  {
#define ASSIGN_COLOR(r,g,b) { rgb(0) = r; rgb(1) = g; rgb(2) = b; }
    if (cname == "red") ASSIGN_COLOR(1, 0, 0)
      else if (cname == "green") ASSIGN_COLOR(0, 1, 0)
        else if (cname == "yellow") ASSIGN_COLOR(1, 1, 0)
          else if (cname == "magenta") ASSIGN_COLOR(1, 0, 1)
            else if (cname == "blue") ASSIGN_COLOR(0, 0, 1)
              else if (cname == "black") ASSIGN_COLOR(0, 0, 0)
                else if (cname == "white") ASSIGN_COLOR(1, 1, 1)
                  else if (cname == "gray") ASSIGN_COLOR(.5, .5, .5)
                    else if (cname == "darkGreen") ASSIGN_COLOR(0, .5, 0)
                      else if (cname == "orange") ASSIGN_COLOR(1, .65, 0)
                        else if (cname == "lightBlue") ASSIGN_COLOR(0.68, .85, .9)
#undef ASSIGN_COLOR
  }

  ~text_element_color (void) { }

  Matrix get_color (void) { return rgb; }

  void accept (text_processor& p);

private:
  Matrix rgb;
};

class
OCTINTERP_API
text_processor
{
public:
  virtual void visit (text_element_string& e) = 0;

  virtual void visit (text_element_symbol&) { }

  virtual void visit (text_element_list& e)
  {
    for (text_element_list::iterator it = e.begin ();
         it != e.end (); ++it)
      {
        (*it)->accept (*this);
      }
  }

  virtual void visit (text_element_subscript& e)
  { e.get_element ()->accept (*this); }

  virtual void visit (text_element_superscript& e)
  { e.get_element ()->accept (*this); }

  virtual void visit (text_element_combined&) { }

  virtual void visit (text_element_fontstyle&) { }

  virtual void visit (text_element_fontname&) { }

  virtual void visit (text_element_fontsize&) { }

  virtual void visit (text_element_color&) { }

  virtual void reset (void) { }

protected:
  text_processor (void) { }

  virtual ~text_processor (void) { }
};

#define TEXT_ELEMENT_ACCEPT(cls) \
inline void \
cls::accept (text_processor& p) \
{ p.visit (*this); }

TEXT_ELEMENT_ACCEPT(text_element_string)
TEXT_ELEMENT_ACCEPT(text_element_symbol)
TEXT_ELEMENT_ACCEPT(text_element_list)
TEXT_ELEMENT_ACCEPT(text_element_subscript)
TEXT_ELEMENT_ACCEPT(text_element_superscript)
TEXT_ELEMENT_ACCEPT(text_element_combined)
TEXT_ELEMENT_ACCEPT(text_element_fontstyle)
TEXT_ELEMENT_ACCEPT(text_element_fontname)
TEXT_ELEMENT_ACCEPT(text_element_fontsize)
TEXT_ELEMENT_ACCEPT(text_element_color)

class
OCTINTERP_API
text_parser
{
public:
  text_parser (void) { }

  virtual ~text_parser (void) { }

  virtual text_element* parse (const std::string& s) = 0;

public:
  static text_element* parse (const std::string& s,
                              const caseless_str& interpreter);
};

class
OCTINTERP_API
text_parser_none : public text_parser
{
public:
  text_parser_none (void) : text_parser () { }

  ~text_parser_none (void) { }

  // FIXME: is it possible to use reference counting to manage the
  // memory for the object returned by the text parser?  That would be
  // preferable to having to know when and where to delete the object it
  // creates...

  text_element* parse (const std::string& s)
  {
    return new text_element_string (s);
  }
};

class
OCTINTERP_API
text_parser_tex : public text_parser
{
public:
  text_parser_tex (void)
    : text_parser (), scanner (0), buffer_state (0), result (0)
  { }

  ~text_parser_tex (void)
  { destroy_lexer (); }

  text_element* parse (const std::string& s);

  void* get_scanner (void) { return scanner; }

  void set_parse_result (text_element* e) { result = e; }

  text_element* get_parse_result (void) { return result; }

private:
  bool init_lexer (const std::string& s);

  void destroy_lexer (void);

private:
  void* scanner;

  void* buffer_state;

  text_element* result;
};

inline text_element*
text_parser::parse (const std::string& s, const caseless_str& interpreter)
{
  std::auto_ptr<text_parser> parser;

  if (interpreter.compare ("tex"))
    parser.reset (new text_parser_tex ());
  else
    parser.reset (new text_parser_none ());

  return parser->parse (s);
}

#endif
