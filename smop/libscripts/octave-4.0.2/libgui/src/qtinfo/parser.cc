/*

Copyright (C) 2009 P. L. Lucas
Copyright (C) 2012-2015 Jacob Dawid

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

// Author: P. L. Lucas
// Author: Jacob Dawid <jacob.dawid@cybercatalyst.com>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "parser.h"
#include "procstream.h"
#include <QFileInfo>
#include <QDir>
#include <QFile>
#include <QUrl>
#include <QRegExp>
#include <QBuffer>

parser::parser(QObject *p)
  : QObject(p)
{
  _compressors_map.insert ("bz2",  "bzip2 -dc \"%1\"");
  _compressors_map.insert ("gz",   "gzip -dc \"%1\"");
  _compressors_map.insert ("lzma", "lzma -dc \"%1\"");
  _compressors_map.insert ("xz",   "xz -dc \"%1\"");
  _compressors_map.insert ("Z",    "gunzip -c \"%1\"");
}

bool
parser::set_info_path (const QString& infoPath)
{
  this->_info_path = infoPath;

  _info_files.clear ();

  QFileInfo info (infoPath);

  bool info_file_exists = info.exists ();
  QHash<QString, QString>::iterator it;
  for (it = _compressors_map.begin (); it != _compressors_map.end (); it++)
    {
      if (info_file_exists)
        break;
      info_file_exists = QFileInfo (info.absoluteFilePath () + "." + it.key ()).exists ();
    }

  if (info_file_exists)
    {
      QString path = info.absolutePath ();
      QString fileName = info.fileName ();

      QDir infoDir (path);
      QStringList filter;
      filter.append (fileName + "*");

      _info_files = infoDir.entryInfoList (filter, QDir::Files);

      parse_info_map ();

      return true;
    }
  else
    return false;
}

QString
parser::get_info_path ()
{
  return _info_path;
}

QIODevice *
parser::open_file (QFileInfo & file_info)
{
  QIODevice *iodevice = 0;
  if (_compressors_map.contains (file_info.suffix ()))
    {
      QString command = _compressors_map.value (file_info.suffix ()).arg (file_info.absoluteFilePath ());
      iprocstream ips (command.toStdString ());

      if (ips.bad ())
        return 0;

      QByteArray result;
      char buffer[1024];

      while (! ips.eof ())
        {
          ips.read (buffer, sizeof (buffer));
          result.append (buffer, ips.gcount ());
        }

      QBuffer *io = new QBuffer (this);
      io->setData (result);

      if (!io->open (QIODevice::ReadOnly | QIODevice::Text))
        return 0;

      iodevice = io;
    }
  else
    {
      QFile *io = new QFile (file_info.absoluteFilePath ());
      if (!io->open (QIODevice::ReadOnly | QIODevice::Text))
        return 0;
      iodevice = io;
    }

  return iodevice;
}

int
parser::is_ref (const QString& node)
{
  if (_ref_map.contains (node))
    {
      node_position ref = _ref_map [node];

      return ref.pos-_node_map [ref._node_name].pos;
    }
  if (_node_map.contains (node))
    {
      return 0;  // node: show from the beginning
    }
  return -1;
}

QString
parser::search_node (const QString& node_arg)
{
  QString node = node_arg;

  QFileInfo file_info;
  QString ref;

  if (_ref_map.contains (node))
    {
      ref = node;
      node = _ref_map [ref]._node_name;
    }

  if (_node_map.contains (node))
    {
      int pos = _node_map [node].pos;
      int realPos;

      real_position (pos, file_info, realPos);

      QIODevice *io = open_file (file_info);
      if (! io)
        {
          return QString ();
        }

      seek (io, realPos);

      QString text = get_next_node (io);
      if (!text.isEmpty())
        {
          return text;
        }

      io->close ();
      delete io;
    }

  return QString ();
}

QString
parser::search_node (const QString& node, QIODevice *io)
{
  while (!io->atEnd ())
    {
      QString text = get_next_node (io);
      if (node == get_node_name (text))
        {
          return text;
        }
    }

  return QString ();
}

QString
parser::get_next_node (QIODevice *io)
{
  QString text;
  QByteArray line, line_buffer;
  char c;
  int i;

  while (!io->atEnd ())
    {
      io->getChar (&c);
      if (c)
        {
          // first char is not equal 0
          io->ungetChar (c);
          line = io->readLine ();
        }
      else
        {
          // 0 was read -> image -> text length changes
          line_buffer = io->readLine ();  // image tag that is not needed
          line = io->readLine ();         // firsts line of text message
          for (i=1; i<line_buffer.size ()+6; i++)  // correct the size
            line.insert (line.size ()-1,QByteArray(" "));   // by adding blanks
        }

      if (line.at (0) == '"' && line.size () == 5)  // end of image construct
        line = " ";

      if (line.at(0) == 31)
        {
          break;
        }
      else
        {
          text.append (line);
        }
    }
  return text;
}

static QString
get_first_line (const QString& text)
{
  int n = text.indexOf ("\n");

  if (n < 0)
    {
      return QString ();
    }

  QString first_line = text.left (n);
  return first_line;
}

static QString
parser_node (const QString& text, const QString& node_name)
{
  QString firstLine = get_first_line (text);
  QStringList nodes = firstLine.split (",");
  for (int i = 0; i < nodes.size (); i++)
    {
      QString node = nodes.at (i).trimmed ();

      if (node.startsWith (node_name))
        {
          return node.remove (0, node_name.size ()).trimmed ();
        }
    }
  return QString ();
}

QString
parser::get_node_name (const QString& text)
{
  return parser_node (text, "Node:");
}

QString
parser::get_node_up (const QString& text)
{
  return parser_node (text, "Up:");
}

QString
parser::get_node_next (const QString& text)
{
  return parser_node (text, "Next:");
}

QString
parser::get_node_prev (const QString& text)
{
  return parser_node (text, "Prev:");
}

static void
replace_links (QString& text)
{
  QRegExp re ("(\\*[N|n]ote|\n\\*)([ |\n]+)([^:]+):([^:\\.,]*)([:,\\.]+)");
  int i = 0, f;

  while ((i = re.indexIn (text,i)) != -1)
    {
      QString type     = re.cap (1);
      QString note     = re.cap (3);
      QString url_link = re.cap (4);
      QString term     = re.cap (5);

      if (url_link.isEmpty ())
        {
          url_link = note;
        }

      term.replace(":","");
      note.replace(":","");
      note.replace (QRegExp ("`([^']+)'"),"\\1");   // no extra format in links

      QRegExp re_break ("(\n[ ]*)");

      if (note == "fig" || note == "tab")
        url_link.prepend("#");

      QString href;
      if (type == "\n*")
        href="\n";

      if (re_break.indexIn (url_link) != -1)
        term += re_break.cap (1);
      else if (re_break.indexIn (re.cap (2)) != -1)
        href = re_break.cap (1) + " ";
      else if (re_break.indexIn (note) != -1)
        term += re_break.cap (1);
      note.replace(re_break,"&nbsp;");

      url_link = url_link.trimmed ();
      url_link.replace ("\n"," ");
      url_link.replace (QRegExp ("  +")," ");
      url_link.replace ("<b>","");
      url_link.replace ("</b>","");
      url_link = QUrl::toPercentEncoding (url_link, "", "'");

      href += "<font style=\"color:DarkGray; font-weight:bold;\">&raquo;</font>";
      href +=  "&nbsp;<a href='" + url_link + "'>" + note + "</a>" + term;
      f = re.matchedLength ();
      text.replace (i,f,href);
      i += href.size ();
    }
}

static void
replace_colons (QString& text)
{
  QRegExp re ("`([^']+)'");
  int i = 0, f;
  while ((i = re.indexIn (text, i)) != -1)
    {
      QString t = re.cap (1);
      QString bold = "<font style=\"color:SteelBlue;font-weight:bold\">" + t +
                     "</font>";

      f = re.matchedLength ();
      text.replace (i,f,bold);
      i += bold.size ();
    }
}

static void
info_to_html (QString& text)
{
  text.replace ("&", "&amp;");
  text.replace ("<", "&lt;");
  text.replace (">", "&gt;");

  text.replace ("\n* Menu:",
                "\n<font style=\"color:DarkRed;font-weight:bold\">Menu:</font>");
  text.replace ("See also:",
                "<font style=\"color:DarkRed;font-style:italic;font-weight:bold\">See also:</font>");
  replace_links (text);
  replace_colons (text);
}

QString
parser::node_text_to_html (const QString& text_arg, int anchorPos,
                           const QString& anchor)
{
  QString text = text_arg;

  QString nodeName = get_node_name (text);
  QString nodeUp   = get_node_up (text);
  QString nodeNext = get_node_next (text);
  QString nodePrev = get_node_prev (text);

  if (anchorPos > -1)
    {
      QString text1 = text.left (anchorPos);
      QString text2 = text.mid (anchorPos);

      int n = text1.indexOf ("\n");
      text1.remove (0, n);

      info_to_html (text1);
      info_to_html (text2);

      text = text1 + "<a name='" + anchor
             + "'/><font style=\"color:DarkBlue; font: bold monospace large;\">&diams;</font><br>&nbsp;"
             + text2;
    }
  else
    {
      int n = text.indexOf ("\n");
      text.remove (0, n);
      info_to_html (text);
    }

  QString navigationLinks = QString (
        "<b>Section:</b> <font style=\"color:DarkRed\">%1</font><br>"
        "<b>Previous Section:</b> <a href='%2'>%3</a><br>"
        "<b>Next Section:</b> <a href='%4'>%5</a><br>"
        "<b>Up:</b> <a href='%6'>%7</a><br>\n"
        )
      .arg (nodeName)
      .arg (QString (QUrl::toPercentEncoding (nodePrev, "", "'")))
      .arg (nodePrev)
      .arg (QString (QUrl::toPercentEncoding (nodeNext, "", "'")))
      .arg (nodeNext)
      .arg (QString (QUrl::toPercentEncoding (nodeUp, "", "'")))
      .arg (nodeUp);

  text.prepend ("<hr>\n<pre style=\"font-family:monospace\">");
  text.append ("</pre>\n<hr><hr>\n");
  text.prepend (navigationLinks);
  text.append (navigationLinks);
  text.prepend ("<html><body>\n");
  text.append ("</body></html>\n");

  return text;

}

void
parser::parse_info_map ()
{
  QRegExp re ("(Node|Ref): ([^\\0177]+)\\0177(\\d+)\n");
  QRegExp re_files ("([^:]+): (\\d+)\n");
  int foundCount = 0;

  for (int i = 0; i < _info_files.size (); i++)
    {
      QFileInfo fileInfo = _info_files.at (i);

      QIODevice *io = open_file (fileInfo);
      if (! io)
        {
          continue;
        }

      QString nodeText;
      while (! (nodeText=get_next_node (io)).isEmpty () && foundCount < 2)
        {
          QString first_line = get_first_line (nodeText);
          if (first_line.startsWith ("Tag"))
            {
              foundCount++;
              int pos = 0;
              QString last_node;

              while ((pos = re.indexIn (nodeText, pos)) != -1)
                {
                  QString type = re.cap (1);
                  QString node = re.cap (2);
                  int index = re.cap (3).toInt ();

                  if (type == "Node")
                    {
                      node_map_item item;
                      item.pos = index;
                      _node_map [node] = item;
                      last_node = node;
                    }
                  else if (type == "Ref")
                    {
                      node_position item;
                      item._node_name = last_node;
                      item.pos = index;
                      _ref_map [node] = item;
                    }
                  pos += re.matchedLength ();
                }
              break;
            }
          else if (first_line.startsWith ("Indirect:"))
            {
              foundCount++;
              int pos = 0;

              while ((pos = re_files.indexIn (nodeText, pos)) != -1)
                {
                  QString fileCap = re_files.cap (1).trimmed ();
                  int index = re_files.cap (2).toInt ();

                  info_file_item item;
                  for (int j = 0; j < _info_files.size (); j++)
                    {
                      QFileInfo info = _info_files.at (j);
                      if (info.fileName ().startsWith (fileCap))
                        {
                          item.file_info = info;
                          break;
                        }
                    }
                  item.real_size = index;
                  _info_file_real_size_list.append (item);
                  pos += re_files.matchedLength ();
                }

            }
        }
      io->close ();
      delete io;
    }
}

void
parser::real_position (int pos, QFileInfo & file_info, int & real_pos)
{
  int header = -1;
  int sum = 0;
  for (int i = 0; i < _info_file_real_size_list.size (); i++)
    {
      info_file_item item = _info_file_real_size_list.at (i);
      if (header == -1)
        {
          file_info = item.file_info;
          header = item.real_size;
        }

      if (pos < item.real_size)
        {
          break;
        }

      file_info = item.file_info;
      sum = item.real_size;
    }
  real_pos = pos - sum + header + 2;
}

void
parser::seek (QIODevice *io, int pos)
{
  char ch;
  while (!io->atEnd () && pos > 0)
    {
      io->getChar (&ch);
      pos--;
    }
}

static void
replace (QString& text, const QRegExp& re, const QString& after)
{
  int pos = 0;

  while ((pos = re.indexIn (text, pos)) != -1)
    {
      QString cap = text.mid (pos,re.matchedLength ());
      QString a (after);
      a = a.arg (cap);
      text.remove (pos, re.matchedLength ());
      text.insert (pos, a);
      pos += a.size ();
    }
}

QString
parser::global_search (const QString& text, int max_founds)
{
  QString results;
  QStringList words = text.split (" ",QString::SkipEmptyParts);

  QString re_program ("(" + words.at (0));
  for (int i = 1; i < words.size (); i++)
    {
      re_program += "|" + words.at (i);
    }
  re_program += ")";

  QRegExp re (re_program, Qt::CaseInsensitive);

  results.append ("<html><body>\n<h1>Search results</h1>\n<b>Results for:</b> ");
  results.append (text);
  results.append ("<br>\n");

  for (int i = 0; i < _info_files.size (); i++)
    {
      QFileInfo file_info = _info_files.at (i);
      QIODevice *io = open_file (file_info);
      if (! io)
        {
          continue;
        }

      QString node_text;
      while (! (node_text = get_next_node (io)).isEmpty ())
        {
          QString firstLine = get_first_line (node_text);
          QString node = get_node_name (node_text);
          if (node.isEmpty ())
            {
              continue;
            }

          int n = node_text.indexOf ("\n");
          node_text.remove (0, n);

          int pos = 0;
          int founds = 0;

          for (; founds < words.size ()
                 && node_text.indexOf (words.at (founds)) >= 0; founds++)
            { }

          if (founds<words.size ())
            {
              continue;
            }
          founds = 0;

          while ((pos = re.indexIn (node_text, pos)) != -1
                 && founds < max_founds)
            {
              int line_start, line_end;
              line_start = node_text.lastIndexOf ("\n", pos);
              line_end = node_text.indexOf ("\n", pos);
              QString line = node_text.mid (line_start,
                                            line_end - line_start).trimmed ();
              pos += re.matchedLength ();

              if (founds == 0)
                {
                  results.append(
                    "<br>\n<font style=\"color:DarkGray; font-weight:bold;\">&raquo;</font> <a href='"
                    + QString(QUrl::toPercentEncoding(node,"","'")) +
                    "'>");
                  results.append (node);
                  results.append ("</a><br>\n");
                }

              replace (line, re, "<i>%1</i>");
              results.append (line);
              results.append ("<br>\n");

              founds++;
            }
        }
      io->close ();
      delete io;
    }

  results.append ("</body></html>");
  return results;
}

QString
parser::find_ref (const QString &ref_name)
{
  QString text = "";

  QHash<QString,node_position>::iterator it;
  for (it=_ref_map.begin (); it!=_ref_map.end (); ++it)
    {
      QString k = it.key ();
      node_position p = it.value ();

      if (k == "XREF" + ref_name)
        {
          // found ref, so return its name
          text = "XREF" + ref_name;
          break;
        }
    }

  if (text.isEmpty ())  // try the statement-nodes
    {
      QHash<QString, node_map_item>::iterator itn;
      for (itn=_node_map.begin (); itn!=_node_map.end (); ++itn)
        {
          QString k = itn.key ();
          if (k == "The " + ref_name + " Statement")
            {
              // found ref, so return its name
              text = k;
              break;
            }
        }
    }

  return text;
}

