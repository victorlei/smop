/*

Copyright (C) 2009 P.L. Lucas
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

#include <QStringList>
#include <QIODevice>
#include <QFileInfoList>
#include <QHash>

/**
 * @class parser
 * This class gets nodes and searchs inside of 'info files'.
 * <p>Each info file has nodes. Every node has the documentation.
 * Info files contains a map with position of each node.</p>
 * <p>What is position?
 * There is a simple answer:
 * If you make a queue with info files, position will be the number of bytes
 * from begining to node position.</p>
 * <p>
 * But is not so easy. There is headers, and qtinfo must not take these
 * headers into account.
 * </p>
 * <p>
 * This class also translates info files to html.
 * </p>
 */
class parser
  : public QObject
{
  Q_OBJECT

public:
  parser (QObject *parent = 0);
  bool set_info_path (const QString& _info_path);
  QString get_info_path ();
  QString search_node (const QString& node);
  QString global_search (const QString& text, int maxFounds);

  QString find_ref (const QString &name);

  /** Checks if this node is reference. If node is reference, it will be
   *  returned its position in text, else it will be returned -1.
    */
  int is_ref (const QString& node);

  /** Translates text of node to Html. If anchorPos is not -1, then anchor is
   *  inserted in that position.
    */
  QString node_text_to_html (const QString& text, int anchorPos = -1,
                             const QString& anchor = QString ());

private:
  struct node_position
  {
    QString _node_name;
    int pos;
  };

  struct node_map_item
  {
    int pos;
  };

  struct info_file_item
  {
    QFileInfo file_info;
    int real_size;
  };

  QString search_node (const QString& node, QIODevice * io);
  QString get_next_node (QIODevice * io);
  QString get_node_name (const QString& text);
  QString get_node_up (const QString& text);
  QString get_node_next (const QString& text);
  QString get_node_prev (const QString& text);

  /** Parses info files and gets map of node positions.*/
  void parse_info_map();

  /** Open info files and uncompress them. */
  QIODevice *open_file(QFileInfo & fileInfo);

  /** Calculates real position of nodes.
    * @param pos position from info file.
    * @param fileInfo returns file what contains that position.
    * @param realPos returns real position inside of fileInfo.
    */
  void real_position (int pos, QFileInfo & file_info, int & real_pos);

  /** Seeks to position pos. */
  void seek (QIODevice *io, int pos);


  QString                       _info_path;
  QFileInfoList                 _info_files;
  QHash<QString, node_map_item> _node_map;
  QHash<QString, node_position> _ref_map;
  QList<info_file_item>         _info_file_real_size_list;
  QHash<QString, QString>       _compressors_map;
};
