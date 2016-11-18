/*

Copyright (C) 2013-2015 John Donoghue

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

#include <QFileIconProvider>
#include <QtAlgorithms>

#include "find-files-model.h"

class find_file_less_than
{
public:
  find_file_less_than (int ord)
  {
    _sortorder = ord;
  }
  QVariant getValue (const QFileInfo &f) const
  {
    QVariant val;
    int col = (_sortorder > 0) ? _sortorder : -_sortorder;

    switch (col-1)
      {
      case 0:
        val = QVariant (f.fileName ());
        break;

      case 1:
        val = QVariant (f.absolutePath ());
        break;

      default:
        break;
      }
    return val;
  }
  bool lessThan (const QVariant &left, const QVariant &right) const
  {
    return
      left.toString ().compare (right.toString (), Qt::CaseInsensitive) < 0;
  }
  bool operator () (const QFileInfo &left, const QFileInfo &right) const
  {
    QVariant leftval = getValue (left);
    QVariant rightval = getValue (right);

    if (_sortorder > 0)
      return lessThan (leftval, rightval);
    else
      return ! lessThan (leftval, rightval);
  }
private:
  int _sortorder;
};


find_files_model::find_files_model (QObject *p)
  : QAbstractListModel (p)
{
  _columnNames.append (tr ("Filename"));
  _columnNames.append (tr ("Directory"));
  _sortorder = 0;
}

find_files_model::~find_files_model ()
{
}

void
find_files_model::clear ()
{
  beginResetModel ();

  _files.clear ();

  endResetModel ();
}

void
find_files_model::addFile (const QFileInfo &info)
{
  beginInsertRows (QModelIndex (), _files.size (), _files.size ());

  QList<QFileInfo>::Iterator it;
  find_file_less_than less_than (_sortorder);

  for (it=_files.begin (); it!=_files.end (); it++)
    {
      if (less_than (info, *it)) break;
    }

  _files.insert (it, info);

  endInsertRows ();
}

int
find_files_model::rowCount (const QModelIndex &) const
{
  return _files.size ();
}

int
find_files_model::columnCount (const QModelIndex &) const
{
  return _columnNames.size ();
}

QVariant
find_files_model::data (const QModelIndex& idx, int role) const
{
  QVariant retval;

  if (idx.isValid ())
    {
      if (role == Qt::DisplayRole)
        {
          switch (idx.column ())
            {
            case 0:
              retval = QVariant (_files[idx.row ()].fileName ());
              break;

            case 1:
              retval = QVariant (_files[idx.row ()].absolutePath ());
              break;

            default:
              break;
            }
        }
      else if (role == Qt:: DecorationRole)
        {
          switch (idx.column ())
            {
            case 0:
              retval = fileIcon (idx);
            default:
              break;
            }
        }
    }

  return retval;
}

QVariant
find_files_model::headerData (int section, Qt::Orientation orientation,
                              int role) const
{
  if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
    return _columnNames[section];
  else
    return QVariant ();
}

void
find_files_model::sort (int column, Qt::SortOrder order)
{
  if (column >= 0)
    {
      if (order == Qt::DescendingOrder)
        _sortorder = -(column+1);
      else
        _sortorder = column+1;
    }
  else
    _sortorder = 0;

  if (_sortorder != 0)
    {
      beginResetModel ();
      qSort (_files.begin (), _files.end (), find_file_less_than (_sortorder));
      endResetModel ();
    }
}

QFileInfo
find_files_model::fileInfo (const QModelIndex & p) const
{
  if (p.isValid ())
    {
      return _files[p.row ()];
    }
  return QFileInfo ();
}

QIcon
find_files_model::fileIcon (const QModelIndex &p) const
{
  QFileIconProvider icon_provider;
  if (p.isValid ())
    {
      return icon_provider.icon (_files[p.row ()]);
    }
  return QIcon ();
}
