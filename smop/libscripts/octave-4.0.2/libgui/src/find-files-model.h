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
#if !defined (octave_find_files_model_h)
#define octave_find_files_model_h

#include <QAbstractListModel>
#include <QStringList>
#include <QList>
#include <QFileInfo>
#include <QIcon>


class find_files_model : public QAbstractListModel
{
  Q_OBJECT

public:
  find_files_model (QObject *p=0);
  ~find_files_model ();

  void clear ();

  void addFile (const QFileInfo &info);

  int rowCount (const QModelIndex & p=QModelIndex ()) const;

  int columnCount (const QModelIndex & p=QModelIndex ()) const;

  QVariant data (const QModelIndex& idx, int role) const;

  QVariant headerData (int section, Qt::Orientation orientation,
                       int role = Qt::DisplayRole) const;

  void sort (int column, Qt::SortOrder order=Qt::AscendingOrder);

  QFileInfo fileInfo (const QModelIndex & p) const;
  QIcon     fileIcon (const QModelIndex &p) const;
private:
  QList<QFileInfo> _files;
  QStringList _columnNames;
  int _sortorder;
};

#endif // octave_find_files_model_h

