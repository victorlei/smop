/*

Copyright (C) 2011-2015 Jacob Dawid

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

#ifndef RESOURCEMANAGER_H
#define RESOURCEMANAGER_H

#include <QDesktopServices>
#include <QIcon>
#include <QMap>
#include <QSettings>
#include <QTranslator>

class resource_manager : public QObject
{
  Q_OBJECT

protected:

public:

  resource_manager (void);

  ~resource_manager ();

  static QSettings *get_settings (void)
  {
    return instance_ok () ? instance->do_get_settings () : 0;
  }

  static QIcon icon (const QString& icon_name, bool fallback = true)
  {
    if (instance_ok ())
      return instance->do_icon (icon_name, fallback);

    return QIcon ();
  }

  static QSettings *get_default_settings (void)
  {
    return instance_ok () ? instance->do_get_default_settings () : 0;
  }

  static QString get_settings_file (void)
  {
    return instance_ok () ? instance->do_get_settings_file () : QString ();
  }

  static void reload_settings (void)
  {
    if (instance_ok ())
      instance->do_reload_settings ();
  }

  static void set_settings (const QString& file)
  {
    if (instance_ok ())
      instance->do_set_settings (file);
  }

  static QString get_gui_translation_dir (void);

  static void config_translators (QTranslator*, QTranslator*, QTranslator*);

  static void update_network_settings (void)
  {
    if (instance_ok ())
      instance->do_update_network_settings ();
  }

  static bool is_first_run (void)
  {
    return instance_ok () ? instance->do_is_first_run () : true;
  }

  static QString storage_class_chars (void) { return "afghip"; }
  static QStringList storage_class_names (void);
  static QList<QColor> storage_class_default_colors (void);

  static QString terminal_color_chars (void) { return "fbsc"; }
  static QStringList terminal_color_names (void);
  static QList<QColor> terminal_default_colors (void);

private:

  static resource_manager *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  // No copying!

  resource_manager (const resource_manager&);

  resource_manager& operator = (const resource_manager&);

  static bool instance_ok (void);

  QString settings_directory;

  QString settings_file;

  QSettings *settings;

  QSettings *default_settings;

  QSettings *do_get_settings (void) const;

  QSettings *do_get_default_settings (void) const;

  QString do_get_settings_file (void);

  QString do_get_settings_directory (void);

  void do_reload_settings (void);

  void do_set_settings (const QString& file);

  void do_update_network_settings (void);

  bool do_is_first_run (void) const;

  QIcon do_icon (const QString& icon, bool fallback);

};

#endif // RESOURCEMANAGER_H
