FCN_FILE_DIRS += deprecated

deprecated_FCN_FILES = \
  deprecated/bicubic.m \
  deprecated/default_save_options.m \
  deprecated/delaunay3.m \
  deprecated/dump_prefs.m \
  deprecated/find_dir_in_path.m \
  deprecated/finite.m \
  deprecated/fmod.m \
  deprecated/fnmatch.m \
  deprecated/gen_doc_cache.m \
  deprecated/gmap40.m \
  deprecated/interp1q.m \
  deprecated/isequalwithequalnans.m \
  deprecated/isstr.m \
  deprecated/java_convert_matrix.m \
  deprecated/java_debug.m \
  deprecated/java_invoke.m \
  deprecated/java_new.m \
  deprecated/java_unsigned_conversion.m \
  deprecated/javafields.m \
  deprecated/javamethods.m \
  deprecated/loadaudio.m \
  deprecated/luinc.m \
  deprecated/mouse_wheel_zoom.m \
  deprecated/nfields.m \
  deprecated/octave_tmp_file_name.m \
  deprecated/playaudio.m \
  deprecated/re_read_readline_init_file.m \
  deprecated/read_readline_init_file.m \
  deprecated/saveaudio.m \
  deprecated/saving_history.m \
  deprecated/setaudio.m \
  deprecated/syl.m \
  deprecated/usage.m

FCN_FILES += $(deprecated_FCN_FILES)

PKG_ADD_FILES += deprecated/PKG_ADD

DIRSTAMP_FILES += deprecated/$(octave_dirstamp)
