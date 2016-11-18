FCN_FILE_DIRS += miscellaneous

miscellaneous_PRIVATE_FCN_FILES = \
  miscellaneous/private/display_info_file.m \
  miscellaneous/private/__w2mpth__.m \
  miscellaneous/private/__xzip__.m

miscellaneous_FCN_FILES = \
  miscellaneous/ans.m \
  miscellaneous/bug_report.m \
  miscellaneous/bunzip2.m \
  miscellaneous/bzip2.m \
  miscellaneous/cast.m \
  miscellaneous/citation.m \
  miscellaneous/comma.m \
  miscellaneous/compare_versions.m \
  miscellaneous/computer.m \
  miscellaneous/copyfile.m \
  miscellaneous/debug.m \
  miscellaneous/delete.m \
  miscellaneous/desktop.m \
  miscellaneous/dir.m \
  miscellaneous/dos.m \
  miscellaneous/edit.m \
  miscellaneous/error_ids.m \
  miscellaneous/fact.m \
  miscellaneous/fileattrib.m \
  miscellaneous/fileparts.m \
  miscellaneous/fullfile.m \
  miscellaneous/genvarname.m \
  miscellaneous/getappdata.m \
  miscellaneous/getfield.m \
  miscellaneous/gunzip.m \
  miscellaneous/gzip.m \
  miscellaneous/info.m \
  miscellaneous/inputname.m \
  miscellaneous/isappdata.m \
  miscellaneous/isdeployed.m \
  miscellaneous/ismac.m \
  miscellaneous/ispc.m \
  miscellaneous/isunix.m \
  miscellaneous/license.m \
  miscellaneous/list_primes.m \
  miscellaneous/ls.m \
  miscellaneous/ls_command.m \
  miscellaneous/menu.m \
  miscellaneous/mex.m \
  miscellaneous/mexext.m \
  miscellaneous/mkoctfile.m \
  miscellaneous/movefile.m \
  miscellaneous/namelengthmax.m \
  miscellaneous/news.m \
  miscellaneous/open.m \
  miscellaneous/orderfields.m \
  miscellaneous/pack.m \
  miscellaneous/paren.m \
  miscellaneous/parseparams.m \
  miscellaneous/perl.m \
  miscellaneous/python.m \
  miscellaneous/recycle.m \
  miscellaneous/rmappdata.m \
  miscellaneous/run.m \
  miscellaneous/semicolon.m \
  miscellaneous/setappdata.m \
  miscellaneous/setfield.m \
  miscellaneous/substruct.m \
  miscellaneous/swapbytes.m \
  miscellaneous/symvar.m \
  miscellaneous/tar.m \
  miscellaneous/tempdir.m \
  miscellaneous/tmpnam.m \
  miscellaneous/unix.m \
  miscellaneous/unpack.m \
  miscellaneous/untar.m \
  miscellaneous/unzip.m \
  miscellaneous/ver.m \
  miscellaneous/version.m \
  miscellaneous/warning_ids.m \
  miscellaneous/what.m \
  miscellaneous/xor.m \
  miscellaneous/zip.m \
  $(miscellaneous_PRIVATE_FCN_FILES)

FCN_FILES += $(miscellaneous_FCN_FILES)

PKG_ADD_FILES += miscellaneous/PKG_ADD

DIRSTAMP_FILES += miscellaneous/$(octave_dirstamp)
