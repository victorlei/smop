FCN_FILE_DIRS += @ftp

@ftp_FCN_FILES = \
  @ftp/ascii.m \
  @ftp/binary.m  \
  @ftp/cd.m  \
  @ftp/close.m  \
  @ftp/delete.m  \
  @ftp/dir.m  \
  @ftp/display.m  \
  @ftp/ftp.m  \
  @ftp/loadobj.m  \
  @ftp/mget.m  \
  @ftp/mkdir.m  \
  @ftp/mput.m  \
  @ftp/rename.m  \
  @ftp/rmdir.m  \
  @ftp/saveobj.m

FCN_FILES += $(@ftp_FCN_FILES)

PKG_ADD_FILES += @ftp/PKG_ADD

DIRSTAMP_FILES += @ftp/$(octave_dirstamp)
