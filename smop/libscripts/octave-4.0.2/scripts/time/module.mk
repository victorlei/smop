FCN_FILE_DIRS += time

time_FCN_FILES = \
  time/addtodate.m \
  time/asctime.m \
  time/calendar.m \
  time/clock.m \
  time/ctime.m \
  time/date.m \
  time/datenum.m \
  time/datestr.m \
  time/datevec.m \
  time/eomday.m \
  time/etime.m \
  time/is_leap_year.m \
  time/now.m \
  time/weekday.m

FCN_FILES += $(time_FCN_FILES)

PKG_ADD_FILES += time/PKG_ADD

DIRSTAMP_FILES += time/$(octave_dirstamp)
