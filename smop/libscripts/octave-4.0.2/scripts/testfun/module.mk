FCN_FILE_DIRS += testfun

testfun_FCN_FILES = \
  testfun/__have_feature__.m \
  testfun/__printf_assert__.m \
  testfun/__prog_output_assert__.m \
  testfun/__run_test_suite__.m \
  testfun/assert.m \
  testfun/demo.m \
  testfun/example.m \
  testfun/fail.m \
  testfun/rundemos.m \
  testfun/runtests.m \
  testfun/speed.m \
  testfun/test.m

FCN_FILES += $(testfun_FCN_FILES)

PKG_ADD_FILES += testfun/PKG_ADD

DIRSTAMP_FILES += testfun/$(octave_dirstamp)
