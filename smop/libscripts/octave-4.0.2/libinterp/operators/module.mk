EXTRA_DIST += operators/module.mk

OPERATORS_SRC = \
  operators/op-b-b.cc \
  operators/op-b-bm.cc \
  operators/op-b-sbm.cc \
  operators/op-bm-b.cc \
  operators/op-bm-bm.cc \
  operators/op-bm-sbm.cc \
  operators/op-cdm-cdm.cc \
  operators/op-cdm-cm.cc \
  operators/op-cdm-cs.cc \
  operators/op-cdm-dm.cc \
  operators/op-cdm-m.cc \
  operators/op-cdm-s.cc \
  operators/op-cell.cc \
  operators/op-chm.cc \
  operators/op-class.cc \
  operators/op-cm-cdm.cc \
  operators/op-cm-cm.cc \
  operators/op-cm-cs.cc \
  operators/op-cm-dm.cc \
  operators/op-cm-m.cc \
  operators/op-cm-pm.cc \
  operators/op-cm-s.cc \
  operators/op-cm-scm.cc \
  operators/op-cm-sm.cc \
  operators/op-cs-cm.cc \
  operators/op-cs-cs.cc \
  operators/op-cs-m.cc \
  operators/op-cs-s.cc \
  operators/op-cs-scm.cc \
  operators/op-cs-sm.cc \
  operators/op-dm-cdm.cc \
  operators/op-dm-cm.cc \
  operators/op-dm-cs.cc \
  operators/op-dm-dm.cc \
  operators/op-dm-m.cc \
  operators/op-dm-s.cc \
  operators/op-dm-scm.cc \
  operators/op-dm-sm.cc \
  operators/op-double-conv.cc \
  operators/op-fcdm-fcdm.cc \
  operators/op-fcdm-fcm.cc \
  operators/op-fcdm-fcs.cc \
  operators/op-fcdm-fdm.cc \
  operators/op-fcdm-fm.cc \
  operators/op-fcdm-fs.cc \
  operators/op-fcm-fcdm.cc \
  operators/op-fcm-fcm.cc \
  operators/op-fcm-fcs.cc \
  operators/op-fcm-fdm.cc \
  operators/op-fcm-fm.cc \
  operators/op-fcm-fs.cc \
  operators/op-fcm-pm.cc \
  operators/op-fcn.cc \
  operators/op-fcs-fcm.cc \
  operators/op-fcs-fcs.cc \
  operators/op-fcs-fm.cc \
  operators/op-fcs-fs.cc \
  operators/op-fdm-fcdm.cc \
  operators/op-fdm-fcm.cc \
  operators/op-fdm-fcs.cc \
  operators/op-fdm-fdm.cc \
  operators/op-fdm-fm.cc \
  operators/op-fdm-fs.cc \
  operators/op-float-conv.cc \
  operators/op-fm-fcdm.cc \
  operators/op-fm-fcm.cc \
  operators/op-fm-fcs.cc \
  operators/op-fm-fdm.cc \
  operators/op-fm-fm.cc \
  operators/op-fm-fs.cc \
  operators/op-fm-pm.cc \
  operators/op-fs-fcm.cc \
  operators/op-fs-fcs.cc \
  operators/op-fs-fm.cc \
  operators/op-fs-fs.cc \
  operators/op-i16-i16.cc \
  operators/op-i32-i32.cc \
  operators/op-i64-i64.cc \
  operators/op-i8-i8.cc \
  operators/op-int-concat.cc \
  operators/op-int-conv.cc \
  operators/op-m-cdm.cc \
  operators/op-m-cm.cc \
  operators/op-m-cs.cc \
  operators/op-m-dm.cc \
  operators/op-m-m.cc \
  operators/op-m-pm.cc \
  operators/op-m-s.cc \
  operators/op-m-scm.cc \
  operators/op-m-sm.cc \
  operators/op-pm-cm.cc \
  operators/op-pm-fcm.cc \
  operators/op-pm-fm.cc \
  operators/op-pm-m.cc \
  operators/op-pm-pm.cc \
  operators/op-pm-scm.cc \
  operators/op-pm-sm.cc \
  operators/op-range.cc \
  operators/op-s-cm.cc \
  operators/op-s-cs.cc \
  operators/op-s-m.cc \
  operators/op-s-s.cc \
  operators/op-s-scm.cc \
  operators/op-s-sm.cc \
  operators/op-sbm-b.cc \
  operators/op-sbm-bm.cc \
  operators/op-sbm-sbm.cc \
  operators/op-scm-cm.cc \
  operators/op-scm-cs.cc \
  operators/op-scm-m.cc \
  operators/op-scm-s.cc \
  operators/op-scm-scm.cc \
  operators/op-scm-sm.cc \
  operators/op-sm-cm.cc \
  operators/op-sm-cs.cc \
  operators/op-sm-m.cc \
  operators/op-sm-s.cc \
  operators/op-sm-scm.cc \
  operators/op-sm-sm.cc \
  operators/op-str-m.cc \
  operators/op-str-s.cc \
  operators/op-str-str.cc \
  operators/op-struct.cc \
  operators/op-ui16-ui16.cc \
  operators/op-ui32-ui32.cc \
  operators/op-ui64-ui64.cc \
  operators/op-ui8-ui8.cc

## These look like included header files to Autotools build process
OPERATORS_INC = \
  operators/op-dm-template.cc \
  operators/op-dms-template.cc \
  operators/op-int.h \
  operators/op-pm-template.cc \
  operators/ops.h

## Special rules for sources which must be built before rest of compilation.
operators/ops.cc: $(OPERATORS_SRC) mkops
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mkops $(OPERATORS_SRC) > $@-t && \
	mv $@-t $@

