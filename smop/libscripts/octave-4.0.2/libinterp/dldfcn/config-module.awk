BEGIN {
  FS = "|";
  nfiles = 0;

  print "## DO NOT EDIT -- generated from module-files by config-module.awk";
  print ""
  print "EXTRA_DIST += \\"
  print "  dldfcn/config-module.sh \\"
  print "  dldfcn/config-module.awk \\"
  print "  dldfcn/module-files \\"
  print "  dldfcn/oct-qhull.h"
  print ""
}
/^#.*/ { next; }
{
  nfiles++;
  files[nfiles] = $1;
  cppflags[nfiles] = $2;
  ldflags[nfiles] = $3;
  libraries[nfiles] = $4;
} END {
  sep = " \\\n";
  print "DLDFCN_SRC = \\";
  for (i = 1; i <= nfiles; i++) {
    if (i == nfiles)
      sep = "\n";
    printf ("  dldfcn/%s%s", files[i], sep);
  }
  print "";

  sep = " \\\n";
  print "DLDFCN_LIBS = $(DLDFCN_SRC:.cc=.la)";
  print "";
  print "if AMCOND_ENABLE_DYNAMIC_LINKING";
  print "";
  print "octlib_LTLIBRARIES += $(DLDFCN_LIBS)";
  print "";
  print "## Use stamp files to avoid problems with checking timestamps";
  print "## of symbolic links";
  print "";
  print "%.oct : %.la"
  print "	$(AM_V_GEN)$(INSTALL_PROGRAM) dldfcn/.libs/$(shell $(SED) -n -e \"s/dlname='\\([^']*\\)'/\\1/p\" < $<) $@"
  print ""
  print "else";
  print "";
  print "noinst_LTLIBRARIES += $(DLDFCN_LIBS)";
  print "";
  print "endif";

  for (i = 1; i <= nfiles; i++) {
    basename = files[i];
    sub (/\.cc$/, "", basename);
    print "";
    printf ("dldfcn_%s_la_SOURCES = dldfcn/%s\n",
            basename, files[i]);
    if (cppflags[i])
      {
        printf ("dldfcn/%s.df: CPPFLAGS += %s\n",
                basename, cppflags[i]);
        printf ("dldfcn_%s_la_CPPFLAGS = $(AM_CPPFLAGS) %s\n",
                basename, cppflags[i]);
      }
    printf ("dldfcn_%s_la_LDFLAGS = -avoid-version -module $(NO_UNDEFINED_LDFLAG) %s $(OCT_LINK_OPTS)\n",
            basename, ldflags[i]);
    printf ("dldfcn_%s_la_LIBADD = $(DLD_LIBOCTINTERP_LIBADD) ../liboctave/liboctave.la %s $(OCT_LINK_DEPS)\n",
            basename, libraries[i]);
  }
}
