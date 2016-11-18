FCN_FILE_DIRS += java

java_FCN_FILES = \
  java/java_get.m \
  java/java_set.m \
  java/javaArray.m \
  java/javaaddpath.m \
  java/javachk.m \
  java/javaclasspath.m \
  java/javamem.m \
  java/javarmpath.m \
  java/usejava.m

FCN_FILES += $(java_FCN_FILES)

PKG_ADD_FILES += java/PKG_ADD

DIRSTAMP_FILES += java/$(octave_dirstamp)

if AMCOND_HAVE_JAVA
JAR_FILES += java/octave.jar
endif

org_octave_dir = org/octave

if AMCOND_HAVE_JAVA
JAVA_SRC = \
  $(org_octave_dir)/ClassHelper.java \
  $(org_octave_dir)/OctClassLoader.java \
  $(org_octave_dir)/Octave.java \
  $(org_octave_dir)/OctaveReference.java \
  $(org_octave_dir)/Matrix.java \
  $(org_octave_dir)/JDialogBox.java \
  $(org_octave_dir)/DlgListener.java \
  $(org_octave_dir)/TeXtranslator.java \
  $(org_octave_dir)/TeXcode.java
endif

JAVA_CLASSES = $(JAVA_SRC:.java=.class)

JAVA_IMAGES = \
  $(org_octave_dir)/images/question.png \
  $(org_octave_dir)/images/error.png \
  $(org_octave_dir)/images/warning.png \
  $(org_octave_dir)/images/information.png \
  $(org_octave_dir)/images/octave.png

java_JAVA_SRC = $(addprefix java/, $(JAVA_SRC))

java_JAVA_CLASSES = $(addprefix java/, $(JAVA_CLASSES))

java_JAVA_IMAGES = $(addprefix java/, $(JAVA_IMAGES))

srcdir_java_JAVA_IMAGES = $(addprefix $(srcdir)/java/, $(JAVA_IMAGES))

%.class : %.java
	$(AM_V_GEN)$(MKDIR_P) java/$(org_octave_dir) && \
	( cd $(srcdir)/java; "$(JAVAC)" -source 1.3 -target 1.3 -d $(abs_builddir)/java $(org_octave_dir)/$(<F) )

java/images.stamp: $(srcdir_java_JAVA_IMAGES)
	$(AM_V_GEN)if [ "x$(srcdir)" != "x." ]; then \
	  $(MKDIR_P) java/$(org_octave_dir)/images; \
	  cp $(srcdir_java_JAVA_IMAGES) java/$(org_octave_dir)/images; \
	fi && \
	touch $@

if AMCOND_HAVE_JAVA
java/octave.jar: java/images.stamp $(java_JAVA_CLASSES)
	$(AM_V_GEN)rm -f $@-t $@ && \
	( cd java; \
	  "$(JAR)" cf octave.jar-t $(JAVA_CLASSES) $(JAVA_IMAGES) ) && \
	mv $@-t $@
endif

EXTRA_DIST += $(JAR_FILES) $(java_JAVA_SRC) $(java_JAVA_IMAGES)

CLEANFILES += $(JAR_FILES) $(java_JAVA_CLASSES)

DISTCLEANFILES += java/images.stamp

