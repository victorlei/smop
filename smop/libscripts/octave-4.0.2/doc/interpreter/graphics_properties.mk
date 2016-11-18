GRAPH_PROP_TEXI_SRC= \
  plot-axesproperties.texi \
  plot-figureproperties.texi \
  plot-imageproperties.texi \
  plot-lineproperties.texi \
  plot-patchproperties.texi \
  plot-rootproperties.texi \
  plot-surfaceproperties.texi \
  plot-textproperties.texi

define gen-propdoc-texi
  rm -f $@-t $@ && \
  $(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "genpropdoc ('$(1)');" > $@-t && \
  mv $@-t $@
endef

plot-axesproperties.texi: genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,axes)

plot-figureproperties.texi: genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,figure)

plot-imageproperties.texi: genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,image)

plot-lineproperties.texi: genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,line)

plot-patchproperties.texi: genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,patch)

plot-rootproperties.texi: genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,root)

plot-surfaceproperties.texi: genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,surface)

plot-textproperties.texi: genpropdoc.m
	$(AM_V_GEN)$(call gen-propdoc-texi,text)
