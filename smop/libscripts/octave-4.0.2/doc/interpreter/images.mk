## DO NOT EDIT -- generated from module-files by config-module.awk

IMAGES_SRC =
IMAGES_EPS =
IMAGES_PDF =
IMAGES_PNG =
IMAGES_TXT =
IMAGES_SRC += geometryimages.m
GEOMETRYIMAGES_EPS = voronoi.eps triplot.eps griddata.eps convhull.eps delaunay.eps inpolygon.eps
IMAGES_EPS += $(GEOMETRYIMAGES_EPS)
voronoi.eps: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('voronoi', 'eps');"
triplot.eps: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('triplot', 'eps');"
griddata.eps: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('griddata', 'eps');"
convhull.eps: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('convhull', 'eps');"
delaunay.eps: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('delaunay', 'eps');"
inpolygon.eps: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('inpolygon', 'eps');"
GEOMETRYIMAGES_PDF = voronoi.pdf triplot.pdf griddata.pdf convhull.pdf delaunay.pdf inpolygon.pdf
IMAGES_PDF += $(GEOMETRYIMAGES_PDF)
voronoi.pdf: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('voronoi', 'pdf');"
triplot.pdf: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('triplot', 'pdf');"
griddata.pdf: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('griddata', 'pdf');"
convhull.pdf: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('convhull', 'pdf');"
delaunay.pdf: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('delaunay', 'pdf');"
inpolygon.pdf: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('inpolygon', 'pdf');"
GEOMETRYIMAGES_PNG = voronoi.png triplot.png griddata.png convhull.png delaunay.png inpolygon.png
IMAGES_PNG += $(GEOMETRYIMAGES_PNG)
voronoi.png: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('voronoi', 'png');"
triplot.png: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('triplot', 'png');"
griddata.png: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('griddata', 'png');"
convhull.png: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('convhull', 'png');"
delaunay.png: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('delaunay', 'png');"
inpolygon.png: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('inpolygon', 'png');"
GEOMETRYIMAGES_TXT = voronoi.txt triplot.txt griddata.txt convhull.txt delaunay.txt inpolygon.txt
IMAGES_TXT += $(GEOMETRYIMAGES_TXT)
voronoi.txt: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('voronoi', 'txt');"
triplot.txt: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('triplot', 'txt');"
griddata.txt: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('griddata', 'txt');"
convhull.txt: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('convhull', 'txt');"
delaunay.txt: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('delaunay', 'txt');"
inpolygon.txt: geometryimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "geometryimages ('inpolygon', 'txt');"
IMAGES_SRC += interpimages.m
INTERPIMAGES_EPS = interpft.eps interpn.eps interpderiv1.eps interpderiv2.eps
IMAGES_EPS += $(INTERPIMAGES_EPS)
interpft.eps: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpft', 'eps');"
interpn.eps: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpn', 'eps');"
interpderiv1.eps: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpderiv1', 'eps');"
interpderiv2.eps: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpderiv2', 'eps');"
INTERPIMAGES_PDF = interpft.pdf interpn.pdf interpderiv1.pdf interpderiv2.pdf
IMAGES_PDF += $(INTERPIMAGES_PDF)
interpft.pdf: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpft', 'pdf');"
interpn.pdf: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpn', 'pdf');"
interpderiv1.pdf: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpderiv1', 'pdf');"
interpderiv2.pdf: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpderiv2', 'pdf');"
INTERPIMAGES_PNG = interpft.png interpn.png interpderiv1.png interpderiv2.png
IMAGES_PNG += $(INTERPIMAGES_PNG)
interpft.png: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpft', 'png');"
interpn.png: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpn', 'png');"
interpderiv1.png: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpderiv1', 'png');"
interpderiv2.png: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpderiv2', 'png');"
INTERPIMAGES_TXT = interpft.txt interpn.txt interpderiv1.txt interpderiv2.txt
IMAGES_TXT += $(INTERPIMAGES_TXT)
interpft.txt: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpft', 'txt');"
interpn.txt: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpn', 'txt');"
interpderiv1.txt: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpderiv1', 'txt');"
interpderiv2.txt: interpimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "interpimages ('interpderiv2', 'txt');"
IMAGES_SRC += plotimages.m
PLOTIMAGES_EPS = plot.eps hist.eps errorbar.eps polar.eps mesh.eps plot3.eps extended.eps
IMAGES_EPS += $(PLOTIMAGES_EPS)
plot.eps: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('plot', 'eps');"
hist.eps: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('hist', 'eps');"
errorbar.eps: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('errorbar', 'eps');"
polar.eps: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('polar', 'eps');"
mesh.eps: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('mesh', 'eps');"
plot3.eps: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('plot3', 'eps');"
extended.eps: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('extended', 'eps');"
PLOTIMAGES_PDF = plot.pdf hist.pdf errorbar.pdf polar.pdf mesh.pdf plot3.pdf extended.pdf
IMAGES_PDF += $(PLOTIMAGES_PDF)
plot.pdf: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('plot', 'pdf');"
hist.pdf: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('hist', 'pdf');"
errorbar.pdf: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('errorbar', 'pdf');"
polar.pdf: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('polar', 'pdf');"
mesh.pdf: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('mesh', 'pdf');"
plot3.pdf: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('plot3', 'pdf');"
extended.pdf: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('extended', 'pdf');"
PLOTIMAGES_PNG = plot.png hist.png errorbar.png polar.png mesh.png plot3.png extended.png
IMAGES_PNG += $(PLOTIMAGES_PNG)
plot.png: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('plot', 'png');"
hist.png: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('hist', 'png');"
errorbar.png: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('errorbar', 'png');"
polar.png: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('polar', 'png');"
mesh.png: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('mesh', 'png');"
plot3.png: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('plot3', 'png');"
extended.png: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('extended', 'png');"
PLOTIMAGES_TXT = plot.txt hist.txt errorbar.txt polar.txt mesh.txt plot3.txt extended.txt
IMAGES_TXT += $(PLOTIMAGES_TXT)
plot.txt: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('plot', 'txt');"
hist.txt: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('hist', 'txt');"
errorbar.txt: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('errorbar', 'txt');"
polar.txt: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('polar', 'txt');"
mesh.txt: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('mesh', 'txt');"
plot3.txt: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('plot3', 'txt');"
extended.txt: plotimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "plotimages ('extended', 'txt');"
IMAGES_SRC += sparseimages.m
SPARSEIMAGES_EPS = gplot.eps grid.eps spmatrix.eps spchol.eps spcholperm.eps
IMAGES_EPS += $(SPARSEIMAGES_EPS)
gplot.eps: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('gplot', 'eps');"
grid.eps: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('grid', 'eps');"
spmatrix.eps: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spmatrix', 'eps');"
spchol.eps: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spchol', 'eps');"
spcholperm.eps: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spcholperm', 'eps');"
SPARSEIMAGES_PDF = gplot.pdf grid.pdf spmatrix.pdf spchol.pdf spcholperm.pdf
IMAGES_PDF += $(SPARSEIMAGES_PDF)
gplot.pdf: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('gplot', 'pdf');"
grid.pdf: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('grid', 'pdf');"
spmatrix.pdf: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spmatrix', 'pdf');"
spchol.pdf: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spchol', 'pdf');"
spcholperm.pdf: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spcholperm', 'pdf');"
SPARSEIMAGES_PNG = gplot.png grid.png spmatrix.png spchol.png spcholperm.png
IMAGES_PNG += $(SPARSEIMAGES_PNG)
gplot.png: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('gplot', 'png');"
grid.png: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('grid', 'png');"
spmatrix.png: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spmatrix', 'png');"
spchol.png: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spchol', 'png');"
spcholperm.png: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spcholperm', 'png');"
SPARSEIMAGES_TXT = gplot.txt grid.txt spmatrix.txt spchol.txt spcholperm.txt
IMAGES_TXT += $(SPARSEIMAGES_TXT)
gplot.txt: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('gplot', 'txt');"
grid.txt: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('grid', 'txt');"
spmatrix.txt: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spmatrix', 'txt');"
spchol.txt: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spchol', 'txt');"
spcholperm.txt: sparseimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "sparseimages ('spcholperm', 'txt');"
IMAGES_SRC += splineimages.m
SPLINEIMAGES_EPS = splinefit1.eps splinefit2.eps splinefit3.eps splinefit4.eps splinefit6.eps
IMAGES_EPS += $(SPLINEIMAGES_EPS)
splinefit1.eps: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit1', 'eps');"
splinefit2.eps: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit2', 'eps');"
splinefit3.eps: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit3', 'eps');"
splinefit4.eps: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit4', 'eps');"
splinefit6.eps: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit6', 'eps');"
SPLINEIMAGES_PDF = splinefit1.pdf splinefit2.pdf splinefit3.pdf splinefit4.pdf splinefit6.pdf
IMAGES_PDF += $(SPLINEIMAGES_PDF)
splinefit1.pdf: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit1', 'pdf');"
splinefit2.pdf: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit2', 'pdf');"
splinefit3.pdf: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit3', 'pdf');"
splinefit4.pdf: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit4', 'pdf');"
splinefit6.pdf: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit6', 'pdf');"
SPLINEIMAGES_PNG = splinefit1.png splinefit2.png splinefit3.png splinefit4.png splinefit6.png
IMAGES_PNG += $(SPLINEIMAGES_PNG)
splinefit1.png: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit1', 'png');"
splinefit2.png: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit2', 'png');"
splinefit3.png: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit3', 'png');"
splinefit4.png: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit4', 'png');"
splinefit6.png: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit6', 'png');"
SPLINEIMAGES_TXT = splinefit1.txt splinefit2.txt splinefit3.txt splinefit4.txt splinefit6.txt
IMAGES_TXT += $(SPLINEIMAGES_TXT)
splinefit1.txt: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit1', 'txt');"
splinefit2.txt: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit2', 'txt');"
splinefit3.txt: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit3', 'txt');"
splinefit4.txt: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit4', 'txt');"
splinefit6.txt: splineimages.m
	$(AM_V_GEN)$(top_builddir)/run-octave -f -q -H -p $(srcdir) --eval "splineimages ('splinefit6', 'txt');"
