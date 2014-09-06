#DOTFILES = $(wildcard *.dot)
#PDFFILES = $(subst .dot .pdf)
#
#$(PDFFILES):

check: audio deprecated elfun general geometry help image
	python runtime.py -v
	python test_runtime.py -v
	python test_matlabarray.py -v
	python test_sparsearray.py -v
	python test_lexer.py -v
	python main.py solver.m && python go.py | wc
	python main.py fastsolver.m && python a.py
	#python main.py -X print_usage.m m/*/*.m -v | grep -i error 
	python a.py
	#python test_parse.py
audio:
	python main.py m/audio/*.m -v -s
deprecated:
	python main.py m/deprecated/*.m -v -s
elfun:
	python main.py m/elfun/*.m -v -s
general:
	python main.py m/general/*.m -v -s
geometry:
	python main.py m/geometry/*.m -v -s
help:
	python main.py -X print_usage.m m/help/*.m -v -s
image:
	python main.py m/image/*.m -v -s -X imwrite.m
#    image
#    io
#    linear-algebra
#    miscellaneous
#    optimization
#    path
#    pkg
#    plot
#    polynomial
#    prefs
#    set
#    signal
#    sparse
#    specfun
#    special-matrix
#    startup
#    statistics
#    strings
#    testfun
#    time

#echo $^
#
#%.pdf : %.dot
#	dot -Tpdf -o $@ $<

