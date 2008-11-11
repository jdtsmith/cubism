
# Makefile for Cubism manual.
# JDS, 4/2006

VPATH = images

# How to make a dvi file from a texinfo file
TEXI2DVI = texi2dvi

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = texi2html --split chapter -number --init-file cubism_texi2html.init

# How to create the PDF file
TEXI2PDF = texi2pdf

# Program to convert image files
CONVERT = convert -compress ZIP 

TEXIFILE  = cubism.texi
INFOFILES = cubism
PNGIMAGEFILES := $(wildcard images/*.png)
PDFIMAGEFILES = $(PNGIMAGEFILES:.png=.pdf)
EPSIMAGEFILES = $(PNGIMAGEFILES:.png=.eps)

.PHONY: all
all: info html pdf

.PHONY: html
html:   cubism.html

.PHONY: info
info:   $(INFOFILES)

.PHONY: pdf
pdf:    cubism.pdf


# Rules for producing output from texinfo source

cubism.pdf: $(TEXIFILE) $(PDFIMAGEFILES)
	$(TEXI2PDF) $<

cubism.html: $(TEXIFILE) $(PNGIMAGEFILES)
	$(TEXI2HTML) $<

$(INFOFILES): $(TEXIFILE) $(PNGIMAGEFILES)
	$(MAKEINFO) $<

# Implicit rules for converting images from PNG to EPS/PDF.

%.eps :: %.png
	$(CONVERT) $< ${<:.png=.eps}

%.pdf :: %.png
	$(CONVERT) $< ${<:.png=.pdf}

# Cleanup

clean:
	rm -f *~
	rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs
	rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps
