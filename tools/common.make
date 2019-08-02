# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t -*-

ifeq ($(shell grep -q -s informatimago /etc/resolv.conf && echo yes || echo no),yes)
LISP=ccl
LISP_FLAGS=--no-init
LISP_LOAD_OPTION=--load
LISP_EVAL_OPTION=--eval
#PDF_VIEWER=/opt/local/bin/xpdf
PDF_VIEWER=open
WGET=wget
WGET_OPTIONS=
SED=gsed
else
LISP=/usr/local/bin/sbcl
LISP_FLAGS=--no-sysinit --no-userinit
LISP_LOAD_OPTION=--load
LISP_LOAD_OPTION=--eval
PDF_VIEWER=/usr/bin/xpdf
WGET=/usr/bin/wget
WGET_OPTIONS=
SED=sed
endif

TOP=$(shell git rev-parse --show-toplevel)
MAKE_RST_DEPENDS=$(TOP)/tools/make-rst-depends
RSTPRE=$(TOP)/tools/rstpre
RSTUML=$(TOP)/tools/rstuml


HELPFMT="$(shell basename $(MAKE)) %-20s \# %s\n"

.PHONY:: all
all::documents

.PHONY:: view
help::
	@printf $(HELPFMT) "view" "Open the PDF files in a viewer."
view:$(PDFS)
	@if [ $(PDF_VIEWER) = open ] ; then \
		$(PDF_VIEWER) $^ ;\
	 else \
		for i in $^ ; do \
			$(PDF_VIEWER) $$i & \
		done ;\
	 fi

DOC_MODULES ?= specifications documentation architecture design
MODULES     ?= $(DOC_MODULES) sources

.PHONY:: project-directories
help::
	@printf $(HELPFMT) "project-directories" "Creates the toplevel project directories."
project-directories::
	mkdir -p analysis architecture bin design documentation resources reviews sources specifications tests tools
	for dir in $(DOC_MODULES) ; do if [ ! -r "$$dir/Makefile" ] ; then ( echo 'DOC_MODULES =' ; echo 'MODULES =' ; echo 'include ../tools/common.make' ) > "$$dir/Makefile" ; fi ; done


.PHONY:: documents
help::
	@printf $(HELPFMT) "documents" "Build all the pdf documents."
documents::
	for module in $(DOC_MODULES) ; do $(MAKE) $(MFLAGS) -C $${module} all ; done


.PHONY:: clean clean-pdf clean-pdfs
help::
	@printf $(HELPFMT) "clean" "Delete spurious files."
clean::clean-pdf
	for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -C $${module} clean ; done
clean-pdf clean-pdfs:
	-rm -f $(PDFS)


.PHONY::  checkout-pdf checkout-pdfs
checkout-pdf checkout-pdfs:clean-pdf
	git checkout $(PDFS)


# Generate the dependencies of restructuredText documents (includes
# and images).  Note: this should be done better by a script, since
# included files may themselves include other files or images.
%.d:%.txt
	$(MAKE_RST_DEPENDS) $< > $@

# Generate PDF from org-mode document.
%.pdf:%.org
	-rm -f $@
	emacs --batch \
		--eval '(find-file "'$<'")' \
		--funcall org-latex-export-to-pdf \
		--kill

# Generate PDF from reStructuredText document.
%.pdf:%.txt
	rst2pdf -o $@ $<

%.pdf:%.rst
	rst2pdf -o $@ $<

# Object diagrams = Class diagrams.
o-%.png:o-%.yuml
	$(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/class/$$(tr '\012' ',' < $< |sed -e 's/,*$$//')" -O $@

# State diagrams = Activity diagrams.
s-%.png:s-%.yuml
	$(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/activity/$$(tr '\012' ',' < $< |sed -e 's/,*$$//')" -O $@

# Use-case diagrams.
u-%.png:u-%.yuml
	$(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/usecase/$$(tr '\012' ',' < $< |sed -e 's/,*$$//')" -O $@




## Example, using rstpre and rstuml:
##
# .SUFFIXES: .in1 .txt .rst .pdf .xml .odt .html
#
# # All the preprocessing of the documents into a rst file should go here:
# %.rst:%.txt
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Preparing $$i" ;\
# 	cd "$$d" \
# 	&& $(RSTPRE) "$$f" \
# 	&& mv "$${f/.txt/.rst}"  "$${f/.txt/.in1}" \
# 	&& echo "Processing UML in $${i/.txt/.in1}" \
# 	&& $(RSTUML) "$${f/.txt/.in1}"
#
# # Following are the rules to process rst files into the target formats:
# %.pdf:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.pdf}" ;\
# 	cd "$$d" \
# 	&& $(RST2PDF)  $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} )  -o "$${f/.rst/.pdf}"  "$$f"
# # --verbose --very-verbose
#
# %.odt:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.odt}" ;\
# 	cd "$$d" \
# 	&& $(RST2ODT) $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} ) "$$f" "$${f/.rst/.odt}"
#
# %.html:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.html}" ;\
# 	cd "$$d" \
# 	@f="$<" \
# 	&& $(RST2HTML) $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} ) "$$f" "$${f/.rst/.html}"
