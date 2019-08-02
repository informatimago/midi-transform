all::documents
	for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -C $${module} $@ ; done

install::
	for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -C $${module} $@ ; done

clean::
	for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -C $${module} $@ ; done

include tools/common.make

PDFS=README.pdf

documents::$(PDFS)

README.pdf:README.org


