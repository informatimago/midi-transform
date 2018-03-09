all::documents
	for module in $(MODULES) ; do $(MAKE) $(MFLAGS) -C $${module} all ; done

clean::
	find . -name \*.dx64fsl -exec rm -f {} \;

include tools/common.make

PDFS=README.pdf

documents::$(PDFS)

README.pdf:README.rst
