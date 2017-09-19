
###---------------------------------------------------------------------
### Variables
###---------------------------------------------------------------------

## Configurable:

PREFIX=/usr/local

#CCL_EXE=/data/languages/ccl/bin/ccl
CCL_EXE=ccl-1.11
CCL=$(CCL_EXE) --no-init --batch
CCL_EVAL=--eval

LISP=$(CCL)
LISP_EVAL=$(CCL_EVAL)


## Should not change that much:
ENSCRIPT_OPT= -X latin1 -TA4 -fCourier9 -FCourier-Bold12 -B -h --header="" --margins=:::12
HELP_FORMAT=$(MAKE) %-16s \# %s\n
VARI_FORMAT=%-20s = %s\n


###---------------------------------------------------------------------
### Targets:
###---------------------------------------------------------------------

all::midi-transform
	@ls -l midi-transform
help::
	@printf "$(HELP_FORMAT)" "all"  "Builds the program."
	@printf "$(HELP_FORMAT)" "help" "Prints this help."


help::
	@printf "$(HELP_FORMAT)" "midi-transform" "Generates the midi-transform executable."
midi-transform: com.informatimago.midi.transform.asd \
				convert-cc-dw8000.lisp \
				dw8000.lisp \
				generate-application.lisp \
				loader.lisp \
				parameter-map-compiler.lisp
	@printf '(push :save-image-and-quit *features*)\n(load "generate-application.lisp")\n'|$(CCL)

help::
	@printf "$(HELP_FORMAT)" "clean" "Delete the midi-transform executable."
clean:
	-@rm -rf midi-transform

help::
	@printf "$(HELP_FORMAT)" "install" "Installs the midi-transform executable in $(PREFIX)/bin."
install::midi-transform
	@install -v -m 755 midi-transform "$(PREFIX)/bin"


README.pdf:README.rst
	rst2pdf README.rst README.pdf

#### THE END ####
