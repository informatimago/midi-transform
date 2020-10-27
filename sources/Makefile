
###---------------------------------------------------------------------
### Variables
###---------------------------------------------------------------------

## Configurable:

PREFIX=/usr/local

#CCL_EXE=/data/languages/ccl/bin/ccl
CCL_EXE=ccl
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

all::application
application:midi-transform
	@ls -l midi-transform
help::
	@printf "$(HELP_FORMAT)" "all"          "Builds the program."
	@printf "$(HELP_FORMAT)" "application"  "Builds the program."
	@printf "$(HELP_FORMAT)" "help" "Prints this help."

help::
	@printf "$(HELP_FORMAT)" "midi-transform" "Generates the midi-transform executable."
midi-transform: com.informatimago.midi.transform.asd \
				generate-application.lisp \
				loader.lisp \
				\
				convert-cc-dw-8000.lisp \
				korg-dss-1.lisp \
				korg-dw-8000.lisp \
				korg.lisp \
				midi-application.lisp \
				parameter-map-compiler.lisp \
				synthesizer.lisp
	@printf '(push :save-image-and-quit *features*)\n(load "generate-application.lisp")\n'|$(CCL)

help::
	@printf "$(HELP_FORMAT)" "install" "Installs the midi-transform executable in $(PREFIX)/bin."
install::midi-transform
	@install -v -m 755 midi-transform "$(PREFIX)/bin"

help::
	@printf "$(HELP_FORMAT)" "clean" "Delete the midi-transform executable."
clean:
	-@rm -rf midi-transform *.bak

doc::documentation
documentation:: documentation/vi61-dw8000-ch1.vi6 README.pdf

documentation/vi61-dw8000-ch1.vi6:documentation/vi61-dw8000.vi6
	vi61-generate-all-channels documentation/vi61-dw8000.vi6

README.pdf:README.rst
	rst2pdf README.rst README.pdf

#### THE END ####
