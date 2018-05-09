all: CoreMIDI com-informatimago midi
CoreMIDI:
	git clone git@framagit.org:patchwork/CoreMIDI.git
com-informatimago:
	git clone git@git.framasoft.org:com-informatimago/com-informatimago.git
midi:
	git clone git@git.framasoft.org:abnotation/midi.git
update:all
	cd CoreMidi          ; git pull
	cd com-informatimago ; git pull
	cd midi              ; git pull
help:
	@printf '%s %30s # %s\n' make all     'Fetches (clone) all dependencies.'
	@printf '%s %30s # %s\n' make update  'Updates (pull)  all dependencies.'
