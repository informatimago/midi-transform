all:print-constants
run:print-constants
	./print-constants
print-constants:print-constants.m
	$(CC) -g3 -ggdb -o print-constants print-constants.m  -framework CoreMIDI -framework Foundation -lobjc
	otool -L print-constants
clean:
	-rm -f print-constants
