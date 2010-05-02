SOURCES = main.erl utils.erl
BEAMS = $(SOURCES:.erl=.beam)

.SUFFIXES: .erl .beam

.erl.beam:
	erlc $<

all: $(BEAMS)

run: all
	erl -run main

clean:
	rm -f $(BEAMS)
