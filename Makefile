SOURCES = main.erl utils.erl
BEAMS = $(SOURCES:.erl=.beam)

.SUFFIXES: .erl .beam

.erl.beam:
	erlc $<

all: $(BEAMS)

run: all
	erl -noshell -s main -s init stop

clean:
	rm -f $(BEAMS)
