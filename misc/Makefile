SOURCES = my_server.erl
BEAMS = $(SOURCES:.erl=.beam)

.SUFFIXES: .erl .beam

.erl.beam:
	erlc -Werror $<

all: $(BEAMS)

run: all
	#erl -noshell -s main -s init stop

clean:
	rm -f $(BEAMS) erl_crash.dump
