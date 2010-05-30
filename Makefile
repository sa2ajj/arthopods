SOURCES = main.erl world.erl food_generator.erl utils.erl grass_field.erl select.erl world_viewer.erl arthopod.erl arthopod_simple.erl
BEAMS = $(SOURCES:.erl=.beam)

.SUFFIXES: .erl .beam

.erl.beam:
	erlc -pa . -Werror $<

all: $(BEAMS)

run: all
	erl -noshell -s main -s init stop

clean:
	rm -f $(BEAMS) erl_crash.dump

arthopod_*.beam: arthopod.beam
