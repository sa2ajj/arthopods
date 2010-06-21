all:
	./rebar compile

clean:
	./rebar clean

run: all
	erl -pa ebin/ -noshell -s main -s init stop

run_app: all
	erl -pa ebin/ -noshell -s arthopods_app -s init stop
