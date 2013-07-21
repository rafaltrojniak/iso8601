EBINS=-pa $(realpath .)/ebin

.PHONY : test

all: compile

compile:
	./rebar compile

test: compile dialyzer eunit

dialyzer:
	dialyzer src/*.erl

eunit: compile
	./rebar eunit

clean:
	./rebar clean

run: all
	erl ${EBINS}
