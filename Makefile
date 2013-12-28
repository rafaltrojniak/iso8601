EBINS=-pa $(realpath .)/ebin
LEX_FILES=$(wildcard src/*.xrl)
YEC_FILES=$(wildcard src/*.yrl)
COMPILE_FILES=$(patsubst %.xrl,%.erl,$(LEX_FILES)) $(patsubst %.yrl,%.erl,$(YEC_FILES))
NOT_COMPILED_FILES=$(foreach file,$(wildcard src/*.erl), $(if $(findstring $(file) ,$(COMPILE_FILES) ),,$(file)))

.PHONY : test

all: compile


compile: $(COMPILE_FILES)
	./rebar compile

%.erl : %.xrl
	erl -noshell -s leex file $(patsubst %.erl,%,$@) -s erlang halt

%.erl : %.yrl
	erl -noshell -s yecc file $(patsubst %.erl,%,$@) -s erlang halt

test: compile dialyzer eunit

dialyzer:
	dialyzer $(NOT_COMPILED_FILES)

init_dialyzer:
	dialyzer --build_plt --apps erts kernel stdlib mnesia

eunit: compile
	./rebar eunit skip_deps=true

clean:
	$(RM) $(COMPILE_FILES) src/*~
	./rebar clean

run: all
	erl ${EBINS}

edoc:
	./rebar doc
