all: compile

compile: src/*.erl
	mkdir -p beam
	erlc -o beam $?
