all: compile

compile: src/*.erl
	erlc -o beam $?
