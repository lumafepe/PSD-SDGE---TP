build:
	gpb/bin/protoc-erl -I. -maps -o src messages.proto
	erlc -I gpb/include -o ebin src/messages.erl
	erlc -o ebin src/*.erl