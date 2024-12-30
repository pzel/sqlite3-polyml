.PHONY: polymlb test testClean clean

all:	 clean test

clean:
	rm -f bin/*

test: polymlb $(shell find | grep *.sql) testClean test/assert.sml
	polymlb -o bin/runTests ./runTests.mlb && ./bin/runTests

testClean:
	mkdir -p tmp
	rm -rf ./tmp/*

test/assert.sml:
	curl https://git.sr.ht/~pzel/assert/blob/master/assert.sml > $@

polymlb:
	command -v polymlb
