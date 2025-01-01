.PHONY: polymlb test testClean clean
LIBDIR := lib/github.com/pzel/sqlite3-polyml
MLB_PATH := -mlb-path-var 'FOO $(shell pwd)/lib'

all:	 clean test

clean:
	rm -f bin/* tmp/*

test: polymlb $(shell find $(LIBDIR) | grep *.sql)
	polymlb $(MLB_PATH) -output bin/runTests $(LIBDIR)/test/runTests.mlb && ./bin/runTests

