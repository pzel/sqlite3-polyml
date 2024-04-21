.PHONY: test resultTest testClean

all:	 test


test: $(shell find | grep *.sql) testClean
	polyc ./sqlTest.sml -o sqlTest && ./sqlTest

resultTest: $(shell find | grep *.sql) testClean
	polyc ./resultTest.sml -o resultTest && ./resultTest

testClean:
	mkdir -p tmp
	rm -rf ./tmp/*

