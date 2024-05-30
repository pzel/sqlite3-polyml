.PHONY: test resultTest testClean

all:	 test

resultTest: $(shell find | grep *.sql) testClean
	polyc ./$@.sml -o $@ && ./$@


test: $(shell find | grep *.sql) testClean assert.sml
	polyc ./sqlTest.sml -o sqlTest && ./sqlTest

testClean:
	mkdir -p tmp
	rm -rf ./tmp/*

assert.sml:
	curl -O https://git.sr.ht/~pzel/assert/blob/master/assert.sml
