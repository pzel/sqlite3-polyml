.PHONY: test testClean


test: $(shell find | grep *.sql) testClean
	polyc ./sqlTest.sml -o sqlTest && ./sqlTest

testClean:
	mkdir -p tmp
	rm -rf ./tmp/*

