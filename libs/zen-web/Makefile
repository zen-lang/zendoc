.PHONY: test

init:
	git submodule init && git submodule update

repl: init
	clj -M:nrepl:test

test: init
	clj -A:test:kaocha

test-ci: init
	clojure -A:test:kaocha
