.PHONY: format
format:
	stylish-haskell --inplace **/*.hs

.PHONY: lint
lint:
	hlint .

.PHONY: profile
profile:
	stack clean
	stack build --profile
	stack exec -- analyze-cache data/personal.txt +RTS -p

.PHONY: test
test:
	stack test --fast

.PHONY: test-watch
test-watch:
	stack test --fast --file-watch

.PHONY: run
run:
	export LANG=C
	stack build --fast --exec "analyze-cache --allow-dots data/personal.txt"
