.PHONY: format
format:
	stylish-haskell --inplace **/*.hs

.PHONY: lint
lint:
	hlint .

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
