main.js: src/Main.elm src/Config.elm
    elm-make src/Main.elm src/Config.elm --output=main.js

.PHONY: build
build:
	@mkdir -p build
	cp src/index.html build
	elm make src/Main.elm src/Config.elm --output build/main.js

.PHONY: deploy
deploy: build
	netlify deploy
