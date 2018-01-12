main.js: Main.elm Config.elm
    elm-make Main.elm Config.elm --output=main.js

.PHONY: build
build:
	@mkdir -p build
	cp index.html build
	elm make Main.elm Config.elm --output build/main.js

.PHONY: deploy
deploy: build
	netlify deploy
