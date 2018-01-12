.PHONY: build
build:
    elm-make Main.elm Config.elm --output=main.js
