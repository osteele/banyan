# Banyan file cache analysis tool

This directory contains Haskell code to decode and re-encode Banyan's path list
cache.

It's for experimenting the path list cache encoding that Banyan uses with
`localStorage`; also, mainly, for me to brush up on Haskell and tooling.

## Setup

1.  Install [Stack](https://docs.haskellstack.org/en/stable/README/).
    On macOS with Homebrew: `haskell-stack`.
2.  `stack setup`

## Run

```bash
stack build --fast --exec "analyze-cache data/test.txt"
```

There's also a `make run` target. This assumes that you've captured a copy of
the cache from running the app:

1.  Run the web app.
2.  Evaluate `JSON.parse(localStorage.files).entries` in the JavaScript
    console.
3.  Save the value of the printed string (not including the initial and final
    quote character '`"`') to `./data/personal.txt`.
4.  `make run`

## Develop

Test: `stack test --fast --file-watch`

Style guides:

* [Snap](http://snapframework.com/docs/style-guide)
* [Ganett](http://docs.ganeti.org/ganeti/2.13/html/dev-codestyle.html#haskell)
