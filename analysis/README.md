# Banyan file cache analysis tool

This directory contains implementation for serializing file path lists and a
scaffold for printing their metrics.

## Setup

1.  Install [Stack](https://docs.haskellstack.org/en/stable/README/).
    On macOS with Homebrew: `haskell-stack`.
2.  `stack setup`

## Run

Collect data by running the web app, and evaluating
`JSON.parse(localStorage.files).entries`
in the JavaScript console. Save this to e.g. `./data/test.txt`.

```bash
stack build --fast --exec "analyze-cache data/test.txt"
```

## Develop

Test: `stack test --fast --file-watch`

Style guides:

* [Snap](http://snapframework.com/docs/style-guide)
* [Ganett](http://docs.ganeti.org/ganeti/2.13/html/dev-codestyle.html#haskell)
