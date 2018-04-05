## Setup

1.  Install stack.
2.  `stack setup`

## Test

`stack test --fast --file-watch`

## Run

Collect data by running the web app, and evaluating
`JSON.parse(localStorage.files).entries`
in the JavaScript console. Save this to e.g. `./data/test.txt`.

```bash
stack build --fast --file-watch --exec "analyze-cache data/test.txt"
```
