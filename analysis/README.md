## Setup

1.  Install stack.
2.  `stack setup`

## Test

`stack test --fast --file-watch`

## Run

```bash
stack build --fast --file-watch --exec "analyze-cache data/test.txt"
```
