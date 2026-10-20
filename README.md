# effectful-cli

Small helpers for writing CLIs with [effectful](https://hackage.haskell.org/package/effectful):
effect constraints, concurrency, prompts, sleep/time, optparse-applicative helpers and a
[monad-logger](https://hackage.haskell.org/package/monad-logger) bridge (stderr or OpenTelemetry).

Port of the private `polysemy-cli` package to effectful.

## Build

```sh
stack build --test
```
