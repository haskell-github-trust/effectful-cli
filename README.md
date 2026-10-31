# effectful-cli

Small helpers for writing CLIs with [effectful](https://hackage.haskell.org/package/effectful):
effect constraints, concurrency, prompts, sleep/time, optparse-applicative helpers and a
[monad-logger](https://hackage.haskell.org/package/monad-logger) bridge (stderr or OpenTelemetry).

## Build

```sh
stack build --test
```

## Release

Requires [just](https://just.systems) and a Hackage token in `HACKAGE_KEY`.

```sh
just docs       # build Haddock tarball
just candidate  # upload package + docs as Hackage candidate
just release    # publish package + docs to Hackage
```
