# AGENTS

Haskell library (stack + hpack). Edit `package.yaml`, never generated `.cabal`.

## Commands

- build and test (GHC 9.10): `stack build --test --ghc-options=-Werror`
- build and test (GHC 9.12): add `--stack-yaml stack-ghc-9.12.yaml`
- format: `fourmolu -i src test` (fourmolu 0.21.0.0, config in `fourmolu.yaml`)
- lint: `hlint src test` (rules in `.hlint.yaml`; HLS has no hlint plugin on GHC 9.10)

CI (`.github/workflows/ci.yml`) runs all of the above; keep it green.

## Style

- `fmap` over `map`
- curried constraints: `Read a => Ord a => a`, not `(Read a, Ord a) => a`;
  not enforced by tooling; constraint aliases such as `CLIEffects` stay tuples
- `-Wredundant-constraints` on: only add `Show e` for `throwError` where needed

## Dependencies

- public Hackage only: no git or private `extra-deps`.
- `http-client-effectful-1.0.0` caps `effectful < 2.7`; GHC 9.12 nightly config
  relaxes bounds with `allow-newer-deps` instead of pinning older
- `package.yaml` version bounds span the versions both CI configs resolve; widen them
  when a snapshot moves
