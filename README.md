# crypto-publicdata

Real-time multi-exchange market data service written in OCaml. Connects
to crypto exchanges in public-data-only mode (no API keys), aggregates
per-symbol ticker data into a single in-memory cache, stores realtime
kline (OHLCV) history for a curated subset of venues, and re-broadcasts
everything over a WebSocket + REST API at the root path.

Status: early scaffolding. The architecture and wire protocol are
described in [`CLAUDE.md`](./CLAUDE.md); code is still landing.

## Prerequisites

- macOS or Linux
- A recent OCaml toolchain (5.x) installed via [opam](https://opam.ocaml.org/)

### Install opam

macOS (Homebrew):

```bash
brew install opam
```

Linux (Debian/Ubuntu):

```bash
sudo apt-get install opam
```

Or follow the [official installer](https://opam.ocaml.org/doc/Install.html).

### Initialize opam (first time only)

```bash
opam init --bare -a -y
```

## Set up the project switch

From the repo root, create a local switch pinned to OCaml 5.2 and install
the dev tools:

```bash
opam switch create . 5.2.0 --deps-only --yes
eval "$(opam env --switch=. --set-switch)"

opam install --yes \
  dune \
  ocaml-lsp-server \
  ocamlformat \
  utop
```

Add the `eval` line to your shell profile (or use [direnv](https://direnv.net/)
with `use opam`) so the switch activates automatically when you `cd` into
the repo.

## Common commands (once the project is scaffolded)

```bash
dune build            # build all
dune runtest          # run tests
dune exec bin/main.exe # run the service
dune fmt              # format via ocamlformat
dune build @check     # type-check without linking
```

Dependencies for runtime libraries (HTTP server, WebSocket client, JSON,
async runtime, storage backend, ...) will be pinned in the project's
`*.opam` files as they land. Re-run `opam install --deps-only --with-test .`
after pulling to stay in sync.
