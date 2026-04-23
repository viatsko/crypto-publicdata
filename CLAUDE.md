# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

`crypto-publicdata` is an OCaml service that connects to many crypto exchanges in **public-data-only mode** (no API keys), aggregates per-symbol ticker data into a single in-memory cache, and re-broadcasts it over a WebSocket + REST API with a compact wire format.

> **State**: scaffold stage. Code layout and toolchain choices below are the intended shape — verify against the repo before citing anything as fact, and update this file as decisions land.

## Intended toolchain

OCaml + dune. Expected commands once scaffolded:

```bash
opam install --deps-only --with-test .   # install deps
dune build                                # build all
dune runtest                              # run tests
dune exec bin/main.exe                    # run service
dune fmt                                  # format (ocamlformat)
dune build @check                         # type-check without linking
```

Async runtime is not yet chosen — Eio and Lwt are both plausible. Whichever lands, the expectation is a single event loop with per-exchange fibers/promises for WS connections plus a tick loop for delta emission. Record the decision here when it's made.

## Architecture (big picture)

The service is a **fan-in → cache → fan-out** pipeline:

1. **Exchange adapters** (one per venue) each open upstream WebSocket(s), normalize frames into a common `Ticker` record, and push updates into the shared aggregator. Adapters must **dedupe no-op pushes at the source** — upstream feeds re-broadcast unchanged values many times per second, and every un-deduped push still costs a cache write and a delta pass downstream. Pattern: per-symbol "last pushed tuple" map, skip if unchanged.
2. **Aggregator** holds the authoritative `exchange → symbol → Ticker` map plus a per-client **shadow** copy used for field-level diffing. Every `rate`-tick, for each subscribed client it diffs current state against that client's shadow and emits only the fields (and symbols) that changed. Empty ticks emit nothing. Removed symbols go in a separate `removed` map.
3. **Server** exposes a WebSocket endpoint (snapshot on subscribe, then deltas on every tick) and REST endpoints for the same data. REST responses use the **same compact single-letter wire format** as WS.

The compact wire format and the opt-in flag machinery (`includeIndex`, `includeTps30s`, `includeTps60s`) are part of the external contract — consumers depend on them. See "External protocol" below.

## External protocol

Clients subscribe with:

```jsonc
{ "op": "subscribe", "channel": "tickers", "rate": 1.0,
  "exchanges": ["binance", "bybit"],  // optional; omit for all
  "includeIndex": false, "includeTps30s": false, "includeTps60s": false }
```

Server → client frames: `connected` (once after upgrade), `snapshot` (once after subscribe), `delta` (per tick, changed fields only), `pong`.

Compact ticker keys on the wire (all numeric, zero values stripped):

| Field       | Key  | Notes |
| ----------- | ---- | ----- |
| bid         | `b`  | |
| ask         | `a`  | |
| last        | `l`  | |
| mark        | `m`  | |
| index       | `i`  | opt-in via `includeIndex` |
| percentage  | `p`  | 24h % change |
| openInterest| `o`  | |
| fundingRate | `f`  | fractional, not quote-per-contract |
| fundingTime | `t`  | unix ms |
| volume      | `v`  | 24h base |
| quoteVolume | `q`  | 24h quote |
| tps (10s)   | `n`  | always on |
| tps (30s)   | `n3` | opt-in via `includeTps30s` |
| tps (60s)   | `n6` | opt-in via `includeTps60s` |

Clients must treat an absent field in a delta as "unchanged" — not zero. The exception is the *first* delta for a newly-listed symbol, which is sent fully populated so the client can insert a row.

Opt-in flags are not just payload-size optimization — `includeIndex = false` also makes the server zero that field on the client's shadow and skip the index comparison entirely in delta computation. Same for `n3` / `n6`. Make sure new opt-in fields are gated the same way or they'll show up on the wire for clients who never asked.

## Trades per second (canonical-base pipeline)

`n` / `n3` / `n6` are **not** per-exchange per-symbol trade rates. They are **per-canonical-base-asset** rolling averages, sourced from a small fixed set of high-quality trade feeds (intended: Binance USDT-M futures, Bybit linear, Hyperliquid perp), and stamped onto every ticker on every enabled exchange whose symbol reduces to the same base. So `binance.BTCUSDT`, `bybit.BTCPERP`, `kraken.PF_XBTUSD`, and `hyperliquid.BTC` all carry the same BTC values.

Windows:

| Field | Window | Purpose |
| ----- | ------ | ------- |
| `n`   | 10s    | reactive — spike detection |
| `n3`  | 30s    | momentum confirmation |
| `n6`  | 60s    | baseline / context |

The relationship between windows is the actionable signal: `n >> n6` = surge, `n ≈ n6` = steady, `n << n6` = fading.

Pipeline shape (to be implemented):

1. **Source feeds** — each source exchange's adapter opens a *second* WebSocket dedicated to trades, subscribes per-symbol, and on each trade event increments the current second's per-base counter in a shared `Tps_aggregator`. Trade-count semantics per source: Binance `aggTrade` → `last_trade_id - first_trade_id + 1`; Bybit `publicTrade.<symbol>` → array length; Hyperliquid `trades` → array length.
2. **Windowing** — `Tps_aggregator` holds a ring of per-second buckets (length = 61, one more than the max window). Every second, a tick pushes a fresh in-progress bucket, drops the oldest if overfull, and recomputes rolling averages `{ten_sec; thirty_sec; sixty_sec}` per base by summing the newest N completed buckets and dividing. Rounded integer division. During warmup, divide by `num_completed` rather than target size. Bases that appear in the previous snapshot but not in any tracked bucket are zeroed so feeds going quiet transition cleanly to zero.
3. **Propagation** — the same loop walks every ticker on every exchange, derives its canonical base via `Symbol_normalizer.canonical_base`, looks up the rates, and assigns all three TPS fields in one pass. Only exchanges whose state actually changed bump their version number, so idle exchanges don't spin the delta machinery.
4. **Delivery** — because TPS fields are regular fields on `Ticker`, the existing delta-diff picks them up automatically. The opt-in flags gate visibility by zeroing the client's shadow and skipping the compare — the shadow *always* holds real values.

### Canonical-base normalization

`Symbol_normalizer.canonical_base : exchange:string -> symbol:string -> string option` is the pure function that turns an exchange-specific symbol into a canonical base asset (e.g. `"BTC"`). It runs on every ticker every propagation tick, so **keep it stateless** — no caching, just per-exchange string parsing. It must cover format families like:

- `<BASE><QUOTE>` concatenation (Binance, Bybit, Bitget).
- `<BASE>-<QUOTE>` dash form (Coinbase spot).
- `<BASE>/USDC` slash form (Hyperliquid spot).
- Prefix strips: `PF_` / `PI_` (Kraken Futures).
- Suffix strips: `-PERP-INTX` (Coinbase INTX), `-PERP` (some Hyperliquid HIP-3), plain `PERP` quote (Bybit USDC-margin perps like `1000BONKPERP`).
- Alias: `XBT → BTC` (Kraken).
- Vendor prefixes: `HYPERDEX|BTC-PERP → BTC` (Hyperliquid HIP-3).

Every new exchange adapter must extend the normalizer **and** add unit tests for its format quirks, because this function silently produces wrong TPS attributions rather than crashing.

### Source-feed rate limits

Each trades WS must respect its source's per-connection subscribe rate limit. Known constraints:

- Binance — 5 msg/s per connection (stagger ~250ms).
- Hyperliquid — 2000 msg/min per IP (stagger ~50ms).
- Bybit — no documented public-WS rate limit (10ms stagger is fine, same as ticker subscribes).

Trades WS loops should also re-fetch their source's market list every ~60s and diff against the currently-subscribed set, so newly-listed perps start contributing TPS within a minute without a full reconnect.

## Klines (realtime OHLCV storage)

In addition to the ticker fan-out, the service stores **realtime kline (OHLCV) data** for a curated subset of exchanges. Klines are a different shape of problem from tickers — they are append-heavy, queried by range, and must survive restarts — so they get their own subsystem rather than living in the ticker aggregator.

Expected shape (to be pinned down as it lands):

- **Source**: each selected exchange's adapter opens a kline WebSocket alongside its ticker/trades feeds and streams candle updates. Most venues push a rolling in-progress candle every few hundred ms plus a final "closed" frame at the interval boundary; the store should only **persist** closed candles and serve the in-progress one from memory.
- **Storage**: durable, range-queryable by `(exchange, symbol, interval, time)`. Appropriate backends are anything columnar / time-series (DuckDB, ClickHouse, Parquet on disk, sqlite with a covering index, ...). Decision pending — record here when made.
- **Intervals**: at minimum `1m`; larger intervals (`5m`, `15m`, `1h`, ...) are typically derived from `1m` on read rather than stored separately. Confirm before implementing per-interval storage.
- **Backfill**: on startup and after any gap, fetch historical candles via REST to fill holes so clients never see a missing bar. The ingest path must be idempotent — repeated writes of the same `(exchange, symbol, interval, open_time)` overwrite rather than duplicate.
- **Exposure**: REST endpoint for range queries (`GET /api/klines?exchange=...&symbol=...&interval=...&from=...&to=...`, served from root — no base prefix) and/or a WS channel for live candle updates. Shape to be decided alongside the ticker channel design so they share subscribe/frame conventions where it makes sense.
- **Selected exchanges**: the kline subset is **smaller** than the ticker subset — storing every venue's candles is wasteful when most clients only want a canonical few. Which exchanges are in the selected set belongs here once chosen.

Klines and tickers share the symbol-normalization story — queries should accept either the exchange-native symbol or the canonical base where that makes sense — but the TPS machinery is independent and does not feed into klines.

## Env vars (intended)

| Variable            | Default       | Purpose                        |
| ------------------- | ------------- | ------------------------------ |
| `PORT`              | 9880          | HTTP/WS server port            |
| `EXCHANGES`         | (all enabled) | Comma-separated exchange names |
| `SLACK_WEBHOOK_URL` | (none)        | Slack webhook for stale alerts |

All routes are served from the root — there is no `BASE_PATH` prefix. WS lives at `/ws`, REST at `/api/...`, health at `/health`.

## Things to decide and record here as they land

- Async runtime (Eio vs Lwt) and the HTTP/WS library stack.
- Exact module layout (`lib/aggregator`, `lib/exchanges/*`, `bin/main`, etc.).
- JSON library (yojson vs something else) and whether compact keys are hand-rolled or derived from `[@@deriving yojson]` renames.
- Test runner (alcotest is the likely default) and how integration tests mock upstream feeds.
- Deployment target — container image + K8s rollout flow, if it matches the existing setup.
