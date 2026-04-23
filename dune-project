(lang dune 3.15)

(name crypto_publicdata)

(generate_opam_files true)

(authors "Valerii Iatsko")
(maintainers "Valerii Iatsko")
(license MIT)
(source
 (github viatsko/crypto-publicdata))

(package
 (name crypto_publicdata)
 (synopsis "Real-time multi-exchange crypto market data aggregator")
 (description
  "Public-data-only crypto market data service. Connects to many exchanges, aggregates per-symbol tickers into a single cache, stores realtime kline history for a curated subset, and re-broadcasts everything over WebSocket + REST at the root path.")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  async
  cohttp
  cohttp-async
  async_websocket
  cohttp_async_websocket
  yojson
  ppx_jane))
