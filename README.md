# servant-prometheus

[![Build status](https://github.com/worm2fed/servant-prometheus/actions/workflows/ci.yml/badge.svg)](https://github.com/worm2fed/servant-prometheus/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/servant-prometheus.svg?logo=haskell)](https://hackage.haskell.org/package/servant-prometheus)

# Servant Performance Counters

This package lets you track performance counters for each of your Servant endpoints using [Prometheus](https://prometheus.io/).

# Usage

`servant-prometheus` knows how to handle all official Servant combinators out of the box.

## Instrumenting your API

To use `servant-prometheus`, you'll need to wrap your WAI application with the provided middleware.

```haskell
import Network.Wai.Handler.Warp
import Prometheus.Servant

main :: IO ()
main = do
  let api    = ...
      server = ...
      app = prometheusMiddleware defaultMetrics api server

  run 8080 app
```

## Custom metrics

Note, that in example above `defaultMetrics` used, which actually provided as an example and good starting point.
If you want to use your custom metrics (eg you want another labels), you'll need to define an implementation for `Metrics mLatencyLabel mActiveLabel` data.
That's not hard, just copy-paste and tweak accordingly to your needs.

## Runtime overhead

Instrumenting your API introduces a non-zero runtime overhead, on the order of 200 - 600 µsec depending upon your machine. It's a good idea to run the benchmarks on your intended production platform to get an idea of how large the overhead will be. You'll need to have `wrk` installed to run the benchmarks.

In general, the runtime overhead should be effectively negligible if your handlers are issuing network requests, such as to databases. If you have handlers that are small, CPU-only, and requested frequently, you will see a performance hit from `servant-prometheus`.
