# servant-prometheus

[![Build status](https://github.com/worm2fed/servant-prometheus/actions/workflows/ci.yml/badge.svg)](https://github.com/worm2fed/servant-prometheus/actions/workflows/ci.yml)

# Servant Performance Counters

This package lets you track performance counters for each of your Servant endpoints using Prometheus.

# Usage

Servant-Prometheus knows how to handle all official Servant combinators out of the box.

## Instrumenting your API
To use Servant-Prometheus, you'll need to wrap your WAI application with the Servant-Prometheus middleware.

```
import Network.Wai.Handler.Warp
import Prometheus.Servant

main :: IO ()
main = do
  let api    = ...
      server = ...
      app = prometheusMiddleware defaultMetrics api server

  run 8080 app
```

## Runtime overhead
Instrumenting your API introduces a non-zero runtime overhead, on the order of 200 - 600 µsec depending upon your machine. It's a good idea to run the benchmarks on your intended production platform to get an idea of how large the overhead will be. You'll need to have `wrk` installed to run the benchmarks.

In general, the runtime overhead should be effectively negligible if your handlers are issuing network requests, such as to databases. If you have handlers that are small, CPU-only, and requested frequently, you will see a performance hit from Servant-EKG.
