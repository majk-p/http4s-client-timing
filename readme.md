# Http4s client timing

This repository is a simple benchmark utility for various http4s client backends. 

## Usage 

Run benchmarks against https://michal.pawlik.dev with client warmup 

```
scala-cli run . -- michal.pawlik.dev true
```

Run benchmarks against https://example.net with no prior client warmup 

```
scala-cli run . -- example.net false
```

## Prerequisites

Install scala-cli https://scala-cli.virtuslab.org/install
