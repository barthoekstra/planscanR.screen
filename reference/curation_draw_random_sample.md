# Draw a reproducible, country-stratified random sample.

The sample is drawn from every supplied country before it is
interleaved. `country` remains part of every identity, so native
document IDs reused by two countries stay distinct. When an explicit
`seed` is supplied, the caller's RNG state is restored on success and
error, including when `.Random.seed` did not exist before the call.

## Usage

``` r
curation_draw_random_sample(
  records,
  countries = NULL,
  n_per_country = NULL,
  seed = NULL,
  n = NULL
)
```

## Arguments

- records:

  A frame with `country` and `document_id` columns.

- countries:

  Countries to sample, in the desired stratum order. `NULL` uses the
  order first observed in `records`.

- n_per_country:

  Positive number of rows per country.

- seed:

  Optional integer seed. `NA` behaves like `NULL`; with `NULL`, sampling
  advances the caller's RNG stream.

- n:

  Legacy alias for `n_per_country`.

## Value

A tibble with `document_id` and `country` columns.
