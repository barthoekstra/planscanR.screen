# Apply a configuration's selection callback to records.

Apply a configuration's selection callback to records.

## Usage

``` r
curation_apply_selection(config, records, controls = NULL)
```

## Arguments

- config:

  A `planscanR_curation_config`.

- records:

  Data frame of enriched records.

- controls:

  Optional named overrides merged over the configuration's explicit
  `selection_controls`.

## Value

The callback result, with a logical `selected` column.
