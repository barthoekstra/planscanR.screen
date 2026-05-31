# Report which ISO-639-1 languages a model has been trained on.

Used by
[`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md)
to warn (one-shot per language/model) when a record's country language
falls outside the model's supported set.

## Usage

``` r
supported_languages(model)
```

## Arguments

- model:

  A `planscanR_embedding_model` object.

## Value

Character vector of ISO-639-1 codes.
