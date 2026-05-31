# Classify a character vector against candidate labels.

Returns a numeric matrix with one row per element of `x` and one column
per label (columns named by the label slugs, i.e. `names(labels)`).
Values are label probabilities. With `multi_label = FALSE` each row sums
to ~1 (softmax over labels); with `multi_label = TRUE` each cell is an
independent probability.

## Usage

``` r
classify_text(model, x, labels, multi_label = FALSE)
```

## Arguments

- model:

  A `planscanR_classifier` object.

- x:

  Character vector to classify.

- labels:

  Named character vector: names are stable slugs (used as output column
  names), values are the natural-language hypothesis phrases fed to the
  model.

- multi_label:

  Logical; passed through to the backend.

## Value

Numeric matrix `[length(x), length(labels)]`.
