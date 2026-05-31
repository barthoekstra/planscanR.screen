# Build a custom planscanR classifier.

Wraps a user-supplied `classify_fn` into an S3 object that participates
in the same interface as the built-in
[`classify_model_zeroshot()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_model_zeroshot.md).
Useful for tests (a deterministic mock) or to plug in a different
backend.

## Usage

``` r
classifier(name, classify_fn)
```

## Arguments

- name:

  Character scalar; recorded in the sidecar so a stored classification
  knows which model produced it.

- classify_fn:

  A function `function(x, labels, multi_label)` where `x` is a character
  vector and `labels` is a named character vector (names = slugs, values
  = hypothesis phrases). Must return a numeric matrix with `length(x)`
  rows and one column per label, columns named by the label slugs
  (`names(labels)`).

## Value

An S3 object of class
`c("planscanR_classifier_custom", "planscanR_classifier")`.
