# Build a custom planscanR embedding model.

Wraps a user-supplied `embed_fn` plus a language inventory into an S3
object that participates in the same interface as the built-in models
(e.g.
[`embedding_model_minilm()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model_minilm.md)).
Pass it as `relevance_model` to
[`planscanR::get_assessments()`](https://barthoekstra.github.io/planscanR/reference/get_assessments.html)
or
[`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md).

## Usage

``` r
embedding_model(name, languages, embed_fn)
```

## Arguments

- name:

  Character scalar. Used in the language-support warning and in the
  sidecar JSON to record which model produced a score.

- languages:

  Character vector of ISO-639-1 codes the model is documented to
  support. Used by
  [`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md)
  to warn when a record's country language falls outside this set.

- embed_fn:

  A function of one argument (`x`, a character vector) that returns a
  numeric matrix with one row per element and a stable embedding
  dimension across calls.

## Value

An S3 object of class
`c("planscanR_embedding_custom", "planscanR_embedding_model")`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Trivial example: hash-based "embeddings" for testing
em <- embedding_model(
  name = "fake-hash",
  languages = c("en", "nl"),
  embed_fn = function(x) {
    matrix(rnorm(length(x) * 32), nrow = length(x))
  }
)
supported_languages(em)
embed_text(em, c("hello", "hallo"))
} # }
```
