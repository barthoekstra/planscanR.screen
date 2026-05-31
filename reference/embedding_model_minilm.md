# Build an embedding model backed by sentence-transformers.

First call lazily initialises a Python `SentenceTransformer` via
[`reticulate::import()`](https://rstudio.github.io/reticulate/reference/import.html).
The model is cached in a session-level environment; subsequent calls
re-use it. The Python package `sentence-transformers` must be available
— see
[`reticulate::py_install()`](https://rstudio.github.io/reticulate/reference/py_install.html).

## Usage

``` r
embedding_model_minilm(
  model_id = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2",
  languages = minilm_languages()
)
```

## Arguments

- model_id:

  Hugging Face model ID. Defaults to
  `"sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"`.

- languages:

  Override the documented language list. Defaults to the 50 languages
  documented for the default model. Ignored if the user is plugging in a
  different model whose language inventory differs.

## Value

A `planscanR_embedding_minilm` S3 object.

## Examples

``` r
if (FALSE) { # \dontrun{
em <- embedding_model_minilm()
supported_languages(em)
v <- embed_text(em, c("wind energy", "windenergie"))
} # }
```
