# Reset the cached Python model for testing or to free memory.

Removes the cached `SentenceTransformer` for a given model ID (or all
cached models if `model_id` is `NULL`). The next call to
`planscanR::embed_text()` on that model will re-load it from disk /
Hugging Face.

## Usage

``` r
reset_embedding_cache(model_id = NULL)
```

## Arguments

- model_id:

  Optional model ID to evict. `NULL` clears all.

## Value

Nothing, invisibly.
