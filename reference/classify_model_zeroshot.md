# Local zero-shot classifier backed by a HuggingFace NLI model.

Lazily constructs a `transformers` `zero-shot-classification` pipeline
on first use (cached for the session). The default model is the
multilingual NLI model
`MoritzLaurer/mDeBERTa-v3-base-xnli-multilingual-nli-2mil7`, which
handles German / Dutch / English text natively — no translation
required.

## Usage

``` r
classify_model_zeroshot(
  model_id = "MoritzLaurer/mDeBERTa-v3-base-xnli-multilingual-nli-2mil7",
  device = NULL,
  batch_size = 16L
)
```

## Arguments

- model_id:

  HuggingFace model ID.

- device:

  Torch device: `"mps"`, `"cuda"`, `"cpu"`, or `NULL` to auto-detect
  (MPS on Apple Silicon, else CUDA, else CPU).

- batch_size:

  GPU batch size for the pipeline (number of text-hypothesis NLI pairs
  evaluated per forward pass). On a single GPU this is the main
  throughput lever: `16` roughly doubles throughput over unbatched on
  MPS. Larger values (e.g. 64) are often *slower* because
  variable-length sequences get padded to the batch maximum, so the long
  outliers dominate — 16 is a good default. There is no benefit to
  process-level parallelism on a single GPU (workers contend for one
  device and each reloads the model).

## Value

A `planscanR_classifier_zeroshot` S3 object.

## Details

Requires the Python packages `transformers`, `torch`, `sentencepiece`
and `protobuf` (the last two are needed by the mDeBERTa tokenizer).
Install with
`reticulate::py_require(c("transformers","torch","sentencepiece","protobuf"))`.

## Examples

``` r
if (FALSE) { # \dontrun{
clf <- classify_model_zeroshot()
labels <- c(wind = "wind energy project", other = "unrelated to energy")
classify_text(clf, c("Windpark Test", "Wohnungsbau"), labels)
} # }
```
