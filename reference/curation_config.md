# Build a configuration for the generic curation services.

All policy and identity inputs are required. The constructor does not
read environment variables or options and does not install a project
default, so independent configurations can coexist in one R process.

## Usage

``` r
curation_config(
  project_id,
  corpus_id,
  config_id,
  topics,
  labels,
  lexicon,
  select_fn,
  selection_controls,
  funnel_stages,
  branding,
  artifact_paths,
  translator = NULL
)
```

## Arguments

- project_id, corpus_id, config_id:

  Explicit curation, corpus, and versioned-configuration identifiers.

- topics:

  Named topic vector used by the caller's scoring workflow.

- labels:

  Named classifier-label vector. An optional `relevant` attribute must
  contain only label names.

- lexicon:

  Named list of keyword vectors.

- select_fn:

  Function accepting `records` and `controls`, returning a same-row data
  frame with a logical `selected` column.

- selection_controls:

  Named list of caller-supplied policy controls.

- funnel_stages:

  Non-empty list of `{key, label, fn}` entries. Each `fn` accepts
  `records` and `context` and returns a logical stage mask.

- branding:

  Explicit named branding values, including `title` and `primary`.
  Optional `secondary`, `success`, `bootswatch`, and `border_radius`
  values configure the app theme. Supplying both `secondary` and
  `success` enables a three-colour navbar gradient.

- artifact_paths:

  Explicit named artifact paths, including `root`.

- translator:

  Optional `curation_translation_provider`; `NULL` is valid.

## Value

A `planscanR_curation_config` object.
