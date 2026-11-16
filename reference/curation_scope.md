# Build a scope shared by curation services.

A scope is explicit and immutable by convention. `project_id` identifies
a curation workspace, `corpus_id` identifies the source corpus, and
`config_id` identifies the versioned curation configuration. Review
repositories may key authored decisions by project and corpus while
retaining the complete scope on imported/exported provenance; artifact
stores use all three values.

## Usage

``` r
curation_scope(project_id, corpus_id, config_id)
```

## Arguments

- project_id:

  Non-empty curation-workspace identifier.

- corpus_id:

  Non-empty corpus identifier.

- config_id:

  Non-empty versioned configuration identifier.

## Value

A `planscanR_curation_scope` object.
