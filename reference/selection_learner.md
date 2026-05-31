# Construct a custom selection learner.

Wraps a parsnip model specification into an S3 object the trainer
understands. Use the built-in constructors
([`selection_learner_logistic()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learners_builtin.md)
and friends) for the common cases, or this generic constructor to plug
in any parsnip classification spec.

## Usage

``` r
selection_learner(name, spec, engine_pkg = character(0), recipe_fn = NULL)
```

## Arguments

- name:

  Character scalar; a stable identifier stored on the fitted model and
  shown in the app.

- spec:

  A parsnip model specification in classification mode (e.g. from
  [`parsnip::logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html)),
  OR — preferably — a zero-argument function that *returns* one. The
  function form keeps construction lazy, so a learner can be created
  (and listed in a UI) without parsnip installed; the spec is only built
  at fit time, behind the
  [`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md)
  dependency check.

- engine_pkg:

  Character vector of package names the engine needs at fit time
  (besides the tidymodels glue). Checked by
  [`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md).

- recipe_fn:

  Optional `function(train, feature_names) -> recipe` overriding the
  default preprocessing. `NULL` uses the package default
  (impute-via-feature-contract, dummy-encode nominals, drop
  zero-variance, normalise numerics).

## Value

An S3 object of class `planscanR_selection_learner`.

## Examples

``` r
if (FALSE) { # \dontrun{
selection_learner(
  "my-logit",
  function() parsnip::set_engine(parsnip::logistic_reg(), "glm")
)
} # }
```
