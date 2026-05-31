# Built-in selection learner.

Logistic regression on the base-R `glm` engine — the default
[`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md)
fits. Needs only the tidymodels glue (parsnip / recipes / rsample /
workflows), no extra modelling backend.

## Usage

``` r
selection_learner_logistic()
```

## Value

A `planscanR_selection_learner`.

## Examples

``` r
selection_learner_logistic()
#> <planscanR_selection_learner> logistic_glm
```
