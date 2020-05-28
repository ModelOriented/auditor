## Test environments
* local OS X install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


# Changes:

## auditor 1.3.0

* new default color palete `DALEX::colors_discrete_drwhy` 
* depend on `R v3.5` to comply with `DALEX`
* Allow for some deprecated names in `plot()` function, to make it compatible with the paper [auditor: an R Package for Model-Agnostic Visual Validation and Diagnostics](https://journal.r-project.org/archive/2019/RJ-2019-036/index.html) in The R Journal, 
* add `arrow_size` to `plot_pca` and increase the default width value to `2`
* add `show_rugs` to `plot_residual_density` which allows to hide the rugs layer
* add `score_auprc` which calculates the AUPRC measure
* add `plot_prc` which plots the Precision-Recall Curve
* use `stringsAsFactors=TRUE` when creating `data.frame` in `model_*()` functions 
* add `coord_fixed` to `plot_coord_fixed`
* add `predict_funcion` and `residual_function` to `audit` function
