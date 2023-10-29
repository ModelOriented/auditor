# auditor 1.3.5 
* removed Unicode characters from plot names (as requested by CRAN)
* fixed two instances of class(x) == "sth" instead of inherits

# auditor 1.3.4 - developer version
* more stable AUC computation #157

# auditor 1.3.3
* updated documentation

# auditor 1.3.1
* bugfix: `score_auc()`
* add spellcheck
* fix spelling
* markdown added to suggests

# auditor 1.3.0
* new default color palette `DALEX::colors_discrete_drwhy` 
* depend on `R v3.5` to comply with `DALEX`
* Allow for some deprecated names in `plot()` function, to make it compatible with the paper [auditor: an R Package for Model-Agnostic Visual Validation and Diagnostics](https://journal.r-project.org/archive/2019/RJ-2019-036/index.html) in The R Journal, 
* add `arrow_size` to `plot_pca` and increase the default width value to `2`
* add `show_rugs` to `plot_residual_density` which allows to hide the rugs layer
* add `score_auprc` which calculates the AUPRC measure
* add `plot_prc` which plots the Precision-Recall Curve
* use `stringsAsFactors=TRUE` when creating `data.frame` in `model_*()` functions 
* add `coord_fixed` to `plot_coord_fixed`
* add `predict_funcion` and `residual_function` to `audit` function

# auditor 1.2.0
* Change `score` parameter to `type` in `score()` function
* Fix a bug in calculating Cook's distances
* Fix a bug in plotting Lift Charts

# auditor 1.1.1
* Add `data` and `cutoff` parameter to several `score_x` functions.
* Add `score_gini`, `score_one_minus_gini`, and `score_r2` functions

# auditor 1.1.0
* Szymon Maksymiuk is now contributor
* New score functions: `score_acc`, `score_f1`, `score_precision`, `score_recall`, `score_specificity`,
    `score_one_minus_acc`, `score_one_minus_auc`, `score_one_minus_f1`, `score_one_minus_precision`,
    `score_one_minus_specificity`, `score_one_minus_recall`

# auditor 1.0.0
* Tomasz Mikołajczyk, Hubert Baniecki, and Michał Burdukiewicz are now contributors :)
* *New names of functions #95, the old ones are now depreciated.*
* All plots gained new drWhy theme.
* `modelEvaluation()` return one consistent data frame.
* Unnecessary Imports and Suggests removed from DESCRIPTION.
* Interactive D3 plots were added.
* Repository with the `auditor` package was moved to new GitHub organization ([ModelOriented](https://github.com/ModelOriented/auditor)).

# auditor 0.3.2
* Automated checks for residuals are added. See the `check_residuals()` function.

# auditor 0.3.1
* Repository with the `auditor` package was moved to new GitHub organization ([MI^2 DataLab](https://github.com/MI2DataLab)).

# auditor 0.3.0
* New five vignettes with examples.
* Second pipeline with new computational functions: `modelResiduals()`, `modelEvaluation()`, `modelFit()`, `modelPerformance()`, `observationInfluence()`.
* New plot: `plotResidualBoxplot()`

# auditor 0.2.1
* In the source code, function `isFALSE()` is replaced by ` == FALSE` to make package compatible with R versions older than 3.5.
