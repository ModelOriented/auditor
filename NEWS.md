# auditor 1.1.0
* Szymon Maksymiuk is now contributor
* New score functions: `score_acc`, `score_f1`, `score_precision`, `score_recall`, `score_specificity`,
    `score_one_minus_acc`, `score_one_minus_auc`, `score_one_minus_f1`, `score_one_minus_precision`,
    `score_one_minus_specificity`, `score_one_minus_recall`

# auditor 1.0.0
* Tomasz Mikołajczyk, Hubert Baniecki, and Michał Burdukiewicz are now contributors :)
* New names of functions #95, the old ones are now depreciated.
* All plots gained new drWhy theme.
* `modelEvaluation()` return one consistent data frame.
* Unnecessary Imports and Suggests removed from DESCRIPTION.
* Interactive D3 plots were added.

# auditor 0.3.2
* Automated checks for residuals are added. See the `check_residuals()` function.

# auditor 0.3.1
* Repository with the `auditor` package was moved to new GitHub organization ([MI^2 DataLab](https://github.com/MI2DataLab/auditor)).

# auditor 0.3.0
* New five vignettes with examples.
* Second pipeline with new computational functions: `modelResiduals()`, `modelEvaluation()`, `modelFit()`, `modelPerformance()`, `observationInfluence()`.
* New plot: `plotResidualBoxplot()`

# auditor 0.2.1
* In the source code, function `isFALSE()` is replaced by ` == FALSE` to make package compatible with R versions older than 3.5.
