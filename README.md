# auditor - regression model verification, validation, and error analysis   
<img src="materials/auditor2.png" width="15%" align="right" />


[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/auditor)](https://cran.r-project.org/package=auditor)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/auditor)](http://cranlogs.r-pkg.org/badges/grand-total/auditor)
[![Build Status](https://travis-ci.org/mi2-warsaw/auditor.svg?branch=master)](https://travis-ci.org/mi2-warsaw/auditor)
[![Tweet](https://img.shields.io/twitter/url/http/shields.io.svg?style=social)](https://twitter.com/intent/tweet?text=The%20auditor%20package%20is%20an%20easy%20to%20use%20unified%20interface%20for%20model-agnostic%20verification,%20validation,%20and%20error%20analysis.%0Ahttps://github.com/mi2-warsaw/auditor%0A&hashtags=rstats,erroranalysis,machinelearning,audit)


<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditorLogo.png" width="20%" /> 
<img src="materials/demo.png" width="60%" align = "center"/>


## auditor's pipeline: **model %>% audit() %>% plot(type=...)**

## Installation

### from GitHub

```
devtools::install_github("mi2-warsaw/auditor")
```
### and from CRAN 

```{r}
install.packages("auditor")
```

## [News](NEWS.md)

## [Reference Manual](https://mi2-warsaw.github.io/auditor/)

## Vignettes (in Preparation)

* [Introduction into model audit](https://mi2-warsaw.github.io/auditor/articles/Intorduction_into_model_audit.html)

* [The Half-normal Plots](https://mi2-warsaw.github.io/auditor/articles/HalfNormal.html)

## Cheatsheets


<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditor_cheatsheet.png"/>
<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditor_cheatsheet_ROC.png"/>

## A Short Overview of Plots

| Plot name                                             | Function                                                               | Regression | Classification | Examples                                                                               |
|-------------------------------------------------------|------------------------------------------------------------------------|------------|----------------|----------------------------------------------------------------------------------------|
| Autocorrelation Function                              | `plotACF()` </br> `plot(..., type = "ACF")`                            |            |                | soon                                                                                   |
| Autocorrelation                                       | `plotAutocorrelation()` </br> `plot(..., type = "Autocorrelation")`    |            |                | soon                                                                                   |
| Influence of observations                             | `plotCooksDistance()` </br> `plot(..., type = "CooksDistance")`        |            |                | soon                                                                                   |
| Cumulative Gain Chart                                 | `plotCumulativeGain()` </br> `plot(..., type = "CumulativeGain")`      |            |                | soon                                                                                   |
| Half-Normal                                           | `plotHalfNormal()` </br> `plot(..., type = "HalfNormal")`              |    yes     |     yes        | [The Half-normal Plots](https://mi2-warsaw.github.io/auditor/articles/HalfNormal.html) |
| LIFT Chart                                            | `plotLIFT()` </br> `plot(..., type = "LIFT")`                          |            |                | soon                                                                                   |
| Model Correlation                                     | `plotModelCorrelation()` </br> `plot(..., type = "ModelCorrelation")`  |            |                | soon                                                                                   |
| Principal Component Analysis of models                | `plotModelPCA()` </br> `plot(..., type = "ModelPCA")`                  |            |                | soon                                                                                   |
| Model Ranking Plot                                    | `plotModelRanking()` </br> `plot(..., type = "ModelRanking")`          |            |                | soon                                                                                   |
| Predicted Response vs Observed or Variable Values     | `plotPrediction()` </br> `plot(..., type = "Prediction")`              |            |                | soon                                                                                   |
| Regression Error Characteristic Curves (REC)          | `plotREC()` </br> `plot(..., type = "REC")`                            |            |                | soon                                                                                   |
| Plot Residuals vs Observed, Fitted or Variable Values | `plotResidual()` </br> `plot(..., type = "Residual")`                  |            |                | soon                                                                                   |
| Residual Density                                      | `plotResidualDensity()` </br> `plot(..., type = "ResidualDensity")`    |            |                | soon                                                                                   |
| Receiver Operating Characteristic (ROC)               | `plotROC()` </br> `plot(..., type = "ROC")`                            |            |                | soon                                                                                   |
| Regression Receiver Operating Characteristic (RROC)   | `plotRROC()` </br> `plot(..., type = "RROC")`                          |            |                | soon                                                                                   |
| Scale-Location plot                                   | `plotScaleLocation()` </br> `plot(..., type = "ScaleLocation")`        |            |                | soon                                                                                   |
| Two-sided Cumulative Distribution Function            |  `plotTwoSidedECDF()` </br> `plot(..., type = "TwoSidedECDF")`         |            |                | soon                                                                                   |

## Acknowledgments
Work on this package was financially supported by the ‘NCN Opus grant 2016/21/B/ST6/02176’.
