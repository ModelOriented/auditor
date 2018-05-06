[![Build Status](https://travis-ci.org/mi2-warsaw/auditor.svg?branch=master)](https://travis-ci.org/mi2-warsaw/auditor)


<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditorLogo.png" width="300" />

# auditor - regression model verification, validation, and error analysis

## auditor's pipeline: **model %>% audit() %>% plot(type=...)**

### Installation
```
devtools::install_github("mi2-warsaw/auditor")
```


### [Reference Manual](https://mi2-warsaw.github.io/auditor/)

### [News](NEWS.md)

<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditor_cheatsheet.png"/>
<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditor_cheatsheet_ROC.png"/>

### Quick preview of the assumptions and functions

| Attribute | Verification | Plot |
|:---|:---|:---|
| Residuals| |Residuals vs fitted values </br> `plotResiduals` |
| Homoscedasticity of the errors | Goldfeld-Quandt score </br> `scoreGQ` | Variable vs square root of the absolute value of the residuals </br>`plotScaleLocation`|
| Uncorrelated errors  |  Durbin-Watson score </br> `scoreDW` </br>  Runs score </br> `scoreRuns`| i-th residual vs (i+1)-th residual </br>`plotAutocorrelation`  |
| Uncorrelated errors  |  | AutoCorrelation Function </br>`plotACF`  |
| Influential observations | Cooks distance </br> `scoreCook`| `plotCook` |
| error distribution | Half-Normal score </br> `scoreHalfNormal`| Half-Normal plot </br> `plotHalfNormal` |


## Acknowledgments
Work on this package was financially supported by the ‘NCN Opus grant 2016/21/B/ST6/02176’.
