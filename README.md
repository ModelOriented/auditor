[![Build Status](https://travis-ci.org/mi2-warsaw/auditor.svg?branch=master)](https://travis-ci.org/mi2-warsaw/auditor)
[![Build status](https://ci.appveyor.com/api/projects/status/16rmrvpbujvsumkt/branch/master?svg=true)](https://ci.appveyor.com/project/agosiewska/auditor/branch/master)

<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditorLogo.png" width="300" />

## auditor - regression model verification, validation, and error analysis

### Installation
```
devtools::install_github("mi2-warsaw/auditor")
```

### [Reference Manual](https://mi2-warsaw.github.io/auditor/)

### [News](NEWS.md)

### Quick preview of the assumptions and functions

| Attribute | Verification | Plot |
|:---|:---|:---|
| Residuals| |Residuals vs fitted values `plotResiduals` |
| Homoscedasticity of the errors | Goldfeld-Quandt score </br> `scoreGQ` | Variable vs square root of the absolute value of the residuals </br>`plotScaleLocation`|
| Uncorrelated errors  |  Durbin-Watson score </br> `scoreDW` </br>  Runs score </br> `scoreRuns`| i-th residual vs (i+1)-th residual </br>`plotAutocorrelation`  |
| Uncorrelated errors  |  | AutoCorrelation Function </br>`plotACF`  |
| Influential observations | Cooks distance </br> `scoreCook`| `plotCook` |
| error distribution | Half-Normal score </br> `scoreHalfNormal`| Half-Normal plot `plotHalfNormal` |


