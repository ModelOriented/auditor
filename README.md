[![Build Status](https://travis-ci.org/mi2-warsaw/auditor.svg?branch=master)](https://travis-ci.org/mi2-warsaw/auditor)
[![Build status](https://ci.appveyor.com/api/projects/status/16rmrvpbujvsumkt/branch/master?svg=true)](https://ci.appveyor.com/project/agosiewska/auditor/branch/master)

<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditorLogo.png" width="300" />

## auditor - audit/verification of regression models

### Installation
```
devtools::install_github("mi2-warsaw/auditor")
```

### [Reference Manual](https://mi2-warsaw.github.io/auditor/)

### [News](NEWS.md)

### Quick preview of the assumptions and functions

|  Assumption | Test  | Plot  |
|:---:|:---:|:---:|
| Homoscedasticity of the residuals | Goldfeld-Quandt </br> `test_gq` | Variable vs square root of the absolute value of the residuals </br>`plot_test_gq`|
| Autocorrelation in the residuals  |  Durbin-Watson </br> `test_dw` </br>  Runs Test </br> `test_runs`| i-th residual vs i+1-th residual </br>`plot_autocorr`  |
|  Outlayers |   |   |
| Influential observations |   |   |
| Multicollinearity of variables |   |   |
