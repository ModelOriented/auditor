[![Build Status](https://travis-ci.org/mi2-warsaw/auditor.svg?branch=master)](https://travis-ci.org/mi2-warsaw/auditor)


<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditorLogo.png" width="300" />

# auditor - regression model verification, validation, and error analysis

## auditor's pipeline: **model %>% audit() %>% plot(type=...)**

### Installation

#### From GitHub

```
devtools::install_github("mi2-warsaw/auditor")
```

#### or from CRAN 
Version on CRAN works for R >=3.5, while version on GitHibu works for R>=3.0. This will be changed in the next update.

```{r}
install.packages("DALEX")
```

### [Reference Manual](https://mi2-warsaw.github.io/auditor/)

### Vignettes (in Preparation)

* [Introduction into model audit](https://mi2-warsaw.github.io/auditor/articles/Intorduction_into_model_audit.html)

* [The half-normal plots](https://mi2-warsaw.github.io/auditor/articles/HalfNormal.html)

### [News](NEWS.md)

<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditor_cheatsheet.png"/>
<img src="https://raw.githubusercontent.com/mi2-warsaw/auditor/master/materials/auditor_cheatsheet_ROC.png"/>


## Acknowledgments
Work on this package was financially supported by the ‘NCN Opus grant 2016/21/B/ST6/02176’.
