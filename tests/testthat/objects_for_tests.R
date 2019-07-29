library(DALEX)
library(auditor)
library(randomForest)

set.seed(123)

# simulate artificial classification data set
n <- 100
artifficial_regr <- data.frame(x1 = rnorm(n), # education
                                  x2 = runif(n), # income
                                  x3 = runif(n), # women
                                  x4 = rnorm(n), # census
                                  x5 = factor(sample(c(1, 2, 3), n, replace = TRUE))) # type
artifficial_regr$y <- artifficial_regr$x1 + artifficial_regr$x2^2 +
  artifficial_regr$x3 * artifficial_regr$x4 + as.numeric(artifficial_regr$x5)

# simulate artificial regression data set
artifficial_classif <- data.frame(x1 = rnorm(n), # pregnant
                                  x2 =  rnorm(n), # glucose
                                  x3 =  rnorm(n), # pressure
                                  x4  = rnorm(n), # triceps
                                  x5  = rnorm(n), # insulin
                                  x6  = rnorm(n), # mss
                                  x7 = rnorm(n), # pedigree
                                  x8  = rnorm(n), # age
                                  y = factor(rbinom(n, 1, prob = 0.5))) # diabetes


# simulate artificial anorexia data set
artifficial_classif_2 <- data.frame(x1 = factor(rbinom(n, 1, prob = 0.5)), # Treat
                                   x2 = rnorm(n), # Prewt
                                   y = rnorm(n)) # Postwt

set.seed(71)

model_lm <- lm(y ~ ., data = artifficial_regr)
model_glm <- glm(y ~ x1 + x2 + offset(x2), family = gaussian, data = artifficial_classif_2)
model_rf <- randomForest(y ~ x2 + x1, data = artifficial_classif_2)
model_class_glm <- glm(y ~ ., family=binomial,	data=artifficial_classif)
model_class_rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
model_class_glm2 <- glm(y ~ x3, family=binomial,	data=artifficial_classif)


au_class_glm2 <- audit(model_class_glm2)

au_lm <- audit(model_lm, label = "lm")
au_glm <- audit(model_glm, label = "glm")
au_rf <- audit(model_rf, label="rf")
au_class_glm <- audit(model_class_glm, label="class glm")
au_class_rf <- audit(model_class_rf)

explainer_lm <- explain(model_lm, data = artifficial_regr, y = artifficial_regr$y)
au_expl_lm <- audit(explainer_lm)

cd_lm <- model_cooksdistance(au_lm)
mp_lm <- model_performance(au_lm)
mf_lm <- model_halfnormal(au_lm)
glm_mr <- model_residual(au_glm, "x2")
rf_mr <- model_residual(au_rf, "x1")
