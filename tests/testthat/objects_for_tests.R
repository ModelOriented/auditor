library(auditor)
library(DALEX)
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
                                  y = rbinom(n, 1, prob = 0.5)) # diabetes


# simulate artificial anorexia data set
artifficial_classif_2 <- data.frame(x1 = factor(rbinom(n, 1, prob = 0.5)), # Treat
                                   x2 = rnorm(n), # Prewt
                                   y = rbinom(n, 1, prob = 0.5)) # Postwt

set.seed(71)

model_lm <- lm(y ~ ., data = artifficial_regr)
model_glm <- glm(y ~ x1 + x2 + offset(x2), family = gaussian, data = artifficial_classif_2)
model_rf <- randomForest(factor(y) ~ x2 + x1, data = artifficial_classif_2)
model_class_glm <- glm(factor(y) ~ ., family=binomial,	data=artifficial_classif)
model_class_rf <- randomForest(Species ~ ., data=iris, importance=TRUE, proximity=TRUE)
model_class_glm2 <- glm(factor(y) ~ x3, family=binomial,	data=artifficial_classif)


exp_class_glm2 <- explain(model_class_glm2, data = artifficial_classif, y = artifficial_classif$y, verbose = FALSE)
exp_lm <- explain(model_lm, label = "lm", data = artifficial_regr, y = artifficial_regr$y, verbose = FALSE)
exp_glm <- explain(model_glm, label = "glm", data = artifficial_classif_2, y = artifficial_classif_2$y, verbose = FALSE)
exp_rf <- explain(model_rf, label="rf", data =artifficial_classif_2, y = artifficial_classif_2$y, verbose = FALSE)
exp_class_glm <- explain(model_class_glm, label="class glm", data = artifficial_classif, y = artifficial_classif$y, verbose = FALSE)

mr_rf <- model_residual(exp_rf)
mr_glm <- model_residual(exp_glm)

cd_lm <- model_cooksdistance(exp_lm)
cd_rf <- model_cooksdistance(exp_rf)

new_score1 <- function(object) sum(sqrt(abs(object$residuals)))
new_score2 <- function(object) sum(sqrt(abs(object$residuals)) + 1)
mp_lm <- auditor::model_performance(exp_lm, score = c("mae", "mse", "rec", "rroc"),
                           new_score = list(n1 = new_score1, n2 = new_score2))
mp_rf <- auditor::model_performance(exp_rf, score = c("mae", "mse", "rec", "rroc"),
                            new_score = list(n1 = new_score1, n2 = new_score2))

hn_glm <- model_halfnormal(exp_glm)
hn_rf <- model_halfnormal(exp_rf)

ev_rf <- model_evaluation(exp_rf)
ev_glm <- model_evaluation(exp_glm)
