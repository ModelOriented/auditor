library(auditor)
library(MASS)
library(DALEX2)
library(randomForest)
library(mlbench)
data("PimaIndiansDiabetes")

set.seed(123)

# simulate artificial prestige data set
n <- 100
Prestige <- data.frame(education = rnorm(n),
                                  income = runif(n),
                                  women = runif(n),
                                  census = rnorm(n),
                                  type = factor(sample(c("a","b","c"), n, replace = TRUE)))
Prestige$prestige <- Prestige$education + Prestige$income^2 +
  Prestige$women * Prestige$census + as.numeric(Prestige$type)

set.seed(71)
PimaIndiansDiabetes$diabetes <- ifelse(PimaIndiansDiabetes$diabetes == "pos", 1,0 )

model.lm <- lm(prestige~education + women + income, data = Prestige)
model.glm <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                 family = gaussian, data = anorexia)
model.rf <- randomForest(Postwt ~ Prewt + Treat, data = anorexia)
model.class.glm <- glm(diabetes~., family=binomial,	data=PimaIndiansDiabetes)
model.class.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)

model.class.glm2 <- glm(diabetes~pressure, family=binomial,	data=PimaIndiansDiabetes)
au.class.glm2 <- audit(model.class.glm2)

au.lm <- audit(model.lm, label = "lm")
au.glm <- audit(model.glm, label = "glm")
au.rf <- audit(model.rf, label="rf")
au.class.glm <- audit(model.class.glm, label="class glm")
au.class.rf <- audit(model.class.rf)

explainer_lm <- explain(model.lm, data = Prestige, y = Prestige$prestige)
au_expl_lm <- audit(explainer_lm)

cd.lm <- observationInfluence(au.lm)
mp.lm <- modelPerformance(au.lm)
mf.lm <- modelFit(au.lm)
glm_mr <- modelResiduals(au.glm, "Prewt")
rf_mr <- modelResiduals(au.rf, "Treat")

