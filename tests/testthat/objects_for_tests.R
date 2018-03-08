library(auditor)
library(car)
library(MASS)
library(randomForest)
library(mlbench)
data("PimaIndiansDiabetes")
set.seed(71)

model.lm <- lm(prestige~education + women + income, data = Prestige)
model.glm <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                 family = gaussian, data = anorexia)
model.rf <- randomForest::randomForest(Postwt ~ Prewt + Treat, data = anorexia)
model.class.glm <- glm(diabetes~., family=binomial,	data=PimaIndiansDiabetes)
model.class.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)

# model.class.glm2 <- glm(diabetes~pressure, family=binomial,	data=PimaIndiansDiabetes)
# au.class.glm2 <- audit(model.class.glm2)

au.lm <- audit(model.lm)
au.glm <- audit(model.glm)
au.rf <- audit(model.rf, label="rf")
au.class.glm <- audit(model.class.glm, label="class glm")
au.class.rf <- audit(model.class.rf)

