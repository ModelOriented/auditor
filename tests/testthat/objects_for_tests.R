library(auditor)
library(car)
library(MASS)
library(randomForest)

set.seed(71)

model.lm <- lm(prestige~education + women + income, data = Prestige)
model.glm <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                 family = gaussian, data = anorexia)
model.rf <- randomForest::randomForest(Postwt ~ Prewt + Treat, data = anorexia)
model.class.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
iris.rf <- randomForest(Species ~ ., data=iris)

au.lm <- audit(model.lm)
au.glm <- audit(model.glm)
au.rf <- audit(model.rf)
au.class.rf <- audit(model.class.rf)
