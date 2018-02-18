library(auditor)
library(car)
library(MASS)
library(randomForest)

model.lm <- lm(prestige~education + women + income, data = Prestige)
model.glm <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                 family = gaussian, data = anorexia)
model.rf <- randomForest::randomForest(Postwt ~ Prewt + Treat, data = anorexia)

au.lm <- audit(model.lm)
au.glm <- audit(model.glm)
au.rf <- audit(model.rf)
