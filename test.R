Gender <- c(0,0,1,1,0,0)
Believer <- c(1,0,1,0,1,0)
Count <- c(400,100,375,134,35,47)
afterlife <- data.frame(Gender,Believer,Count)
afterlife
install.packages("VGAM")
library(VGAM)

fit <- vglm(Count~Gender,Believer, family=cumulative, data=afterlife)
summary(fit)
warning(fit)

glm(formula = Count~Gender+Believer,family=binomial(link="logit"))
glm(Count~Gender+Believer)

??family.glm
