#let only look at fords
## does the cause relate to passengers and age in (just) Fords?
library(Sleuth3)
my_ford_df <- subset(ex2018, Make == "Ford")

fit.ford.all <- glm(Cause ~ VehicleAge*Passengers, family=binomial(link = "logit"), data=my_ford_df)
summary(fit.ford.all)
# AIC 132.14
step.fit.ford <- step(fit.ford.all,scope=list(lower=.~1,
                                              upper=.~ .))
# step chose just mains
summary(step.fit.ford)
anova(step.fit.ford, fit.ford.all, test="Chisq")

# is either model 'adequate'
## mains
res.dev <- summary(fit.ford.all)$deviance
res.dev
dof <- summary(fit.ford.all)$df.residual
dof
1 - pchisq(res.dev,dof) 
# and it turns out that I had been forgetting the 1 and minus at the beginning


