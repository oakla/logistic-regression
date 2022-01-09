#Let's try using step to fit all mains and interactions
my_df <- ex2018
my_df$Make <- factor(my_df$Make, levels=c("Other", "Ford"))
# first lets redo the main effects
fit.all.main <- glm(Cause ~ VehicleAge + Passengers + Make, family=binomial(link = logit), data=ex2018)
summary(fit.all.main)
#now we can check for interactions
fit.int <- glm(Cause ~ VehicleAge*Passengers*Make, family = binomial(link = "logit"), data=my_df)
summary(fit.int)
#AIC is slightly higher
##let try step to see if we can improve things
fit.int.reduced <- step(fit.int, test="Chisq")
summary(fit.int.reduced)
#AIC = 201.41 - slightly better than mains only
anova(fit.all.main, fit.int.reduced, test="Chisq")

## is this new model (with an interaction term) adequate?
residual.deviance <- summary(fit.int.reduced)$deviance
d.o.f <- summary(fit.int.reduced)$df.residual
pchisq(residual.deviance,d.o.f)
#no... fuck


## is this new model (with an interaction term) adequate?
residual.deviance <- summary(fit1)$deviance
d.o.f <- summary(fit1)$df.residual
pchisq(residual.deviance,d.o.f)
#no... fuck

# what if we just use vehicleage
fit.basic <- glm(Cause ~ VehicleAge, data=my_df, family=binomial(link="logit"))
step.fit <- step(fit.basic,scope=list(lower=.~1,
                                      upper=.~ VehicleAge*Passengers
                                      *Make))
summary(step.fit)
# best model, lower AIC (201.41)