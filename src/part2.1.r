#2. Construct a logistic regression model. Which predictors are significant? Is there evidence for any interaction effects? Does the best model pass a goodness-of-fit test?


## fit all the Xs
fit.all.main <- glm(Cause ~ VehicleAge + Passengers + Make, family=binomial(link = logit), data=ex2018)
summary(fit.all.main)

### each X seems to be doing something
anova(fit.all.main, test="Chisq")

## is this model adequate?
residual.deviance <- summary(fit.all.main)$deviance
d.o.f <- summary(fit.all.main)$df.residual
1-pchisq(residual.deviance,d.o.f)
#yes

## Let's trying plotting stuff to see if we can see what the problem is
### first let's do some eye-balling
pairs(my_df)
# this doesn't reallty work because there are only two levels, as opposed to the Toxic Trout data, which has several dose levels and a differnt proportion for each one. Our scalar explanatory variable can only be compared to a binary repsonse... unless we make a proportion for each level of passenger...
range(my_df$Passengers)
passengers <- seq(0,11,by=1)
tire_freq = integer(12)
totals = integer(12)
for(i in passengers){
  temp_df <- subset(my_df, Passengers ==i)
  tires.freq <- as.data.frame(table(temp_df$Cause))[2,2]
  not.tires.freq <- as.data.frame(table(temp_df$Cause))[1,2]
  tire_freq[i] <- tires.freq
  totals[i] <- not.tires.freq + tires.freq
}
pass.probs <- tire_freq/totals
pass.odds <- (1-pass.probs)/pass.probs
plot(passengers,log(pass.odds),ylab="odds of not-tires")
### sadly that did not show anything interesting
### what if we only plot the fords
range(my_df$Passengers)
passengers <- seq(0,11,by=1)
tire_freq = integer(12)
totals = integer(12)
for(i in passengers){
  temp_df <- subset(my_df, Passengers ==i)
  temp_df <- subset(temp_df, Make=="Ford")
  tires.freq <- as.data.frame(table(temp_df$Cause))[2,2]
  not.tires.freq <- as.data.frame(table(temp_df$Cause))[1,2]
  tire_freq[i] <- tires.freq
  totals[i] <- not.tires.freq + tires.freq
}
pass.probs <- tire_freq/totals
pass.odds <- (1-pass.probs)/pass.probs
plot(passengers,log(pass.odds),ylab="odds of not-tires")
# it looks the same
## just the non-fords
for(i in passengers){
  temp_df <- subset(my_df, Passengers ==i)
  temp_df <- subset(temp_df, Make=="Other")
  tires.freq <- as.data.frame(table(temp_df$Cause))[2,2]
  not.tires.freq <- as.data.frame(table(temp_df$Cause))[1,2]
  tire_freq[i] <- tires.freq
  totals[i] <- not.tires.freq + tires.freq
}
pass.probs <- tire_freq/totals
pass.odds <- (1-pass.probs)/pass.probs
plot(passengers,log(pass.odds),ylab="odds of not-tires")

### let's try vehicle age
range(my_df$VehicleAge)
v.a <- seq(0,5,by=1)
tire_freq = integer(6)
totals = integer(6)
for(i in passengers){
  temp_df <- subset(my_df, VehicleAge ==i)
  tires.freq <- as.data.frame(table(temp_df$Cause))[2,2]
  not.tires.freq <- as.data.frame(table(temp_df$Cause))[1,2]
  tire_freq[i] <- tires.freq
  totals[i] <- not.tires.freq + tires.freq
}
probs <- tire_freq/totals
odds <- (1-probs)/probs
plot(v.a,log(odds),ylab="odds of not-tires")

###lets do quick fit with va squared
fit.all.main <- glm(Cause ~ VehicleAge+ I(VehicleAge^2) + Passengers + Make, family=binomial(link = logit), data=ex2018)
summary(fit.all.main)
## is this model adequate?
residual.deviance <- summary(fit.all.main)$deviance
d.o.f <- summary(fit.all.main)$df.residual
pchisq(residual.deviance,d.o.f)
#no
### fuck it, lets just use step to search all the possible interactions