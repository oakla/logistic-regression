install.packages("oddsratio")
library(oddsratio)
library(Sleuth3)
my_df <- ex2018
my_df$Make <- factor(my_df$Make, levels=c("Other", "Ford"))
my_df$Cause <- factor(my_df$Cause, levels=c("NotTire", "Tire")) #apparently this is the default
#3. Use the model to give 95% CIs for the multiplicative effect on the odds of:
#   -the vehicle being a Ford,
#   -increasing the age by 1 year
#   -increasing the number of passengers by 1.
# *Interpret what these results mean for the overall questions of interest.

fit.1 <- glm(formula = Cause ~ VehicleAge + Make + Passengers + Make:Passengers, family = binomial(link = "logit"), data = my_df)

or_glm(data=my_df, model=fit.1, incr = list(Passengers=1,
                                            VehicleAge=1
))

exp(confint(fit.1)) #same result for confints above

exp(fit.1$coefficients)

#### The results are and meanings
# 
# Coeff -   Multiplicative effect - Meaning
# Ford  -   5.65                  - increase in odds
# VehicleAge- 2.37                - ditto
# etc