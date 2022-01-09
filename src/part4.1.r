# 4. Use your model to make predictions for each combination of VehicleAge, Passengers and Make. Visualise how the predictions differ for vehicles of different ages and number of passengers for both Fords and other vehicles. The following code (modified from Week 5) may be useful.

passengers <- seq(0,10,1)
ages <- seq(0,5,1)
makes <- c("Other","Ford")
grid <- expand.grid(Passengers=passengers, VehicleAge=ages, Make=makes)

pred.response <- predict(fit.1, newdata=grid,  type="response")
data.frame(Prediction=pred.response, grid)


#table(my_df$Cause, Prediction > 0.5)
# this can be used for plotting... I think

pred.response <- predict(fit.1, type="response")
df_plus <- data.frame(Prediction=pred.response, my_df)

# subset was being a pain in the arse
#subset(df_plus, Make=="Ford",Passengers > 4)
small.set <- df_plus[df_plus$Make == "Ford" & df_plus$Passengers > 3 & df_plus$VehicleAge>2,]
small.set[,c(1,5)]
# in the small.set we can see that there are a good number of Tire causes (as awful as that is)

# mini version
table(small.set$Cause, small.set$Prediction < 0.5)
# full
table(df_plus$Cause, df_plus$Prediction < 0.5)
 


outcomes.table <- table(df_plus$Cause, df_plus$Prediction < 0.5)
class(outcomes.table)
cbind(outcomes.table)

colnames(outcomes.table) <- c("accuracy")
