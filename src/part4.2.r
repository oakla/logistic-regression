# Graphing
## Get the model
my_df <- ex2018
my_df$Make <- factor(my_df$Make, levels=c("Other", "Ford"))
my_df$Cause <- factor(my_df$Cause, levels=c("NotTire", "Tire")) #apparently this is the default
fit.1 <- glm(formula = Cause ~ VehicleAge + Make + Passengers + Make:Passengers, family = binomial(link = "logit"), data = my_df)

pred.response <- predict(fit.1, newdata=grid,  type="response")
all.combos.preds<-data.frame(Prediction=pred.response, grid)

install.packages("gg.plot2")
library(ggplot2)
p <- ggplot(data=all.combos.preds, aes(x=Passengers, y=VehicleAge, size=Prediction, color=Make)) + geom_point()
p + 
  #scale_color_gradient(low="cyan",high="red") +
  xlab("Number of Passengers") +
  geom_jitter()+
  ylab("Age of Vehicle (years)") +
  ggtitle("All the Variables in one monster plot") +
  scale_fill_manual(name="Area") +
  #guides(size= FALSE) + 
  labs(color=expression ("Make"),
       caption = "Figure is a graphic that shows predictions of Cause made using the threee other variables.")
