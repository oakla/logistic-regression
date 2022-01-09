#1. Using techniques from Week 10 calculate the relevant odds ratio and create a confidence interval for it. The following command may be useful. table(ex2018$Make, ex2018$Cause)
## lets try to get the odds ratio
install.packages("Sleuth3")
library(Sleuth3)
my_df <- ex2018
my_df$Make <- factor(my_df$Make, levels=c("Other", "Ford"))

fisher.test(table(my_df$Make,my_df$Cause))
# this gives an odds ratio of 15.7628

#lets check that odds ratio against what we would calculate ourselves
prop.test(table(my_df$Make,my_df$Cause))
my_tab = table(my_df$Make,my_df$Cause)
(my_tab[1,1] * my_tab[2,2])/(my_tab[2,1]*my_tab[1,2])
  
(my_tab[1,1] * my_tab[2,2])/(my_tab[2,1]*my_tab[1,2])
