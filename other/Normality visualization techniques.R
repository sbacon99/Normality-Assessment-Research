install.packages("dplyr")
install.packages("ggpubr")

library("dplyr")
library("ggpubr")

set.seed(1234)

# Density Plot
ggdensity(mtcars$mpg, 
          main = "Density plot of mpg",
          xlab = "mpg")

ggqqplot(mtcars$mpg)
?ggqqplot
