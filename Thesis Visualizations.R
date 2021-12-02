# packages
install.packages("ggplot2")
install.packages('qqplotr')
install.packages('e1071')
install.packages("gridExtra")

library(gridExtra)
library(ggplot2)
library(qqplotr)
library(e1071)

#####################

unif <- data.frame(value = runif(10000, -3, 3))
norm <- data.frame(value = rnorm(10000, 0, 1))
f <- data.frame(value = rf(10000, 10, 20))


unifPlot <- ggplot(unif, aes(x = value)) + geom_histogram(aes(y = ..density.., ), color = "blue", fill = "grey") +
  xlab("Value") + ylab("")
unifPlot

normPlot <- ggplot(norm, aes(x = value)) + geom_histogram(aes(y = ..density.., ), colour = "blue", fill = "grey") + xlab("") + ylab("Density")
normPlot

fPlot <- ggplot(f, aes(x = value)) + geom_histogram(aes(y = ..density.., ), colour = "blue", fill = "grey") + xlab("") + ylab("")
fPlot

grid.arrange(normPlot, unifPlot, fPlot, nrow = 1)

#####################

set.seed(21)
norm1 <- data.frame(value = rnorm(100, mean = 0, sd = 1))
norm2 <- data.frame(value = rnorm(1000, mean = 0, sd = 1))
norm3 <- data.frame(value = rnorm(10000, mean = 0, sd = 1))

norm1plot <- ggplot(data = norm1, mapping = aes(value)) +
  geom_histogram(aes(y=..density..)) + labs(x = "Value", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = mean(norm1$value),sd = sd(norm1$value)),col = "red",size = 1)

norm2plot <- ggplot(data = norm2, mapping = aes(value)) +
  geom_histogram(aes(y=..density..)) + labs(x = "Value", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = mean(norm2$value),sd = sd(norm2$value)),col = "red",size = 1)

norm3plot <- ggplot(data = norm3, mapping = aes(value)) +
  geom_histogram(aes(y=..density..)) + labs(x = "Value", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = mean(norm3$value),sd = sd(norm3$value)),col = "red",size = 1)

grid.arrange(norm1plot, norm2plot, norm3plot, nrow = 1)
