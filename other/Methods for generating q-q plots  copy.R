# Generating data in R
set.seed(1234)
?rnorm

# General formula for generating random data
# rnorm(number_of_values, mean, sd)
rnorm(20,mean = 0, sd = 1)


# USING THE 'GGPLOT2' PACKAGE

# install.packages("ggplot2")
# library(ggplot2)
# Generating qqplot -- qqplot()
data <- mtcars
?qqnorm

qqnorm(data$mpg, main = "Q-Q Plot for mtcars Dataset", 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")

# Generating the reference line -- qqline()
qqline(data$mpg, col = "steelblue", lwd = 2) 

# Generating qqplot using ggplot2
?geom_qq_line
?stat_qq_line
?stat_qq
plot <- ggplot(mtcars, aes(sample = mpg))

# complete q-q plot -- using "ggplot2" library
plot + stat_qq() + stat_qq_line()



# USING THE 'QQPLOTR' PACKAGE
install.packages("qqplotr")
library(qqplotr)
plot + stat_qq_band() + stat_qq_line() + stat_qq_point()

?stat_qq_band
?stat_qq_line
?stat_qq_point

?kurtosis()
library(base)
kurtosis(mtcars$mpg)
