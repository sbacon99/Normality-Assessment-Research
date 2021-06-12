# CODE TO HELP EXPLAIN A Q-Q PLOT

set.seed(1234)
rnorm(50, mean = 50 )

summary(mtcars$mpg)
sd(mtcars$mpg)
# mean mpg = 20.09, sd mpg = 6.026948

normal <- rnorm(32, mean = 20.09, sd = 6.026948)

