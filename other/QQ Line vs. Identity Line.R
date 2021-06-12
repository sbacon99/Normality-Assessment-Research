library(ggplot2)
library(qqplotr)

View(mtcars)

p <- ggplot(mtcars, aes(sample = mpg)) +
  stat_qq_point(detrend = FALSE, identity = TRUE) +
   stat_qq_line(detrend = FALSE, identity = TRUE) + 
  labs(title = "Q-Q Plot with Identity Line")
  
# R Default
q <- ggplot(mtcars, aes(sample = mpg)) +
  stat_qq_point(detrend = TRUE, identity = FALSE) +
  stat_qq_line(detrend = TRUE, identity = FALSE) +
  labs(title = "Q-Q Plot with QQ Line")


p
q

?labs
