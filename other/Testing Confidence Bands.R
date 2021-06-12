# Testing confidence bands with the mtcars dataset
install.packages('ggplot2')
library(ggplot2)
install.packages('qqplotr')
library(qqplotr)

x <- data.frame(mtcars$mpg)

gg <- ggplot(data = x, mapping = aes(sample = norm)) +
  geom_qq_band(bandType = "ks", mapping = aes(fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(fill = "TS"), alpha = 0.5) +
  geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.5) +
  geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")
gg
