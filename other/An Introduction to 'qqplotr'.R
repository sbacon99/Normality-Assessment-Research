# Confidence Bands in R

install.packages("devtools")
library(devtools)
devtools::install_github("aloy/qqplotr")

x <- 
require(qqplotr)
library(qqplotr)

set.seed(0)
smp <- data.frame(norm = rf(50, 5, 10))

gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
gg

?stat_qq_band
# 

gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
  geom_qq_band(bandType = "ks", mapping = aes(fill = "KS"), alpha = 0.5) +
  geom_qq_band(bandType = "ts", mapping = aes(fill = "TS"), alpha = 0.5) +
  geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.5) +
  #geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), alpha = 0.5) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_fill_discrete("Bandtype")
gg

# Using new dataset
di <- "exp" # exponential distribution
dp <- list(rate = 2) # exponential rate parameter

gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
  stat_qq_band(distribution = di, dparams = dp) +
  stat_qq_line(distribution = di, dparams = dp) +
  stat_qq_point(distribution = di, dparams = dp) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
gg

di <- "exp"
dp <- list(rate = 2)
de <- TRUE # enabling the detrend option

gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
  stat_qq_band(distribution = di, dparams = dp, detrend = de) +
  stat_qq_line(distribution = di, dparams = dp, detrend = de) +
  stat_qq_point(distribution = di, dparams = dp, detrend = de) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
gg

install.packages("lattice")
data("barley", package = "lattice")

gg <- ggplot(data = barley, mapping = aes(sample = yield, color = site, fill = site)) +
  stat_qq_band(alpha=0.5) +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~ site) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
gg


# Probability Plots
gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
  stat_pp_band() +
  stat_pp_line() +
  stat_pp_point() +
  labs(x = "Probability Points", y = "Cumulative Probability")
gg

dp <- list(mean = 2, sd = 2) # shifted and rescaled Normal parameters

gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
  stat_pp_band(dparams = dp) +
  stat_pp_line() +
  stat_pp_point(dparams = dp) +
  labs(x = "Probability Points", y = "Cumulative Probability")
gg

gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
  stat_pp_band() +
  stat_pp_line(ab = c(.2, .5)) + # intercept = 0.2, slope = 0.5
  stat_pp_point() +
  labs(x = "Probability Points", y = "Cumulative Probability")
gg

di <- "exp"
dp <- list(rate = .022) # value is based on some empirical tests
de <- TRUE

gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
  stat_pp_band(distribution = di, detrend = de, dparams = dp) +
  stat_pp_line(detrend = de) +
  stat_pp_point(distribution = di, detrend = de, dparams = dp) +
  labs(x = "Probability Points", y = "Cumulative Probability") +
  scale_y_continuous(limits = c(-.5, .5))
gg

install.packages("shinyBS")
runShinyExample()
