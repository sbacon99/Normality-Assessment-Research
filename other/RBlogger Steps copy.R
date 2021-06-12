# Working through RBlogger Article

plot(lm(mpg ~ wt, mtcars), which = 2, id.n = 0)

# generate data for this post
set.seed(20200825)
x <- sort(rnorm(20, 10, 3))
x

# naive quantiles 
rank(x) / length(x)
#>  [1] 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75
#> [16] 0.80 0.85 0.90 0.95 1.00

# This leaves us with a quantile of 1.00, which create problems

# 0 and 1 quantiles of a normal distribution
qnorm(c(0, 1))
#> [1] -Inf  Inf

q <- (rank(x) - .5) / length(x)
q
#>  [1] 0.025 0.075 0.125 0.175 0.225 0.275 0.325 0.375 0.425 0.475 0.525 0.575
#> [13] 0.625 0.675 0.725 0.775 0.825 0.875 0.925 0.975

# Now, we can see that our values have adjusted by 0.25

# R has a function that can do this procedure...See below
ppoints
#> function (n, a = if (n <= 10) 3/8 else 1/2) 
#> {
#>     if (length(n) > 1L) 
#>         n <- length(n)
#>     if (n > 0) 
#>         (1L:n - a)/(n + 1 - 2 * a)
#>     else numeric()
#> }
#> 
#> 

?qnorm # QUESTION: so this function allows us to convert from quantile to value??
round(qnorm(q, 100, 15))
#>  [1]  71  78  83  86  89  91  93  95  97  99 101 103 105 107 109 111 114 117 122
#> [20] 129

qnorm(q, mean(x), sd(x))
#>  [1]  4.087038  5.639679  6.502415  7.146107  7.680649  8.150992  8.580590
#>  [8]  8.983711  9.370120  9.747252 10.121407 10.498539 10.884947 11.288068
#> [15] 11.717667 12.188009 12.722552 13.366243 14.228980 15.781621

library(tibble)
library(ggplot2)

d <- tibble(
  x_sample = x, 
  quantile = ppoints(length(x_sample)),
  x_theoretical = qnorm(q, mean(x_sample), sd(x_sample))
)

# What is this function doing...
# - taking the original sample data from the top of the screen 'x'
# - creating a qq plot using 'x' as the sample data and randomly generating theoretical data
# QUESTION: 

ggplot(d) + 
  geom_point(aes(x = x_theoretical, y = x_sample)) +
  labs(
    x = "theoretical quantiles", 
    y = "sample quantiles"
  )


d$z_theoretical <- qnorm(d$quantile, 0, 1)

ggplot(d) + 
  geom_point(aes(x = z_theoretical, y = x_sample), size = 3) +
  geom_qq(aes(sample = x_sample), color = "red") + 
  labs(
    x = "theoretical quantiles", 
    y = "sample quantiles"
  )

# Adding the qq line

p <- ggplot(d) + 
  geom_point(aes(x = z_theoretical, y = x_sample)) + 
  geom_abline(
    aes(intercept = mean, slope = sd, color = "Naive: mean + SD * x"),
    data = tibble(sd = sd(d$x_sample), mean = mean(d$x_sample))
  ) +
  geom_qq_line(
    aes(sample = x_sample, color = "R: Something else"),
    # use the abline glyph in the legend
    key_glyph = draw_key_abline, fullrange = TRUE
  ) +
  labs(
    color = "Q-Q line",
    x = "theoretical quantiles", 
    y = "sample quantiles"
  ) + 
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top", legend.justification =  "left")
p  

?geom_qq_line

  
anchors <- tibble(
  x = qnorm(c(.25, .75)),
  y = quantile(d$x_sample, c(.25, .75))
)

p + 
  geom_point(
    aes(x = x, y = y), 
    data = anchors, 
    shape = 3, 
    size = 5, 
    stroke = 1.1, 
    color = "blue"
  )


# "We can alternatively use the median as a robust estimator of [the mean] and
# the interquartile range / 1.349 as a robust estimator of [the standard
# deviation]. (The more conventional estimates [of the sample mean and SD]
# will not work well when the data are substantially non-normal.)" [p. 39].

p + 
  geom_abline(
    aes(
      intercept = mean, 
      slope = sd, 
      color = "Robust: median + IQR / 1.349 * x"
    ),
    data = tibble(
      sd = IQR(d$x_sample) / 1.349, 
      mean = median(d$x_sample)
    )
  ) 

# Adding a confidence band

# Given z scores and n, produce a standard error
se_z <- function(z, n) {
  sqrt(pnorm(z) * (1 - pnorm(z)) / n) / dnorm(z)
}

band <- tibble(
  z = seq(-2.2, 2.2, length.out = 300),
  n = length(d$x_sample),
  
  sample_sd = sd(d$x_sample),
  se = sample_sd * se_z(z, n),
  line = mean(d$x_sample) + sample_sd * z,
  upper = line + 2 * se,
  lower = line - 2 * se,
  
  robust_sd = IQR(d$x_sample) / 1.349,
  robust_line = median(d$x_sample) + z * robust_sd,
  robust_se =  robust_sd * se_z(z, n),
  robust_upper = robust_line + 2 * robust_se,
  robust_lower = robust_line - 2 * robust_se,
)

ggplot(d) + 
  geom_point(aes(x = z_theoretical, y = x_sample)) + 
  
  geom_abline(
    aes(intercept = mean, slope = sd, color = "Naive"),
    data = tibble(sd = sd(d$x_sample), mean = mean(d$x_sample))
  ) +
  
  geom_ribbon(
    aes(x = z, ymax = upper, ymin = lower, color = "Naive"), 
    data = band, 
    fill = NA, show.legend = FALSE
  ) +  geom_ribbon(
    aes(x = z, ymax = robust_upper, ymin = robust_lower, color = "Naive"), 
    data = band, 
    fill = NA, show.legend = FALSE
  )+
  labs(
    color = "Q-Q line",
    x = "theoretical quantiles", 
    y = "sample quantiles"
  ) + 
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top", legend.justification =  "left")

# Performing this on mtcars dataset

# Set margins on Q-Q plots
par(mar = c(4, 2, 1, 2))

# Use patchwork to capture the plots and combine them 
# into a side by side display.
install.packages('patchwork')
install.packages('gridGraphics')
library(patchwork)
library(gridGraphics)
p1 <- wrap_elements(~ car::qqPlot(x))

p2 <- wrap_elements(~ {
  car::qqPlot(x)
  lines(band$z, band$robust_line, col = "black", lwd = 2)
  lines(band$z, band$robust_upper, col = "black", lwd = 2)
  lines(band$z, band$robust_lower, col = "black", lwd = 2)
}) # look at this section in article

p1 + p2

# Worm plot
# removing the vertical component from a qq-plot

par(mar = c(4.5, 4.5, 1, 2))

# I don't know what's up with that error message.

# use scale() to transform in to z-score
install.packages('gamlss')
library(gamlss)
gamlss::wp(
  resid = scale(d$x_sample), 
  xlim.all = 2.5, 
  line = FALSE
)
#> Error in coef(fit): object 'fit' not found

d$line <- mean(d$x_sample) + d$z_theoretical * sd(d$x_sample)

ggplot(d) + 
  geom_point(
    aes(x = z_theoretical, y = x_sample - line)
  ) + 
  geom_hline(yintercept = 0) +
  geom_ribbon(
    # don't add the line to the SEs
    aes(x = z, ymax = 2 * se, ymin = - 2 * se), 
    data = band, 
    fill = NA, 
    color = "black"
  ) + 
  labs(
    color = "Q-Q line",
    x = "theoretical quantiles", 
    y = "centered sample quantiles"
  ) + 
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top", legend.justification =  "left")

?geom_qq_band
?geom_ribbon

# Creating a function in 'ggplot2' that has the capability of creating worm plots
stat_worm <- function(
  mapping = NULL, 
  data = NULL,
  geom = "point", 
  position = "identity",
  ...,
  # important part
  robust = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    # important part
    stat = StatWorm,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    # important part
    params = list(
      robust = robust,
      na.rm = na.rm,
      ...
    ),
  )
}

StatWorm <- ggproto(
  "StatWorm",
  Stat,
  default_aes = aes(
    y = after_stat(sample), 
    x = after_stat(theoretical)
  ),
  required_aes = c("sample"),
  compute_group = function(data, scales, robust = FALSE) {
    sample <- sort(data$sample)
    n <- length(sample)
    quantiles <- ppoints(n)
    
    if (robust) {
      mean <- median(sample)
      sd <- IQR(sample) / 1.349
    } else {
      mean <- mean(sample)
      sd <- sd(sample)
    }
    
    scaled_theoretical <- qnorm(quantiles, mean, sd)
    theoretical <- qnorm(quantiles)
    
    data.frame(
      # detrended
      sample = sample - scaled_theoretical, 
      theoretical = theoretical
    )
  }
)

ggplot(d) + 
  stat_worm(aes(sample = x_sample), robust = FALSE) +
  # test against above code
  geom_point(aes(x = z_theoretical, y = x_sample - line)) +
  labs(y = "centered sample")

d$robust_line <- 
  median(d$x_sample) + d$z_theoretical * IQR(d$x_sample) / 1.349

ggplot(d) + 
  stat_worm(aes(sample = x_sample), robust = TRUE) +
  # test against above code
  geom_point(aes(x = z_theoretical, y = x_sample - robust_line)) +
  labs(y = "centered sample")

stat_worm_band <- function(
  mapping = NULL, 
  data = NULL,
  geom = "ribbon", 
  position = "identity",
  ...,
  # important part
  robust = FALSE,
  band_width = .95,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    # important part
    stat = StatWormBand,
    geom = GeomRibbonHollow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    # important part
    params = list(
      robust = robust,
      band_width = band_width,
      na.rm = na.rm,
      ...
    ),
  )
}

GeomRibbonHollow <- ggproto(
  # Make this geom
  "GeomRibbonHollow", 
  # inheriting from:
  GeomRibbon,
  # but changing this:
  default_aes = aes(
    colour = "black", 
    fill = NA, 
    size = 0.5, 
    linetype = 1,
    alpha = NA)
)

StatWormBand <- ggproto(
  "StatWormBand", 
  Stat,
  default_aes = aes(
    x = after_stat(theoretical),
  ),
  required_aes = c("sample"),
  compute_group = function(data, scales, robust = FALSE, band_width = .95) {
    sample <- sort(data$sample)
    n <- length(sample)
    quantiles <- ppoints(n)
    
    if (robust) {
      mean <- median(sample)
      sd <- IQR(sample) / 1.349
    } else {
      mean <- mean(sample)
      sd <- sd(sample)
    }
    
    theoretical <- qnorm(quantiles)
    z_range <- seq(min(theoretical), max(theoretical), length.out = 80)
    
    # i.e., convert .95 to .025 and .975 and convert those to z scores
    band_z <- qnorm((1 + c(-band_width, band_width)) / 2)
    
    se_z <- function(z, n) {
      sqrt(pnorm(z) * (1 - pnorm(z)) / n) / dnorm(z)
    }
    
    data.frame(
      theoretical = z_range,
      ymin = band_z[1] * se_z(z_range, n) * sd,
      ymax = band_z[2] * se_z(z_range, n) * sd
    )
  }
)

ggplot(d) + 
  stat_worm(aes(sample = x_sample)) +
  # original version from above
  geom_ribbon(
    aes(x = z, ymax = upper - line, ymin = lower - line), 
    data = band, 
    fill = NA,
    color = "blue"
  ) +
  stat_worm_band(aes(sample = x_sample)) + 
  labs(y = "centered sample")

ggplot(d) + 
  stat_worm(aes(sample = x_sample), robust = TRUE) +
  # for comparison
  geom_ribbon(
    aes(
      x = z, 
      ymax = robust_upper - robust_line, 
      ymin = robust_lower - robust_line
    ), 
    data = band, 
    fill = NA,
    color = "blue"
  ) +
  stat_worm_band(aes(sample = x_sample), robust = TRUE) + 
  labs(y = "centered sample")

