# Creating visualizations for the Review of Relevant Scholarship

install.packages('ggplot2')
library(ggplot2)
install.packages('qqplotr')
library(qqplotr)
set.seed(1234)

# Image 1: basic normal distribution

normData <- data.frame(value = rnorm(10000, mean = 10, sd = 5))  

image1 <- ggplot(normData, aes(x = value)) +
  geom_histogram(aes(y = ..count.., ), 
                 binwidth = .5, colour = "blue", fill = "white") +
  xlab("Value") + ylab("Frequency") +
  ggtitle("Figure 1a")

image1

# Image 2: skewed distribution

skewedData <- data.frame(value = rnbinom(10000, 5, .5))

image2 <- ggplot(skewedData, aes(x = value)) +
  geom_histogram(aes(y = ..count.., ), 
                 binwidth = .5, colour = "red", fill = "white") +
  xlab("Value") + ylab("Frequency") +
  ggtitle("Figure 1b")

image2

# Image 3: Q-Q Plot with no reference line

data <- data.frame(values = mtcars$mpg)

image3 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_point(colour = "black") +
  
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Figure 2a")

image3

# Image 4: Detrended Q-Q Plot with reference line

image4 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_point(colour = "black", detrend = TRUE) +
  stat_qq_line(detrend = TRUE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Figure 2c")

image4


# Image 5: Trended Q-Q Plot with reference line and confidence band

image5 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_line(detrend = FALSE) +
  geom_qq_band(bandType = "ts", fill = 'red', alpha = 0.5) +
  stat_qq_point(colour = "black", detrend = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Figure 2b")

image5

