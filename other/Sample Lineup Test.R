# Creating lineup test 

#install.packages('ggplot2')
#install.packages('qqplotr')
#library(ggplot2)
#library(qqplotr)
#set.seed(1234)

# Randomly generated data...
g1g5data <- data.frame(value =   )
g4g6data <- data.frame(value = )

# Graph 1: Randomly Generated Data, Robust

g1 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Raondom Data, Robust Q-Q Line", subtitle = "'mtcars' Dataset")

g1

# Graph 2: Static Data, Robust

g2 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Static Data, Robust Q-Q Line", subtitle = "'mtcars' Dataset")

g2

# Graph 3: Static Data, w/ CI Bands

g3 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_line() +
  geom_qq_band(bandType = "ts", fill = 'red', alpha = 0.5) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Static Data, CI Bands", subtitle = "'mtcars' Dataset")

g3

# Graph 4: Randomly Generated Data, w/ CI Bands
g4 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_line() +
  geom_qq_band(bandType = "ts", fill = 'red', alpha = 0.5) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Random Data, CI Bands", subtitle = "'mtcars' Dataset")

g4

# Graph 5: Randomly Generated, Non-Robust

g5 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = TRUE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Random Data, Non-Robust Q-Q Line", subtitle = "'mtcars' Dataset")

g5

# Graph 6: Randomly Generated Data, w/out CI Bands



# Graph 7: Static Data, Non-Robust
g7 <- ggplot(data = data, mapping = aes(sample = values)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = TRUE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Static Data, Non-Robust Q-Q Line", subtitle = "'mtcars' Dataset")

g7

# Graph 8: Static Data, w/out CI Bands


# Questions for Dr. T
# 1. How should we go about creating the random data?
# 2. For the with/without CI bands (3 & 8, 4 & 6), do we want 
#    robust or non-robust Q-Q Lines. Maybe we could use two types of Bands
#    instead...