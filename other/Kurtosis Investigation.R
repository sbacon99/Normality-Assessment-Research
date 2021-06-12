# Kurtosis Exploration

install.packages('ggplot2')
library(ggplot2)
install.packages('qqplotr')
library(qqplotr)
set.seed(1234)

install.packages('e1071')
library(e1071)


# 3 types of kurtosis: mesokurtic (no excess), leptokurtic (positive excess),
# and platykurtic (negative excess)


# Kurtosis for Normal Distribution -- mesokurtic
set.seed(12345)
normVector <- rnorm(30, mean = 10, sd = 5)
kurtosis(normVector)
?kurtosis

set.seed(12345)
normFrame <- data.frame(value = rnorm(30, mean = 10, sd = 5))

normQQ <- ggplot(data = normFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Normal Distribution (mesokurtic)")

normQQ

shapiro.test(normVector)

?sw
# Kurtosis for F Distribution -- leptokurtic
set.seed(1234)
fVector <- rf(20, 2, 8)
shapiro.test(fVector)

kurtosis(fVector)



hist(fVector)

set.seed(1234)
fFrame <- data.frame(value = rf(20, 2, 8))

shapiro.test(fVector)

fQQ <- ggplot(data = fFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("F Distribution (leptokurtic)")

fQQ

# Kurtosis for Cauchy Distribution -- leptokurtic
?rcauchy
cauchyData <- rcauchy(50)
kurtosis(cauchyData)

hist(cauchyData)

cauchyFrame <- data.frame(value = rcauchy(55))

cauchyQQ <- ggplot(data = cauchyFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Cauchy Distribution (leptokurtic)")

cauchyQQ

# Uniform Distribution 

unifData <- runif(30, 10, 20)
kurtosis(unifData)

hist(unifData)

unifFrame <- data.frame(value = runif(30, 10, 20))

unifQQ <- ggplot(data = unifFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Uniform Distribution ")

unifQQ


