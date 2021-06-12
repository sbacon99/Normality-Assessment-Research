# Survey QQ-Plot Data

install.packages('ggplot2')
library(ggplot2)
install.packages('qqplotr')
library(qqplotr)
set.seed(1234)

install.packages('e1071')
library(e1071)

library(rsconnect)

# DATASET 1 
# Always run set.seed function before running rest of code
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
normFrame1 <- data.frame(value = mydata[,100])
normVector1 <- normFrame1$value

Dataset1 <- ggplot(data = normFrame1, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 1: Normal")

Dataset1

shapiro.test(normVector1)
t.test(normVector1)

# DATASET 2 (graph 2)
# Always run set.seed function before running rest of code
set.seed(4321)
fFrame2 <- data.frame(value = rf(20, 3, 10))
fVector2 <- fFrame2$value
shapiro.test(fVector2)

fFrame2
fVector2

Dataset2 <- ggplot(data = fFrame2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  stat_qq_band() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 2: Non-normal (F Dist.)")

Dataset2

kurtosis(fVector2)
shapiro.test(fVector2)
t.test(fVector2)


# DATASET 3 (graph 3)
# Always run set.seed function before running rest of code
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
normFrame3 <- data.frame(value = mydata[,318])
normVector3 <- normFrame3$value

normFrame3
normVector3

Dataset3 <- ggplot(data = normFrame3, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 3: Normal")

Dataset3

shapiro.test(normVector3)
t.test(normVector3)


# DATASET 4 (graph 4)
# Always run set.seed function before running rest of code
set.seed(7894)
unifFrame4 <- data.frame(value = runif(20, min = 5, max = 25))
unifVector4 <- unifFrame4$value


unifFrame4
unifVector4

Dataset4 <- ggplot(data = unifFrame4, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 4: Uniform")

Dataset4

kurtosis(unifVector4)
shapiro.test(unifVector4)
t.test(unifVector4)


# DATASET 5 (graph 5)
# Always run set.seed function before running rest of code
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
normFrame5 <- data.frame(value = mydata[,7])
normVector5 <- normFrame5$value

normFrame5
normVector5

Dataset5 <- ggplot(data = normFrame5, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 5: Normal")

Dataset5

shapiro.test(normVector5)
t.test(normVector5)


# DATASET 6 (graph 6)
# Always run set.seed function before running rest of code
set.seed(19581)
unifFrame6 <- data.frame(value = runif(20, min = 10, max = 20))
unifVector6 <- unifFrame6$value


unifFrame6
unifVector6

Dataset6 <- ggplot(data = unifFrame6, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 6: Uniform")

Dataset6

kurtosis(unifVector6)
shapiro.test(unifVector6)
t.test(unifVector6)


# DATASET 7 (graph 7)
# Always run set.seed function before running rest of code
set.seed(651133)
fFrame7 <- data.frame(value = rf(20, 2, 10))
fVector7 <- fFrame7$value

fFrame7
fVector7

Dataset7 <- ggplot(data = fFrame7, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 7: Non-normal (F Dist.)")

Dataset7

kurtosis(fVector7)
shapiro.test(fVector7)
t.test(fVector7)

# DATASET 8 
# Always run set.seed function before running rest of code
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
normFrame8 <- data.frame(value = mydata[,16])
normVector8 <- normFrame8$value

Dataset8 <- ggplot(data = normFrame8, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 8: Normal")
Dataset8

shapiro.test(normVector8)
t.test(normVector8)

# TEST DATASET (graph 0)
# Always run set.seed function before running rest of code
set.seed(321441)
normFrame0 <- data.frame(value = rnorm(20, mean = 15, sd = 5))
normVector0 <- normFrame0$value

normFrame0
normVector0

Dataset0 <- ggplot(data = normFrame0, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("Dataset 0: Normal")

Dataset0

kurtosis(normVector0)
shapiro.test(normVector0)
t.test(normVector0)



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


