# Survey Data ALL GRAPHS

install.packages('ggplot2')
library(ggplot2)
install.packages('qqplotr')
library(qqplotr)
install.packages('e1071')
library(e1071)
# All confidence bands are POINTWISE bands

# DATASET 0 (Graph 0)

# Always run set.seed function before running rest of code
set.seed(12345)
normFrame0 <- data.frame(value = rnorm(30, mean = 10, sd = 5))
normVector0 <- normFrame0$value
normFrame0

normVector0

Dataset0noRef <- ggplot(data = normFrame0, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

Dataset0ref <- ggplot(data = normFrame0, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

Dataset0bands <- ggplot(data = normFrame0, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

Dataset0noRef
Dataset0ref
Dataset0bands

# Dataset 1 (Graph 1)

# Always run set.seed function before running rest of code
set.seed(189893)
normFrame1 <- data.frame(value = rnorm(30, mean = 10, sd = 5))
normVector1 <- normFrame1$value

normFrame1
normVector1

Dataset1noRef <- ggplot(data = normFrame1, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset1ref <- ggplot(data = normFrame1, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset1bands <- ggplot(data = normFrame1, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset1noRef
Dataset1ref
Dataset1bands

# Dataset 2 (Graph 2)

# Always run set.seed function before running rest of code
set.seed(4321)
fFrame2 <- data.frame(value = rf(30, 3, 10))
fVector2 <- fFrame2$value

fFrame2
fVector2

Dataset2noRef <- ggplot(data = fFrame2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset2ref <- ggplot(data = fFrame2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset2bands <- ggplot(data = fFrame2, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset2noRef
Dataset2ref
Dataset2bands

# Dataset 3 (Graph 3)

# Always run set.seed function before running rest of code
set.seed(11111)
normFrame3 <- data.frame(value = rnorm(30, mean = 10, sd = 5))
normVector3 <- normFrame3$value

normFrame3
normVector3

Dataset3noRef <- ggplot(data = normFrame3, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset3ref <- ggplot(data = normFrame3, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset3bands <- ggplot(data = normFrame3, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset3noRef
Dataset3ref
Dataset3bands

# Dataset 4 (Graph 4)

# Always run set.seed function before running rest of code
set.seed(789)
unifFrame4 <- data.frame(value = runif(30, min = 5, max = 25))
unifVector4 <- unifFrame4$value

unifFrame4
unifVector4

Dataset4noRef <- ggplot(data = unifFrame4, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset4ref <- ggplot(data = unifFrame4, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset4bands <- ggplot(data = unifFrame4, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset4noRef
Dataset4ref
Dataset4bands

# Dataset 5 (Graph 5)

# Always run set.seed function before running rest of code
set.seed(11)
normFrame5 <- data.frame(value = rnorm(30, mean = 15, sd = 5))
normVector5 <- normFrame5$value

normFrame5
normVector5

Dataset5noRef <- ggplot(data = normFrame5, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset5ref <- ggplot(data = normFrame5, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset5bands <- ggplot(data = normFrame5, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset5noRef
Dataset5ref
Dataset5bands

# Dataset 6 (Graph 6)

# Always run set.seed function before running rest of code
set.seed(1339)
unifFrame6 <- data.frame(value = runif(30, min = 10, max = 20))
unifVector6 <- unifFrame6$value

unifFrame6
unifVector6

Dataset6noRef <- ggplot(data = unifFrame6, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset6ref <- ggplot(data = unifFrame6, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset6bands <- ggplot(data = unifFrame6, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset6noRef
Dataset6ref
Dataset6bands

# Dataset 7 (Graph 7)

# N = 30
# Always run set.seed function before running rest of code
set.seed(65133)
fFrame7 <- data.frame(value = rf(30, 2, 10))
fVector7 <- fFrame7$value

fFrame7
fVector7

Dataset7noRef <- ggplot(data = fFrame7, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset7ref <- ggplot(data = fFrame7, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset7bands <- ggplot(data = fFrame7, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset7noRef
Dataset7ref
Dataset7bands

# N = 20
set.seed(12345)
fFrame7_2 <- data.frame(value = rf(20, 2, 10))
fVector7_2 <- fFrame7_2$value

fFrame7_2
fVector7_2

Dataset7noRef_2 <- ggplot(data = fFrame7_2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset7ref_2 <- ggplot(data = fFrame7_2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset7bands_2 <- ggplot(data = fFrame7_2, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset7noRef_2
Dataset7ref_2
Dataset7bands_2

# Dataset 8 (Graph 8)

# N = 30
# Always run set.seed function before running rest of code
set.seed(32144441)
normFrame8 <- data.frame(value = rnorm(30, mean = 15, sd = 5))
normVector8 <- normFrame8$value

normFrame8
normVector8

Dataset8noRef <- ggplot(data = normFrame8, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset8ref <- ggplot(data = normFrame8, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset8bands <- ggplot(data = normFrame8, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset8noRef
Dataset8ref
Dataset8bands

# N = 20
set.seed(11111)
normFrame8_2 <- data.frame(value = rnorm(20, mean = 15, sd = 5))
normVector8_2 <- normFrame8_2$value

normFrame8_2
normVector8_2

Dataset8noRef_2 <- ggplot(data = normFrame8_2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset8ref_2 <- ggplot(data = normFrame8_2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset8bands_2 <- ggplot(data = normFrame8_2, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset8noRef_2
Dataset8ref_2
Dataset8bands_2

