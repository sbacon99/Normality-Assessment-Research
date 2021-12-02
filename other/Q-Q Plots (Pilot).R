# Samples (PILOT)

install.packages('ggplot2')
library(ggplot2)
install.packages('qqplotr')
library(qqplotr)
install.packages('e1071')
library(e1071)

# All confidence bands are POINTWISE bands

# DATASET 0 (Graph 0)
set.seed(12345)
normFrame0 <- data.frame(value = rnorm(30, mean = 10, sd = 5))
normVector0 <- normFrame0$value

Dataset0noRef <- ggplot(data = normFrame0, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Neither") +
  theme(plot.title = element_text(size = 10))

Dataset0ref <- ggplot(data = normFrame0, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Reference Line") +
  theme(plot.title = element_text(size = 10))

Dataset0bands <- ggplot(data = normFrame0, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Ref. Line and Bands") +
  theme(plot.title = element_text(size = 10))

Dataset0noRef
Dataset0ref
Dataset0bands

shapiro.test(normVector0)
t.test(normVector0)

# DATASET 1 (Graph 1)
set.seed(189893)
normFrame1 <- data.frame(value = rnorm(30, mean = 10, sd = 5))
normVector1 <- normFrame1$value

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

shapiro.test(normVector1)
t.test(normVector1)

grid.arrange(Dataset1noRef, Dataset1ref, nrow = 1)


# DATASET 2 (Graph 2)
set.seed(4321)
fFrame2 <- data.frame(value = rf(30, 3, 10))
fVector2 <- fFrame2$value

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

shapiro.test(fVector2)
t.test(fVector2)

# DATASET 3 (Graph 3)
set.seed(11111)
normFrame3 <- data.frame(value = rnorm(30, mean = 10, sd = 5))
normVector3 <- normFrame3$value

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

shapiro.test(normVector3)
t.test(normVector3)

# DATASET 4 (Graph 4)
set.seed(789)
unifFrame4 <- data.frame(value = runif(30, min = 5, max = 25))
unifVector4 <- unifFrame4$value

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

shapiro.test(unifVector4)
t.test(unifVector4)

# DATASET 5 (Graph 5)
set.seed(11)
normFrame5 <- data.frame(value = rnorm(30, mean = 15, sd = 5))
normVector5 <- normFrame5$value

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

shapiro.test(normVector5)
t.test(normVector5)

# DATASET 6 (Graph 6)
set.seed(1339)
unifFrame6 <- data.frame(value = runif(30, min = 10, max = 20))
unifVector6 <- unifFrame6$value

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

shapiro.test(unifVector6)
t.test(unifVector6)

# DATASET 7 (Graph 7)
# (N = 20)
set.seed(12345)
fFrame7_2 <- data.frame(value = rf(20, 2, 10))
fVector7_2 <- fFrame7_2$value

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

shapiro.test(fVector7_2)
t.test(fVector7_2)

# DATASET 8 (Graph 8)
# (N = 20)
set.seed(11111)
normFrame8_2 <- data.frame(value = rnorm(20, mean = 15, sd = 5))
normVector8_2 <- normFrame8_2$value

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

shapiro.test(normVector8_2)
t.test(normVector8_2)

