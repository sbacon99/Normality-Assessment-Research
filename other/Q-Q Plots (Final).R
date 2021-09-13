# Samples (FINAL)

install.packages("ggplot2")
library(ggplot2)
install.packages('qqplotr')
library(qqplotr)
install.packages('e1071')
library(e1071)

# DATASET 0 (graph 0)
set.seed(21)
frame0 <- data.frame(value = rnorm(20, mean = 15, sd = 5))
vector0 <- frame0$value

Dataset0noRef <- ggplot(data = frame0, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(0,30))

Dataset0ref <- ggplot(data = frame0, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+ coord_cartesian(ylim = c(0,30))

Dataset0bands <- ggplot(data = frame0, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+ coord_cartesian(ylim = c(0,30))

Dataset0ref

shapiro.test(vector0)
t.test(vector0)

# DATASET 1 (graph 1)
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
frame1 <- data.frame(value = mydata[,7])
vector1 <- frame1$value

Dataset1noRef <- ggplot(data = frame1, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+ coord_cartesian(ylim = c(-10,35))

Dataset1ref <- ggplot(data = frame1, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+ coord_cartesian(ylim = c(-10,35))

Dataset1bands <- ggplot(data = frame1, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-10,35))

Dataset1noRef
Dataset1ref
Dataset1bands

shapiro.test(vector1)
t.test(vector1)

# DATASET 2 (graph 2)
set.seed(2320)
mydata <- replicate(1000,rf(20,3,10))
frame2 <- data.frame(value = mydata[,3])
vector2 <- frame2$value

Dataset2noRef <- ggplot(data = frame2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-1.5,3))

Dataset2ref <- ggplot(data = frame2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-1.5,3))

Dataset2bands <- ggplot(data = frame2, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-1.5,3))

Dataset2noRef
Dataset2ref
Dataset2bands

shapiro.test(vector2)
t.test(vector2)

# DATASET 3 (graph 3)
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
frame3 <- data.frame(value = mydata[,318])
vector3 <- frame3$value

Dataset3noRef <- ggplot(data = frame3, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-6,20))

Dataset3ref <- ggplot(data = frame3, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-6,20))

Dataset3bands <- ggplot(data = frame3, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-6,20))

Dataset3noRef
Dataset3ref
Dataset3bands

shapiro.test(vector3)
t.test(vector3)

# DATASET 4 (graph 4)
set.seed(2320)
mydata <- replicate(1000,runif(20,min=5,max=25))
frame4 <- data.frame(value = mydata[, 25])
vector4 <- frame4$value

Dataset4noRef <- ggplot(data = frame4, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-5,40))

Dataset4ref <- ggplot(data = frame4, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-5,40))

Dataset4bands <- ggplot(data = frame4, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-5,40))

Dataset4noRef
Dataset4ref
Dataset4bands

shapiro.test(vector4)
t.test(vector4)

# DATASET 5 (graph 5)
set.seed(2320)
mydata <- replicate(1000,rnorm(20,20,15))
frame5 <- data.frame(value = mydata[,16])
vector5 <- frame5$value

Dataset5noRef <- ggplot(data = frame5, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-30,80))

Dataset5ref <- ggplot(data = frame5, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-30,80))

Dataset5bands <- ggplot(data = frame5, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-30,80))

Dataset5noRef
Dataset5ref
Dataset5bands

shapiro.test(vector5)
t.test(vector5)

# DATASET 6 (graph 6)
set.seed(2320)
mydata <- replicate(1000,runif(20,min=5,max=25))
frame6 <- data.frame(value = mydata[,9])
vector6 <- frame6$value

Dataset6noRef <- ggplot(data = frame6, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(0,35))

Dataset6ref <- ggplot(data = frame6, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(0,35))

Dataset6bands <- ggplot(data = frame6, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(0,35))

Dataset6noRef
Dataset6ref
Dataset6bands

shapiro.test(vector6)
t.test(vector6)

# DATASET 7 (graph 7)
set.seed(2320)
mydata <- replicate(1000,rf(20,3,10))
frame7 <- data.frame(value = mydata[,405])
vector7 <- frame7$value

Dataset7noRef <- ggplot(data = frame7, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-1,4))

Dataset7ref <- ggplot(data = frame7, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-1,4))

Dataset7bands <- ggplot(data = frame7, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-1,4))

Dataset7noRef
Dataset7ref
Dataset7bands

shapiro.test(vector7)
t.test(vector7)

# DATASET 8 (graph 8)
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
frame8 <- data.frame(value = mydata[,100])
vector8 <- frame8$value

Dataset8noRef <- ggplot(data = frame8, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(0,30))

Dataset8ref <- ggplot(data = frame8, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(0,30))

Dataset8bands <- ggplot(data = frame8, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(0,30))

Dataset8noRef
Dataset8ref
Dataset8bands

shapiro.test(vector8)
t.test(vector8)