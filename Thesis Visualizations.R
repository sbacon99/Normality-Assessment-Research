# packages
install.packages("ggplot2")
install.packages('qqplotr')
install.packages('e1071')
install.packages("gridExtra")

library(gridExtra)
library(ggplot2)
library(qqplotr)
library(e1071)

# figures created in this script...
# f1: Normal Distribution Histogram
# f2: Q-Q Plots for Various Sample Distributions
# f3: Q-Q Plots with Shapiro-Wilk Scores
# f4: Histograms of F, Uniform, and Normal Distributions
# f5: More Histograms of F, Uniform, and Normal Distributions
# f6: Histograms of F Distribution
# f7: Q-Q Plots of F Distribution
# f8: Histograms of Uniform Distribution
# f9: Q-Q Plots of Uniform Distribution
# f10: Histograms of Normal Distribution
# f11: Q-Q Plots of Normal Distribution
# f12: Boxplots for F, Uniform, and Normal Distributions
# f13: Variations of Q-Q Plots
# f14: Confidence Bands for Q-Q Plots
# f15: Survey Plots for Normal, F, and Uniform Distribution
# f16: Survey Plots Containing Outliers
# f17: Plot 1 without Axis Correction
# f18: Plot 1 with Axis Correction

# Figure 1 #
set.seed(123)
norm <- data.frame(value = rnorm(10000, 0, 1))

f1 <- ggplot(norm, aes(x = value)) + geom_histogram(aes(y = ..density.., ), colour = "blue", fill = "grey") + 
  stat_function(fun = dnorm,args = list(mean = mean(norm$value), sd = sd(norm$value)), col = "red", size = 1) + xlab("Value") + ylab("Density") 

f1

# Figure 2 #
set.seed(123)
f <- data.frame(value = rf(5000, 5, 10))
unif <- data.frame(value = runif(5000, 5, 10))
norm <- data.frame(value = rnorm(10000, 0, 1))

fPlot <- ggplot(data = f, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "", y = "Sample Quantiles")
unifPlot <- ggplot(data = unif, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "Theoretical Quantiles", y = "")
normPlot <- ggplot(data = norm, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "", y = "")

f2 <- grid.arrange(fPlot, unifPlot, normPlot, nrow = 1)

# Figure 3 #
set.seed(134)
u1 <- runif(25, 10, 20)
u2 <- runif(25, 10, 20)

shapiro.test(u1)
shapiro.test(u2)

frame1 <- data.frame(u1)
frame2 <- data.frame(u2)

plot1 <- ggplot(data = frame1, mapping = aes(sample = u1)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  annotate(geom="text", x=12, y=21, label="W = 0.92", color="black", size = 5) + 
  annotate(geom="text", x=12, y=19.5, label="p-value = 0.063 ", color="black", size = 5)

plot2 <- ggplot(data = frame2, mapping = aes(sample = u2)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  annotate(geom="text", x=11, y=17.5, label="W = 0.92", color="black", size = 5) + 
  annotate(geom="text", x=11, y=16.6, label="p-value = 0.040 ", color="black", size = 5)

f3 <- grid.arrange(plot1, plot2, nrow = 1)

# Figure 4 #
set.seed(123)
unif <- data.frame(value = runif(10000, -3, 3))
norm <- data.frame(value = rnorm(10000, 0, 1))
f <- data.frame(value = rf(10000, 10, 20))

fPlot <- ggplot(f, aes(x = value)) + geom_histogram(aes(y = ..density.., ), colour = "red", fill = "grey") + xlab("") + ylab("Density")
unifPlot <- ggplot(unif, aes(x = value)) + geom_histogram(aes(y = ..density.., ), color = "black", fill = "grey") + xlab("Value") + ylab("")
normPlot <- ggplot(norm, aes(x = value)) + geom_histogram(aes(y = ..density.., ), colour = "blue", fill = "grey") + xlab("") + ylab("")

fPlot
unifPlot
normPlot

f4 <- grid.arrange(fPlot, unifPlot, normPlot, nrow = 1)

# Figure 5 #
set.seed(123)
norm <- data.frame(value = rnorm(10000, 0, 1))
f <- data.frame(value = rf(10000, 500, 500))
unif <- data.frame(value = runif(10000, -1, 1))

fPlot <- ggplot(f, aes(x = value)) + geom_histogram(aes(y = ..density.., ), colour = "red", fill = "grey") + xlab("") + ylab("Density")
unifPlot <- ggplot(unif, aes(x = value)) + geom_histogram(aes(y = ..density.., ), color = "black", fill = "grey") + xlim(c(-3, 3)) + xlab("Value") + ylab("")
normPlot <- ggplot(norm, aes(x = value)) + geom_histogram(aes(y = ..density.., ), colour = "blue", fill = "grey") + xlab("") + ylab("")

fPlot
unifPlot
normPlot

f5 <- grid.arrange(fPlot, unifPlot, normPlot, nrow = 1)


# Figure 6 #
set.seed(123)
f1 <- data.frame(value = rf(10000, 5, 10))
f2 <- data.frame(value = rf(10000, 10, 20))
f3 <- data.frame(value = rf(10000, 50, 100))

f1plot <- ggplot(data = f1, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "red", fill = "grey") + labs(x = "", y = "Density") 
f2plot <- ggplot(data = f2, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "red", fill = "grey") + labs(x = "Value", y = "") 
f3plot <- ggplot(data = f3, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "red", fill = "grey") + labs(x = "", y = "") 

f1plot
f2plot
f3plot

f6 <- grid.arrange(f1plot, f2plot, f3plot, nrow = 1)

# Figure 7 #
set.seed(123)
f1 <- data.frame(value = rf(5000, 5, 10))
f2 <- data.frame(value = rf(5000, 10, 20))
f3 <- data.frame(value = rf(5000, 50, 100))

f1qq <- ggplot(data = f1, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "", y = "Sample Quantiles")
f2qq <- ggplot(data = f2, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "Theoretical Quantiles", y = "")
f3qq <- ggplot(data = f3, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "", y = "")

f1qq
f2qq
f3qq

shap1 <- shapiro.test(rf(5000, 5, 10))
shap2 <- shapiro.test(rf(5000, 10, 20))
shap3 <- shapiro.test(rf(5000, 50, 100))

f7 <- grid.arrange(f1qq, f2qq, f3qq, nrow = 1)

# Figure 8 #
set.seed(123)
unifdata1 <- data.frame(value = runif(5000, -1, 1))
unifdata2 <- data.frame(value = runif(5000, 5, 10))
unifdata3 <- data.frame(value = runif(5000, 0, 100))

unifplot1 <- ggplot(data = unifdata1, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "black", fill = "grey") + labs(x = "", y = "Density")
unifplot2 <- ggplot(data = unifdata2, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "black", fill = "grey") + labs(x = "Value", y = "")
unifplot3 <- ggplot(data = unifdata3, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "black", fill = "grey") + labs(x = "", y = "")

f8 <- grid.arrange(unifplot1, unifplot2, unifplot3, nrow = 1)

# Figure 9 #
set.seed(123)
unifdata1 <- data.frame(value = runif(5000, -1, 1))
unifdata2 <- data.frame(value = runif(5000, 5, 10))
unifdata3 <- data.frame(value = runif(5000, 0, 100))

unif1 <- ggplot(data = unifdata1, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "", y = "Sample Quantiles")  +
  theme(plot.title = element_text(size = 10))
unif2 <- ggplot(data = unifdata2, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "Theoretical Quantiles", y = "")  +
  theme(plot.title = element_text(size = 10))
unif3 <- ggplot(data = unifdata3, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "", y = "")  +
  theme(plot.title = element_text(size = 10))

f9 <- grid.arrange(unif1, unif2, unif3, nrow = 1)

# Figure 10 #
set.seed(21)
norm1 <- data.frame(value = rnorm(5000, mean = 0, sd = 1))
norm2 <- data.frame(value = rnorm(5000, mean = 5, sd = 2))
norm3 <- data.frame(value = rnorm(5000, mean = 0, sd = 10))

norm1plot <- ggplot(data = norm1, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "blue", fill = "grey") + labs(x = "", y = "Density") +
  stat_function(fun = dnorm, args = list(mean = mean(norm1$value),sd = sd(norm1$value)),col = "red",size = 1)

norm2plot <- ggplot(data = norm2, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "blue", fill = "grey") + labs(x = "Value", y = "") +
  stat_function(fun = dnorm, args = list(mean = mean(norm2$value),sd = sd(norm2$value)),col = "red",size = 1)

norm3plot <- ggplot(data = norm3, mapping = aes(value)) +
  geom_histogram(aes(y=..density..), colour = "blue", fill = "grey") + labs(x = "", y = "") +
  stat_function(fun = dnorm, args = list(mean = mean(norm3$value),sd = sd(norm3$value)),col = "red",size = 1)

f10 <- grid.arrange(norm1plot, norm2plot, norm3plot, nrow = 1)

# Figure 11 #
set.seed(21)
norm1 <- data.frame(value = rnorm(5000, mean = 0, sd = 1))
norm2 <- data.frame(value = rnorm(5000, mean = 5, sd = 2))
norm3 <- data.frame(value = rnorm(5000, mean = 0, sd = 10))

normplot1 <- ggplot(data = norm1, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "", y = "Sample Quantiles")
normplot2 <- ggplot(data = norm2, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "Theoretical Quantiles", y = "")
normplot3 <- ggplot(data = norm3, mapping = aes(sample = value)) + stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") + labs(x = "", y = "")

f11 <- grid.arrange(normplot1, normplot2, normplot3, nrow = 1)

# Figure 12 #
set.seed(211)
norm <- data.frame(value = rnorm(5000, mean = 0, sd = 1))
f <- data.frame(value = rf(5000, 5, 10))
unif <- data.frame(value = runif(5000, -5, 5))

colnames(norm) <- "normal"
colnames(f) <- "f"
colnames(unif) <- "uniform"

normMelted <- melt(norm)
fMelted <- melt(f)
unifMelted <- melt(unif)

combined <- rbind(fMelted, unifMelted)
combined <- rbind(combined, normMelted)

colors <- c("blue", "red", "black")

f12 <- ggplot(combined, aes(x=variable, y=value, color=variable)) + geom_boxplot() + 
    labs(x = "Distribution", y = "Value") + scale_color_manual(values = c("blue", "red", "black")) + 
    theme(legend.position = "none")
f12

# Figure 13 #
set.seed(2320)
mydata <- replicate(1000,rnorm(20,20,15))
norm <- data.frame(value = mydata[,16])

basic <- ggplot(data = norm, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-30,80))

allFeatures <- ggplot(data = norm, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-30,80))

detrended <- ggplot(data = norm, mapping = aes(sample = value)) + stat_qq_point(colour = "black", detrend = TRUE) +
  stat_qq_line(identity = FALSE, color = "black", detrend = TRUE) + labs(x = "", y = "") + labs(x = "Theoretical Quantiles", y = "Differences")

f13 <- grid.arrange(basic, allFeatures, detrended, nrow = 1)

# Figure 14 #
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
frame3 <- data.frame(value = mydata[,318])
vector3 <- frame3$value

# pointwise
pointwise <- ggplot(data = frame3, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-6,20))

# bootstrap
bootstrap <- ggplot(data = frame3, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink", bandType = "boot") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-6,20))

#ks
ks <- ggplot(data = frame3, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink", bandType = "ks") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-6,20))

#ts
ts <- ggplot(data = frame3, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink", bandType = "ts") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-6,20))

f14 <- grid.arrange(pointwise, bootstrap, ks, ts, nrow = 2)

# Figure 15 #

# Normal
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
normFrame <- data.frame(value = mydata[,7])
normVect <- normFrame$value

normNone <- ggplot(data = normFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "", y = "")+ coord_cartesian(ylim = c(-10,35))

normRef <- ggplot(data = normFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "", y = "Sample Quantiles")+ coord_cartesian(ylim = c(-10,35))

normBands <- ggplot(data = normFrame, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "", y = "") + coord_cartesian(ylim = c(-10,35))

# F
mydata <- replicate(1000,rf(20,3,10))
fFrame <- data.frame(value = mydata[,3])
fVect <- fFrame$value

fNone <- ggplot(data = fFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "", y = "") + coord_cartesian(ylim = c(-1.5,3)) 

fRef <- ggplot(data = fFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "", y = "") + coord_cartesian(ylim = c(-1.5,3))

fBands <- ggplot(data = fFrame, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "") + coord_cartesian(ylim = c(-1.5,3))

# Uniform
mydata <- replicate(1000,runif(20,min=5,max=25))
unifFrame <- data.frame(value = mydata[, 25])
unifVect <- unifFrame$value

unifNone <- ggplot(data = unifFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "", y = "") + coord_cartesian(ylim = c(-5,40))

unifRef <- ggplot(data = unifFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "", y = "") + coord_cartesian(ylim = c(-5,40))

unifBands <- ggplot(data = unifFrame, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "", y = "") + coord_cartesian(ylim = c(-5,40)) 

normNone
normRef
normBands
fNone
fRef
fBands
unifNone
unifRef
unifBands

f15 <- grid.arrange(normNone, fNone, unifNone,
                    normRef, fRef, unifRef,
                    normBands, fBands, unifBands,
                    nrow = 3)

# Figure 16 #

# dataset 3
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
frame3 <- data.frame(value = mydata[,318])

Dataset3bands <- ggplot(data = frame3, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  coord_cartesian(ylim = c(-6,20)) + ggtitle("Plot 3") + 
  theme(plot.title = element_text(hjust = 0.5))

# dataset 6
set.seed(2320)
mydata <- replicate(1000,runif(20,min=5,max=25))
frame6 <- data.frame(value = mydata[,9])

Dataset6bands <- ggplot(data = frame6, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  coord_cartesian(ylim = c(0,35)) + ggtitle("Plot 6") + 
  theme(plot.title = element_text(hjust = 0.5))

# dataset 7
set.seed(2320)
mydata <- replicate(1000,rf(20,3,10))
frame7 <- data.frame(value = mydata[,405])

Dataset7bands <- ggplot(data = frame7, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  coord_cartesian(ylim = c(-1,4)) + ggtitle("Plot 7") + 
  theme(plot.title = element_text(hjust = 0.5))

# dataset 8
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
frame8 <- data.frame(value = mydata[,100])

Dataset8bands <- ggplot(data = frame8, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + 
  coord_cartesian(ylim = c(0,30)) + ggtitle("Plot 8") + 
  theme(plot.title = element_text(hjust = 0.5))

f16 <- grid.arrange(Dataset3bands, Dataset6bands, 
             Dataset7bands, Dataset8bands, nrow = 2)

# Figure 17 #
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
frame1 <- data.frame(value = mydata[,7])

noRefNoCorrection <- ggplot(data = frame1, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

bandsNoCorrection <- ggplot(data = frame1, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

f17 <- grid.arrange(noRefNoCorrection, bandsNoCorrection, nrow = 1)

# Figure 18 #
set.seed(2320)
mydata <- replicate(1000,rnorm(20,10,5))
frame1 <- data.frame(value = mydata[,7])

noRefCorrection <- ggplot(data = frame1, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+ coord_cartesian(ylim = c(-10,35))

bandsCorrection <- ggplot(data = frame1, mapping = aes(sample = value)) +
  stat_qq_band(identity = FALSE, fill = "pink") +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + coord_cartesian(ylim = c(-10,35))

f18 <- grid.arrange(noRefCorrection, bandsCorrection, nrow = 1)

##### END OF VISUALIZATIONS #####


#### F-Distribution ####
########################
set.seed(123)
x <- rf(100000, df1 = 3, df2 = 10)
hist(x, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,3), 
     ylim = c(0,1),
     xlab = "F",
     main = "")
curve(df(x, df1 = 3, df2 = 10), from = 0, to = 4, n = 5000, col= 'red', lwd=2, add = T)


set.seed(12345)
fFrame7_2 <- data.frame(value = rf(10000, 3, 10))
fVector7_2 <- fFrame7_2$value

f <- ggplot(data = fFrame7_2, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")  +
  theme(plot.title = element_text(size = 10))
f

#### Uniform-Distribution ####
########################
x <- runif(10000, 5, 25)
hist(x, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,30), 
     ylim = c(0,.1),
     xlab = "Uniform",
     main = "")
curve(dunif(x, 5, 25), from = 0, to = 30, n = 5000, col= 'red', lwd=2, add = T)

set.seed(12345)
unifFrame <- data.frame(value = runif(10000, 5, 25))
unifVector <- fFrame7_2$value

unif <- ggplot(data = unifFrame, mapping = aes(sample = value)) +
  stat_qq_point(colour = "black") +
  stat_qq_line(identity = FALSE, color = "red") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")  +
  theme(plot.title = element_text(size = 10))
unif
