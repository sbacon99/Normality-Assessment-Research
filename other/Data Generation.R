# Simulation to generate data for Final QQ-Plots
# Sam Bacon and Laura Taylor

set.seed(2320)
#mydata <- replicate(1000,rf(20,3,10))
#mydata <- replicate(1000,rnorm(20,10,5))
#mydata <- replicate(1000,runif(20,min=1,max=1.5))
mydata <- replicate(1000,runif(20,min=5,max=25))
#dim(mydata)

check <- NULL
sw.pvalue <- NULL
for(i in 1:1000){
  ci <- t.test(mydata[,i],mu=10,conf.level=0.95)$conf.int
  check[i] <- (ci[1] <= 15 & ci[2] >= 15)
  sw.pvalue[i] <- shapiro.test(mydata[,i])$p.value
  if(check[i]==0 & sw.pvalue[i] <= 0.05){print(i)} # 
}

# Since we used a seed! print out column 3

sampledata <- mydata[,3]

sum(check)/1000 # accuracy of confidence interval
sum(sw.pvalue <= 0.05)/1000 # accuracy of SW test
sum((check==0)*(sw.pvalue <= 0.05)) #SW test says assumption not ok, and our CI would have missed mu
sum((check==1)*(sw.pvalue <= 0.05)) #SW test says assumption not ok, but our CI would have been correct
sum((check==0)*(sw.pvalue > 0.05)) #SW test gives ok, but our CI would have missed mu
sum((check==1)*(sw.pvalue > 0.05)) #SW test gives ok, and our CI captured mu
### IDEA! Set a seed and then use the check and sw.pvalue vectors
### to track down a data set that would fit into a category we are
### interested in.

#Check for your data sets:
set.seed(4321)
x <- rf(20,3,10)
t.test(x,mu=1.25,conf.level=.95) #In this case, the data
# does lead us to correct conclusions (CI contains 1.25
# and p-value is large).

out <- shapiro.test(x)

t.test(sampledata,mu=1.25,conf.level=.95)
shapiro.test(sampledata)

