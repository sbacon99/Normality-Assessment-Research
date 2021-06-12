##F distribution
#Consider some sample sizes & df to investigate

# runif(15, 10, 20)

set.seed(2320)

df1 <- 2
df2 <- 10
n <- 20

#mean of F: df2/(df2-2)
mu <- 15

N <- 1000
capture <- NULL
for(i in 1:N){
  x <- runif(15, 10, 20)
  out <- t.test(x)
  capture[i] <- (out$conf.int[[1]] <= mu)*(mu <= out$conf.int[[2]])
  if(i==22){
    hist(x)
    qqnorm(x)
    print(x)
  }
}
sum(capture)

hist(x)
qqnorm(x)

capture[1000]



##NORMAL
#Consider some sample sizes & df to investigate

set.seed(2320)

n <- 20

mu <- 50

N <- 1000
capture <- NULL
for(i in 1:N){
  x <- rnorm(n,50,2.5)
  out <- t.test(x)
  capture[i] <- (out$conf.int[[1]] <= mu)*(mu <= out$conf.int[[2]])
  if(i==7){
    hist(x)
    qqnorm(x)
    print(x)
    out <- t.test(x)
    print(out$conf.int[[1]])
    print(out$conf.int[[2]])
  }
}
sum(capture)