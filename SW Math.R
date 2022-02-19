# S-W Math

set.seed(1234)

# create data
data <- rnorm(10,0,1)
sorted <- sort(data)
sorted


sorted[10]-sorted[1]
sorted[9]-sorted[2]
sorted[8]-sorted[3]
sorted[7]-sorted[4]
sorted[6]-sorted[5]

mean(data)

mean(data)

# SS Calculation
SS <- (sorted[1]-mean(data))^2 + (sorted[2]-mean(data))^2 + (sorted[3]-mean(data))^2 + 
  (sorted[4]-mean(data))^2 + (sorted[5]-mean(data))^2 + (sorted[6]-mean(data))^2 + 
  (sorted[7]-mean(data))^2 + (sorted[8]-mean(data))^2 + (sorted[9]-mean(data))^2 + 
  (sorted[10]-mean(data))^2
 
# b^2 calculation
b <- (sorted[10]-sorted[1])*0.5739 + (sorted[9]-sorted[2])*0.3291 + (sorted[8]-sorted[3])*0.2141 +
  (sorted[7]-sorted[4])*0.1224 + (sorted[6]-sorted[5])*0.0399

b^2/SS

shapiro.test(data)

  
