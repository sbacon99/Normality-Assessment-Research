install.packages("googlesheets4")
library(googlesheets4)


pre_survey <- data.frame(response1=character(), response2=character(), response3=character())
survey <- data.frame(response1=character(), response2=character(), response3=character(),
                     response4=character(), response5=character(), response6=character(),
                     response7=character(), response8 =character())
post_survey <- data.frame(response1=character(), response2=character(), response3=character(),
                          response4=character(), response5=character())

# This code will create the google sheet
ss <- gs4_create("survey_responses", sheets = c("pre_survey", "survey", "post_survey"))

# Each participant will need their reponses stored in a dataframe
participant1 <- data.frame(response1="no evidence", response2=1, response3=1,
                           response4=1, response5=1, response6=1,
                           response7=1, response8=1)
participant1

# Then, the data frame will be appended to the google sheet
ss %>% sheet_append(participant1, sheet = "pre_survey")

sheet_append('https://docs.google.com/spreadsheets/d/19cmrZGalGJljvOGCVF2tPs1k9C6YHMroOh4kQhS3GzA/edit#gid=308877971', participant1)

?sheet_append
?gs4_auth()
??gs4_create
?radioButtons
?paste
?renderUI
?submitButton
?read_sheet

ind1 <- sample(1:3, 3, replace = FALSE)
ind2 <- sample(1:3, 3, replace = FALSE)
ind3 <- sample(1:3, 2, replace = FALSE)

indFrame <- data.frame(t(c(ind1, ind2, ind3)))
indFrame
class(indFrame)
indicators <- list(ind1 = ind1, ind2 = ind2, ind3 = ind3)
indicators

as.data.frame(indicators)

set.seed(1234)
keys <- round(runif(50, min = 10000, max = 20000), 0)
keyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1i_kF-5rrdqSD9eXss-eyOmmhGi3_Y5wDqFCEk2kPkus/edit#gid=0')
sheet <- read_sheet(keyLocation, sheet = "used_keys")
usedKeys <- sheet$Pin
openKeys <- setdiff(keys, usedKeys)
yourKey <- sample(openKeys, 1)
paste('Your personalized key is:  ', yourKey, '. Click on the following link to enter your email and incentive key: ')
yourKeyFrame <- as.data.frame(yourKey)
keyLocation %>% sheet_append(yourKeyFrame, sheet = "sent_keys") 

keyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1i_kF-5rrdqSD9eXss-eyOmmhGi3_Y5wDqFCEk2kPkus/edit#gid=0')
sheet <- read_sheet(keyLocation, sheet = "sent_keys")    
sheet$Pin
x <- setdiff(sheet$Pin, 0)
x

?setdiff

setdiff(sheet$Pin, 17597)
keyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1i_kF-5rrdqSD9eXss-eyOmmhGi3_Y5wDqFCEk2kPkus/edit#gid=0')
sent_keys <- read_sheet(keyLocation, sheet = "sent_keys")   
sent_keys
used_keys <- read_sheet(keyLocation, sheet = "used_keys") 
used_keys
setdiff(17597,used_keys$Pin)
setdiff(sent_keys$Pin, 17597)

# COMPLETE
if(match(17597, sent_keys$Pin) && is.na(match(17597, used_keys$Pin))) {
    valid <- 1
}

match(17597, sent_keys$Pin)
is.na(match(17597, used_keys$Pin)) 

# Generating userID
? in

?t

userKeyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=1525817837')
sheet <- read_sheet(userKeyLocation, sheet = "pre_survey")
length(sheet$userID)
userID <- length(sheet$userID) +1

responses_survey <- data.frame(userID, input$response0, input$response1, input$response2, input$response3,
                               input$response4, input$response5, input$response6,
                               input$response7, input$response8)

?match
