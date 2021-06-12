# Generating new keys for survey
set.seed(1234)
keys <- round(runif(50, min = 10000, max = 20000), 0)
keys
sheet <- read_sheet(gs4_get('https://docs.google.com/spreadsheets/d/1i_kF-5rrdqSD9eXss-eyOmmhGi3_Y5wDqFCEk2kPkus/edit#gid=0'), sheet = "used_keys")
sheet
usedKeys <- sheet$Pin
usedKeys
openKeys <- setdiff(keys, usedKeys)
openKeys
yourKey <- sample(openKeys, 1)
yourKey

?setdiff

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
