# Random generator simulation
values <- sample(1:3, 2000, replace = FALSE)
oneCounter <- 0
twoCounter <- 0
threeCounter <- 0
for(i in 1:2000){
  if(values[i] == 1){
    oneCounter <- oneCounter +1
  }  
  else if(values[i] == 2){
    twoCounter <- twoCounter +1
  }
  else if(values[i] == 3){
    threeCounter <- threeCounter +1
  }
}

oneCounter/2000
twoCounter/2000
threeCounter/2000

sum(values[1:100]==3)

# Creating dataframe of random survey orders
df <- data.frame(plot1 = integer(), plot2 = integer(), plot3 = integer(),
                 plot4 = integer(), plot5 = integer(), plot6 = integer(), plot7 = integer(),
                 plot8 = integer())


for(i in 1:2000){
  order <- data.frame(t(unlist(c(sample(1:3, 3, replace = FALSE), sample(1:3, 3, replace = FALSE), sample(1:3, 2, replace = FALSE)))))
  names(order) = c("plot1","plot2","plot3","plot4","plot5","plot6","plot7","plot8")
  df <- rbind(df, order)
}

# Append values to google sheet
tt <- gs4_get('https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=2012575888')
tt %>% sheet_append(df, sheet = "order_upload")

# userID
spreadsheet <- gs4_get('https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1497437448')
sheet <- read_sheet(spreadsheet, sheet = "survey_order")
userID <- length(sheet)

userKeyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=2012575888')
out <- range_read_cells(userKeyLocation, range = "order_upload!A11:H11")
out




