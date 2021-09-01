# Pilot study data
install.packages("gridExtra")
library(gridExtra)
install.packages("grid")
library(grid)
install.packages("gridtext")
library(gridtext)
install.packages("ggplot2")
library(ggplot2)
install.packages("googlesheets4")
library(googlesheets4)
install.packages('dplyr')
library(dplyr)

# raw data
pilot_pre <- read_sheet("https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=961487422", sheet = "pre_survey")
pilot_survey <- read_sheet("https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=961487422", sheet = "survey")
post_survey <- read_sheet("https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=961487422", sheet = "post_survey" )
survey_order <- read_sheet("https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=961487422", sheet = "survey_order")

pilot_pre
pilot_survey
post_survey
survey_order

correct <- c("normal","normal","non-normal","normal","non-normal","normal","non-normal","non-normal","normal")
pilot_survey[1,] 

nrow(pilot_survey)
compare <- data.frame

x <- data.frame(zero=logical(),one=logical(),two=logical(),three=logical(),four=logical(),five=logical(),six=logical(),seven=logical())

for (i in 1:nrow(pilot_survey)) {
  comparison <- select(pilot_survey, -c(userID))[i,] == correct
  total <- cbind(comparison, sum(comparison==TRUE))
  x <- rbind(x,total)
}

answers_demographics <- cbind(pilot_pre,x)

# average correct responses based on level
level <- aggregate(answers_demographics$V10, list(answers_demographics$pre1), FUN=mean)

level_bar <- ggplot(level, aes(x=Group.1, y=x, fill=Group.1)) + geom_bar(stat="identity") + theme(legend.position="none") +
      geom_text(aes(label=round(x, 3)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank())
level_box <- ggplot(answers_demographics, aes(x=pre1, y=V10, fill=pre1)) + geom_boxplot() + xlab("Experience level") + ylab("") +
      theme(legend.position="none")


#average correct responses based on course experience
experience <- aggregate(answers_demographics$V10, list(answers_demographics$pre2), FUN=mean)

experience_bar <- ggplot(experience, aes(x=Group.1, y=x, fill=Group.1)) + geom_bar(stat="identity") + theme(legend.position="none") +
      geom_text(aes(label=round(x, 3)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank())
experience_box <- ggplot(answers_demographics, aes(x=pre2, y=V10, fill=pre2)) + geom_boxplot() + xlab("Courses completed") + ylab("") +
      theme(legend.position="none")


#average correct responses based on 'have you seen a q-q plot before?'
exposure <- aggregate(answers_demographics$V10, list(answers_demographics$pre3), FUN=mean)

exposure_bar <- ggplot(exposure, aes(x=Group.1, y=x, fill=Group.1)) + geom_bar(stat="identity") + theme(legend.position="none") +
      geom_text(aes(label=round(x, 3)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank())
exposure_box <- ggplot(answers_demographics, aes(x=pre3, y=V10, fill=pre3)) + geom_boxplot() + xlab("Seen q-q plot?") + ylab("") +
      theme(legend.position="none")


level_bar
level_box

experience_bar
experience_box

exposure_bar
exposure_box

yleft <- textGrob(expression(paste("Average Correct Responses")), 
                  rot = 90, gp = gpar(fontsize = 12))

grid.arrange(level_bar, experience_bar, exposure_bar, level_box, experience_box, exposure_box, nrow = 2, top = "Preliminary Visualizations", left = yleft)

