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
install.packages("reshape2")
library(reshape2)

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

plot0 <- survey_order[,0:2]

plot1ord <- survey_order %>% select(c(userID, survey1_ord))
plot2ord <- survey_order %>% select(c(userID, survey2_ord))
plot3ord <- survey_order %>% select(c(userID, survey3_ord))
plot4ord <- survey_order %>% select(c(userID, survey4_ord))
plot5ord <- survey_order %>% select(c(userID, survey5_ord))
plot6ord <- survey_order %>% select(c(userID, survey6_ord))
plot7ord <- survey_order %>% select(c(userID, survey7_ord))
plot8ord <- survey_order %>% select(c(userID, survey8_ord))

plot1ans <- answers_demographics %>% select(c(userID, survey1_dist))
plot2ans <- answers_demographics %>% select(c(userID, survey2_dist))
plot3ans <- answers_demographics %>% select(c(userID, survey3_dist))
plot4ans <- answers_demographics %>% select(c(userID, survey4_dist))
plot5ans <- answers_demographics %>% select(c(userID, survey5_dist))
plot6ans <- answers_demographics %>% select(c(userID, survey6_dist))
plot7ans <- answers_demographics %>% select(c(userID, survey7_dist))
plot8ans <- answers_demographics %>% select(c(userID, survey8_dist))

p1 <- left_join(plot1ans, plot1ord, by="userID")
p2 <- left_join(plot2ans, plot2ord, by="userID")
p3 <- left_join(plot3ans, plot3ord, by="userID")
p4 <- left_join(plot4ans, plot4ord, by="userID")
p5 <- left_join(plot5ans, plot5ord, by="userID")
p6 <- left_join(plot6ans, plot6ord, by="userID")
p7 <- left_join(plot7ans, plot7ord, by="userID")
p8 <- left_join(plot8ans, plot8ord, by="userID")

for (i in 1:nrow(p1)){
  if (p1[i,2]==1){
    p1[i,4] <- "correct"
  }
  else{
    p1[i,4] <- "incorrect"
  }
}

for (i in 1:nrow(p2)){
  if (p2[i,2]==1){
    p2[i,4] <- "correct"
  }
  else{
    p2[i,4] <- "incorrect"
  }
}

for (i in 1:nrow(p3)){
  if (p3[i,2]==1){
    p3[i,4] <- "correct"
  }
  else{
    p3[i,4] <- "incorrect"
  }
}

for (i in 1:nrow(p4)){
  if (p4[i,2]==1){
    p4[i,4] <- "correct"
  }
  else{
    p4[i,4] <- "incorrect"
  }
}

for (i in 1:nrow(p5)){
  if (p5[i,2]==1){
    p5[i,4] <- "correct"
  }
  else{
    p5[i,4] <- "incorrect"
  }
}

for (i in 1:nrow(p6)){
  if (p6[i,2]==1){
    p6[i,4] <- "correct"
  }
  else{
    p6[i,4] <- "incorrect"
  }
}

for (i in 1:nrow(p7)){
  if (p7[i,2]==1){
    p7[i,4] <- "correct"
  }
  else{
    p7[i,4] <- "incorrect"
  }
}

for (i in 1:nrow(p8)){
  if (p8[i,2]==1){
    p8[i,4] <- "correct"
  }
  else{
    p8[i,4] <- "incorrect"
  }
}

p1total <- dcast(p1, survey1_ord ~ V4)
p2total <- dcast(p2, survey2_ord ~ V4)
p3total <- dcast(p3, survey3_ord ~ V4)
p4total <- dcast(p4, survey4_ord ~ V4)
p5total <- dcast(p5, survey5_ord ~ V4)
p6total <- dcast(p6, survey6_ord ~ V4)
p7total <- dcast(p7, survey7_ord ~ V4)
p8total <- dcast(p8, survey8_ord ~ V4)

melted1 <- melt(p1total, id="survey1_ord")
melted1$survey1_ord[melted1$survey1_ord == 1] <- "no features"
melted1$survey1_ord[melted1$survey1_ord == 2] <- "ref. line"
melted1$survey1_ord[melted1$survey1_ord == 3] <- "ref. line and bands"

melted2 <- melt(p2total, id="survey2_ord")
melted2$survey2_ord[melted2$survey2_ord == 1] <- "no features"
melted2$survey2_ord[melted2$survey2_ord == 2] <- "ref. line"
melted2$survey2_ord[melted2$survey2_ord == 3] <- "ref. line and bands"

melted3 <- melt(p3total, id="survey3_ord")
melted3$survey3_ord[melted3$survey3_ord == 1] <- "no features"
melted3$survey3_ord[melted3$survey3_ord == 2] <- "ref. line"
melted3$survey3_ord[melted3$survey3_ord == 3] <- "ref. line and bands"

melted4 <- melt(p4total, id="survey4_ord")
melted4$survey4_ord[melted4$survey4_ord ==  1] <- "no features"
melted4$survey4_ord[melted4$survey4_ord == 2] <- "ref. line"
melted4$survey4_ord[melted4$survey4_ord == 3] <- "ref. line and bands"

melted5 <- melt(p5total, id="survey5_ord")
melted5$survey5_ord[melted5$survey5_ord == 1] <- "no features"
melted5$survey5_ord[melted5$survey5_ord == 2] <- "ref. line"
melted5$survey5_ord[melted5$survey5_ord == 3] <- "ref. line and bands"

melted6 <- melt(p6total, id="survey6_ord")
melted6$survey6_ord[melted6$survey6_ord == 1] <- "no features"
melted6$survey6_ord[melted6$survey6_ord == 2] <- "ref. line"
melted6$survey6_ord[melted6$survey6_ord == 3] <- "ref. line and bands"

melted7 <- melt(p7total, id="survey7_ord")
melted7$survey7_ord[melted7$survey7_ord == 1] <- "no features"
melted7$survey7_ord[melted7$survey7_ord == 2] <- "ref. line"
melted7$survey7_ord[melted7$survey7_ord == 3] <- "ref. line and bands"

melted8 <- melt(p8total, id="survey8_ord")
melted8$survey8_ord[melted8$survey8_ord == 1] <- "no features"
melted8$survey8_ord[melted8$survey8_ord == 2] <- "ref. line"
melted8$survey8_ord[melted8$survey8_ord == 3] <- "ref. line and bands"

bar1 <- ggplot(melted1, aes(x=survey1_ord, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=value), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9)) + xlab("Plot features") +
  ylab("Responses")
bar1

bar2 <- ggplot(melted2, aes(x=survey2_ord, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=value), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
bar2

bar3 <- ggplot(melted3, aes(x=survey3_ord, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=value), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
bar3

bar4 <- ggplot(melted4, aes(x=survey4_ord, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=value), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
bar4

bar5 <- ggplot(melted5, aes(x=survey5_ord, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=value), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
bar5

bar6 <- ggplot(melted6, aes(x=survey6_ord, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=value), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
bar6

bar7 <- ggplot(melted7, aes(x=survey7_ord, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=value), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
bar7

bar8 <- ggplot(melted8, aes(x=survey8_ord, y=value, fill=variable)) + geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label=value), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
bar8

grid.arrange(bar1, grid.arrange(Dataset1noRef,Dataset1ref,Dataset1bands), nrow=1, top = "Plot 1 (Pilot)")
grid.arrange(bar2, grid.arrange(Dataset2noRef,Dataset2ref,Dataset2bands), nrow=1, top = "Plot 2 (Pilot)")
grid.arrange(bar3, grid.arrange(Dataset3noRef,Dataset3ref,Dataset3bands), nrow=1, top = "Plot 3 (Pilot)")
grid.arrange(bar4, grid.arrange(Dataset4noRef,Dataset4ref,Dataset4bands), nrow=1, top = "Plot 4 (Pilot)")
grid.arrange(bar5, grid.arrange(Dataset5noRef,Dataset5ref,Dataset5bands), nrow=1, top = "Plot 5 (Pilot)")
grid.arrange(bar6, grid.arrange(Dataset6noRef,Dataset6ref,Dataset6bands), nrow=1, top = "Plot 6 (Pilot)")
grid.arrange(bar7, grid.arrange(Dataset7noRef_2,Dataset7ref_2,Dataset7bands_2), nrow=1, top = "Plot 7 (Pilot)")
grid.arrange(bar8, grid.arrange(Dataset8noRef_2,Dataset8ref_2,Dataset8bands_2), nrow=1, top = "Plot 8 (Pilot)")

t1 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t1) <- names

t1[1,1] <- "none"
t1[2,1] <- "ref line"
t1[3,1] <- "ref line and bands"

for (i in 1:nrow(p1)){
  
}

t2 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t2) <- names

t2[1,1] <- "none"
t2[2,1] <- "ref line"
t2[3,1] <- "ref line and bands"


t3 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t3) <- names

t3[1,1] <- "none"
t3[2,1] <- "ref line"
t3[3,1] <- "ref line and bands"

t4 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t4) <- names

t4[1,1] <- "none"
t4[2,1] <- "ref line"
t4[3,1] <- "ref line and bands"

t5 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t5) <- names

t5[1,1] <- "none"
t5[2,1] <- "ref line"
t5[3,1] <- "ref line and bands"

t6 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t6) <- names

t6[1,1] <- "none"
t6[2,1] <- "ref line"
t6[3,1] <- "ref line and bands"

t7 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t7) <- names

t7[1,1] <- "none"
t7[2,1] <- "ref line"
t7[3,1] <- "ref line and bands"

t8 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t8) <- names

t8[1,1] <- "none"
t8[2,1] <- "ref line"
t8[3,1] <- "ref line and bands"

answers <- c("normal","non-normal","normal","non-normal","normal","non-normal","non-normal","normal")
