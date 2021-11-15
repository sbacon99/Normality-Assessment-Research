# Final data visualizations

#libraries
install.packages("gridExtra")
install.packages("grid")
install.packages("gridtext")
install.packages("ggplot2")
install.packages("googlesheets4")
install.packages('dplyr')
install.packages("reshape2")

library(grid)
library(gridExtra)
library(gridtext)
library(ggplot2)
library(googlesheets4)
library(dplyr)
library(reshape2)
library(tidyr)


# raw data
final_pre <- read_sheet("https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1629775005", sheet = "pre_survey")
final_survey <- read_sheet("https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1629775005", sheet = "survey")
post_survey <- read_sheet("https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1629775005", sheet = "post_survey" )
survey_order <- read_sheet("https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1629775005", sheet = "survey_order")

# solutions
correct_test <- c("normal","normal","non-normal","normal","non-normal","normal","non-normal","non-normal","normal")
correct_notest <- c("normal","non-normal","normal","non-normal","normal","non-normal","non-normal","normal")

correct2_test <- c("normal","normal","f","normal","uniform","normal","uniform","f","normal")

outliers_test <- c("no","no","no","yes","no","no","yes","yes","yes")

# create answers_demographics table (demographics + responses)
x <- data.frame(zero=logical(),one=logical(),two=logical(),three=logical(),four=logical(),five=logical(),six=logical(),seven=logical())

for (i in 1:nrow(final_survey)) {
  comparison <- select(final_survey, -c(userID, survey0_dist))[i,] == correct_notest
  total <- cbind(comparison, sum(comparison==TRUE))
  x <- rbind(x,total)
}

answers_demographics <- cbind(final_pre,x)

p1_merge <- merge(x=final_survey,y=survey_order,by="userID")

survey_order[1,2:9]

survey_order$normNoFeaturesCount <- 0
survey_order$normRefLineCount <- 0
survey_order$normBandsCount <- 0
survey_order$noFeaturesCount <- 0
survey_order$refLineCount <- 0
survey_order$bandsCount <- 0

for (j in 1:nrow(survey_order)){
  for (i in 2:9){
    if (survey_order[j,i] == 3){
      survey_order[j,]$bandsCount <- survey_order[j,]$bandsCount +1
    }
    if (survey_order[j,i] == 2){
      survey_order[j,]$refLineCount <- survey_order[j,]$refLineCount +1
    }
    if (survey_order[j,i] == 1){
      survey_order[j,]$noFeaturesCount <- survey_order[j,]$noFeaturesCount +1
    }
  }
}

merge <- merge(x=final_survey,y=survey_order,by="userID")

sum(merge$bandsCount)
sum(merge$refLineCount)
sum(merge$noFeaturesCount)


for (j in 1:nrow(merge)){
  for (i in 3:10){
    if (merge[j,i] == "normal" && merge[j,i+8] == 1){
      merge[j,]$normNoFeaturesCount = merge[j,]$normNoFeaturesCount +1
    }
    if (merge[j,i] == "normal" && merge[j,i+8] == 2){
      merge[j,]$normRefLineCount = merge[j,]$normRefLineCount +1
    }
    if (merge[j,i] == "normal" && merge[j,i+8] == 3){
      merge[j,]$normBandsCount = merge[j,]$normBandsCount +1
    }
  }
}
sum(merge$normNoFeaturesCount)
sum(merge$normRefLineCount)
sum(merge$normBandsCount)

merge$noFeaturesNormCount2 <- 0
merge$refLineNormCount2 <- 0
merge$bandsNormCount2 <- 0

merge$noFeaturesNonNormCount2 <- 0
merge$refLineNonNormCount2 <- 0
merge$bandsNonNormCount2 <- 0

for (i in 1:123){
  if (merge[i,6] == "normal" && merge[i,14] == 1){
    merge[i,]$noFeaturesNormCount2 <- merge[i,]$noFeaturesNormCount2+1
  }
  if (merge[i,6] == "non-normal" && merge[i,14] == 1){
    merge[i,]$noFeaturesNonNormCount2 <- merge[i,]$noFeaturesNonNormCount2+1
  }
  if (merge[i,6] == "normal" && merge[i,14] == 2){
    merge[i,]$refLineNormCount2 <- merge[i,]$refLineNormCount2+1
  }
  if (merge[i,6] == "non-normal" && merge[i,14] == 2){
    merge[i,]$refLineNonNormCount2 <- merge[i,]$refLineNonNormCount2+1
  }
  if (merge[i,6] == "normal" && merge[i,14] == 3){
    merge[i,]$bandsNormCount2 <- merge[i,]$bandsNormCount2+1
  }
  if (merge[i,6] == "non-normal" && merge[i,14] == 3){
    merge[i,]$bandsNonNormCount2 <- merge[i,]$bandsNonNormCount2+1
  }
}

sum(merge$noFeaturesNormCount2)
sum(merge$noFeaturesNonNormCount2)
sum(merge$refLineNormCount2)
sum(merge$refLineNonNormCount2)
sum(merge$bandsNormCount2)
sum(merge$bandsNonNormCount2)

merge$noFeatureCount4 <- 0
merge$refLineCount4 <- 0
merge$bandCount4 <- 0

for (i in 1:123){
  if (merge[i,12] == 1){
    merge[i,]$noFeatureCount4 <- merge[i,]$noFeatureCount4+1
  }
  if (merge[i,12] == 2){
    merge[i,]$refLineCount4 <- merge[i,]$refLineCount4+1
  }
  if (merge[i,12] == 3){
    merge[i,]$bandCount4 <- merge[i,]$bandCount4+1
  }
}

sum(merge$noFeatureCount4)
sum(merge$refLineCount4)
sum(merge$bandCount4)

p <- ggplot(answers_demographics[answers_demographics$pre1 != "null",], aes(x = pre1, y = V9)) + geom_boxplot() + geom_point(position = "jitter")
p + xlab("Experience Level") + ylab("Average Correct Responses")

nrow(answers_demographics[answers_demographics$pre1 == "undergraduate",])
nrow(answers_demographics[answers_demographics$pre1 == "graduate",])
nrow(answers_demographics[answers_demographics$pre1 == "professional",])


curr <- select(final_survey, -survey0_dist)
curr$outliersAllCorrect <- 0
curr$outliers1Correct <- 0
curr$outliers0Correct <- 0
curr$noOutliersAllCorrect <- 0
curr$noOutliers1Correct <- 0
curr$noOutliers0Correct <- 0

for (i in 1:123){
  # outliers
  if (curr[i,4] == "normal" && curr[i,9] == "normal"){
    curr[i,]$outliersAllCorrect <- curr[i,]$outliersAllCorrect +1
  }
  if (curr[i,4] == "normal" && curr[i,9] != "normal"){
    curr[i,]$outliers1Correct <- curr[i,]$outliers1Correct +1
  }
  if (curr[i,4] != "normal" && curr[i,9] == "normal"){
    curr[i,]$outliers1Correct <- curr[i,]$outliers1Correct +1
  }
  if (curr[i,4] != "normal" && curr[i,9] != "normal"){
    curr[i,]$outliers0Correct <- curr[i,]$outliers0Correct +1
  }
  # no outliers
  if (curr[i,2] == "normal" && curr[i,6] == "normal"){
    curr[i,]$noOutliersAllCorrect <- curr[i,]$noOutliersAllCorrect +1
  }
  if (curr[i,2] == "normal" && curr[i,6] != "normal"){
    curr[i,]$noOutliers1Correct <- curr[i,]$noOutliers1Correct +1
  }
  if (curr[i,2] != "normal" && curr[i,6] == "normal"){
    curr[i,]$noOutliers1Correct <- curr[i,]$noOutliers1Correct +1
  }
  if (curr[i,2] != "normal" && curr[i,6] != "normal"){
    curr[i,]$noOutliers0Correct <- curr[i,]$noOutliers0Correct +1
  }
  
}

type <- c(rep("no outliers\n(plots 1 and 5)" , 3) , rep("outliers\n(plots 3 and 8)" , 3))
score <- rep(c("all correct" , "1 correct" , "0 correct"), 2)
value <- c(sum(curr$noOutliersAllCorrect), sum(curr$noOutliers1Correct), sum(curr$noOutliers0Correct), sum(curr$outliersAllCorrect), sum(curr$outliers1Correct), sum(curr$outliers0Correct))
data <- data.frame(type,score,value)

p2 <- ggplot(data, aes(fill=score, y=value, x=type)) + 
  geom_bar(position="dodge", stat="identity") + ylab("Score") + xlab("") + geom_text(position="dodge", stat="identity", label = value)
p2

# average correct responses based on demographics

  # demographic question #1
accuracy_pre1 <- aggregate(answers_demographics$V9, list(answers_demographics$pre1), FUN=mean)[-2,]

pre1_bar <- ggplot(accuracy_pre1, aes(x=reorder(Group.1, -x), y=x, fill=Group.1)) + geom_bar(stat="identity") + theme(legend.position="none") + ylab("Avg. Correct") + xlab("Current Status") + 
      geom_text(aes(label=round(x, 2)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank()) + ggtitle("Average Correct Based on Experience")
pre1_box <- ggplot(filter(answers_demographics, pre1!="null"), aes(x=pre1, y=V9, fill=pre1)) + geom_boxplot() + xlab("Experience level") + ylab("") +
      theme(legend.position="none") + ggtitle("Average Correct Based on Experience")


  # demographic question #2
accuracy_pre2 <- aggregate(answers_demographics$V9, list(answers_demographics$pre2), FUN=mean)[-4,]

pre2_bar <- ggplot(accuracy_pre2, aes(x=reorder(Group.1, -x), y=x, fill=Group.1)) + geom_bar(stat="identity") + theme(legend.position="none") +
  geom_text(aes(label=round(x, 2)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank()) + ggtitle("Average Correct Based on Coursework")
pre2_box <- ggplot(filter(answers_demographics, pre2!="null"), aes(x=pre2, y=V9, fill=pre2)) + geom_boxplot() + xlab("Courses completed") + ylab("") +
  theme(legend.position="none")

  # demographic quesiton #3
accuracy_pre3 <- aggregate(answers_demographics$V9, list(answers_demographics$pre3), FUN=mean)[-3,]

pre3grouped <- filter(answers_demographics, pre3 != "null")
pre3grouped$pre3group <- if_else(pre3grouped$pre3 == "yes", "yes", "no/unsure")

pre3_bar <- ggplot(accuracy_pre3, aes(x=reorder(Group.1, -x), y=x, fill=Group.1)) + geom_bar(stat="identity") + theme(legend.position="none") + ylab("Avg. Correct") +
  geom_text(aes(label=round(x, 2)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank()) + ggtitle("Average Correct Based on Experience")
pre3_box <- ggplot(filter(answers_demographics, pre3!="null"), aes(x=pre3, y=V9, fill=pre3)) + geom_boxplot() + xlab("Seen q-q plot?") + ylab("") +
  theme(legend.position="none")


grid.arrange(pre1_bar, pre1_box)
grid.arrange(pre2_bar, pre2_box)
grid.arrange(pre3_bar, pre3_box)




pre3out <- aov(pre3grouped$V9~pre3grouped$pre2)
summary(pre3out)


pre2filter <- filter(answers_demographics, pre2 != "null")
pre2out <- aov(pre2filter$V9~pre2filter$pre2)
summary(pre2out)

out2 <- aov(pre2filter$V9~pre2filter$pre2)
summary(out2)

temp2 <- filter(answers_demographics, pre3 != "null")
temp2$pre3group <- if_else(temp2$pre3 == "yes", "yes", "no/unsure")

joined <- inner_join(answers_demographics, survey_order, by="userID")
s1 <- ggplot(joined, aes(x=survey1_ord, y=survey1_dist)) + geom_bar(position=position_dodge()) + theme(legend.position="none") + ylab("Avg. Correct") +
  geom_text(aes(label=round(x, 2)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank()) + ggtitle("Average Correct Based on Experience")
s1

#average correct responses based on 'have you seen a q-q plot before?'
exposure <- aggregate(answers_demographics$V10, list(answers_demographics$pre3), FUN=mean)

exposure_bar <- ggplot(exposure[-3,], aes(x=reorder(Group.1, -x), y=x, fill=Group.1)) + geom_bar(stat="identity") + theme(legend.position="none") + ylab("Avg. Correct") +
      geom_text(aes(label=round(x, 2)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank()) + ggtitle("Average Correct Based on Experience")
exposure_box <- ggplot(answers_demographics, aes(x=pre3, y=V10, fill=pre3)) + geom_boxplot() + xlab("Seen q-q plot?") + ylab("") +
      theme(legend.position="none")

yleft <- textGrob(expression(paste("Average Correct Responses")), 
                  rot = 90, gp = gpar(fontsize = 12))

# averages based on demographic questions
grid.arrange(level_bar, experience_bar, exposure_bar, level_box, experience_box, exposure_box, nrow = 2, top = "Preliminary Visualizations", left = yleft)

# table manipulation
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

# bar chart -- correctness by plot type
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


# more table manipulation w/ visualizations
# bar chart -- answer based on plot type

t1 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t1) <- names

t1[1,1] <- "none"
t1[2,1] <- "ref line"
t1[3,1] <- "ref line and bands"

t1[1,2] <- 3
t1[1,3] <- 0
t1[2,2] <- 20
t1[2,3] <- 3
t1[3,2:3] <- '0'

t1_long <- gather(t1, response, count, normal:'non-normal') #Create long format
t1_long$count <- as.numeric(t1_long$count)

t1_bar <- ggplot(t1_long, aes(x=features, y=count, fill=response)) + geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=count), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
t1_bar

t2 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t2) <- names

t2[1,1] <- "none"
t2[2,1] <- "ref line"
t2[3,1] <- "ref line and bands"

t2[1,2] <- melted2[4,3]
t2[1,3] <- melted2[1,3]
t2[2,2] <- melted2[5,3]
t2[2,3] <- melted2[2,3]
t2[3,2] <- melted2[6,3]
t2[3,3] <- melted2[3,3]

t2_long <- gather(t2, response, count, normal:'non-normal') #Create long format
t2_long$count <- as.numeric(t2_long$count)

t2_bar <- ggplot(t2_long, aes(x=features, y=count, fill=response)) + geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=count), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
t2_bar

t3 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t3) <- names

t3[1,1] <- "none"
t3[2,1] <- "ref line"
t3[3,1] <- "ref line and bands"

t3[1,2] <- melted3[1,3]
t3[1,3] <- melted3[3,3]
t3[2,2:3] <- 'NA'
t3[3,2] <- melted3[2,3]
t3[3,3] <- melted3[4,3]

t3_long <- gather(t3, response, count, normal:'non-normal') #Create long format
t3_long$count <- as.numeric(t3_long$count)

t3_bar <- ggplot(t3_long, aes(x=features, y=count, fill=response)) + geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=count), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
t3_bar

t4 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t4) <- names

t4[1,1] <- "none"
t4[2,1] <- "ref line"
t4[3,1] <- "ref line and bands"

t4[1,2] <- melted4[4,3]
t4[1,3] <- melted4[1,3]
t4[2,2] <- melted4[5,3]
t4[2,3] <- melted4[2,3]
t4[3,2] <- melted4[6,3]
t4[3,3] <- melted4[3,3]

t4_long <- gather(t4, response, count, normal:'non-normal') #Create long format
t4_long$count <- as.numeric(t4_long$count)

t4_bar <- ggplot(t4_long, aes(x=features, y=count, fill=response)) + geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=count), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
t4_bar

t5 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t5) <- names

t5[1,1] <- "none"
t5[2,1] <- "ref line"
t5[3,1] <- "ref line and bands"

t5[1,2] <- melted5[1,3]
t5[1,3] <- melted5[3,3]
t5[2,2:3] <- 'NA'
t5[3,2] <- melted5[2,3]
t5[3,3] <- melted5[4,3]

t5_long <- gather(t5, response, count, normal:'non-normal') #Create long format
t5_long$count <- as.numeric(t5_long$count)

t5_bar <- ggplot(t5_long, aes(x=features, y=count, fill=response)) + geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=count), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
t5_bar

t6 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t6) <- names

t6[1,1] <- "none"
t6[2,1] <- "ref line"
t6[3,1] <- "ref line and bands"

t6[1,3] <- melted6[1,3]
t6[1,2] <- melted6[3,3]
t6[2,3] <- melted6[2,3]
t6[2,2] <- melted6[4,3]
t6[3,2:3] <- 'NA'

t6_long <- gather(t6, response, count, normal:'non-normal') #Create long format
t6_long$count <- as.numeric(t6_long$count)

t6_bar <- ggplot(t6_long, aes(x=features, y=count, fill=response)) + geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=count), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
t6_bar

t7 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t7) <- names

t7[1,1] <- "none"
t7[2,1] <- "ref line"
t7[3,1] <- "ref line and bands"

t7[1,3] <- melted7[1,3]
t7[1,2] <- melted7[3,3]
t7[2,3] <- melted7[2,3]
t7[2,2] <- melted7[4,3]
t7[3,2:3] <- 'NA'

t7_long <- gather(t7, response, count, normal:'non-normal') #Create long format
t7_long$count <- as.numeric(t7_long$count)

t7_bar <- ggplot(t7_long, aes(x=features, y=count, fill=response)) + geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=count), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
t7_bar

t8 <- data.frame(matrix(ncol=3,nrow=3))
names <- c("features","normal","non-normal")
colnames(t8) <- names

t8[1,2] <- melted8[1,3]
t8[1,3] <- melted8[4,3]
t8[2,2] <- melted8[2,3]
t8[2,3] <- melted8[5,3]
t8[3,2] <- melted8[3,3]
t8[3,3] <- melted8[6,3]

t8[1,1] <- "none"
t8[2,1] <- "ref line"
t8[3,1] <- "ref line and bands"

t8_long <- gather(t8, response, count, normal:'non-normal') #Create long format
t8_long$count <- as.numeric(t8_long$count)

t8_bar <- ggplot(t8_long, aes(x=features, y=count, fill=response)) + geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=count), color="black", size=3.5, vjust = -.5, position = position_dodge(0.9))+ xlab("Plot features")+
  ylab("Responses")
t8_bar

grid.arrange(t1_bar, grid.arrange(Dataset1noRef,Dataset1ref,Dataset1bands), nrow=1, top = "Plot 1 (Pilot)")
grid.arrange(t2_bar, grid.arrange(Dataset2noRef,Dataset2ref,Dataset2bands), nrow=1, top = "Plot 2 (Pilot)")
grid.arrange(t3_bar, grid.arrange(Dataset3noRef,Dataset3ref,Dataset3bands), nrow=1, top = "Plot 3 (Pilot)")
grid.arrange(t4_bar, grid.arrange(Dataset4noRef,Dataset4ref,Dataset4bands), nrow=1, top = "Plot 4 (Pilot)")
grid.arrange(t5_bar, grid.arrange(Dataset5noRef,Dataset5ref,Dataset5bands), nrow=1, top = "Plot 5 (Pilot)")
grid.arrange(t6_bar, grid.arrange(Dataset6noRef,Dataset6ref,Dataset6bands), nrow=1, top = "Plot 6 (Pilot)")
grid.arrange(t7_bar, grid.arrange(Dataset7noRef_2,Dataset7ref_2,Dataset7bands_2), nrow=1, top = "Plot 7 (Pilot)")
grid.arrange(t8_bar, grid.arrange(Dataset8noRef_2,Dataset8ref_2,Dataset8bands_2), nrow=1, top = "Plot 8 (Pilot)")

# chi-square test for plots
chisq1 <- chisq.test(t1[,2:3])
chisq2 <- chisq.test(t2[,2:3])
chisq3 <- chisq.test(t3[,2:3])
chisq4 <- chisq.test(t4[,2:3])
chisq5 <- chisq.test(t5[,2:3])
chisq6 <- chisq.test(t6[,2:3])
chisq7 <- chisq.test(t7[,2:3])
chisq8 <- chisq.test(t8[,2:3])
