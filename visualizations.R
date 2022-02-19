# FINAL DATA VISUALIZATIONS
# Updated 2/3/2022

#libraries
install.packages("gridExtra")
install.packages("grid")
install.packages("gridtext")
install.packages("ggplot2")
install.packages("googlesheets4")
install.packages('dplyr')
install.packages("reshape2")
install.packages("janitor")
install.packages("multcomp")

library(grid)
library(gridExtra)
library(gridtext)
library(ggplot2)
library(googlesheets4)
library(dplyr)
library(reshape2)
library(tidyr)
library(janitor)
library(multcomp)

# dataframes created in this script...
# (d1) df_correct_responses: table of correct responses by userID
# (d2) df_answers_demographics: contains userID, demographic questions, 
#                           individual correct responses, and total count
#                           of correct responses
# (d3) df_dem1_counts: response counts grouped by Experience
# (d4) df_dem2_counts response counts grouped by Courses Completed
# (d5) df_dem3_counts response counts grouped by "Seen a Q-Q Plot?"
# (d6) df_dem1_table: table of average correct responses based on Experience Level (dem.1)
# (d7) df_dem2_table: table of average correct responses based on Courses Completed (dem.2)
# (d8) df_dem3_table: table of average correct responses based on "Seen a Q-Q Plot?" (dem.3)
# (d9) df_plot_variations: table of plot variation totals for each userID; overall totals at bottom
#                                 responses based on variations
# (d10) df_correct_by_variation: table of total correct based on plot variation (includes totals)
# (d11) df_incorrect_by_variation: table of total incorrect based on plot variation (totals included)
# (d12) df_responses_by_variation: table of 'normal' responses and total
#                                  based on variation
# (d13) df_outlier_comparison_normal: comparing responses for normal distributions,
#                                     outliers (p1 and p5) vs. no outliers (p3 and p8)
# (d14) df_outlier_comparison_uniform: comparing responses for uniform distributions,
#                                     outliers (p6) vs. no outliers (p4)
# (d15) df_outlier_comparison_f: comparing responses for f distributions, outliers (p7) vs. no outliers (p2)
# (d16) df_correct_chisq: ANSWERING Q1; contains chi-sq values of correct responses for all 8 plots
# (d17) df_incorrect_chisq: ANSWERING Q1: contains chi-sq values of incorrect responses for all 8 plots
# (d18) anova_experience_level: ANSWERING Q2; anova for correct responses based on exp.
 
# visualizations created in this script...
# (v1) dem1_bar: average correct responses based on Experience level (dem.1)
# (v2) dem1_box: average correct responses based on Experience level (dem.1)
# (v3) dem1_combo: grid.arrange of v1 and v2
# (v4) dem2_bar: average correct responses based on Courses Completed (dem.2)
# (v5) dem2_box: average correct responses based on Courses Completed (dem.2)
# (v6) dem2_combo: grid.arrange of v4 and v5
# (v7) dem3_bar: average correct responses based on "Seen a Q-Q Plot?" (dem.3)
# (v8) dem3_box: average correct responses based on "Seen a Q-Q Plot?" (dem.3)
# (v9) dem3_combo: grid.arrange of v7 and v8

############
# raw data #
############
final_pre <- read_sheet("https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1629775005", sheet = "pre_survey")
final_survey <- read_sheet("https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1629775005", sheet = "survey")
post_survey <- read_sheet("https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1629775005", sheet = "post_survey" )
survey_order <- read_sheet("https://docs.google.com/spreadsheets/d/1cTMfOSR_l9S57YTX_i1SXhJvl0SVcVbU550NJD8OHSw/edit#gid=1629775005", sheet = "survey_order")

#############
# solutions #
#############
correct <- c("normal","non-normal","normal","non-normal","normal","non-normal","non-normal","normal")
distributions <- c("normal","f","normal","uniform","normal","uniform","f","normal")
outliers <- c("no","no","yes","no","no","yes","yes","yes")

##############
# dataframes #
##############

# d1 #
x <- data.frame(zero=logical(),one=logical(),two=logical(),three=logical(),four=logical(),five=logical(),six=logical(),seven=logical())

for (i in 1:nrow(final_survey)) {
  comparison <- subset(final_survey[i,], select = -c(userID, survey0_dist)) == correct
  total <- cbind(comparison, sum(comparison==TRUE))
  x <- rbind(x,total)
}

colnames(x)[colnames(x) == "V9"] <- "Total_Correct"
df_correct_responses <- cbind(final_pre[,1], x)

# d2 #
df_answers_demographics <- cbind(final_pre, x)

# d3 #
df_dem1_counts <- table(df_answers_demographics$pre1)

# d4 #
df_dem2_counts <- table(df_answers_demographics$pre2)

# d5 #
df_dem3_counts <- table(df_answers_demographics$pre3)

# d6 #
df_dem1_table <- aggregate(df_answers_demographics$"Total_Correct", list(df_answers_demographics$pre1), FUN=mean)[-2,] # last bit removes "null" responses

# d7 #
df_dem2_table <- aggregate(df_answers_demographics$"Total_Correct", list(df_answers_demographics$pre2), FUN=mean)[-4,]

# d8 #
df_dem3_table <- aggregate(df_answers_demographics$"Total_Correct", list(df_answers_demographics$pre3), FUN=mean)[-3,]

# d9 #
x2 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = -c(survey0_dist))

x2$noFeaturesCount <- 0
x2$refLineCount <- 0
x2$bandsCount <- 0

for (j in 1:nrow(x2)){
  for (i in 10:17){
    if (x2[j,i] == 3){
      x2[j,]$bandsCount <- x2[j,]$bandsCount +1
    }
    if (x2[j,i] == 2){
      x2[j,]$refLineCount <- x2[j,]$refLineCount +1
    }
    if (x2[j,i] == 1){
      x2[j,]$noFeaturesCount <- x2[j,]$noFeaturesCount +1
    }
  }
}

df_plot_variations = x2[-c(2:17)]
df_plot_variations <- df_plot_variations %>% adorn_totals("row")

# d10 #
x3 <- subset(merge(x=df_correct_responses,y=survey_order, by="userID"))

x3$noFeaturesCorrect <- 0
x3$refLineCorrect <- 0
x3$bandsCorrect <- 0

for (i in 1:nrow(x3)){ # iterating through all rows
  for (j in 2:9){
    if (x3[i,j] == 1){
      if (x3[i,j+9] == 1){
        x3[i,]$noFeaturesCorrect <- x3[i,]$noFeaturesCorrect + 1
      }
      if (x3[i,j+9] == 2){
        x3[i,]$refLineCorrect <- x3[i,]$refLineCorrect + 1
      }
      if (x3[i,j+9] == 3){
        x3[i,]$bandsCorrect <- x3[i,]$bandsCorrect + 1
      }
    }
  }
}

df_correct_by_variation <- subset(x3, select = c(userID, noFeaturesCorrect, refLineCorrect, bandsCorrect))
df_correct_by_variation <- merge(df_correct_by_variation, df_plot_variations, by = "userID")
df_correct_by_variation <- df_correct_by_variation %>% adorn_totals("row")

# d11 #
x3 <- subset(merge(x=df_correct_responses,y=survey_order, by="userID"))

x3$noFeaturesCorrect <- 0
x3$refLineCorrect <- 0
x3$bandsCorrect <- 0

for (i in 1:nrow(x3)){ # iterating through all rows
  for (j in 2:9){
    if (x3[i,j] == 0){
      if (x3[i,j+9] == 1){
        x3[i,]$noFeaturesCorrect <- x3[i,]$noFeaturesCorrect + 1
      }
      if (x3[i,j+9] == 2){
        x3[i,]$refLineCorrect <- x3[i,]$refLineCorrect + 1
      }
      if (x3[i,j+9] == 3){
        x3[i,]$bandsCorrect <- x3[i,]$bandsCorrect + 1
      }
    }
  }
}

df_incorrect_by_variation <- subset(x3, select = c(userID, noFeaturesCorrect, refLineCorrect, bandsCorrect))
df_incorrect_by_variation <- merge(df_incorrect_by_variation, df_plot_variations, by = "userID")
df_incorrect_by_variation <- df_incorrect_by_variation %>% adorn_totals("row")



# d12 #
x4 <- merge(x=subset(final_survey, select = -c(survey0_dist)), y=survey_order, by="userID")
x4$normNoFeatures <- 0
x4$normRefLine <- 0
x4$normBands <- 0

for (i in 1:nrow(x4)){ # iterating through all rows
  for (j in 2:9){
    if (x4[i,j] == "normal"){
      if (x4[i,j+8] == 1){
        x4[i,]$normNoFeatures <- x4[i,]$normNoFeatures + 1
      }
      if (x4[i,j+8] == 2){
        x4[i,]$normRefLine <- x4[i,]$normRefLine + 1
      }
      if (x4[i,j+8] == 3){
        x4[i,]$normBands <- x4[i,]$normBands + 1
      }
    }
  }
}

df_responses_by_variation <- subset(x4, select = c(userID, normNoFeatures, normRefLine, normBands))
df_responses_by_variation <- df_responses_by_variation %>% adorn_totals("row")

# d13 #
x5 <- subset(final_survey, select = -c(survey0_dist))
x5$outliersAllCorrect <- 0
x5$outliers1Correct <- 0
x5$outliers0Correct <- 0
x5$noOutliersAllCorrect <- 0
x5$noOutliers1Correct <- 0
x5$noOutliers0Correct <- 0

for (i in 1:123){
  # outliers
  if (x5[i,4] == "normal" && x5[i,9] == "normal"){
    x5[i,]$outliersAllCorrect <- x5[i,]$outliersAllCorrect +1
  }
  if (x5[i,4] == "normal" && x5[i,9] != "normal"){
    x5[i,]$outliers1Correct <- x5[i,]$outliers1Correct +1
  }
  if (x5[i,4] != "normal" && x5[i,9] == "normal"){
    x5[i,]$outliers1Correct <- x5[i,]$outliers1Correct +1
  }
  if (x5[i,4] != "normal" && x5[i,9] != "normal"){
    x5[i,]$outliers0Correct <- x5[i,]$outliers0Correct +1
  }
  # no outliers
  if (x5[i,2] == "normal" && x5[i,6] == "normal"){
    x5[i,]$noOutliersAllCorrect <- x5[i,]$noOutliersAllCorrect +1
  }
  if (x5[i,2] == "normal" && x5[i,6] != "normal"){
    x5[i,]$noOutliers1Correct <- x5[i,]$noOutliers1Correct +1
  }
  if (x5[i,2] != "normal" && x5[i,6] == "normal"){
    x5[i,]$noOutliers1Correct <- x5[i,]$noOutliers1Correct +1
  }
  if (x5[i,2] != "normal" && x5[i,6] != "normal"){
    x5[i,]$noOutliers0Correct <- x5[i,]$noOutliers0Correct +1
  }
}

df_outlier_comparison_normal <- x5[-c(2:9)]
df_outlier_comparison_normal <- df_outlier_comparison_normal %>% adorn_totals("row")

# d14 #
x6 <- subset(final_survey, select = -c(survey0_dist))
x6$outliersCorrect <- 0
x6$outliersIncorrect <- 0
x6$noOutliersCorrect <- 0
x6$noOutliersIncorrect <- 0

for (i in 1:123){
  # outliers
  if (x6[i,7] == "non-normal"){
    x6[i,]$outliersCorrect <- x6[i,]$outliersCorrect +1
  }
  if (x6[i,7] != "non-normal"){
    x6[i,]$outliersIncorrect <- x6[i,]$outliersIncorrect +1
  }
  # no outliers
  if (x6[i,5] == "non-normal"){
    x6[i,]$noOutliersCorrect <- x6[i,]$noOutliersCorrect +1
  }
  if (x6[i,5] != "non-normal"){
    x6[i,]$noOutliersIncorrect <- x6[i,]$noOutliersIncorrect +1
  }
}

df_outlier_comparison_uniform <- x6[-c(2:9)]
df_outlier_comparison_uniform <- df_outlier_comparison_uniform %>% adorn_totals("row")

# d15 #
x7 <- subset(final_survey, select = -c(survey0_dist))
x7$outliersCorrect <- 0
x7$outliersIncorrect <- 0
x7$noOutliersCorrect <- 0
x7$noOutliersIncorrect <- 0

for (i in 1:123){
  # outliers
  if (x7[i,8] == "non-normal"){
    x7[i,]$outliersCorrect <- x7[i,]$outliersCorrect +1
  }
  if (x7[i,8] != "non-normal"){
    x7[i,]$outliersIncorrect <- x7[i,]$outliersIncorrect +1
  }
  # no outliers
  if (x7[i,3] == "non-normal"){
    x7[i,]$noOutliersCorrect <- x7[i,]$noOutliersCorrect +1
  }
  if (x7[i,3] != "non-normal"){
    x7[i,]$noOutliersIncorrect <- x7[i,]$noOutliersIncorrect +1
  }
}

df_outlier_comparison_f <- x7[-c(2:9)]
df_outlier_comparison_f <- df_outlier_comparison_f %>% adorn_totals("row")

# d16 #

# split by plot
p1 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey1_dist, survey1_ord)) 
p2 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey2_dist, survey2_ord)) 
p3 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey3_dist, survey3_ord)) 
p4 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey4_dist, survey4_ord))
p5 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey5_dist, survey5_ord)) 
p6 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey6_dist, survey6_ord)) 
p7 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey7_dist, survey7_ord)) 
p8 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey8_dist, survey8_ord)) 

# get totals
p9 <- aggregate(p1$survey1_dist, by=list(p1$survey1_ord), FUN=length)
p10 <- aggregate(p2$survey2_dist, by=list(p2$survey2_ord), FUN=length)
p11 <- aggregate(p3$survey3_dist, by=list(p3$survey3_ord), FUN=length)
p12 <- aggregate(p4$survey4_dist, by=list(p4$survey4_ord), FUN=length)
p13 <- aggregate(p5$survey5_dist, by=list(p5$survey5_ord), FUN=length)
p14 <- aggregate(p6$survey6_dist, by=list(p6$survey6_ord), FUN=length)
p15 <- aggregate(p7$survey7_dist, by=list(p7$survey7_ord), FUN=length)
p16 <- aggregate(p8$survey8_dist, by=list(p8$survey8_ord), FUN=length)

# how many were correct
p17 <- p1 %>% filter(survey1_dist == "normal")
p17 <- aggregate(p17$survey1_dist, by=list(p17$survey1_ord), FUN=length)

p18 <- p2 %>% filter(survey2_dist == "non-normal")
p18 <- aggregate(p18$survey2_dist, by=list(p18$survey2_ord), FUN=length)

p19 <- p3 %>% filter(survey3_dist == "normal")
p19 <- aggregate(p19$survey3_dist, by=list(p19$survey3_ord), FUN=length)

p20 <- p4 %>% filter(survey4_dist == "non-normal")
p20 <- aggregate(p20$survey4_dist, by=list(p20$survey4_ord), FUN=length)

p21 <- p5 %>% filter(survey5_dist == "normal")
p21 <- aggregate(p21$survey5_dist, by=list(p21$survey5_ord), FUN=length)

p22 <- p6 %>% filter(survey6_dist == "non-normal")
p22 <- aggregate(p22$survey6_dist, by=list(p22$survey6_ord), FUN=length)

p23 <- p7 %>% filter(survey7_dist == "non-normal")
p23 <- aggregate(p23$survey7_dist, by=list(p23$survey7_ord), FUN=length)

p24 <- p8 %>% filter(survey8_dist == "normal")
p24 <- aggregate(p24$survey8_dist, by=list(p24$survey8_ord), FUN=length)

df_correct_chisq <- merge(p17, p18, by = "Group.1") %>% merge(p19, by = "Group.1") %>% 
  merge(p20, by = "Group.1") %>% merge(p21, by = "Group.1") %>% 
  merge(p22, by = "Group.1") %>% merge(p23, by = "Group.1") %>% 
  merge(p24, by = "Group.1") 

names(df_correct_chisq)[1] <- "feature"
names(df_correct_chisq)[2] <- "plot1"
names(df_correct_chisq)[3] <- "plot2"
names(df_correct_chisq)[4] <- "plot3"
names(df_correct_chisq)[5] <- "plot4"
names(df_correct_chisq)[6] <- "plot5"
names(df_correct_chisq)[7] <- "plot6"
names(df_correct_chisq)[8] <- "plot7"
names(df_correct_chisq)[9] <- "plot8"

df_correct_chisq[1,1] <- "none"
df_correct_chisq[2,1] <- "refline"
df_correct_chisq[3,1] <- "bands"

df_correct_chisq[nrow(df_correct_chisq)+1,] <- NA
df_correct_chisq[nrow(df_correct_chisq)+1,] <- NA

df_correct_chisq[4,1] <- "chi-square"
df_correct_chisq[5,1] <- "p-value"

for (i in 2:9){
  out <- chisq.test(df_correct_chisq[1:3, i], p = c(1/3, 1/3, 1/3))
  df_correct_chisq[4,i] <- out$statistic
  df_correct_chisq[5,i] <- out$p.value 
}

# get percentages
p17$x <- p17$x / sum(p17$x)
p18$x <- p18$x / sum(p18$x)
p19$x <- p19$x / sum(p19$x)
p20$x <- p20$x / sum(p20$x)
p21$x <- p21$x / sum(p21$x)
p22$x <- p22$x / sum(p22$x)
p23$x <- p23$x / sum(p23$x)
p24$x <- p24$x / sum(p24$x)

# d17 #

# split by plot
p1 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey1_dist, survey1_ord)) 
p2 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey2_dist, survey2_ord)) 
p3 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey3_dist, survey3_ord)) 
p4 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey4_dist, survey4_ord))
p5 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey5_dist, survey5_ord)) 
p6 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey6_dist, survey6_ord)) 
p7 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey7_dist, survey7_ord)) 
p8 <- subset(merge(x=final_survey,y=survey_order,by="userID"), select = c(survey8_dist, survey8_ord)) 

# get totals
p9 <- aggregate(p1$survey1_dist, by=list(p1$survey1_ord), FUN=length)
p10 <- aggregate(p2$survey2_dist, by=list(p2$survey2_ord), FUN=length)
p11 <- aggregate(p3$survey3_dist, by=list(p3$survey3_ord), FUN=length)
p12 <- aggregate(p4$survey4_dist, by=list(p4$survey4_ord), FUN=length)
p13 <- aggregate(p5$survey5_dist, by=list(p5$survey5_ord), FUN=length)
p14 <- aggregate(p6$survey6_dist, by=list(p6$survey6_ord), FUN=length)
p15 <- aggregate(p7$survey7_dist, by=list(p7$survey7_ord), FUN=length)
p16 <- aggregate(p8$survey8_dist, by=list(p8$survey8_ord), FUN=length)

# how many were incorrect
p17 <- p1 %>% filter(survey1_dist != "normal")
p17 <- aggregate(p17$survey1_dist, by=list(p17$survey1_ord), FUN=length)

p18 <- p2 %>% filter(survey2_dist != "non-normal")
p18 <- aggregate(p18$survey2_dist, by=list(p18$survey2_ord), FUN=length)

p19 <- p3 %>% filter(survey3_dist != "normal")
p19 <- aggregate(p19$survey3_dist, by=list(p19$survey3_ord), FUN=length)

p20 <- p4 %>% filter(survey4_dist != "non-normal")
p20 <- aggregate(p20$survey4_dist, by=list(p20$survey4_ord), FUN=length)

p21 <- p5 %>% filter(survey5_dist != "normal")
p21 <- aggregate(p21$survey5_dist, by=list(p21$survey5_ord), FUN=length)

p22 <- p6 %>% filter(survey6_dist != "non-normal")
p22 <- aggregate(p22$survey6_dist, by=list(p22$survey6_ord), FUN=length)

p23 <- p7 %>% filter(survey7_dist != "non-normal")
p23 <- aggregate(p23$survey7_dist, by=list(p23$survey7_ord), FUN=length)

p24 <- p8 %>% filter(survey8_dist != "normal")
p24 <- aggregate(p24$survey8_dist, by=list(p24$survey8_ord), FUN=length)

df_incorrect_chisq <- merge(p17, p18, by = "Group.1") %>% merge(p19, by = "Group.1") %>% 
  merge(p20, by = "Group.1") %>% merge(p21, by = "Group.1") %>% 
  merge(p22, by = "Group.1") %>% merge(p23, by = "Group.1") %>% 
  merge(p24, by = "Group.1") 

names(df_incorrect_chisq)[1] <- "feature"
names(df_incorrect_chisq)[2] <- "plot1"
names(df_incorrect_chisq)[3] <- "plot2"
names(df_incorrect_chisq)[4] <- "plot3"
names(df_incorrect_chisq)[5] <- "plot4"
names(df_incorrect_chisq)[6] <- "plot5"
names(df_incorrect_chisq)[7] <- "plot6"
names(df_incorrect_chisq)[8] <- "plot7"
names(df_incorrect_chisq)[9] <- "plot8"

df_incorrect_chisq[1,1] <- "none"
df_incorrect_chisq[2,1] <- "refline"
df_incorrect_chisq[3,1] <- "bands"

df_incorrect_chisq[nrow(df_incorrect_chisq)+1,] <- NA
df_incorrect_chisq[nrow(df_incorrect_chisq)+1,] <- NA

df_incorrect_chisq[4,1] <- "chi-square"
df_incorrect_chisq[5,1] <- "p-value"

for (i in 2:9){
  out <- chisq.test(df_incorrect_chisq[1:3, i], p = c(1/3, 1/3, 1/3))
  df_incorrect_chisq[4,i] <- out$statistic
  df_incorrect_chisq[5,i] <- out$p.value 
}

# get percentages
p17$x <- p17$x / sum(p17$x)
p18$x <- p18$x / sum(p18$x)
p19$x <- p19$x / sum(p19$x)
p20$x <- p20$x / sum(p20$x)
p21$x <- p21$x / sum(p21$x)
p22$x <- p22$x / sum(p22$x)
p23$x <- p23$x / sum(p23$x)
p24$x <- p24$x / sum(p24$x)


# d18 #

# checking variances
group_by(df_answers_demographics, pre2) %>% summarize(var(Total_Correct))

undergrads <- filter(df_answers_demographics, pre1 == "undergraduate")
undergrads <- filter(undergrads, pre2 != "null")

group_by(undergrads, pre2) %>% summarize(var(Total_Correct))

# testing homogeneity of variances
bartlett <- bartlett.test(Total_Correct ~ pre2, data = filter(undergrads, pre2 != "null"))
bartlett

# anova test on for correct responses
out <- aov(Total_Correct ~ pre2, data = undergrads)
anova_experience_level <- summary(out)

# confidence intervals
intervals <- pairwise.t.test(undergrads$Total_Correct, filter(undergrads, pre2 != "null")$pre2, p.adjust.method = "bonferroni")

tukey <- simint(undergrads$Total_Correct ~ undergrads$pre2, data = undergrads, conf.level = 0.95)

### END OF DATAFRAMES ###

##################
# Visualizations #
##################

# v1 #
dem1_bar <- ggplot(df_dem1_table, aes(x=Group.1, y=x, fill=Group.1)) + geom_bar(stat="identity") + 
  theme(legend.position="none") + ylab("Avg. Correct") +  
  geom_text(aes(label=round(x, 2)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank()) + 
  scale_x_discrete(limits = c("undergraduate", "graduate", "professional"))

# v2 #
temp1 <- filter(df_answers_demographics, pre1!="null")
dem1_box <- ggplot(temp1, aes(x=pre1, y=temp1$"Total_Correct", fill = pre1)) + geom_boxplot() + 
  ylab("") + theme(legend.position="none")  + geom_jitter() + theme(axis.title.x=element_blank()) +
  scale_x_discrete(limits = c("undergraduate", "graduate", "professional"))

# v3 #
dem1_combo <- grid.arrange(dem1_bar, dem1_box, nrow = 1)

# v4 #
dem2_bar <- ggplot(df_dem2_table, aes(x=Group.1, y=x, fill=Group.1)) + geom_bar(stat="identity") + 
  theme(legend.position="none") + ylab("Avg. Correct") +  
  geom_text(aes(label=round(x, 2)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank()) + 
  scale_x_discrete(limits = c("0 courses", "1-2 courses", "3+ courses"))

# v5 #
dem2_box <- ggplot(temp2, aes(x=pre2, y=temp2$"Total_Correct", fill = pre2)) + geom_boxplot() + 
  ylab("") + theme(legend.position="none")  + geom_jitter() + theme(axis.title.x=element_blank()) +
  scale_x_discrete(limits = c("0 courses", "1-2 courses", "3+ courses"))

# v6 #
dem2_combo <- grid.arrange(dem2_bar, dem2_box, nrow = 1)

# v7 #
dem3_bar <- ggplot(df_dem3_table, aes(x=Group.1, y=x, fill=Group.1)) + geom_bar(stat="identity") + 
  theme(legend.position="none") + ylab("Avg. Correct") +  
  geom_text(aes(label=round(x, 2)), vjust=1.6, color="white", size=3.5) + theme(axis.title.x=element_blank()) + 
  scale_x_discrete(limits = c("yes", "no", "not sure"))

# v8 #
temp3 <- filter(df_answers_demographics, pre3!="null")
dem3_box <- ggplot(temp3, aes(x=pre3, y=temp3$"Total_Correct", fill = pre3)) + geom_boxplot() + 
  ylab("") + theme(legend.position="none")  + geom_jitter() + theme(axis.title.x=element_blank()) +
  scale_x_discrete(limits = c("yes", "no", "not sure"))

# v9 #
dem3_combo <- grid.arrange(dem3_bar, dem3_box, nrow = 1)

### END OF VISUALIZATIONS ###
