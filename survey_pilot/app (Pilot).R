# A survey designed to present a variety of Q-Q Plots to survey participants for
# normality assessment
# (PILOT)
# Created by Sam Bacon

#install.packages("shinydashboard")
#install.packages("shinythemes")

library(shiny)
library(shinythemes)
library(googlesheets4)
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)


ui <- fluidPage(
    
    theme = shinytheme("flatly"),
    
    conditionalPanel(condition="input.agr==0",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     p("The purpose of this study is to `gain a better understanding as to what variations of 
                     Q-Q Plots allow viewers to accurately assess the normality of data. Your participation 
                     in this research is completely optional. You may choose not to participate, and you may also 
                     withdraw from the study at any time by exiting the survey. There will be no penalty for 
                     not participating or for withdrawing from the survey, although participants who withdraw 
                     will not be eligible to receive the incentive ($5 Amazon gift card)."),
                     br(),
                     p("Participation in this study includes the following survey which will take approximately 
                       five minutes to complete. The survey consists of three demographic questions to 
                       gauge your statistical experience and familiarity with Quantile-Quantile (Q-Q) Plots. 
                       Afterwards, you will be asked to view 9 Q-Q Plots. For each visualization, you will 
                       determine whether the data appears to meet the normality assessment. Finally, you 
                       will be asked three feedback questions about which visualizations you did and did 
                       not prefer. This survey does not require you to share any identifiable information. 
                       However, upon completion of the survey, you will have the option to share your email 
                       address in a separate survey to receive a $5 Amazon gift card. The first 50 respondents will
                       receive the incentive. If you share your email 
                       address, it will be added to a list that can only be accessed by the co-investigator. 
                       Your email address will not be shared with any additional parties, and it will be 
                       removed from our records as soon as you have received the incentive via email. "),
                     strong("If you fail to select a response, the survey will end immediately and your answers will not be counted."),
                     br(),
                     p("Please contact one of the primary researchers with any questions about the survey:"),
                     p("Sam Bacon (sbacon3@elon.edu)"),
                     p("Dr. Laura Taylor (ltaylor18@elon.edu)"),
                     textOutput("agr"),
                     br(),
                     p("By clicking “Agree” below you acknowledge that you have read and understand the above 
                     information, you are 18 or older, and you agree to participate."),
                     br(),
                     actionButton("agr","Agree"),
                     br()
    ),
    
    # Demographic Questions Panel
    conditionalPanel(condition="input.agr==1 && input.beg == 0",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     fluidRow(
                         column(12,
                                h3("Demographic Questions:")
                         )
                     ),
                     

                     h4("Please respond to the following questions regarding your prior 
                       experience with statistics and Q-Q Plot visualization tools:"),
                     
                     # Demographic Question 1
                     radioButtons("dem1", "What is your current academic level?",
                                  c("Select a response" = "null",
                                    "Undergraduate" = "undergraduate",
                                    "Graduate" = "graduate",
                                    "Professional" = "professional")
                                  ),
                     # Demographic Question 2
                     radioButtons("dem2", "How many statistics courses have you completed?",
                                  c("Select a response" = "null",
                                    "0" = "0 courses",
                                    "1-2" = "1-2 courses",
                                    "More than 2" = "3+ courses")),
                     # Demographic Question 3
                     radioButtons("dem3", "Have you ever seen a Q-Q Plot before?", 
                                  c("Select a response" = "null",
                                    "Yes" = "yes",
                                    "No" = "no",
                                    "Not sure" = "not sure")),
                     
                     
                     h5("Progress: 0% Complete", align = "right"),
                     textOutput("demographic"),
                     button <- uiOutput("demoCont"),
                     actionButton("beg","Continue")
                     
                     
                     
    ),
    
    # Instructions and Plot0 Panel
    conditionalPanel(condition = "input.beg == 1 && input.plot0 == 0",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     fluidRow(
                         column(12, 
                                h3("Instructions:")
                         )
                     ),
                     h4("Please read the following instructions before proceeding..."),
                     
                     p("For each of the following Q-Q Plots, you will respond by selecting whether 
                        the data appears to come from a normally distributed population. If you are unable 
                        to draw a conclusion based on the plot, please select that response. Please select
                        an answer for the Q-Q Plot shown below before proceeding."),
                     
                     # Plot 0 upload
                    plotOutput("plot0"),
                    
                     # Plot 0 response
                     radioButtons("response0", "Based on the Q-Q Plot of sample data,",
                                  c("Select a response" = "null",
                                    "the population distribution is normal" = "normal",
                                    "the population distribution is non-normal" = "non-normal",
                                    "we cannot determine the population distribution" = "unable to tell")),
                     
                     h5("Progress: 9% Complete", align = "right"),
                    textOutput("selection0"),
                     actionButton("plot0","Begin Survey")
    ),
    
    # Plot1 Panel
    conditionalPanel(condition = "input.plot0 == 1 && input.plot1 == 0 && input.response0 != 'null'",
                     
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     fluidRow(
                         column(12, 
                                h3("QQ-Plot #1")
                         )
                     ),
                     
                    plotOutput("plot1"),
                     
                     # Plot 1 response
                    radioButtons("response1", "Based on the Q-Q Plot of sample data, . . .",
                                 c("Select a response" = "null",
                                   "the population distribution is normal" = "normal",
                                   "the population distribution is non-normal" = "non-normal",
                                   "we cannot determine the population distribution" = "unable to tell")),

                     h5("Progress: 18% Complete", align = "right"),
                    textOutput("selection1"),
                    actionButton("plot1", "Continue")
    ),
     
    # Plot2 Panel
    conditionalPanel(condition = "input.plot1 == 1 && input.plot2 == 0
                     && input.response1 != 'null'",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     fluidRow(
                         column(12, h3("QQ-Plot #2")
                         )
                     ),
                     
                     # Plot 2 upload
                     plotOutput("plot2"),
                     
                     # Plot 2 response
                     radioButtons("response2", "Based on the Q-Q Plot of sample data, . . .",
                                  c("Select a response" = "null",
                                    "the population distribution is normal" = "normal",
                                    "the population distribution is non-normal" = "non-normal",
                                    "we cannot determine the population distribution" = "unable to tell")),

                     
                     
                     h5("Progress: 27% Complete", align = "right"),
                     textOutput("selection2"),
                     actionButton("plot2","Continue")
    ), 
    
    # Plot3 Panel
    conditionalPanel(condition = "input.plot2 == 1 && input.plot3 == 0",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     fluidRow(
                         column(12, h3("QQ-Plot #3")
                         )
                     ),
                     
                     # Plot 3 upload
                     plotOutput("plot3"),
                     
                     # Plot 3 response
                     radioButtons("response3", "Based on the Q-Q Plot of sample data, . . .",
                                  c("Select a response" = "null",
                                    "the population distribution is normal" = "normal",
                                    "the population distribution is non-normal" = "non-normal",
                                    "we cannot determine the population distribution" = "unable to tell")),

                     
                     h5("Progress: 36% Complete", align = "right"),                   
                     textOutput("selection3"),
                     actionButton("plot3","Continue")
    ),     
    
    # Plot4 Panel
    conditionalPanel(condition = "input.plot3 == 1 && input.plot4 == 0 && input.response3 != 'null'",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     fluidRow(
                         column(12, h3("QQ-Plot #4")
                         )
                     ),
                     
                     # Plot 4 upload
                     plotOutput("plot4"),   
                     
                     # Plot 4 response
                     radioButtons("response4", "Based on the Q-Q Plot of sample data, . . .",
                                  c("Select a response" = "null",
                                    "the population distribution is normal" = "normal",
                                    "the population distribution is non-normal" = "non-normal",
                                    "we cannot determine the population distribution" = "unable to tell")),

                     
                     h5("Progress: 45% Complete", align = "right"),
                     textOutput("selection4"),
                     actionButton("plot4","Continue")
    ), 
    
    # Plot5 Panel
    conditionalPanel(condition = "input.plot4 == 1 && input.plot5 == 0 
                     && input.response4 != 'null'",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     fluidRow(
                         column(12, h3("QQ-Plot #5")
                         )
                     ),
                     
                     # Plot 5 upload
                     plotOutput("plot5"), 
                     
                     # Plot 5 response
                     radioButtons("response5", "Based on the Q-Q Plot of sample data, . . .",
                                  c("Select a response" = "null",
                                    "the population distribution is normal" = "normal",
                                    "the population distribution is non-normal" = "non-normal",
                                    "we cannot determine the population distribution" = "unable to tell")),

                     
                     h5("Progress: 54% Complete", align = "right"),
                     textOutput("selection5"),
                     actionButton("plot5","Continue")
    ), 
    
    # Plot6 Panel
    conditionalPanel(condition = "input.plot5 == 1
                     && input.plot6 == 0 && input.response5 != 'null'",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     fluidRow(
                         column(12, h3("QQ-Plot #6")
                         )
                     ),
                     
                     # Plot 6 upload
                     plotOutput("plot6"),   
                     
                     # Plot 6 response
                     radioButtons("response6", "Based on the Q-Q Plot of sample data, . . .",
                                  c("Select a response" = "null",
                                    "the population distribution is normal" = "normal",
                                    "the population distribution is non-normal" = "non-normal",
                                    "we cannot determine the population distribution" = "unable to tell")),

                     
                     h5("Progress: 63% Complete", align = "right"),
                     textOutput("selection6"),
                     actionButton("plot6","Continue")
    ), 
    
    # Plot7 Panel
    conditionalPanel(condition = "input.plot6 == 1 && input.plot7 == 0 && input.response6 != 'null'",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     fluidRow(
                         column(12, h3("QQ-Plot #7")
                         )
                     ),
                     
                     # Plot 7 upload
                     plotOutput("plot7"),
                     
                     # Plot 7 response
                     radioButtons("response7", "Based on the Q-Q Plot of sample data, . . .",
                                 c("Select a response" = "null",
                                   "the population distribution is normal" = "normal",
                                   "the population distribution is non-normal" = "non-normal",
                                   "we cannot determine the population distribution" = "unable to tell")),

                     
                     h5("Progress: 72% Complete", align = "right"), 
                     textOutput("selection7"),
                     actionButton("plot7","Continue")
    ), 
    
    # Plot8 Panel
    conditionalPanel(condition = "input.plot7 == 1 && input.plot8 == 0 && input.response7 != 'null'",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     br(),
                     fluidRow(
                         column(12, h3("QQ-Plot #8")
                         )
                     ),
                     
                     # Plot 8 upload
                     plotOutput("plot8"),    
                     
                     # Plot 8 response
                     radioButtons("response8", "Based on the Q-Q Plot of sample data, . . .",
                                  c("Select a response" = "null",
                                    "the population distribution is normal" = "normal",
                                    "the population distribution is non-normal" = "non-normal",
                                    "we cannot determine the population distribution" = "unable to tell")),

                     h5("Progress: 81% Complete", align = "right"),
                     textOutput("selection8"),
                     actionButton("plot8","Continue")
    ),
    
    # Post-Survey Questions Panel
    conditionalPanel(condition = "input.plot8 == 1 && input.submit == 0",
                     br(),
                     h1("Assessing Normality Using Q-Q Plots"),
                     fluidRow(
                         column(12,
                                # insert instructions
                                h3("Post-Survey Questions:")
                         )
                     ),
                     h4("Please answer the following questions about your experiences with 
                         the QQ-Plots."),
                     br(),
                     h4("Rank the visualizations in terms of helpfulness with assessing normality. 
                        A rank of 1 should denote the most helpful visualization and 3 should denote the least helpful 
                        visualization"),
                     # Post-Survey Question 1
                     radioButtons("rank1", "QQ-Plot with no additional features",
                                  c("1" = "1",
                                    "2" = "2",
                                    "3" = "3")),
                     # Post-Survey Question 2
                     radioButtons("rank2", "QQ-Plot with reference line only",
                                  c("1" = "1",
                                    "2" = "2",
                                    "3" = "3")),
                     # Post-Survey Question 3
                     radioButtons("rank3", "QQ-Plot with reference line and confidence band",
                                  c("1" = "1",
                                    "2" = "2",
                                    "3" = "3")),
                     # Post-Survey Question 4
                     textInput("rank4", "Which of the visualizations is your most-preferred for assessing normality? Why?", 
                               value = "", width = 700, placeholder = NULL),
                     # Post-Survey Question 5
                     textInput("rank5", "Which of the visualizations is your least-preferred for assessing normality? Why?", 
                               value = "", width = 700, placeholder = NULL),
                     h5("Progress: 90% Complete", align = "right"),
                     actionButton("submit","Finish Survey")
                     
                     
    ),
    
    # Survey Complete!
    conditionalPanel(condition = "input.submit == 1",
                     br(),
                     
                     h1("Assessing Normality Using Q-Q Plots"),
                     fluidRow(
                         column(12, h3("Uploading your responses..."))
                     ),
                     
                     textOutput("submission"),
                     br(),
                     textOutput("incentiveKey")
                    
                     
    )
    
)



server <- function(input, output) {
    
    myIndicators <- reactive({
        ind1 <- sample(1:3, 3, replace = FALSE)
        ind2 <- sample(1:3, 3, replace = FALSE)
        ind3 <- sample(1:3, 2, replace = FALSE)
        
        userKeyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=1525817837')
        sheet <- read_sheet(userKeyLocation, sheet = "survey_order")
        userID <- length(sheet$userID) +1
        
        indFrame <- data.frame(userID, t(c(ind1, ind2, ind3)))
        ss <- gs4_get('https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=302243109')
        ss %>% sheet_append(indFrame, sheet = "survey_order")
        
        indList <- list(ind1 = ind1, ind2 = ind2, ind3 = ind3, userID = userID)
        indList
    })
    
    output$plot0 <- renderImage({
        filename <- normalizePath(file.path('./images','Dataset0ref.png'))
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE)
    
    output$plot1 <- renderImage({
        indicators <- myIndicators()
        plot1ind <- indicators$ind1[[1]]
        Name <- c("Dataset1noRef","Dataset1bands","Dataset1ref")
        filename <- normalizePath(file.path('./images',paste(Name[[plot1ind]],'.png',sep='')))
        
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE)
    
    output$plot2 <- renderImage({
        indicators <- myIndicators()
        plot2ind <- indicators$ind1[[2]]
        Name <- c("Dataset2noRef","Dataset2bands","Dataset2ref")
        filename <- normalizePath(file.path('./images',paste(Name[[plot2ind]],'.png',sep='')))
        
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE)
   
    output$plot3 <- renderImage({
        indicators <- myIndicators()
        plot3ind <- indicators$ind1[[3]]
        Name <- c("Dataset3noRef","Dataset3bands","Dataset3ref")
        filename <- normalizePath(file.path('./images',paste(Name[[plot3ind]],'.png',sep='')))
        
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE) 

    output$plot4 <- renderImage({
        indicators <- myIndicators()
        plot4ind <- indicators$ind2[[1]]
        Name <- c("Dataset4noRef","Dataset4bands","Dataset4ref")
        filename <- normalizePath(file.path('./images',paste(Name[[plot4ind]],'.png',sep='')))
        
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE)
    
    output$plot5 <- renderImage({
        indicators <- myIndicators()
        plot5ind <- indicators$ind2[[2]]
        Name <- c("Dataset5noRef","Dataset5bands","Dataset5ref")
        filename <- normalizePath(file.path('./images',paste(Name[[plot5ind]],'.png',sep='')))
        
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE)
    
    output$plot6 <- renderImage({
        indicators <- myIndicators()
        plot6ind <- indicators$ind2[[3]]
        Name <- c("Dataset6noRef","Dataset6bands","Dataset6ref")
        filename <- normalizePath(file.path('./images',paste(Name[[plot6ind]],'.png',sep='')))
        
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE)
    
    output$plot7 <- renderImage({
        indicators <- myIndicators()
        plot7ind <- indicators$ind3[[1]]
        Name <- c("Dataset7noRef_2","Dataset7bands_2","Dataset7ref_2")
        filename <- normalizePath(file.path('./images',paste(Name[[plot7ind]],'.png',sep='')))
        
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE)
    
    output$plot8 <- renderImage({
        indicators <- myIndicators()
        plot8ind <- indicators$ind3[[2]]
        Name <- c("Dataset8noRef_2","Dataset8bands_2","Dataset8ref_2")
        filename <- normalizePath(file.path('./images',paste(Name[[plot8ind]],'.png',sep='')))
        
        list(src = filename, width = 350, height = 262.5)
    }, deleteFile = FALSE)
    
    
    # Creating list of responses
    
    output$submission <- renderText({
        
        # Generating userID
        userKeyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=1525817837')
        sheet <- read_sheet(userKeyLocation, sheet = "post_survey")
        length(sheet$userID)
        userID <- myIndicators()[[4]]
        
        responses_pre <- data.frame(userID, input$dem1, input$dem2, input$dem3)
        responses_survey <- data.frame(userID, input$response0, input$response1, input$response2, input$response3,
                                     input$response4, input$response5, input$response6,
                                     input$response7, input$response8)
        responses_post <- data.frame(userID, input$rank1, input$rank2, input$rank3, 
                                     input$rank4, input$rank5)
      
        if(input$submit[1] == 0){
            paste("")
        } else{
            # submit score
            ss2 <- gs4_get('https://docs.google.com/spreadsheets/d/1mWz5Io7d4BUEqWRJVzwnjXOJD7zfZBVt6z0xHuyoHbI/edit#gid=302243109')
            ss2 %>% sheet_append(responses_pre, sheet = "pre_survey")
            ss2 %>% sheet_append(responses_survey, sheet = "survey")
            ss2 %>% sheet_append(responses_post, sheet = "post_survey")
            paste("We just received your response. Thanks for submitting! Use the incentive key below to complete the incentive survey: https://sbacon3.shinyapps.io/incentive_survey/ ")
        }
    })
    
    # Generating incentive keys
    
    output$incentiveKey <- renderText({
        #set.seed(1234)
        keys <- round(runif(50, min = 10000, max = 99999), 0)
        keyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1i_kF-5rrdqSD9eXss-eyOmmhGi3_Y5wDqFCEk2kPkus/edit#gid=0')
        sheet1 <- read_sheet(keyLocation, sheet = "used_keys")
        sheet2 <- read_sheet(keyLocation, sheet = "sent_keys")
        usedKeys <- sheet1$Used
        sentKeys <- sheet2$Sent
        openKeys <- setdiff(setdiff(keys, usedKeys), sentKeys)
        yourKey <- sample(openKeys, 1)
        yourKeyFrame <- as.data.frame(yourKey)
        keyLocation %>% sheet_append(yourKeyFrame, sheet = "sent_keys") 
        paste('Incentive key:  ', yourKey)
        })
    
    # Making sure that an option is selected...
    output$demographic <- renderText({
        if(input$dem1 == "null" || input$dem2 == "null" || input$dem3 == "null"){
            paste("Please select a response before continuing.")
            
        }
        else{
        }
    })
    
    output$selection0 <- renderText({
        if(input$response0 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })
    output$selection1 <- renderText({
        if(input$response1 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })
    output$selection2 <- renderText({
        if(input$response2 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })
    output$selection3 <- renderText({
        if(input$response3 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })
    output$selection4 <- renderText({
        if(input$response4 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })
    output$selection5 <- renderText({
        if(input$response5 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })
    output$selection6 <- renderText({
        if(input$response6 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })
    output$selection7 <- renderText({
        if(input$response7 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })
    output$selection8 <- renderText({
        if(input$response8 == "null"){
            paste("Please select a response before continuing.")
        }
        else{
        }
    })


}

# Run the application 
shinyApp(ui = ui, server = server)
