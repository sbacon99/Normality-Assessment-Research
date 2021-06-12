# An incentive survey (PILOT)
# Sam Bacon

#install.packages("shinydashboard")
#install.packages("shinythemes")

library(shiny)
library(shinythemes)
library(googlesheets4)
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)


ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    titlePanel("Incentive Survey"),

    textInput("emailInput", "Email address:", 
              value = "", width = 700, placeholder = NULL),
    br(),
    
    textInput("codeInput", "Please enter your incentive code", 
              value = "", width = 700, placeholder = NULL),
    
    textOutput("incentiveCode"),
    actionButton("submit","Submit"),
    
    conditionalPanel(condition = "input.submit == 1",
        br(),
        p("Your responses have been submitted. If your incentive code was valid, you will receive your incentive via email shortly."),
        p("You may now exit this window")
    ),

)

server <- function(input, output) {

    output$incentiveCode <- renderText({
        
       
        keyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/155fMb2opqCRTEstJg-qaUAv1SqDPxl6NfX2ciQqj9Zo/edit#gid=0')
        sent_keys <- read_sheet(keyLocation, sheet = "sent_keys")
        used_keys <- read_sheet(keyLocation, sheet = "used_keys") 
        criteria1 <- as.double(input$codeInput) %in% sent_keys$Sent
        criteria2 <- length(intersect(as.character(input$codeInput),used_keys$Used))==0
 
        
        if(criteria1 && criteria2){

            if(input$submit){
            ss <- gs4_get('https://docs.google.com/spreadsheets/d/155fMb2opqCRTEstJg-qaUAv1SqDPxl6NfX2ciQqj9Zo/edit#gid=0')
            ss %>% sheet_append(as.data.frame(input$codeInput), sheet = "used_keys")
            ss %>% sheet_append(as.data.frame(input$emailInput), sheet = "emails")
            }
            
            output<- "Incentive code is valid. Click 'submit' below to complete the survey."
            paste(output)
        }
        
        else if(!criteria1 && criteria2){
            output <- "Sorry. The code you have entered is invalid."
            print(output)
        }
        else if(criteria1 && !criteria2){
            output <- "Sorry. This code has already been redeemed."
            print(output)
        }
        else{

        }
        
       
    })
}

shinyApp(ui = ui, server = server)
