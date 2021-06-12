# Final Application
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(qqplotr)
library(e1071)
library(stringr)
library(DT)
library(dplyr)
library(readr)
library(shinythemes)
  
    shinyApp(
        ui = dashboardPagePlus(skin = "blue-light",
          header = dashboardHeaderPlus(
            title = tagList(
              span(class = "logo-lg", "Q-Q Plots"), 
              icon = icon("chart-bar")),
            enable_rightsidebar = FALSE,
                left_menu = tagList(
                    dropdownBlock(
                        id = "myDropdown1",
                        title = "Generate a Dataset",
                        icon = "sliders",
                        br(),
                        selectInput(
                            inputId = "dist",
                            label = "Distribution",
                            c("Normal" = "normal",
                              "F" = "f",
                              "Uniform" = "unif")
                        ),
                        textAreaInput(
                            inputId = "size",
                            label = NULL,
                            placeholder = "Enter Sample Size",
                            width = 160,
                            resize = "horizontal",
                            rows = 1
                        ),
                        conditionalPanel(condition = "input.dist == 'normal'", 
                                         fluidRow(
                                           column(4,
                                                  textAreaInput(
                                                    inputId = "mean",
                                                    label = NULL,
                                                    placeholder = "Mean",
                                                    width = 100,
                                                    rows = 1,
                                                    resize = "none"
                                                  ),
                                           ),
                                           
                                           column(4, offset = 1,
                                                  textAreaInput(
                                                    inputId = "sd",
                                                    label = NULL,
                                                    placeholder = "StDev",
                                                    width = 100,
                                                    rows = 1,
                                                    resize = "none"
                                                  )
                                           )
                                         )
                                  ),
                        
                        conditionalPanel(condition = "input.dist == 'f'", 
                                         fluidRow(
                                           column(4,
                                                  textAreaInput(
                                                    inputId = "ndf",
                                                    label = NULL, 
                                                    placeholder = "Num. df",
                                                    width = 100,
                                                    rows = 1,
                                                    resize = "none"
                                                  )
                                           ),
                                           
                                           column(4, offset = 1,
                                                  textAreaInput(
                                                    inputId = "ddf",
                                                    label = NULL,
                                                    placeholder = "Den. df",
                                                    width = 100,
                                                    rows = 1,
                                                    resize = "none"
                                                  )
                                           )
                                         )
                        ),
                        
                        conditionalPanel(condition = "input.dist == 'unif'", 
                                           fluidRow(
                                             column(4,
                                                    textAreaInput(
                                                      inputId = "min",
                                                      label = NULL,
                                                      placeholder = "Minimum",
                                                      width = 100,
                                                      rows = 1,
                                                      resize = "none"
                                                    ),
                                             ),
                                             
                                             column(4, offset = 1,
                                                    textAreaInput(
                                                      inputId = "max",
                                                      label = NULL,
                                                      placeholder = "Maximum",
                                                      width = 100,
                                                      rows = 1,
                                                      resize = "none"
                                                    )
                                             )
                                           )
                        ),
                        
                        
                
                        prettySwitch(
                          inputId = "refLine",
                          label = "Reference line",
                          fill = FALSE, 
                          status = "primary"
                        ),
                        
                        prettySwitch(
                          inputId = "bands",
                          label = "Confidence bands",
                          fill = FALSE, 
                          status = "primary"
                        ),
                        
                        actionButton(
                          inputId = 'action1',
                          label = "Generate Plot"
                        ),
                        
                        actionButton(
                          inputId = 'reset1',
                          label = "Reset"
                        )
                    ),
                    
                    dropdownBlock(
                      id = "myDropdown2",
                      title = "Manual Import",
                      icon = "sliders",
                      br(),
                      textAreaInput(
                        inputId = "paste",
                        label = NULL,
                        placeholder = "Paste values (one per line)",
                          value = NULL,
                          width = 250,
                          rows = 3,
                          resize = 'vertical'
                      ),
                      
                      textAreaInput(
                        inputId = "titleInput",
                        label = NULL,
                        placeholder = "Add a title here:",
                        value = NULL,
                        width = 250,
                        rows = 1,
                        resize = 'none'
                      ),
                      
                      prettySwitch(
                        inputId = "refLine2",
                        label = "Reference line",
                        fill = FALSE, 
                        status = "primary"
                      ),
                      
                      prettySwitch(
                        inputId = "bands2",
                        label = "Confidence bands",
                        fill = FALSE, 
                        status = "primary"
                      ),
                      
                      actionButton(
                        inputId = 'action2',
                        label = "Generate Plot"
                      ),
                      
                      actionButton(
                        inputId = 'reset2',
                        label = "Reset"
                      )
                  ),
                  dropdownBlock(
                    id = "myDropdown3",
                    title = "Upload a File",
                    icon = "sliders",
                    
                    br(),
                    
                    fileInput(
                      "fileInput",
                      label = 'Upload a file (Excel or .csv)',
                      multiple = FALSE,
                      buttonLabel = 'Browse...',
                      placeholder = "No file selected"
                    ),
                    
                    # conditionalPanel(condition = "input.fileInput != NULL", 
                    #       varSelectInput("variable", "Variable:", input$fileInput)
                    # ),
                    
                    

                    
                    prettySwitch(
                      inputId = "refLine3",
                      label = "Reference line",
                      fill = FALSE, 
                      status = "primary"
                    ),
                    
                    prettySwitch(
                      inputId = "bands3",
                      label = "Confidence bands",
                      fill = FALSE, 
                      status = "primary"
                    ),
                    
                    actionButton(
                      inputId = 'action3',
                      label = "Generate Plot"
                    ),
                    
                    actionButton(
                      inputId = 'reset3',
                      label = "Reset"
                    )
                  )
                )
          ),
            
          sidebar <- dashboardSidebar(
            sidebarMenu(
              id = "tabs",
              menuItem("Home", icon = icon("home"), tabName = "home", badgeColor = "green"),
              menuItem("Importance of Q-Q Plots", icon = icon("question"), tabName = "qqplot", badgeColor = "green"),
              menuItem("Using the Applet", icon = icon("laptop"), tabName = "instructions", badgeColor = "green"),
              menuItem("About the Creator", icon = icon("address-card"), tabName = "author", badgeColor = "green")
            )
          ),
          
          body = dashboardBody(
            tags$head(tags$style(HTML('
            .main-header .logo {
            font-family: "Georgia", Times, "Times New Roman", serif;
            font-weight: bold;
            font-size: 24px;
          }
        '))),
          tabItems(
            tabItem("home",
                    h2(strong("Normality Assessment Using Q-Q Plots"), align = "center",style = "font-family: 'Times', serif")
            ),
            tabItem("qqplot",
              h2(strong("The Importance of Q-Q Plots"), align = "center",style = "font-family: 'Times', serif"),
              p("The validity of any sound statistical analysis is predicated 
              on the fact that the relevant dataset meets certain criteria. These 
              prerequisites ensure that different features of the data, including but 
              not limited to sample size, independence, and shape, are suitable for 
              different statistical tests. In other words, the types of analyses that 
              we can perform are largely dependent on the data parameters. Of these 
              qualities, perhaps the most prolific is the distribution pattern of the 
              data. Ideally, the data will approximately mimic a Normal distribution, 
              a bell-shaped curve centered at the average with most values falling 
              within three standard deviations of the center. 
              This condition is paramount for parametric statistical methods."),
              br(),
              p("There are a variety of formal and informal methods for assessing the normality of 
              a data set. Formal tests for normality generate a numerical summary of the dataset, 
              compare this summary to the expected summaries for a normally distributed version, 
              and identify regions containing significant deviations. Each test employs a different 
              numerical summary based on statistical theory. While popular, formal methods lose a 
              a significant amount of power as sample size decreases."),
              br(),
              p("As a result, informal assessment methods, such as Q-Q Plots, have become an
              increasingly popular and accessible way to evaluate normality. These visualizations
                are created by plotting the theoretical quantiles against the sample quantiles. 
                Simply put, a standard Q-Q Plot with a tight line of points at roughly a 45-degree 
                angle suggests that the quantiles of the sample data match the quantiles of the 
                theoretical normal distribution. Thus, the data is normally distributed."),
              br(),
              p("Current research is very optimistic about Q-Q Plots and their ability to identify 
                non-normal data more consistently than classical tests. Additionally, Q-Q Plots have 
                the inherent ability to determine where a dataset deviates from the theoretical normal 
                quantiles. The primary drawback of these tools is their subjective nature. As with most 
                informal methods, the viewer must make the final judgement about the distribution of 
                the data. The purpose of this applet is to increase user's familiarity with Q-Q Plots by
                exposing them to a variety of visualizations for Normal and non-Normal data.")),
               tabItem("instructions",
              h2(strong("Using the Applet"), align = "center",style = "font-family: 'Times', serif"),
              p("This applet allows users to generate Q-Q Plots using randomly generated data or 
                their own values. After generating/importing the data, you can choose from a variety
                of common plot features, such as 45-degree reference line or confidence bands. Once
                you create your Q-Q Plot, you will also be able to access a variety of other visualizations 
                and sample statistics."),
              br(),
              p("Start by selecting a data set. If you would like to randomly generate values from a 
                pre-determined distribution, select 'Generate a Dataset' from the dropdown menu."),
              fluidRow(
                column(5,
                       strong("'Generate a Dataset'"),
                       br(),
                       p("First, select the distribution of the parent population from which you
                         would like to draw your sample."),
                       br(),
                       p("Next, enter your desired sample size and any other relevant sample 
                         statistics for the chosen distribution."),
                       br(),
                       p("If you would like the Q-Q Plot to contain a referece line and/or confidence
                         bands, highlight those options."),
                       br(),
                       p("Click 'Generate Plot' to view your Q-Q Plot, as well as descriptive 
                         statistics and other visualizations.")
                ),
                column(5, offset = 1,
                       strong("'Manual Import'"),
                       br(),
                       p("Paste your values into the input box, one value per row"),
                       br(),
                       p("Next, add a title if necessary."),
                       br(),
                       p("Select whether you would like your visualization to display
                         a reference line and/or confidence bands."),
                       br(),
                       p("Click 'Generate Plot' to view your Q-Q Plot, as well as descriptive 
                         statistics and other visualizations.")
                       
                )
              )),
            tabItem("author",
              h2(strong("About the Creator"), align = "center",style = "font-family: 'Times', serif")
            )
          ),
            setShadow(class = "dropdown-menu"),
            setShadow(class = "box"),
               

             
            br(),
            fluidRow(
              column(width = 6, DTOutput("data")),
              column(width = 5, tableOutput("descriptive"),
                     tableOutput("normality"))
            ),
            textOutput("test"),
            plotOutput("scatter1"),
            br(),
            br(),
            plotOutput("hist"),
            br(),
            br(),
            plotOutput("box"),


                setShadow(class = "dropdown-menu")
          ),
  
          rightsidebar = rightSidebar(),
          title = "DashboardPage"
        ),
        
        server = function(input, output) {
          
          
          myDataInput <- reactive({
            values <- vector()
            split <- strsplit(input$paste, "\n")
            values <- append(values, as.numeric(split[[1]][]))
            
            data.frame(value = values)
          })
          
           myTableReader <- reactive({
              file <- input$fileInput
              if(is.null(file))
                return(NULL)
              
              table <- read_csv(file$datapath)
           })
              
            myTableReaderDF <- reactive({
              file <- input$fileInput
              if(is.null(file))
                return(NULL)
                
              table <- read_csv(file$datapath)
              data.frame(table)
            })
            
           
          
          myDataGenerator <- reactive({
            set.seed(round(runif(1, min = 1, max = 10000)))
            
            if(input$dist == "normal"){
              plot <- data.frame(value = rnorm(as.numeric(input$size), mean = as.numeric(input$mean), sd = as.numeric(input$sd)))
            }
            if(input$dist == "f"){
              plot <- data.frame(value = rf(as.numeric(input$size), as.numeric(input$ndf), as.numeric(input$ddf)))
            }
            if(input$dist == "unif"){
              plot <- data.frame(value = runif(as.numeric(input$size), min = as.numeric(input$min), max = as.numeric(input$max)))
            }
            plot
          })

          # RESET PLOT
          observeEvent(input$reset1,{
            
            output$data <- renderDT(
              data.frame(),
              colnames = "values",
              caption = 'Imported Dataset',
            )
            
            output$descriptive <- renderTable({
              cbind(data.frame(),
                    data.frame())
            }, caption = "Descriptive Statistics",
            caption.placement = getOption("xtable.caption.placement", "top"))
            
            output$normality <- renderTable({
              data.frame()
            })
            
            output$scatter1 <- renderPlot({
   
            })
            
            output$hist <- renderPlot({
            })
            
            output$box <- renderPlot({
            })
            
          })
          
          # RESET PLOT
          observeEvent(input$reset2,{
            
            output$data <- renderDT(
              data.frame(),
              colnames = "values",
              caption = 'Imported Dataset',
            )
            
            output$descriptive <- renderTable({
              cbind(data.frame(),
                    data.frame())
            }, caption = "Descriptive Statistics",
            caption.placement = getOption("xtable.caption.placement", "top"))
            
            output$normality <- renderTable({
              data.frame()
            })
            
            
            output$scatter1 <- renderPlot({
            })
            
            output$hist <- renderPlot({
            })
            
            output$box <- renderPlot({
              print(ggplot() + geom_boxplot())
            })
          })
          
          observeEvent(input$reset3,{
           
            output$scatter1 <- renderPlot({
              print(ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 10)) 
            })
            
            output$hist <- renderPlot({
            })
            
            output$box <- renderPlot({
            })
            
          })
          
          #IMPORT FILE
          observeEvent(input$action3,{
          
            output$data <- renderDT(
              
              datatable(myTableReader(), selection = list(target = 'column'))
            )
            
            output$descriptive <- renderTable(
              caption = "Descriptive Statistics",
              caption.placement = getOption("xtable.caption.placement", "top"),
              cbind(data.frame(mean = apply(myTableReaderDF(),2,mean), std = apply(myTableReaderDF(),2,sd)),
                    data.frame(min = apply(myTableReaderDF(),2,min), max = apply(myTableReaderDF(),2,max)))
            )
            
            output$normality <- renderTable(

              caption = "Shapiro-Wilk normality test",
              caption.placement = getOption("xtable.caption.placement", "top"),
              data.frame(shapiro.test(myTableReaderDF())[1],shapiro.test(myTableReaderDF()$value)[2])
            )
          })
          
          # PASTE DATASET
          observeEvent(input$action2,{
            
            output$data <- renderDT(
              datatable(myDataInput(),
                        options = list(lengthMenu = c(5, 10, 15, 20)
                        )
              )
            )
            
            output$descriptive <- renderTable(
              caption = "Descriptive Statistics",
              caption.placement = getOption("xtable.caption.placement", "top"),
              cbind(data.frame(mean = apply(myDataInput(),2,mean), std = apply(myDataInput(),2,sd)),
                data.frame(min = apply(myDataInput(),2,min), max = apply(myDataInput(),2,max)))
            )
            
            output$normality <- renderTable(
              caption = "Shapiro-Wilk normality test",
              caption.placement = getOption("xtable.caption.placement", "top"),
              data.frame(shapiro.test(myDataInput()$value)[1],shapiro.test(myDataInput()$value)[2])
            )
            
            output$scatter1 <- renderPlot({
              
              # Reference line == TRUE
              if(input$refLine2 == TRUE){
                
                # Bands == TRUE
                if(input$bands2 == TRUE){
          
                  print(ggplot(data = myDataInput(), mapping = aes(sample = value)) +  
                        stat_qq_band(identity = FALSE, fill = "pink") +
                        stat_qq_point(colour = "black") +  
                        stat_qq_line(identity = FALSE) +
                        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                        ggtitle(input$titleInput))
                }
                
                # Bands == FALSE
                else{
                  
                  print(ggplot(data = myDataInput(), mapping = aes(sample = value)) +  
                        stat_qq_point(colour = "black") +  
                        stat_qq_line(identity = FALSE) +
                        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                        ggtitle(input$titleInput))
                }
              }
              
              # Reference line == FALSE
              if(input$refLine2 == FALSE){
                
                # Bands == TRUE
                if(input$bands2 == TRUE){
                  print(ggplot(data = myDataInput(), mapping = aes(sample = value)) +  
                        stat_qq_band(identity = FALSE, fill = "pink") +
                        stat_qq_point(colour = "black") +  
                        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                        ggtitle(input$titleInput))
                }
                
                # Bands == FALSE
                else{
                  print(ggplot(data = myDataInput(), mapping = aes(sample = value)) +  
                        stat_qq_point(colour = "black") +  
                        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                        ggtitle(input$titleInput))
                }
              }
            })
          })
          
          # GENERATED DATASETS
          observeEvent(input$action1, {
            
            output$data <- renderDT(
              datatable(myDataGenerator(),
                        options = list(lengthMenu = c(5, 10, 15, 20)
                        )
              )
            )
              
            output$descriptive <- renderTable({
              cbind(data.frame(mean = apply(myDataGenerator(),2,mean), std = apply(myDataGenerator(),2,sd)),
                    data.frame(min = apply(myDataGenerator(),2,min), max = apply(myDataGenerator(),2,max)))
            }, caption = "Descriptive Statistics",
            caption.placement = getOption("xtable.caption.placement", "top"))
              
            output$normality <- renderTable(
              caption = "Shapiro-Wilk normality test",
              caption.placement = getOption("xtable.caption.placement", "top"),
              data.frame(shapiro.test(myDataGenerator()$value)[1],shapiro.test(myDataGenerator()$value)[2])
              

            )
            
            output$box <- renderPlot({
            
              data = myDataGenerator()
              breaks <- pretty(range(data$value), n = nclass.FD(data$value), min.n = 1)
              bwidth <- breaks[2]-breaks[1]
            
              print(ggplot(data, aes(value)) + geom_boxplot(color = "navy", fill = "grey") +
                ggtitle("Boxplot of Variable Counts"))
            })
            
            output$hist <- renderPlot({
              
              data = myDataGenerator()
              breaks <- pretty(range(data$value), n = nclass.FD(data$value), min.n = 1)
              bwidth <- breaks[2]-breaks[1]
              
              print(ggplot(data, aes(value)) + geom_histogram(aes(y = ..density..), binwidth = bwidth, color = "navy", fill = "grey") +
                      ggtitle("Histogram of Variable Density") +
                      stat_function(fun = dnorm, args = list(mean = mean(data$value), sd = sd(data$value))))
            })
            
            output$scatter1 <- renderPlot({
              #set.seed(round(runif(1, min = 1, max = 10000)))
              
              # Normal distribution
              if(input$dist == "normal"){
                normFrame <- myDataGenerator()
                
                # Reference line == TRUE
                if(input$refLine == TRUE){
                  
                  # Bands == TRUE
                  if(input$bands == TRUE){
                    
                    print(ggplot(data = normFrame, mapping = aes(sample = value)) +  
                            stat_qq_band(identity = FALSE, fill = "pink") +
                            stat_qq_point(colour = "black") +  
                            stat_qq_line(identity = FALSE) +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Normal Distribution with Reference Line and Confidence Bands"))
                  }
                  
                  # Bands == FALSE
                  else{
                    
                    print(ggplot(data = normFrame, mapping = aes(sample = value)) +  
                            stat_qq_point(colour = "black") +  
                            stat_qq_line(identity = FALSE) +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Normal Distribution with Reference Line")) +
                      theme(plot.title = element_text(size = 25, face = "bold")) 
                  }
                }
                
                # Reference line == FALSE
                if(input$refLine == FALSE){
                  
                  # Bands == TRUE
                  if(input$bands == TRUE){
                    print(ggplot(data = normFrame, mapping = aes(sample = value)) +  
                            stat_qq_band(identity = FALSE, fill = "pink") +
                            stat_qq_point(colour = "black") +  
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Normal Distribution with Confidence Bands")) +
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                  
                  # Bands == FALSE
                  else{
                    print(ggplot(data = normFrame, mapping = aes(sample = value)) +  
                            stat_qq_point(colour = "black") +  
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Normal Distribution with no Features")) + 
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                }
              }
              
              # F - distribution
              if(input$dist == "f"){
                fFrame <- myDataGenerator()
                
                # Reference line == TRUE
                if(input$refLine == TRUE){
                  
                  # Bands == TRUE
                  if(input$bands == TRUE){
                    
                    print(ggplot(data = fFrame, mapping = aes(sample = value)) +
                            stat_qq_band(identity = FALSE, fill = "pink") +
                            stat_qq_point(colour = "black") +
                            stat_qq_line(identity = FALSE) +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("F Distribution with Reference Line and Confidence Bands")) +
                      theme(plot.title = element_text(hjust = 0.5))
                    
                  }
                  
                  # Bands == FALSE
                  else{
                    
                    print(ggplot(data = fFrame, mapping = aes(sample = value)) +  
                            stat_qq_point(colour = "black") +  
                            stat_qq_line(identity = FALSE) +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("F Distribution with Reference Line")) +
                            theme(plot.title = element_text(hjust = 0.5))
                  }
                }
                
                # Reference line == FALSE
                if(input$refLine == FALSE){
                  
                  # Bands == TRUE
                  if(input$bands == TRUE){
                    
                    print(ggplot(data = fFrame, mapping = aes(sample = value)) +
                            stat_qq_band(identity = FALSE, fill = "pink") +
                            stat_qq_point(colour = "black") +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("F Distribution with Confidence Bands")) +
                            theme(plot.title = element_text(hjust = 0.5))
                  }
                  
                  # Bands == FALSE
                  else{
                    print(ggplot(data = fFrame, mapping = aes(sample = value)) +  
                            stat_qq_point(colour = "black") +  
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("F Distribution with no Features")) +
                            theme(plot.title = element_text(hjust = 0.5))
                  }
                }
              }
              
              # Uniform Distribution
              if(input$dist == "unif"){
                unifFrame <- myDataGenerator()
                
                # Reference line == TRUE
                if(input$refLine == TRUE){
                  
                  # Bands == TRUE
                  if(input$bands == TRUE){
                    
                    print(ggplot(data = unifFrame, mapping = aes(sample = value)) +
                            stat_qq_band(identity = FALSE, fill = "pink") +
                            stat_qq_point(colour = "black") +
                            stat_qq_line(identity = FALSE) +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Uniform Distribution with Reference Line and Confidence Bands")) +
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                  
                  # Bands == FALSE
                  else{
                    
                    print(ggplot(data = unifFrame, mapping = aes(sample = value)) +  
                            stat_qq_point(colour = "black") +  
                            stat_qq_line(identity = FALSE) +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Uniform Distribution with Reference Line")) +
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                }
                
                # Reference line == FALSE
                if(input$refLine == FALSE){
                  
                  # Bands == TRUE
                  if(input$bands == TRUE){
                    
                    print(ggplot(data = unifFrame, mapping = aes(sample = value)) +
                            stat_qq_band(identity = FALSE, fill = "pink") +
                            stat_qq_point(colour = "black") +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles")  +
                            ggtitle("Uniform Distribution with Confidence Bands"))+
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                  
                  # Bands == FALSE
                  else{
                    print(ggplot(data = unifFrame, mapping = aes(sample = value)) +  
                            stat_qq_point(colour = "black") +  
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Uniform Distribution with no Features")) +
                            theme(plot.title = element_text(hjust = 0.5))
                  }
                }
              }
              
              # Chi - Square Distribution
              if(input$dist == "chi"){
                size <- as.numeric(input$size)
                chiFrame <- data.frame(value = rchisq(size, df = 10, ncp = 0))
                
                # Reference line == TRUE
                if(input$refLine == TRUE){
                  
                  # Bands == TRUE
                  if(input$bands == TRUE){
                    
                    print(ggplot(data = chiFrame, mapping = aes(sample = value)) +
                            stat_qq_band(identity = FALSE, fill = "pink") +
                            stat_qq_point(colour = "black") +
                            stat_qq_line(identity = FALSE) +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Chi-Square Distribution with Reference Line and Confidence Bands")) +
                            theme(plot.title = element_text(hjust = 0.5))
                  }
                  
                  # Bands == FALSE
                  else{
                    
                    print(ggplot(data = chiFrame, mapping = aes(sample = value)) +
                            stat_qq_point(colour = "black") +
                            stat_qq_line(identity = FALSE) +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Chi-Square Distribution with Reference Line")) +
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                }
                
                # Reference line == FALSE
                if(input$refLine == FALSE){
                  
                  # Bands == TRUE
                  if(input$bands == TRUE){
                    
                    print(ggplot(data = chiFrame, mapping = aes(sample = value)) +
                            stat_qq_band(identity = FALSE, fill = "pink") +
                            stat_qq_point(colour = "black") +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Chi-Square Distribution with Confidence Bands")) +
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                  
                  # Bands == FALSE
                  else{
                    print(ggplot(data = chiFrame, mapping = aes(sample = value)) +
                            stat_qq_point(colour = "black") +
                            labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
                            ggtitle("Chi-Square Distribution with no Features")) +
                      theme(plot.title = element_text(hjust = 0.5))
                  }
                }
              }
            })
          })

          # USER - CREATED DATASETS
         observeEvent(input$action2, {
           output$hist <- renderPlot({
             
             data = myDataInput()
             breaks <- pretty(range(data$value), n = nclass.FD(data$value), min.n = 1)
             bwidth <- breaks[2]-breaks[1]
             
             print(ggplot(data, aes(value)) + geom_histogram(color = "navy", fill = "grey", binwidth = bwidth) +
                  ggtitle("Histogram of Variable") +
                  stat_function(fun = dnorm, args = list(mean = mean(data$value), sd = sd(data$value))))
            })
           
           output$box <- renderPlot({
             
             data = myDataInput()
             
             
             print(ggplot(data, aes(value)) + geom_boxplot(color = "navy", fill = "grey") +
                     ggtitle("Boxplot of Variable Counts"))
           })
          })
      }
    )



