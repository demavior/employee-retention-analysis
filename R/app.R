#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Analysis of Employee Retention"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          numericInput("below",
                       "By Age below (descriptive anaylitics):",
                       40),
          numericInput("above",
                       "By Age above (descriptive anaylitics):",
                       0),
          numericInput("predict",
                       "By Ages Below (Predictive Factors):",
                       60),
          width = 3
          ),

        # Show a plot of the generated distribution
        mainPanel(
           h2("Descriptive Analytics"),
           plotOutput("ActiveDptPlot"),
           plotOutput("ActivePlot"),
           h2("Predictive Analytics"),
           plotOutput("ForestFactorsPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'blue', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of times')
    })
    
    # Active Employee Age Range
    output$ActiveDptPlot <- renderPlot({
      pTitle = "Departments of Active Employees"
      if (!is.na(input$below))
        pTitle=paste(pTitle," below ",input$below)
      if (!is.na(input$above)&&input$above!=0)
        pTitle=paste(pTitle," above ",input$above)
      ggplot( active_employees %>%
                filter(meanAge > ifelse(is.na(input$above),0,input$above),
                       meanAge < ifelse(is.na(input$below),999,input$below)) %>%
                arrange(desc(meanAge))  %>%
                count(department_name) %>%
                filter(n>20), 
             aes(x=department_name, y = n, fill=department_name))+
        geom_col()+
        ggtitle(pTitle)+
        labs(x="Number of Employees",
             y="Department Name",
             fill="Department") +
        theme_classic()
      
    })
    # Important Factors Random Forest
    output$ActivePlot <- renderPlot({
      pTitle = "Reasons for Termination over the Years"
      if (!is.na(input$below))
        pTitle=paste(pTitle," below ",input$below)
      if (!is.na(input$above)&&input$above!=0)
        pTitle=paste(pTitle," above ",input$above)
      ggplot(employee_data %>% filter(STATUS != "ACTIVE",age < ifelse(is.na(input$below),999,input$below),
                                      age > ifelse(is.na(input$above),0,input$above)),
             aes(x = factor(STATUS_YEAR), fill = termreason_desc)) +
        geom_bar(position = "stack") +
        theme_classic() +
        labs(title = pTitle, 
             x = "Years", 
             y = "Count",
             fill = "Termination Reason") 
      
    })
    
    # Important Factors Random Forest
    output$ForestFactorsPlot <- renderPlot({
      if (is.na(input$predict))
        varImpPlot(model, main = "Key Factors to determine employee status")
      else{
        if (input$predict == 60)
          varImpPlot(model2, main = "Key Factors to determine employee status for ages below 60")
        else{
          employee_data_x <- employee_data %>% filter(age < ifelse(is.na(input$predict),999,input$predict))
          # Splitting the data into training and testing sets
          set.seed(5) # For reproducibility
          trainIndex <- createDataPartition(employee_data_x$STATUS, p = .8, list = FALSE)
          trainData <- employee_data_x[trainIndex, ]
          modelX <- randomForest(STATUS ~ city_name + age + length_of_service + gender_short + department_name, 
                                 data = trainData, classwt = c("ACTIVE" = 1, "TERMINATED" = 2))
          varImpPlot(modelX, main = paste("Key Factors to determine employee status for ages below ",input$predict))
        }
      }
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
