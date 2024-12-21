str(emp_train)

employee_data$EmployeeID<-as.factor(employee_data$EmployeeID)
employee_data$city_name<-as.factor(employee_data$city_name)
employee_data$department_name<-as.factor(employee_data$department_name)
employee_data$job_title<-as.factor(employee_data$job_title)
employee_data$store_name<-as.factor(employee_data$store_name)
employee_data$gender_short<-as.factor(employee_data$gender_short)
employee_data$termreason_desc<-as.factor(employee_data$termreason_desc)
employee_data$termtype_desc<-as.factor(employee_data$termtype_desc)
#employee_data$STATUS_YEAR<-as.factor(employee_data$STATUS_YEAR)
employee_data$BUSINESS_UNIT<-as.factor(employee_data$BUSINESS_UNIT)
employee_data$functional_unit<-as.factor(employee_data$functional_unit)
employee_data$region<-as.factor(employee_data$region)

emp_train <- subset(employee_data, STATUS_YEAR != 2015)
library(ROSE)  # "Random Over Sampling Examples"; generates synthetic balanced samples
trainData_rose <- ROSE(STATUS ~ ., data = emp_train, seed=123)$data
# Tables to show balanced dataset sample sizes
table(trainData_rose$STATUS)




#################################

## Using caret library for gbm on the ROSE balanced dataset
# Results are not shown for sake of brevity
set.seed(432)
control1 <- trainControl(method = 'cv', number = 3,
                         returnResamp='none',
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE)
gbmodel <- train(STATUS ~ .,
                 data = trainData[c("STATUS_YEAR", "city_name", "age", "length_of_service", "gender_short", "department_name","STATUS")],
                 method = 'gbm',
                 trControl = control1,
                 metric = "ROC",
                 preProc = c("center", "scale"))
?train
# summary(gbmodel)  # outputs variable importance list & plot 
gbprediction <- predict(object =
                          gbmodel,
                        testData[c("STATUS_YEAR", "city_name", "age", "length_of_service", "gender_short", "department_name","STATUS")],
                        type = 'raw')
confusionMatrix(data = gbprediction,
                reference = testData$STATUS,
                positive = 'Yes', mode = 'prec_recall')




#################################


# 'STATUS' is the binary target variable (1 for TERMINATED, 0 for ACTIVE)
# 'age', 'length_of_service', and other_features include additional features you want to include

# Convert 'STATUS' to a binary variable (0 for ACTIVE, 1 for TERMINATED)
employee_data$STATUS <- as.numeric(employee_data$STATUS) - 1

# Fit logistic regression model
logistic_model <- glm(STATUS ~ age + length_of_service + region + functional_unit + 
                        gender_short + STATUS_YEAR + BUSINESS_UNIT,
                      data = employee_data, family = "binomial", maxit = 1000)

# Print model summary
summary(logistic_model)




ggplot(employee_data, aes(x = age, fill = STATUS)) +
  geom_line(stat = "count", position = "identity") +
  labs(title = "Distribution of Status by Age",
       x = "Age",
       y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red")) +
  theme_minimal()



















# Assuming 'length_of_service' is the time-to-event variable
# 'STATUS' is the event indicator (1 for TERMINATED, 0 for ACTIVE)
# 'other_features' includes additional features you want to include

library(survival)

# Create a survival object
surv_obj <- Surv(time = employee_data$length_of_service, event = employee_data$STATUS)

# Combine with other features if necessary
data_for_model <- mutate(employee_data, time = surv_obj$time, event = surv_obj$event)

# Create a Cox Proportional Hazards Model
cox_model <- coxph(Surv(time, event) ~ other_features, data = data_for_model)

# Print model summary
summary(cox_model)




###################################

df_forecast <- rbind(df_forecast, data.frame(
  STATUS_YEAR = 2016,
  STATUS = "LOWER 80% FORECAST",
  count = forecast(model_active, h = 1)$lower[, "80%"][1]
))
df_forecast <- rbind(df_forecast, data.frame(
  STATUS_YEAR = 2016,
  STATUS = "UPPER 80% FORECAST",
  count = forecast(model_active, h = 1)$upper[, "80%"][1]
))



employee_year_status<- employee_data_u60 %>% 
  group_by(STATUS, STATUS_YEAR, termreason_desc) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = c( "STATUS","termreason_desc"), values_from = Count, values_fill = 0) %>%
  rename(YEAR=STATUS_YEAR, ACTIVE = `ACTIVE_Not Applicable`, Resignation = TERMINATED_Resignaton, 
         Layoff = TERMINATED_Layoff) %>% #,Retirement = TERMINATED_Retirement
  mutate(TERMINATED = Resignation+Layoff,TOTAL = ACTIVE + TERMINATED , AVG_TOTAL = mean(TOTAL)) #+Retirement



ts_terminated<- ts(employee_year_status$TERMINATED, start = min(employee_year_status$YEAR), end = max(employee_year_status$YEAR), frequency = 1)


employee_data_u60 %>% 
  group_by(STATUS, age) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = STATUS, values_from = Count, values_fill = 0) %>%
  mutate(TOTAL = ACTIVE + TERMINATED, AVG_ACTIVE = mean(ACTIVE)) %>%
  arrange(desc(TERMINATED))




employee_counts <- employee_data_u60 %>%
  group_by(city_name, STATUS) %>%
  summarise(Count = n()) %>%
  mutate(percTermin = Count/(lag(Count)+Count)) %>%
  arrange(desc(percTermin))

#######################################################



ui <- dashboardPage(
  dashboardHeader(title = "Employee Analytics Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    tabItems(
      # Descriptive Analytics Tab
      tabItem(
        tabName = "descriptive",
        fluidRow(
          box(title = "Distribution of Age",
              plotOutput("age_plot", height = 250)),
          box(title = "Length of Service vs. Age",
              plotOutput("length_vs_age_plot", height = 250))
        )
      ),
      
      # Predictive Analytics Tab
      tabItem(
        tabName = "predictive",
        fluidRow(
          box(title = "Coefficient Plot",
              plotOutput("coeff_plot", height = 250)),
          box(title = "Residual Deviance",
              verbatimTextOutput("residual_deviance"))
        )
      )
    )
  )
)


# Define server
server <- function(input, output) {
  
  # Descriptive Analytics
  
  output$age_plot <- renderPlot({
    ggplot(employee_data, aes(x = age)) +
      geom_histogram(fill = "blue", bins = 30) +
      labs(title = "Distribution of Age")
  })
  
  output$length_vs_age_plot <- renderPlot({
    ggplot(employee_data, aes(x = age, y = length_of_service)) +
      geom_point(color = "green") +
      labs(title = "Length of Service vs. Age")
  })
  
  # Predictive Analytics
  
  output$coeff_plot <- renderPlot({
    # Assuming you have a glm model named `model`
    # model <- glm(STATUS ~ age + length_of_service + region + STATUS_YEAR + BUSINESS_UNIT, family = "binomial", data = employee_data)
    coef_plot <- coefplot::coefplot(model)
    print(coef_plot)
  })
  
  output$residual_deviance <- renderText({
    # Assuming you have a glm model named `model`
    # model <- glm(STATUS ~ age + length_of_service + region + STATUS_YEAR + BUSINESS_UNIT, family = "binomial", data = employee_data)
    paste("Residual Deviance: ", summary(model)$deviance)
  })
}

# Run the app
shinyApp(ui, server)

#########################################


data.frame(
  Model = c("Model 1", "Model 2"),
  Accuracy = c(metrics2[1], metrics3[1]),
  Precision = c(metrics2[2], metrics3[2]),
  Recall = c(metrics2[3], metrics3[3]),
  F1_Score = c(metrics2[4], metrics3[4])
)
