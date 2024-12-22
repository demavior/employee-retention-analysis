library(tidyr) 
library(forcats)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(shiny)
library(shinydashboard)

# 1. Import Data
# Using CSV file named "HR_Analytics.csv"
employee_data <- read.csv("data/raw/MFG10YearTerminationData.csv")

# 2. Tidy Data and Data Transformations
# Remove unnecessary columns (gender_full) and verifying missing values
employee_data <- subset(employee_data, select= -c(gender_full, birthdate_key, terminationdate_key, recorddate_key,orighiredate_key
)) 
sum(is.na(employee_data))

# Make sure of unique data
employee_data <- unique(employee_data)

employee_data


# Combining Departments into Operational Units
# {r op_unit, echo=FALSE}
# Create a mapping table for department to functional units
department_mapping <- data.frame(
  functional_unit = c("Finance", "Finance", "Finance", "Finance", 
                      "HR", "HR", "HR", "HR", "HR", "IT", "IT",
                      "Management", "Management", "Management", "Operations", "Operations",
                      "Operations", "Operations", "Operations", "Operations", "Operations", 
                      "Legal", "Legal", "Legal"),
  department_name = c('Accounts Payable', 'Accounting', 'Compensation', 'Accounts Receiveable', 
                      "Recruiter", "Recruitment", "Training", 'Employee Records', "Labor Relations",
                      'Information Technology', 'HR Technology',
                      'Store Manager', 'Store Management',  'Executive',
                      'Meats', 'Dairy', 'Produce', 'Bakery', 'Processed Foods', 'Customer Service Manager',
                      'Customer Service',
                      'Investment', 'Audit', 'Legal'
                      
  )
)
# Merge the data frame with the mapping table
employee_data <- left_join(employee_data, department_mapping, by = "department_name")


# Generate region based on cities
city_to_region <- data.frame(
  city_name = c('Vancouver', 'Terrace', 'Nanaimo', 'Nelson', 'Kelowna', 'Victoria', 
           'Kamloops', 'Fort St John', 'Surrey', 'Vernon', 'Quesnel', 'Chilliwack', 
           'Dawson Creek', 'Squamish', 'New Westminster', 'Port Coquitlam', 'Cortes Island', 'Burnaby', 
           'Bella Bella', 'Cranbrook', 'Williams Lake', 'Trail', 'Prince George', 'Richmond', 
           'Grand Forks', 'West Vancouver', 'Abbotsford', 'Aldergrove', 'Langley', 'North Vancouver', 
           'White Rock', 'New Westminister', 'Fort Nelson', 'Haney', 'Valemount', 'Ocean Falls', 
           'Princeton', 'Dease Lake', 'Pitt Meadows', 'Blue River'),
  region = c('Southwest', 'Southwest', 'Island/Coast', 'Kootenay Region', 'Thompson-Okanagan', 'Island/Coast', 
             'Thompson-Okanagan', 'North Coast and Nechako', 'Southwest', 'Thompson-Okanagan', 'Cariboo Region', 'Southwest',
             'North Coast and Nechako', 'Southwest', 'Southwest', 'Southwest', 'Island/Coast', 'Southwest', 
             'Island/Coast', 'Kootenay Region', 'Cariboo Region', 'Kootenay Region', 'North Coast and Nechako', 'Southwest',
             'Kootenay Region', 'Southwest', 'Southwest', 'Southwest', 'Southwest', 'Southwest', 
             'Southwest', 'Southwest', 'North Coast and Nechako', 'Southwest', 'Southwest', 'Island/Coast', 
             'Southwest', 'North Coast and Nechako', 'Southwest', 'Southwest')
)

# Merge the data frame with the mapping table
employee_data <- left_join(employee_data, city_to_region, by = "city_name")





# 3.Exploratory Data Analysis (EDA)
# Summary Statistics
# Calculate summary statistics for specific columns
summary(employee_data[, c("age", "length_of_service","department_name","job_title")])
#summary(employee_data)
str(employee_data)
dim(employee_data)

# Histogram of 'age'
hist(employee_data$age, main = "Distribution of Employee Ages", xlab = "Age")

# Boxplot of 'length_of_service' by 'department_name'
boxplot(length_of_service ~ department_name, data = employee_data,
        main = "Length of Service by Department", xlab = "Department", ylab = "Length of Service")
# Boxplot of 'Age' by 'department_name'
boxplot(age ~ department_name, data = employee_data,
        main = "Age by Department", xlab = "Department", ylab = "Age")

# Bar chart of 'gender'
barplot(table(employee_data$gender_short), main = "Gender Distribution", xlab = "Gender", ylab = "Count")

# Scatterplot of 'age' vs. 'length_of_service'
plot(employee_data$age, employee_data$length_of_service,
     main = "Scatterplot of Age vs. Length of Service",
     xlab = "Age", ylab = "Length of Service")

###################################################


  employee_data %>% 
    group_by(department_name, STATUS_YEAR, STATUS) %>%
    summarise(Total = n(),) %>%
    mutate(Terminated = sum(STATUS == "TERMINATED"), Retention = Total/(Total+Terminated))



###################################################

employees <- employee_data %>%
  group_by(EmployeeID, gender_short, city_name, department_name, job_title, store_name, termreason_desc, 
           termtype_desc, BUSINESS_UNIT) %>%
  summarize(Age=max(age), maxServiceLength = max(length_of_service), STATUS_YEAR = max(STATUS_YEAR), 
            STATUS=max(STATUS), .groups = "drop")

active_employees <- employees %>% filter(STATUS == "ACTIVE")
terminated_employees <- employee_data %>% filter(STATUS != "ACTIVE")

meanAge=mean(active_employees$Age)

# Distribution of the target variable 'STATUS'
ggplot(employee_data, aes(x = STATUS, fill = STATUS)) +
  geom_bar() +
  labs(title = "Distribution of Employee Status", 
       x = "Status", 
       y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))


# Horizontal bar plot for Age
ggplot(employee_data, aes(x = age, fill = STATUS)) +
  geom_bar() +
  labs(title = "Distribution of Age by Status", x = "Age", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Histogram and density plot for Age
ggplot(employee_data, aes(x = age, fill = factor(STATUS))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Distribution of Age by Status", x = "Age", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Histogram and density plot for length_of_service
ggplot(employees, aes(x = maxServiceLength, fill = factor(STATUS))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Distribution of Service Length by Status", x = "Service Length", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Bar plot for gender_short
ggplot(employee_data, aes(x = gender_short, fill = STATUS)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Gender by Status", x = "Gender", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Horizontal bar plot for city_name
ggplot(employee_data, aes(x = fct_infreq(city_name), fill = STATUS)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Distribution of City by Status", y = "City", x = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Faceted bar plot for department_name
ggplot(employee_data, aes(x = fct_infreq(department_name), fill = STATUS)) +
  geom_bar() +
  facet_wrap(~department_name, scales = "free") +
  labs(title = "Distribution of Department by Status", x = "Department", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Horizontal bar plot for job_title
ggplot(employee_data, aes(x = fct_infreq(job_title), fill = STATUS)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Distribution of Job Title by Status", y = "Job Title", x = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Stacked bar plot for store_name
ggplot(employees, aes(x = as.factor(store_name), fill = STATUS)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Store by Status", x = "Store", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Horizontal bar plot for store_name
ggplot(employee_data, aes(x = as.factor(store_name), fill = STATUS)) +
  geom_bar() +
  labs(title = "Distribution of Store by Status", y = "Store", x = "Count") +
  theme(axis.text.y = element_text(size=rel(0.8))) +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red")) +
  coord_flip()

# Bar plot for termreason_desc
ggplot(employees, aes(x = termreason_desc, fill = STATUS)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Termination Reason by Status", x = "Termination Reason", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Bar plot for termtype_desc
ggplot(employees, aes(x = termtype_desc, fill = STATUS)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Termination Type by Status", x = "Termination Type", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Bar plot for region
ggplot(employee_data, aes(x = region, fill = STATUS)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Regions by Status", x = "region", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Bar plot for BUSINESS_UNIT
ggplot(employee_data, aes(x = BUSINESS_UNIT, fill = STATUS)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Business Unit by Status", x = "Business Unit", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))
#####################################

# Bar plot for STATUS per YEAR
ggplot(employee_data, aes(x = factor(STATUS_YEAR), fill = STATUS)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Years by Status", x = "Status Year", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Get the number of Active and Terminated employee by year

employee_year_status <- employee_data %>% 
  group_by(STATUS, STATUS_YEAR) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = STATUS, values_from = Count, values_fill = 0) %>%
  select(STATUS_YEAR, ACTIVE, TERMINATED)

# Displaying the table using kable
kable(employee_year_status)

# Get terminated employees
terminated_employees <- employee_data %>% filter(STATUS != "ACTIVE")

# Stacked bar plot Reason of termination by year
ggplot(terminated_employees, aes(x = factor(STATUS_YEAR), fill = termreason_desc)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Years Unit by Reason of termination", x = "Years", y = "Count") +
  scale_fill_manual(values = c("Retirement" = "blue", "Resignaton" = "red" , "Layoff" = "green"))



# 4. Descriptive Analytics

# Get total number of employees per department
department_summary <- employee_data %>%
  #filter(STATUS_YEAR == 2006) %>%
  group_by(department_name, STATUS_YEAR) %>%
  summarize(Average_Age = mean(age), Average_Tenure = mean(length_of_service), Total_Employees = n())

# Calculate the total number of unique employees
unique_employees <- nrow(employee_data %>%
  select(EmployeeID) %>%
  distinct())
total_employees <- nrow(unique_employees)
cat("Number of Unique Employees:", total_employees, "\n")

# Calculate the number of terminated employees
terminated_employees <- sum(employee_data$STATUS == "TERMINATED")

# Calculate the termination rate
termination_rate <- (terminated_employees / total_employees) * 100
cat("Termination Rate:", termination_rate, "%\n")


# Filtering data for the previous years
#previous_years_data <- employee_data %>% filter(STATUS_YEAR < 2015)


# 5. Modeling

################# STATUS BY YEAR #######################

# Bar plot for STATUS per YEAR
ggplot(employee_data, aes(x = factor(STATUS_YEAR), fill = STATUS)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Years by Status", x = "Status Year", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Stack bar plot for Termination reasons over the Years
ggplot(employee_data %>% filter(STATUS != "ACTIVE"), aes(x = factor(STATUS_YEAR), fill = termreason_desc)) +
  geom_bar(position = "stack") +
  theme_classic() +
  labs(title = "Reasons for Termination over the Years", 
       x = "Years", 
       y = "Count",
       fill = "Termination Reason") 

# Table of Active and Terminated per year
employee_year_status<- employee_data %>% 
  group_by(STATUS, STATUS_YEAR) %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = STATUS, values_from = Count, values_fill = 0) %>%
  mutate(TOTAL = ACTIVE + TERMINATED, AVG_ACTIVE = mean(ACTIVE)) %>%
  rename(YEAR=STATUS_YEAR)

kable(employee_year_status)


# Predicting ACTIVE and TERMINATED for 2016
library(forecast)

# Create a time series object
ts_active <- ts(employee_year_status$ACTIVE, start = min(employee_year_status$YEAR), end = max(employee_year_status$YEAR), frequency = 1)

# Build a time series model (e.g., exponential smoothing)
model_active <- ets(ts_active)

# Forecast for 2016
forecast(model_active, h = 1)

# Bar plot for STATUS per YEAR
df_forecast = data.frame(
  STATUS_YEAR = 2016,
  STATUS = "ACTIVE FORECAST",
  count = forecast(model_active, h = 1)$mean[1]
)

ggplot(employee_data, aes(x = factor(STATUS_YEAR), fill = STATUS)) +
  geom_bar(position = "dodge") +
  geom_col(data = df_forecast, aes(x = factor(STATUS_YEAR), y = count, fill = STATUS), position = "dodge", width = 0.5) +
  labs(title = "Distribution of Years by Status", x = "Status Year", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red", "ACTIVE FORECAST" = "navy"))




############## ANALYZING FACTORS #################

# Convert STATUS to a factor
employee_data$STATUS <- as.factor(employee_data$STATUS)

# Splitting the data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(employee_data$STATUS, p = .7, list = FALSE)
trainData <- employee_data[trainIndex, ]
testData <- employee_data[-trainIndex, ]

# Building a random forest classifier
model <- randomForest(STATUS ~ city_name + age + length_of_service + gender_short + department_name, 
                      data = trainData, classwt = c("ACTIVE" = 1, "TERMINATED" = 1))

# Print and plot the variable importance
print(varImp(model, scale = FALSE))
varImpPlot(model)

# Making predictions
predictions <- predict(model, newdata = testData%>% select(-STATUS))

# Evaluating the predictions
confusionMatrix(predictions , testData$STATUS)

# Horizontal bar plot for Age
ggplot(employee_data %>% filter(STATUS != "ACTIVE"), aes(x = age, fill = termreason_desc)) +
  geom_bar() +
  labs(title = "Distribution of Age by Termination", x = "Age", y = "Count",fill = "Termination Reason") +
  scale_fill_manual(values = c(Retirement = "green", Layoff="red", Resignaton="blue"))


############## EVALUATING YOUNGER THAN 60 YEARS OLD #################


# Exclude employees 60 years or older
employee_data_u60 <- employee_data %>% filter(age < 60)

# Splitting the data into training and testing sets
set.seed(5) # For reproducibility
trainIndex <- createDataPartition(employee_data_u60$STATUS, p = .8, list = FALSE)
trainData <- employee_data_u60[trainIndex, ]
testData <- employee_data_u60[-trainIndex, ]

# Building a random forest classifier
model2 <- randomForest(STATUS ~ city_name + age + length_of_service + gender_short + department_name, 
                       data = trainData, classwt = c("ACTIVE" = 1, "TERMINATED" = 2))
model3 <- randomForest(STATUS ~ region + age + length_of_service + gender_short + functional_unit, 
                       data = trainData, classwt = c("ACTIVE" = 1, "TERMINATED" = 1))

# Print and plot the variable importance
print(varImp(model2, scale = FALSE))
print(varImp(model3, scale = FALSE))
varImpPlot(model2)
varImpPlot(model3)

# Making predictions
predictions2<- predict(model2, newdata = testData%>% select(-STATUS))
predictions3<- predict(model3, newdata = testData%>% select(-STATUS))

# Evaluating the predictions
confusionMatrix(predictions2, testData$STATUS)
confusionMatrix(predictions3, testData$STATUS)

############# ANALYZING FACTORS #################


# Filter data for the top 15 cities with the most terminated employees
top_15_terminated_cities <- employee_data_u60 %>%
  filter(STATUS == "TERMINATED") %>%
  group_by(city_name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(15) %>%
  pull(city_name)

# Filter employee_data_u60 for the top 15 cities
filtered_data <- employee_data_u60 %>%
  filter(city_name %in% top_15_terminated_cities)


# Horizontal bar plot for city_name
ggplot(filtered_data, aes(x = fct_infreq(city_name), fill = STATUS)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Distribution of City by Status", x = "City", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))

# Horizontal bar plot for Age
ggplot(employee_data_u60, aes(x = age, fill = STATUS)) +
  geom_bar() +
  labs(title = "Distribution of Age by Status", x = "Age", y = "Count") +
  scale_fill_manual(values = c("ACTIVE" = "blue", "TERMINATED" = "red"))


########## EVALUATION 2016 ######################

# Get data for next year 2016
employees_2016 <- employee_data_u60 %>% filter(STATUS == "ACTIVE" & STATUS_YEAR == 2015) %>%
  mutate(STATUS_YEAR = STATUS_YEAR+1,age=age+1,length_of_service=length_of_service+1)

# Making predictions for the next year
employees_2016$STATUS <- predict(model2, newdata = employees_2016)

# Calculate prediction probabilites of employees who will be terminated
employees_2016_probs <- predict(model2, newdata = employees_2016, type="prob")
employees_2016 <- as.data.frame(cbind(employees_2016, employees_2016_probs))

# View the predictions to show terminated employees
employees_2016[employees_2016$STATUS == "TERMINATED", ]

# View the percentage 
employees_2016 %>% filter(TERMINATED >0.4) %>%
  arrange(desc(TERMINATED))
    
# Save a data frame to an .rds file
saveRDS(employee_data, file = "data/processed/employee_data.rds")