##########################################################
## CYO Project
## Hotel Booking Prediction
## HarvardX: PH125.9x - Capstone Project
## R Script File
## Author: Marwa J. Nafakh
##########################################################

##########################################################
## Hotel Bookings Prediction R Script File
## Generates models for hotel booking predictions and their associated accuracy values
########################################################## 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dplyr)) install.packages("dplyr")
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(rattle)) install.packages("rattle")
if(!require(randomForest)) install.packages("randomForest")
if(!require(corrplot)) install.packages("corrplot")
if(!require("e1071")) install.packages("e1071")
if(!require("class")) install.packages("class")

#--------------------------------------------------------
## Read and explore the hotel bookings file
#--------------------------------------------------------

# File URL "https://www.kaggle.com/jessemostipak/hotel-booking-demand/download"
# github repository link "https://github.com/MarwaJN/CYO-Project.git"
hotel_data<-read.csv("hotel_bookings.csv")
str(hotel_data)

# Calculating total nights stayed at hotel for each customer in a new column
hotel_data <- hotel_data %>% mutate(total_nights = stays_in_weekend_nights + stays_in_week_nights)

# Calculating total total cost of stay for each customer in a new column
hotel_data <- hotel_data %>% mutate(total_cost = adr * total_nights)

# Check the added two columns
head(hotel_data)


#--------------------------------------------------------
## Clean hotel_data dataset
#--------------------------------------------------------

# Convert characters variables into factors for further analysis
hotel_data <- hotel_data %>% 
  mutate(
    hotel = as.factor(hotel),
    meal = as.factor(meal),
    arrival_date_year = as.factor(arrival_date_year),
    arrival_date_month = as.factor(arrival_date_month),
    country = as.factor(country),
    market_segment = as.factor(market_segment),
    distribution_channel = as.factor(distribution_channel),
    reserved_room_type = as.factor(reserved_room_type),
    assigned_room_type = as.factor(assigned_room_type),
    deposit_type = as.factor(deposit_type),
    agent = as.factor(agent),
    company = as.factor(company),
    customer_type = as.factor(customer_type),
    reservation_status = as.factor(reservation_status)
  )


# Check for any missing value in the hotel_data dataset
any(is.na(hotel_data))

# Find any missing values in the dataset and return the column name
list_NA <- colnames(hotel_data)[apply(hotel_data, 2, anyNA)]
list_NA

# Replace the missing values in the Children Column in the hotel_data dataset with the babies column value

missing_list <- length(hotel_data$children)
for (i in 1:missing_list){
  if(is.na(hotel_data$children[i]))
    hotel_data$children[i] <- hotel_data$babies[i]
}

#--------------------------------------------------------
## Data Exploration "hotel_data" dataset
#--------------------------------------------------------

dim(hotel_data)
str(hotel_data)
summary(hotel_data)
class(hotel_data)

#Calculating and displaying the number of rows and columns in the train dataset
paste('There are ',nrow(hotel_data),'rows', 'and ', 
      ncol(hotel_data), 'columns in the hotel data dataset')

# Displaying a table of the two options of reservations
table(hotel_data$hotel)
# It is noted that City Hotel had much more reservations than Resort Hotels

# Display pie_chart of the canceled bookings
hotel_pie <- table(hotel_data$is_canceled)
hotel_cancel <- c("Not Canceled", "Canceled")
percent <- round(hotel_pie/sum(hotel_pie)*100)
hotel_cancel <- paste(hotel_cancel,percent)
hotel_cancel <- paste(hotel_cancel,"%", sep="")
pie(hotel_pie, hotel_cancel, main = "Cancelled Bookings Distribution")

# Display pie_chart of the hotels variable
hotel_pie <- table(hotel_data$hotel)
hotel_type <- names(hotel_pie)
percent <- round(hotel_pie/sum(hotel_pie)*100)
hotel_type <- paste(hotel_type,percent)
hotel_type <- paste(hotel_type,"%", sep="")
pie(hotel_pie, hotel_type, main = "Hotel Bookings Distribution")

# Display pie_chart of the Reservation Status of the Booking
hotel_pie <- table(hotel_data$reservation_status)
hotel_status <- names(hotel_pie)
percent <- round(hotel_pie/sum(hotel_pie)*100)
hotel_status <- paste(hotel_status,percent)
hotel_status <- paste(hotel_status,"%", sep="")
pie(hotel_pie, hotel_status, main = "Hotel Bookings Reservation Status Distribution")

# Display country with highest number of reservations for both city and resort
hotel_data %>% group_by(hotel,country)%>%
  summarize(No. = n())%>%
  arrange(desc(No.))
# Portugal has the highest number of hotel bookings

# Display market segment with the highest number of bookings for both city and resort hotels
hotel_data %>% group_by(hotel, market_segment)%>%
  summarize(No. = n())%>%
  arrange(desc(No.))
# Online City Hotel bookings through agent had the highest record

#--------------------------------------------------------
## Understanding Cancellation Behavior
#--------------------------------------------------------
# Display booking status per year
hotel_data %>% ggplot(aes(x=arrival_date_year, fill = factor(is_canceled)))+
  geom_bar()+
  labs(title="Displaying Booking Status per Years", 
       x= "Year of Arrival", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme_light()
# it is noted that greatest number of cancellation occurred in the 2016

# Display booking status per month
hotel_data %>% ggplot(aes(x=arrival_date_month, fill = factor(is_canceled)))+
  geom_bar()+
  labs(title="Displaying Booking Status per Month", 
       x= "Month of Arrival", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Display booking status per No. of children
hotel_data %>% ggplot(aes(x=as.factor(children), fill = factor(is_canceled)))+
  geom_bar()+
  labs(title="Displaying Booking Status per No. of Children", 
       x= "No. of Children", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme_light()
# It is noted that number of children in the booking didn't have a major impact on the cancellation variable

# Display booking status per deposit type
hotel_data %>% ggplot(aes(x=deposit_type, fill = factor(is_canceled)))+
  geom_bar()+
  labs(title="Displaying Booking Status per Deposit Type", 
       x= "Deposit Type", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# It is noted that the majority of cancellation transactions occurred for those with no deposit provided at 
# the time of booking

# Display booking status per distribution channel
hotel_data %>% ggplot(aes(x=distribution_channel, fill = factor(is_canceled)))+
  geom_bar()+
  labs(title="Displaying Booking Status per Distribution Channel", 
       x= "Distribution Channel", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# Bookings through Travel Agents and Tour Operators had the greatest number of cancellations compared to other distribution channels

# Display booking status per customer type
hotel_data %>% ggplot(aes(x=customer_type, fill = factor(is_canceled)))+
  geom_bar()+
  labs(title="Displaying Booking Status per Customer Type", 
       x= "Customer Type", 
       y= "Cancellation Count")+
  scale_fill_discrete(name= "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# Transient Customers had the highest number of cancellations

# Display booking status per repeated guests
hotel_data %>% ggplot(aes(x=as.factor(is_repeated_guest), fill = factor(is_canceled)))+
  geom_bar()+
  labs(title="Displaying Booking Status per Repeated Guests", 
       x= "Repeated Guests", 
       y= "Cancellation Count")+
  scale_fill_discrete(name= "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme_light()
# Majority of cancellations occurred for non repeating guests

# Display booking status per stayed nights
hotel_data %>% ggplot(aes(x=total_nights, fill = factor(is_canceled)))+
  geom_bar()+
  labs(title="Displaying Booking Status per Stayed Nights", 
       x= "No. of Stayed Nights", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  xlim(1,15)+
  theme_light()
# It is noted that as the number of nights is increasing the number of cancellations is decreasing

# Display booking status per total cost of stay
hotel_data %>% ggplot(aes(x=total_cost, fill = factor(is_canceled)))+
  geom_histogram()+ 
  labs(title="Displaying Booking Status per Total Cost of Stay", 
       x= "Total Cost", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  xlim(0,1500)+
  theme_light()
# It is noted that as the total cost of stay increases the number of cancellations decreases 

# Display booking status per lead time
hotel_data %>% ggplot(aes(x=lead_time, fill = factor(is_canceled)))+
  geom_histogram()+ 
  labs(title="Displaying Booking Status per Lead Time", 
       x= "Lead Time", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  xlim(0,400)+
  theme_light()
# It is apparent that the shorter the time lead the higher number of cancellations is

# Display booking status per days in waiting list
hotel_data %>% filter(days_in_waiting_list>1) %>%
  ggplot(aes(x=days_in_waiting_list, fill = factor(is_canceled)))+
  geom_histogram()+ 
  labs(title="Displaying Booking Status per Days in Waiting Lists", 
       x= "Days in Waiting List", 
       y= "Cancellation Count")+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme_light()
# It is noted from the histogram visual that when the days in waiting list are between 1 and 100 the cancellations are the highest compared to the rest of intervals
# Also when days in waiting list are around 400 almost all of the bookings are canceled

# Display booking Status across Market Segments
hotel_data %>% ggplot(aes(x=total_nights, fill=factor(is_canceled)))+
  geom_histogram()+
  scale_fill_discrete(name = "Booking Status",
                      breaks = c("0", "1"),
                      label = c("Not Canceled", "Canceled"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(0,500)+
  facet_wrap(~market_segment)
# It is noted form the visual that when total nights of stays are around 20 major of cancellations occurred in the Direct, Online and Corporate market segments

#--------------------------------------------------------
## Create Data Partitions for training and validation purposes
#--------------------------------------------------------

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = hotel_data$is_canceled, times = 1, p = 0.1, list = FALSE)
hotel_train <- hotel_data[-test_index,]
dim(hotel_train)
temp <- hotel_data[test_index,]

# Validation data set is 10% of the hotel_data 
hotel_valid <- temp
dim(hotel_valid)
# Clean memory
rm(temp, test_index)

#--------------------------------------------------------
## Data Analysis and Modeling
#--------------------------------------------------------

# In order to start the modeling process the factor variables has been converted to numeric variables in our training set 
conv_numeric <- hotel_train %>% mutate_if(is.factor, as.numeric)

# The correlation values has been calculated for the target variable "is_canceled"
correlations <- cor(conv_numeric$is_canceled, conv_numeric[,c("is_canceled","hotel","is_repeated_guest", "company", 
                                                              "agent", "adults", "children", "babies", "is_repeated_guest", "meal", 
                                                              "customer_type", "adr", "total_nights", "total_cost", "days_in_waiting_list", 
                                                              "required_car_parking_spaces", "arrival_date_year","lead_time", 
                                                              "stays_in_weekend_nights", "stays_in_week_nights", "country", "market_segment", 
                                                              "distribution_channel", "previous_bookings_not_canceled", "previous_cancellations",
                                                              "reserved_room_type", "assigned_room_type", "booking_changes", "deposit_type")])

# Then plot the correlation coefficient
corrplot(correlations, method="circle")
# It is apparent from the plot that the following variables have strong relation to cancellation
# deposit_type, country, distribution_channel, company, lead_time, previous_cancellations, required_car_parking

# Then the factors with the strong relation to the target variable will be selected for further modeling and analysis
hotel_train <- hotel_train[c("is_canceled", "country", "deposit_type", "distribution_channel", "company", "lead_time", "required_car_parking_spaces", "previous_cancellations")]
colnames(hotel_train)
hotel_valid <- hotel_train[c("is_canceled", "country", "deposit_type", "distribution_channel", "company", "lead_time", "required_car_parking_spaces", "previous_cancellations")]
colnames(hotel_valid)

# Convert factors to numeric values for modeling purposes
hotel_train <- hotel_train %>% mutate_if(is.factor, as.numeric)
hotel_valid <- hotel_valid %>% mutate_if(is.factor, as.numeric)

## glm Model
#--------------------------------------------------------
set.seed(1, sample.kind="Rounding")
# Generate glm model
glm_model <- glm(is_canceled~.,family="binomial", data = hotel_train)
summary(glm_model)

# Predict the model on the validation dataset
pred_glm <- predict(glm_model, hotel_valid, type="response")
# Record the model prediction results in a binary form of 0 and 1 
pred_glm_class <-ifelse(pred_glm>0.5,"1","0") 

# Record the prediction against actual data in the validation dataset
glm_pred_table <- table(pred_glm_class, hotel_valid$is_canceled, dnn = c("predicted","actual"))
glm_pred_table

# Calculate model accuracy based on the prediction table "pred_table" where prediction met actual in the validation dataset hotel_valid
glm_accuracy <- ((glm_pred_table[1,1]+glm_pred_table[2,2])/nrow(hotel_valid))*100

model_results <- data.frame(Method_Name = "Logestic Regression Model", Accuracy = glm_accuracy)
model_results

# Store and Update Model Results Table
model_results %>% knitr::kable()

## Classification Tree Model
#--------------------------------------------------------
set.seed(1, sample.kind="Rounding")
# Generate the classification tree model
class_tree_model <- rpart(is_canceled~., data = hotel_train, method="class")

# Plot the classification tree 
rpart.plot(class_tree_model)

# Predict the model on the validation dataset
pred_class_tree <- predict(class_tree_model, as.data.frame(hotel_valid), type = "class")

# Display prediction results
class_tree_pred_table <- table(pred_class_tree, hotel_valid$is_canceled, dnn = c("Predicted","Actual"))
class_tree_pred_table

# Calculate accuracy of the class tree model
class_tree_accuracy <- ((class_tree_pred_table[1,1]+class_tree_pred_table[2,2])/nrow(hotel_valid))*100

model_results <- bind_rows(model_results, data.frame(Method_Name = "Classification Tree Model", Accuracy = class_tree_accuracy))
model_results

# Store and Update Model Results Table
model_results %>% knitr::kable()

## Random Forest Model
#--------------------------------------------------------
set.seed(1, sample.kind="Rounding")

# Generate random forest model
rf_model <- randomForest(is_canceled~., data = hotel_train, ntree= 50)

# Predict the model on the validation dataset
pred_rf <- predict(rf_model,hotel_valid,type="response")

# Record the model prediction results in a binary form of 0 and 1 
pred_rf_class <-ifelse(pred_rf>0.5,"1","0") 

# Record the prediction against actual data in the validation dataset
rf_pred_table <- table(pred_rf_class, hotel_valid$is_canceled, dnn = c("predicted","actual"))
rf_pred_table

# Calculate accuracy of the Random Forest Model
rf_accuracy <- ((rf_pred_table[1,1]+rf_pred_table[2,2])/nrow(hotel_valid))*100

model_results <- bind_rows(model_results, data.frame(Method_Name = "Random Forest Model", Accuracy = rf_accuracy))
model_results

# Store and Update Model Results Table
model_results %>% knitr::kable()

# Plotting the Accuracy values of the three models
model_results %>% ggplot(aes(nrow(hotel_valid),Accuracy, color=Method_Name))+geom_point()

