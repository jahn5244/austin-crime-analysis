install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(dplyr)
crime <- Untitled_spreadsheet

clearance_date <- as.Date(crime$`Clearance Date`, format = '%m/%d/%Y')
occurence_date <- as.Date(crime$`Occurred Date`, format = '%m/%d/%Y')

crime$days_till_clearance <- as.numeric(clearance_date - occurence_date)



# Some of the variables that I can observe later could be the relationship between days_till_clearance
# and Highest Offense Description and Family Violence

names(crime)[names(crime) == "Highest Offense Description"] <- "highest_offense"
names(crime)[names(crime) == "Family Violence"] <- "family_violence"
names(crime)[names(crime) == "Location Type"] <- "location_type"
names(crime)[names(crime) == "Council District"] <- "council_district"
names(crime)[names(crime) == "Clearance Status"] <- "clearance_stat"


crime_reduced_filtered <- crime[!is.na(crime$location_type),]

crime_reduced_filtered |>
  ggplot(aes(x = location_type, y = days_till_clearance, fill = family_violence)) +
  geom_bar(stat = 'identity', position = 'dodge', na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

crime |>
  group_by(highest_offense) |>
  summarise(mean_days = mean(coalesce(days_till_clearance,0),na.rm = TRUE))|>
  ggplot(aes(x = reorder(highest_offense, -mean_days), y = mean_days)) +
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size = 5 )) 

crime |>
  group_by(highest_offense) |>
  summarise(mean_days = mean(days_till_clearance,na.rm = TRUE)) |>
  print(n = 32)

crime_filtered_location <- crime[!is.na(crime$location_type),]
crime_filtered_location |>
  group_by(location_type) |>
  summarise(mean_days = mean(coalesce(days_till_clearance, 0),na.rm = TRUE))|>
  ggplot(aes(x = reorder(location_type, -mean_days), y = mean_days,)) +
  geom_bar(stat = 'identity', na.rm = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 8 )) 

crime |>
  ggplot(aes(x = council_district)) +
  geom_bar(na.rm = TRUE) +
  scale_x_continuous(breaks = 1:10)

crime_filtered_clearance <- crime[!is.na(crime$clearance_stat),]

crime_filtered_clearance |>
  ggplot(aes(x = clearance_stat)) +
  geom_bar(na.rm = TRUE,fill = 'blue') 

crime |>
  ggplot(aes(x = family_violence)) +
  geom_bar(na.rm = TRUE)

crime |>
  ggplot(aes(x=highest_offense,y = days_till_clearance)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

crime$time_of_day <- ifelse(crime$`Occurred Time` < 400, "Early Morning",
                            ifelse(crime$`Occurred Time` < 1100, "Morning",
                                   ifelse(crime$`Occurred Time` < 1400, "Noon",
                                          ifelse(crime$`Occurred Time` < 1700, "Afternoon",
                                                 ifelse(crime$`Occurred Time` < 2000, "Evening",
                                                        ifelse(crime$`Occurred Time` <= 2400, "Night")
                                                 )
                                          )
                                   )
                            )
                        )

crime_reduced_filtered <- crime[!is.na(crime$location_type),]

crime_reduced_filtered %>%
  group_by(location_type, time_of_day) %>%
  summarise(total_of_crime = n()) %>%
  ggplot(aes(x = reorder(location_type, -total_of_crime), y = total_of_crime, fill = time_of_day)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Total Family Violence Incidents by Location and Time of Day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

crime |>
  ggplot(aes(x = family_violence, fill = time_of_day)) +
  geom_bar(position = "dodge") + 
  scale_fill_discrete(breaks=c('Early Morning', 'Morning', 'Noon', 'Afternoon','Evening','Night' ))
                    

# Second Part of the Project (Classification and Analysis)

# I can do one part of the classification measuring the linear regression model
# of the days_till_clearance based on family_violence.
set.seed(123)
crime_done <- subset(crime, !is.na(crime$days_till_clearance) & !is.na(crime$clearance_stat))

fit_lin <- lm(days_till_clearance ~ clearance_stat, data = crime_done)

summary(fit_lin)

install.packages("plotROC")
library(plotROC)

crime_done |> 
  # Calculate predicted values
  mutate(predicted = predict(fit_lin)) |>
  # Just show the variables of interest
  select(clearance_stat, days_till_clearance, predicted)

# Doing cross-validation using training and testing data.
# Initial steps is setting up training and testing data which is done
# using the sample_process variable

sample_process <- sample(c(TRUE, FALSE), 
                         nrow(crime_done),
                         replace = TRUE, 
                         prob = c(0.7, 0.3)) 

train_data <- crime_done[sample_process, ]
test_data <- crime_done[!sample_process, ]

# Now, we are creating the linear regression model based on the training data.
train_model <- lm(days_till_clearance ~ clearance_stat, data = train_data)

# Calculate performance with RMSE
sqrt(mean((
  # residual = observed - predicted
  crime_done$days_till_clearance - predict(fit_lin, newdata = crime_done))^2, 
  na.rm = TRUE))

# Using the created training model, we can use it to calculate the rmse of both
# the training data and the testing data.

rmse_train <- sqrt(mean((train_data$days_till_clearance - predict(train_model, newdata = train_data))^2, na.rm = TRUE))
rmse_train

rmse_test <- sqrt(mean((test_data$days_till_clearance - predict(train_model, newdata = test_data))^2, na.rm = TRUE))
rmse_test
# The training data RMSE yielded 377.0461 in comparison to 391.488 for the
# testing data.
# This indicates that the performance of the model is worse for new data.

# The overall high value of the RMSE also indicates that the location_type
# is a bad predictor for the total days till clearance, since it is better if
# the value of RMSE is lower.

summary(fit_lin)$adj.r.squared



# Now, we will do the cross-validation using the 5 fold cross-validation.

# First, we initialize the number of folds and the rows in the data set is randomly
# ordered.
set.seed(323)
k = 5
data <- crime_done[sample(nrow(crime_done)), ] 
folds <- cut(seq(1:nrow(data)), breaks = k, labels = FALSE)

perf_k <- NULL

for (i in 1:k){
  train_not_i <- data[folds != i, ]
  test_i <- data[folds == i,]
  
  train_model <- lm(days_till_clearance ~ clearance_stat, data = train_not_i)
  perf_k[i] <- sqrt(mean((
    test_i$days_till_clearance - predict(train_model, newdata = test_i))^2, 
    na.rm = TRUE))
}

perf_k

mean(perf_k)
sd(perf_k)

# The large variation in the RMSE values indicate that the data is overfitting.
# These were the results from inputting the code: 
#[1] 219.2212 163.2612 699.2421 145.9980 423.5140
#mean(perf_k)
#[1] 330.2473
# sd(perf_k)
# [1] 233.9951
 
  
# This is way too much code for evaluating just ONE OUTCOME AND ONE VARIABLE.

  
# For the second part of the evaluation (Since we have to do three parts)
# I will be using the outcome variable of family violence based on council 
# district


crime_family <- subset(crime,!is.na(crime$council_district))
crime_family$family_violence <- ifelse(crime_family$family_violence == 'Y',1,0)

new_lin <- glm(family_violence ~ council_district, data = crime_family, family = 'binomial')

summary(new_lin)
ROC <- ggplot(crime_family) + 
  geom_roc(aes(d = family_violence, m = council_district), n.cuts = 10)
ROC

# Calculating the AUC value

calc_auc(
  # Make a ROC curve
  ggplot(crime_family) + 
    geom_roc(aes(
      # Outcome is Survived
      d = family_violence,
      # Probability of surviving based on the logistic model
      m = predict(new_lin, type = "response")))
)$AUC

# Running the cross validation using the 5-fold cross-validation 
set.seed(323)
k = 5 

# Randomly order rows in the dataset
data <- crime_family[sample(nrow(crime_family)), ] 

# Create k folds from the dataset
folds <- cut(seq(1:nrow(data)), breaks = k, labels = FALSE)

perf_k <- NULL

# Use a for-loop to get performance for each k-fold
for(i in 1:k){
  # Split data into train and test data
  train_not_i <- data[folds != i, ] # train data = all observations except in fold i
  test_i <- data[folds == i, ]  # test data = observations in fold i
  
  # Train model on train data (all but fold i)
  train_model <- glm(family_violence ~ council_district, data = train_not_i, family = 'binomial')
  
  # Performance listed for each test data = fold i
  perf_k[i] <- calc_auc(
    # Make a ROC curve
    ggplot(test_i) + 
      geom_roc(aes(
        # Outcome is Day or Night
        d = family_violence,
        # Probability of Day or Night based on the logistic model
        m = predict(train_model, newdata = test_i, type = "response")))
  )$AUC
}

# Performance for each fold 
perf_k

# Average performance over all k folds and variation
mean(perf_k)
sd(perf_k)

#
#
#
# For the final part of the classification values, the outcome variable can be
#  highest offense and time of day.
crime_final <- subset(crime, !is.na(crime$days_till_clearance))

# Had to create a new categorical variable to ensure that 
# the glm model can be calculated. So i created the day_or_night variabel
crime_final$day_or_night <- ifelse((crime_final$`Occurred Time` > 700) & (crime_final$`Occurred Time` < 2000), "Day", "Night")
crime_final$day_or_night <- ifelse(crime_final$day_or_night == 'Day',1,0)

                                                
final_lin <- glm(day_or_night ~ days_till_clearance, data = crime_final, family = 'binomial')

calc_auc(
  # Make a ROC curve
  ggplot(crime_final) + 
    geom_roc(aes(
      # Outcome is Survived
      d = day_or_night,
      # Probability of surviving based on the logistic model
      m = predict(final_lin, type = "response")))
)$AUC

# Running the cross validation using the 5-fold cross-validation 
set.seed(323)

k = 5 

# Randomly order rows in the dataset
data <- crime_final[sample(nrow(crime_final)), ] 

# Create k folds from the dataset
folds <- cut(seq(1:nrow(data)), breaks = k, labels = FALSE)

perf_k <- NULL

# Use a for-loop to get performance for each k-fold
for(i in 1:k){
  # Split data into train and test data
  train_not_i <- data[folds != i, ] # train data = all observations except in fold i
  test_i <- data[folds == i, ]  # test data = observations in fold i
  
  # Train model on train data (all but fold i)
  train_model <- glm(day_or_night ~ days_till_clearance, data = train_not_i, family = 'binomial')
  
  # Performance listed for each test data = fold i
  perf_k[i] <- calc_auc(
    # Make a ROC curve
    ggplot(test_i) + 
      geom_roc(aes(
        # Outcome is Day or Night
        d = day_or_night,
        # Probability of Day or Night based on the logistic model
        m = predict(train_model, newdata = test_i, type = "response")))
  )$AUC
}

# Performance for each fold 
perf_k

# Average performance over all k folds and variation
mean(perf_k)
sd(perf_k)

# Possible indications of underfitting?
  