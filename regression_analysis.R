#This regression is to model the relationship between miles driven and risk factor. 
#This can provide insights into how mileage influences risk, 
#which is useful for managing and predicting fleet safety.




# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Reading the individual data files
risk_factor <- read_csv('riskfactor.csv')
driver_mileage <- read_csv('drivermilage.csv')

# Renaming the columns to remove the default prefix and to correct any discrepancies
risk_factor <- rename(risk_factor, driverid = default.riskfactor.driverid,
                      events = default.riskfactor.events,
                      totmiles = default.riskfactor.totmiles,
                      riskfactor = default.riskfactor.riskfactor)

driver_mileage <- rename(driver_mileage, driverid = default.drivermileage.driverid, totmiles = default.drivermileage.totmiles)

# Merging the datasets on 'driverid'
#combined_data <- merge(risk_factor, driver_mileage, by = "driverid")
combined_data <- merge(risk_factor, driver_mileage, by = "driverid", suffixes = c("_riskfactor", "_drivermileage"))


# Running the regression analysis with 'totmiles' and 'events' as predictors for 'riskfactor'
model <- lm(riskfactor ~ totmiles_riskfactor + events, data = combined_data)

# Summary of the regression model
summary_model <- summary(model)

# Print the summary to view the coefficients and statistics
print(summary_model)

# Visualization of the regression model
plot_model <- ggplot(combined_data, aes(x = totmiles_riskfactor, y = riskfactor)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Risk Factor Prediction Model",
       x = "Total Miles Driven (Risk Factor Data)",
       y = "Risk Factor")

# Print the plot
print(plot_model)

# Predicting the risk factor using the model for an example new data point
new_data <- data.frame(totmiles_riskfactor = c(100000), events = c(3))
predicted_risk <- predict(model, newdata = new_data)
predicted_risk




# looks like there is overfitting lets check lessgo
# Check for outliers using a boxplot
boxplot(combined_data$totmiles_riskfactor, main="Total Miles - Risk Factor")
boxplot(combined_data$events, main="Events")

# Removing outliers for demonstration purposes (adjust according to your analysis)
combined_data1 <- combined_data %>% 
  filter(between(totmiles_riskfactor, quantile(totmiles_riskfactor, 0.05), quantile(totmiles_riskfactor, 0.95))) %>%
  filter(between(events, quantile(events, 0.05), quantile(events, 0.95)))

# Transform 'events' with a logarithmic transformation if it's highly skewed
combined_data1$events_log <- log(combined_data1$events + 1)
combined_data1$totmiles <- scale(combined_data1$totmiles_riskfactor)

# Run the regression analysis with the transformed 'events' variable
model1 <- lm(riskfactor ~ totmiles_riskfactor + events_log, data = combined_data1)

# Summary of the regression model
summary_model1 <- summary(model1)

# Print the summary to view the coefficients and statistics
print(summary_model1)

ggplot(combined_data1, aes(x = totmiles_riskfactor, y = riskfactor)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Risk Factor Prediction Model",
       x = "Total Miles Driven (Risk Factor Data)",
       y = "Risk Factor")

# Prediction with the new model (remember to transform new input data in the same way)
new_data1 <- data.frame(totmiles_riskfactor = c(100000), events_log = log(c(2) + 1))
predicted_risk1 <- predict(model1, newdata = new_data1)

# Output the predicted risk factor
print(predicted_risk1)


#Lets scale the data


combined_data2 <- merge(risk_factor, driver_mileage, by = "driverid", suffixes = c("_riskfactor", "_drivermileage"))

# Scale the 'totmiles_riskfactor' variable to have mean = 0 and standard deviation = 1
combined_data2$totmiles_scaled <- scale(combined_data2$totmiles_riskfactor)

# Running the regression analysis with the scaled 'totmiles' and original 'events'
model_scaled <- lm(riskfactor ~ totmiles_scaled + events, data = combined_data2)

# Summary of the regression model with scaled data
summary_model_scaled <- summary(model_scaled)

# Print the summary to view the coefficients and statistics
print(summary_model_scaled)

# Visualization of the regression model with scaled data
plot_model_scaled <- ggplot(combined_data2, aes(x = totmiles_scaled, y = riskfactor)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Risk Factor Prediction Model with Scaled Miles",
       x = "Scaled Total Miles Driven",
       y = "Risk Factor")

# Print the plot
print(plot_model_scaled)

# Predicting the risk factor using the new model with scaled miles
# Scale the new 'totmiles' data point similarly before prediction
new_miles_scaled <- scale(c(100000), center = mean(combined_data2$totmiles_riskfactor), scale = sd(combined_data2$totmiles_riskfactor))
new_data_scaled <- data.frame(totmiles_scaled = new_miles_scaled, events = c(2))
predicted_risk_scaled <- predict(model_scaled, newdata = new_data_scaled)

# Output the predicted risk factor
print(predicted_risk_scaled)

