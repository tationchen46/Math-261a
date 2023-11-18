# Load necessary libraries
library(openintro)
library(caTools)

# Load dataset
loans <- loans_full_schema

# Randomly split the dataset into training and test sets
set.seed(123) # Set a random seed for reproducibility
split <- sample.split(loans$interest_rate, SplitRatio = 0.7) # Adjust SplitRatio as needed

# Creating training and test datasets
training_set <- subset(loans, split == TRUE)
test_set <- subset(loans, split == FALSE)

# Proceed with preprocessing on the training set
project <- subset.data.frame(training_set, select = c(interest_rate, grade, annual_income, total_credit_lines, num_historical_failed_to_pay, total_credit_limit, debt_to_income, installment, loan_purpose, term, application_type, homeownership, loan_amount, public_record_bankrupt, total_credit_utilized, num_total_cc_accounts))

# Find and handle missing elements in the training set
missing_elements <- which(is.na(project), arr.ind = TRUE)
print(missing_elements)
project <- na.omit(project)

#fit the full model
fit <- lm(interest_rate ~ grade + annual_income + total_credit_lines + 
            num_historical_failed_to_pay + total_credit_limit + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + public_record_bankrupt + 
            total_credit_utilized + num_total_cc_accounts, 
          data = project)
summary(fit)
plot(project)

# Assuming lm_model is your linear regression model
residuals <- residuals(fit)

# Get the names of quantitative predictors in your model
quantitative_predictors <- c("interest_rate", "annual_income", "total_credit_lines", "total_credit_limit", "debt_to_income", "installment", "loan_amount", "total_credit_utilized", "num_total_cc_accounts")

# Set up a multi-panel plot

# Loop through each quantitative predictor
for (predictor in quantitative_predictors) {
  # Extract the predictor values and residuals
  predictor_values <- project[[predictor]]
  residuals <- residuals(fit)
  
  # Check lengths
  if (length(predictor_values) == length(residuals)) {
    # Set up a multi-panel plot
    par(mfrow = c(1, 2))
    
    # Residual plot
    plot(predictor_values, residuals,
         main = paste("Residuals vs.", predictor),
         xlab = predictor, ylab = "Residuals", pch = 20)
    abline(0,0)
    
    # Pause to view each plot, press Enter to continue
    cat("Press Enter to continue...")
    readline()
  } else {
    warning("Length mismatch for", predictor)
  }
}

# QQ plot
qqnorm(residuals)
qqline(residuals)

# Reset the plotting layout
par(mfrow = c(1, 1))

#fiited again residual
plot(fitted(fit), residuals, main = "Residuals vs predicted", xlab = "predicted", ylab = "Residuals")

# Plot interest_rate against annual_income
plot(project$annual_income, project$interest_rate,
     main = "Interest Rate vs. Annual Income",
     xlab = "Annual Income", ylab = "Interest Rate", pch = 20)
# the trend between annual income and interest rate has log(x)

# Optional: Fit a linear model and add a regression line to the plot
fit_simple <- lm(interest_rate ~ annual_income, data = project)
abline(fit_simple, col = "red")

# log transformation of 'annual_income'
project$annual_income_log <- log(project$annual_income)

# (Optional) Fit the model with the transformed 'annual_income'
fit <- lm(interest_rate ~ grade + annual_income_log + total_credit_lines + 
            num_historical_failed_to_pay + total_credit_limit + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + public_record_bankrupt + 
            total_credit_utilized + num_total_cc_accounts, 
          data = project)
residuals <- residuals(fit)
# Residual plot
plot(project[[c("annual_income_log")]], residuals,
     main = paste("Residuals vs.", "annual_income_log"),
     xlab = "annual_income_log", ylab = "Residuals", pch = 20)
abline(0,0)

# Plot interest_rate against total_credit_limit
plot(project$total_credit_limit, project$interest_rate,
     main = "Interest Rate vs. total_credit_limit",
     xlab = "total_credit_limit", ylab = "Interest Rate", pch = 20)
# the trend between annual income and interest rate has log(x)

# Apply log transformation and handle potential division by zero
project$total_credit_limit_log <- ifelse(project$total_credit_limit == 0, 0,log(project$total_credit_limit))

# Remove rows with NA values
project <- na.omit(project)

# (Optional) Fit the model with the transformed 'annual_income'
fit <- lm(interest_rate ~ grade + annual_income_log + total_credit_lines + 
            num_historical_failed_to_pay + total_credit_limit_log + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + public_record_bankrupt + 
            total_credit_utilized + num_total_cc_accounts, 
          data = project)
residuals <- residuals(fit)
# Residual plot
plot(project[[c("total_credit_limit_log")]], residuals,
     main = paste("Residuals vs.", "total_credit_limit_log"),
     xlab = "total_credit_limit_log", ylab = "Residuals", pch = 20)
abline(0,0)

# QQ plot
qqnorm(residuals)
qqline(residuals)
#fiited again residual
plot(fitted(fit), residuals, main = "Residuals vs predicted", xlab = "predicted", ylab = "Residuals")

# Load necessary library
library(MASS)
boxcox(fit, lambda = seq(-1,1 , by = 0.1))

fit <- lm(I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
            num_historical_failed_to_pay + total_credit_limit_log + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + public_record_bankrupt + 
            total_credit_utilized + num_total_cc_accounts, 
          data = project)

