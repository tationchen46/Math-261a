library(openintro)
loans <- loans_full_schema


project <- subset.data.frame(loans, select = c(interest_rate, grade, annual_income, total_credit_lines, num_historical_failed_to_pay, total_credit_limit, debt_to_income, installment, loan_purpose, term, application_type, homeownership, loan_amount, public_record_bankrupt, total_credit_utilized, num_total_cc_accounts))

# Find missing elements using is.na()
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
par(mfrow = c(2, length(quantitative_predictors)))

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
    
    # QQ plot
    qqnorm(residuals)
    qqline(residuals)
    
    # Reset the plotting layout
    par(mfrow = c(1, 1))
    
    # Pause to view each plot, press Enter to continue
    cat("Press Enter to continue...")
    readline()
  } else {
    warning("Length mismatch for", predictor)
  }
}

