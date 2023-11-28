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
fit.full <- lm(interest_rate ~ grade + annual_income + total_credit_lines + 
            as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + as.factor(public_record_bankrupt) + 
            total_credit_utilized + num_total_cc_accounts, data = project)
summary(fit.full)
#plot(project)

# Assuming lm_model is your linear regression model
residuals <- residuals(fit.full)

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
     main = "Interest Rate vs. Annual Income",xlab = "Annual Income", ylab = "Interest Rate", pch = 20)
# the trend between annual income and interest rate has log(x)

# Optional: Fit a linear model and add a regression line to the plot
fit_simple <- lm(interest_rate ~ annual_income, data = project)
abline(fit_simple, col = "red")

# log transformation of 'annual_income'
project$annual_income_log <- log(project$annual_income)

# (Optional) Fit the model with the transformed 'annual_income'
fit <- lm(interest_rate ~ grade + annual_income_log + total_credit_lines + 
            as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + as.factor(public_record_bankrupt) + 
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
            as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + as.factor(public_record_bankrupt) + 
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
            as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + as.factor(public_record_bankrupt) + 
            total_credit_utilized + num_total_cc_accounts, 
          data = project)

fit.f0 <- lm(I(interest_rate^0.5) ~ 1, data = project)
add1(fit.f0, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add grade
fit.f1 <- lm(I(interest_rate^0.5) ~ grade, data = project)
add1(fit.f1, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
#add total_credit_limit_log
fit.f2 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log, data = project)
add1(fit.f2, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
#add term
fit.f3 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + term, data = project)
add1(fit.f3, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add debt_to_income
fit.f4 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + term, data = project)
add1(fit.f4, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add total_credit_utilized
fit.f5 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + term + 
               total_credit_utilized, data = project)
add1(fit.f5, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add num_total_cc_accounts
fit.f6 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + term + 
               total_credit_utilized + num_total_cc_accounts, data = project)
add1(fit.f6, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add loan_amount
fit.f7 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + term + 
               loan_amount + total_credit_utilized + num_total_cc_accounts, data = project)
add1(fit.f7, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add installment
fit.f8 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + installment + term + 
               loan_amount + total_credit_utilized + num_total_cc_accounts, data = project)
add1(fit.f8, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add loan_purpose
fit.f9 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + installment + loan_purpose + 
               term + loan_amount + total_credit_utilized + num_total_cc_accounts, data = project)
add1(fit.f9, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")

library(leaps)
# Double check order in which to add, since a lot of the p-values are the same.
forward.selection <- regsubsets(x = cbind(project$grade , project$annual_income_log , project$total_credit_lines , 
                                            as.factor(project$num_historical_failed_to_pay) , project$total_credit_limit_log , project$debt_to_income , 
                                            project$installment , project$loan_purpose , project$term , project$application_type , 
                                            project$homeownership , project$loan_amount , as.factor(project$public_record_bankrupt) , 
                                            project$total_credit_utilized , project$num_total_cc_accounts), y = I(project$interest_rate^0.5), method = "forward")
summary(forward.selection)

#backwards selection
fit.b0 <- lm(I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
               as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
               installment + loan_purpose + term + application_type + 
               homeownership + loan_amount + as.factor(public_record_bankrupt) + 
               total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.b0, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_lines + 
        as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
        installment + loan_purpose + term + application_type + 
        homeownership + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# remove total_credit_lines
fit.b1 <- lm(I(interest_rate^0.5) ~ grade + annual_income_log + 
               as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
               installment + loan_purpose + term + application_type + 
               homeownership + loan_amount + as.factor(public_record_bankrupt) + 
               total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.b1, I(interest_rate^0.5) ~ grade + annual_income_log + 
        as.factor(num_historical_failed_to_pay) + total_credit_limit_log + debt_to_income + 
        installment + loan_purpose + term + application_type + 
        homeownership + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# remove as.factor(num_historical_failed_to_pay)
fit.b2 <- lm(I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_limit_log + debt_to_income + 
               installment + loan_purpose + term + application_type + 
               homeownership + loan_amount + as.factor(public_record_bankrupt) + 
               total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.b2, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_limit_log + debt_to_income + 
        installment + loan_purpose + term + application_type + 
        homeownership + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# remove homeownership
fit.b3 <- lm(I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_limit_log + debt_to_income + 
               installment + loan_purpose + term + application_type + loan_amount + as.factor(public_record_bankrupt) + 
               total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.b3, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_limit_log + debt_to_income + 
        installment + loan_purpose + term + application_type + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# remove as.factor(public_record_bankrupt)
fit.b4 <- lm(I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_limit_log + debt_to_income + 
               installment + loan_purpose + term + application_type + loan_amount + 
               total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.b4, I(interest_rate^0.5) ~ grade + annual_income_log + total_credit_limit_log + debt_to_income + 
        installment + loan_purpose + term + application_type + loan_amount + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# remove annual_income_log
fit.b5 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + 
               installment + loan_purpose + term + application_type + loan_amount + 
               total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.b5, I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + 
        installment + loan_purpose + term + application_type + loan_amount + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# remove application_type
fit.b6 <- lm(I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + 
               installment + loan_purpose + term + loan_amount + 
               total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.b6, I(interest_rate^0.5) ~ grade + total_credit_limit_log + debt_to_income + 
        installment + loan_purpose + term + loan_amount + 
        total_credit_utilized + num_total_cc_accounts, test = "F")

# exhaustive selection with table
exhaustive.selection <- regsubsets(x = cbind(project$grade , project$annual_income_log , project$total_credit_lines , 
                                             as.factor(project$num_historical_failed_to_pay) , project$total_credit_limit_log , project$debt_to_income , 
                                             project$installment , project$loan_purpose , project$term , project$application_type , 
                                             project$homeownership , project$loan_amount , as.factor(project$public_record_bankrupt) , 
                                             project$total_credit_utilized , project$num_total_cc_accounts), y = I(project$interest_rate^0.5), method = "exhaustive", all.best = FALSE, nbest = 2)
summary(exhaustive.selection)

Cp <- summary(exhaustive.selection)$cp
AdjR2 <- summary(exhaustive.selection)$adjr2
SSRes <- summary(exhaustive.selection)$rss
R2 <- summary(exhaustive.selection)$rsq
Matrix <- summary(exhaustive.selection)$which
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(6979-p)

output <- cbind(p, Matrix, SSRes, MSRes, R2, AdjR2, Cp)
colnames(output)[3:17] <- c("grade", "ai", "tclines", "nhftp", "tclimit", "dti", "i", "lp", "t", "at", "h", "la", "prb", "tcu", "ntcca") 
output

cor(project[,c("annual_income_log", "total_credit_lines", "total_credit_limit_log" , "debt_to_income", "installment", "term", 
               "loan_amount", "total_credit_utilized", "num_total_cc_accounts")])

# with full fit model

# forward selection
fit.full.f1 <- lm(interest_rate ~ 1, data = project)
add1(fit.full.f1, interest_rate ~ grade + annual_income + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add grade
fit.full.f2 <- lm(interest_rate ~ grade, data = project)
add1(fit.full.f2, interest_rate ~ grade + annual_income + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add term
fit.full.f3 <- lm(interest_rate ~ grade + term, data = project)
add1(fit.full.f3, interest_rate ~ grade + annual_income + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add total_credit_limit
fit.full.f4 <- lm(interest_rate ~ grade + total_credit_limit + term, data = project)
add1(fit.full.f4, interest_rate ~ grade + annual_income + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add debt_to_income
fit.full.f5 <- lm(interest_rate ~ grade + total_credit_limit + debt_to_income + term, data = project)
add1(fit.full.f5, interest_rate ~ grade + annual_income + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add num_total_cc_accounts
fit.full.f6 <- lm(interest_rate ~ grade + total_credit_limit + debt_to_income + term + num_total_cc_accounts, data = project)
add1(fit.full.f6, interest_rate ~ grade + annual_income + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add total_credit_utilized
fit.full.f7 <- lm(interest_rate ~ grade + total_credit_limit + debt_to_income + term + total_credit_utilized + num_total_cc_accounts, data = project)
add1(fit.full.f7, interest_rate ~ grade + annual_income + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")
# add application_type
fit.full.f8 <- lm(interest_rate ~ grade + total_credit_limit + debt_to_income + term + application_type + 
                    total_credit_utilized + num_total_cc_accounts, data = project)
add1(fit.full.f8, interest_rate ~ grade + annual_income + total_credit_lines + 
       as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
       installment + loan_purpose + term + application_type + 
       homeownership + loan_amount + as.factor(public_record_bankrupt) + 
       total_credit_utilized + num_total_cc_accounts, test = "F")

# backwards with full model without trans

drop1(fit.full, interest_rate ~ grade + annual_income + total_credit_lines + 
        as.factor(num_historical_failed_to_pay) + total_credit_limit + debt_to_income + 
        installment + loan_purpose + term + application_type + 
        homeownership + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
#drop num_historical_failed_to_pay
fit.full.b1 <- lm(interest_rate ~ grade + annual_income + total_credit_lines + total_credit_limit + debt_to_income + 
                    installment + loan_purpose + term + application_type + 
                    homeownership + loan_amount + as.factor(public_record_bankrupt) + 
                    total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.full.b1, interest_rate ~ grade + annual_income + total_credit_lines + total_credit_limit + debt_to_income + 
        installment + loan_purpose + term + application_type + 
        homeownership + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# drop annual_income
fit.full.b2 <- lm(interest_rate ~ grade + total_credit_lines + total_credit_limit + debt_to_income + 
                    installment + loan_purpose + term + application_type + 
                    homeownership + loan_amount + as.factor(public_record_bankrupt) + 
                    total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.full.b2, interest_rate ~ grade + total_credit_lines + total_credit_limit + debt_to_income + 
        installment + loan_purpose + term + application_type + 
        homeownership + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# drop homeownership
fit.full.b3 <- lm(interest_rate ~ grade + total_credit_lines + total_credit_limit + debt_to_income + 
                    installment + loan_purpose + term + application_type + loan_amount + as.factor(public_record_bankrupt) + 
                    total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.full.b3, interest_rate ~ grade + total_credit_lines + total_credit_limit + debt_to_income + 
        installment + loan_purpose + term + application_type + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# remove total_credit_lines
fit.full.b4 <- lm(interest_rate ~ grade + total_credit_limit + debt_to_income + 
                    installment + loan_purpose + term + application_type + loan_amount + as.factor(public_record_bankrupt) + 
                    total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.full.b4, interest_rate ~ grade + total_credit_limit + debt_to_income + 
        installment + loan_purpose + term + application_type + loan_amount + as.factor(public_record_bankrupt) + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# drop public_record_bankrupt
fit.full.b5 <- lm(interest_rate ~ grade + total_credit_limit + debt_to_income + 
                    installment + loan_purpose + term + application_type + loan_amount + 
                    total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.full.b5, interest_rate ~ grade + total_credit_limit + debt_to_income + 
        installment + loan_purpose + term + application_type + loan_amount + 
        total_credit_utilized + num_total_cc_accounts, test = "F")
# drop application_type
fit.full.b6 <- lm(interest_rate ~ grade + total_credit_limit + debt_to_income + 
                    installment + loan_purpose + term + loan_amount + 
                    total_credit_utilized + num_total_cc_accounts, data = project)
drop1(fit.full.b6, interest_rate ~ grade + total_credit_limit + debt_to_income + 
        installment + loan_purpose + term + loan_amount + 
        total_credit_utilized + num_total_cc_accounts, test = "F")

# exhaustive table
exhaustive.selection <- regsubsets(x = cbind(project$grade , project$annual_income , project$total_credit_lines , 
                                             as.factor(project$num_historical_failed_to_pay) , project$total_credit_limit , project$debt_to_income , 
                                             project$installment , project$loan_purpose , project$term , project$application_type , 
                                             project$homeownership , project$loan_amount , as.factor(project$public_record_bankrupt) , 
                                             project$total_credit_utilized , project$num_total_cc_accounts), y = project$interest_rate, method = "exhaustive", all.best = FALSE, nbest = 2)
summary(exhaustive.selection)

Cp <- summary(exhaustive.selection)$cp
AdjR2 <- summary(exhaustive.selection)$adjr2
SSRes <- summary(exhaustive.selection)$rss
R2 <- summary(exhaustive.selection)$rsq
Matrix <- summary(exhaustive.selection)$which
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(6979-p)

output <- cbind(p, Matrix, SSRes, MSRes, R2, AdjR2, Cp)
colnames(output)[3:17] <- c("grade", "ai", "tclines", "nhftp", "tclimit", "dti", "i", "lp", "t", "at", "h", "la", "prb", "tcu", "ntcca") 
output