library(openintro)
loans <- loans_full_schema

project <- subset.data.frame(loans, select = c(interest_rate, grade, annual_income, total_credit_lines, num_historical_failed_to_pay, total_credit_limit, debt_to_income, installment, loan_purpose, term, application_type, homeownership, loan_amount, public_record_bankrupt, total_credit_utilized, num_total_cc_accounts))

#fit the full model
fit <- lm(interest_rate ~ grade + annual_income + total_credit_lines + 
            num_historical_failed_to_pay + total_credit_limit + debt_to_income + 
            installment + loan_purpose + term + application_type + 
            homeownership + loan_amount + public_record_bankrupt + 
            total_credit_utilized + num_total_cc_accounts, 
          data = project)

fit_debt_to_income <- lm(interest_rate ~ debt_to_income, data = project)
plot(fit_debt_to_income)

fit_installment <- lm(interest_rate ~ installment, data = project)
plot(fit_installment)

fit_loan_amount <- lm(interest_rate ~ loan_amount, data = project)
plot(fit_loan_amount)

fit_total_credit_utilized <- lm(interest_rate ~ total_credit_utilized, data = project)
plot(fit_total_credit_utilized)

fit_num_total_cc_accounts <- lm(interest_rate ~ num_total_cc_accounts, data = project)
plot(fit_num_total_cc_accounts)
