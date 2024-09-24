# Generating synthetic data 
set.seed(123) # For reproducibility 
n <- 100 # Number of patients 

treatment <- rep(c("A", "B"), each = n/2) 

outcome <- c(rnorm(n/2, mean = 20, sd = 5), rnorm(n/2, mean = 18, sd = 5)) # Treatment A has a slightly higher effect 

patient_id <- 1:n 



data <- data.frame(patient_id, treatment, outcome) 



# Introduce some missing data in the outcome 

set.seed(234) 

missing_indices <- sample(1:n, 20) # Randomly choose 20 outcomes to be missing 

data$outcome[missing_indices] <- NA 

complete_cases_data <- na.omit(data) # Remove missing data 



# Perform t-test to compare treatments 

primary_result <- t.test(outcome ~ treatment, data = complete_cases_data) 

print(primary_result)


# Simple imputation with the mean of available outcomes 

mean_outcome <- mean(data$outcome, na.rm = TRUE) 

data$outcome[is.na(data$outcome)] <- mean_outcome 



# Perform t-test after imputation 

sensitivity_result <- t.test(outcome ~ treatment, data = data) 

print(sensitivity_result) 