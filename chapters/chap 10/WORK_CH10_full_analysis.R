install.packages("AER") # For 2SLS regression
install.packages("gmm") # For GMM estimators
install.packages("lmtest") # For diagnostic tests
# Load the packages
library(AER)
library(gmm)
library(lmtest)


set.seed(123) # For reproducibility
n <- 1000 # Number of observations
sqft <- runif(n, 500, 1500) # Square footage
loc_quality <- sample(1:10, n, replace = TRUE) # Location quality
age <- sample(1:100, n, replace = TRUE) # Building age
renovated <- rbinom(n, 1, prob = 0.3) # Whether the unit is recently renovated
rent_price <- 500 + 0.5*sqft + 150*loc_quality - 2*age + 300*renovated + rnorm(n, 0, 250) # Rental price

# Endogenous variable
investor_interest <- 0.3*loc_quality + 0.02*sqft + rnorm(n, 0, 1) # Instrument
data <- data.frame(sqft, loc_quality, age, renovated, rent_price, investor_interest)


# 2SLS regression with ivreg
iv_model <- ivreg(rent_price ~ sqft + loc_quality + age + renovated | sqft + loc_quality + age + investor_interest, data = data)
summary(iv_model)
gmm_model <- gmm(rent_price ~ sqft + loc_quality + age + renovated, x = cbind(sqft, loc_quality, age, investor_interest), data = data)
summary(gmm_model)


## Diagnostics and tests
# Test for overidentifying restrictions using Hansen's J test
hansen_j_test <- summary(iv_model, diagnostics = TRUE)$diagnostics
hansen_j_test

# Wald test to check joint significance of coefficients
# Example: Test if 'sqft' and 'age' coefficients are jointly zero
waldtest(iv_model, . ~ . - sqft - age)

# Test for weak instruments
# First stage regression: endogenous variable on instruments and exogenous variables
first_stage <- lm(sqft ~ investor_interest + loc_quality + age + renovated, data = data)

# F-test for weak instruments
summary(first_stage)

# Calculate the F-statistic
F_statistic <- summary(first_stage)$fstatistic[1]
F_statistic

