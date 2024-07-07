
# GENERATE DATA

set.seed(123) # For reproducibility
n <- 500 # Number of observations
packaging <- rbinom(n, 1, 0.5) # Treatment assignment
weather <- runif(n, 5, 30) # Weather conditions
festival_proximity <- rbinom(n, 1, 0.5) # Whether close to a festival or not
store_capacity <- rnorm(n, 100, 20) # Store capacity
competitors_pricing <- runif(n, 50, 150) # Competitors' pricing strategies

# Outcome model (True effect + noise)
sales <- 200 + 50 * packaging + 10 * weather - 15 * festival_proximity +
  0.5 * store_capacity - 0.25 * competitors_pricing + rnorm(n, 0, 50)

data <- data.frame(packaging, weather, 
                   festival_proximity, store_capacity, 
                   competitors_pricing, sales)


# ESTIMATE PROPENSITY SCORES

# Logistic regression for propensity scores
propensity_model <- glm(packaging ~ weather + festival_proximity + store_capacity + competitors_pricing,
                        family = binomial(), data = data)
data$propensity_score <- predict(propensity_model, type = "response")


# OUTCOME REGRESSION MODEL
outcome_model <- lm(sales ~ packaging + weather + festival_proximity + store_capacity + competitors_pricing, data = data)


# DOUBLY ROBUST ESTIMATION
# Inverse Probability of Treatment Weighting (IPTW)
weights <- ifelse(packaging == 1, 1 / data$propensity_score, 1 / (1 - data$propensity_score))

# Predicted sales under treatment and control, using the outcome model
predicted_sales_treated <- predict(outcome_model, newdata = transform(data, packaging = 1))
predicted_sales_control <- predict(outcome_model, newdata = transform(data, packaging = 0))

# DR Estimation
dr_estimate <- mean(weights * (packaging * sales + (1 - packaging) * predicted_sales_treated -
                                 packaging * predicted_sales_control - (1 - packaging) * sales))




## AER SANDWICH

# Install packages if not already installed
if (!require("AER")) install.packages("AER")
if (!require("sandwich")) install.packages("sandwich")

# Load packages
library(AER)
library(sandwich)

# MODIFY OUTCOME 
outcome_model <- lm(sales ~ packaging + weather + festival_proximity + store_capacity + competitors_pricing, data = data)

# ADJUST VARIANCE ESTIMATE
robust_standard_errors <- sqrt(diag(vcovHC(outcome_model, type = "HC1")))
summary(outcome_model, robust = robust_standard_errors)


## GIVEN OUTCOME MODEL ALRDY FITTED
# Given: outcome_model is already fitted

# Calculate robust standard errors using vcovHC from the sandwich package
robust_se <- sqrt(diag(vcovHC(outcome_model, type = "HC1")))

# Update the summary of the outcome model with robust standard errors
coefs <- summary(outcome_model)$coefficients
robust_summary <- cbind(coefs[,1:2], Robust.Std.Error = robust_se, coefs[,3:4])

# Print the updated summary table with robust standard errors
print(robust_summary)



# ADV DOUBLY ROBUST ESTIMATION

if (!require("tmle")) install.packages("tmle")
if (!require("SuperLearner")) install.packages("SuperLearner")

library(tmle)
library(SuperLearner)


# Install necessary packages if they are not already installed
packages_needed <- c("tmle", "SuperLearner", "randomForest")
packages_to_install <- packages_needed[!packages_needed %in% installed.packages()[,"Package"]]
if(length(packages_to_install)) install.packages(packages_to_install)

# Load the installed packages into the R session
library(tmle)
library(SuperLearner)
library(randomForest)
# Install the gbm package if it's not already installed
if (!require("gbm", character.only = TRUE)) {
  install.packages("gbm")
  library(gbm)
} else {
  # The package is already installed, just load it
  library(gbm)
}

# Check to ensure the gbm package is


# DEFINE SUPERLEARNER
# Define the list of candidate learners
candidate_learners <- c("SL.glm", "SL.randomForest", "SL.gbm")

# Define Super Learner for the treatment model
treatment_learner <- SuperLearner(Y = data$packaging, X = data[, c("weather", "festival_proximity", "store_capacity", "competitors_pricing")],
                                  SL.library = candidate_learners, method = "method.NNLS")

# Define Super Learner for the outcome model
outcome_learner <- SuperLearner(Y = data$sales, X = data[, c("packaging", "weather", "festival_proximity", "store_capacity", "competitors_pricing")],
                                SL.library = candidate_learners, method = "method.NNLS")


# Applying TMLE for causal effect estimation
tmle_result <- tmle(Y = data$sales, A = data$packaging, 
                    W = data[, c("weather", "festival_proximity", "store_capacity", "competitors_pricing")],
                    Q.SL.library = outcome_learner, g.SL.library = treatment_learner)

# Extracting and printing the estimated Average Treatment Effect (ATE)
print(tmle_result$estimates$ATE)

