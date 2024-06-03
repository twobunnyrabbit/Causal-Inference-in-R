# Load necessary libraries
library(MASS)
library(ipw)
library(dplyr)

# Synthetic Data Generation
set.seed(123) # For reproducibility
n <- 500 # Number of observations
df <- data.frame(
  treatment = rbinom(n, 1, 0.5), # Binary treatment indicator
  outcome = rnorm(n), # Continuous outcome variable
  confounders = matrix(rnorm(n * 3), ncol = 3) # Matrix of confounders
)

# Estimate propensity score
ps_model <- glm(treatment ~ confounders.1+confounders.2+confounders.3, family = "binomial", data = df)
df$propensity_score <- predict(ps_model, type = "response")

# Calculate weights
df$weights <- ifelse(df$treatment == 1, 1 / df$propensity_score, 1 / (1 - df$propensity_score))

# Estimate ATE
ate <- with(df, sum(weights * treatment * outcome) / sum(weights * treatment) - 
              sum(weights * (1 - treatment) * outcome) / sum(weights * (1 - treatment)))
print(ate)
