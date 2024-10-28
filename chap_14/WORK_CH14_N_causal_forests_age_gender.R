# Install required packages
#install.packages("grf")
#install.packages("tidyverse")
#install.packages("caret")

# Load the libraries
library(grf)
library(tidyverse)
library(caret)

# Generating synthetic data
set.seed(123)

n <- 1000

data <- tibble(
  user_id = 1:n,
  age = round(runif(n, 18, 65)),
  gender = sample(c("Male", "Female"), n, replace = TRUE),
  location = sample(c("Urban", "Suburban", "Rural"), n, replace = TRUE),
  previous_engagement = runif(n, 0, 1),
  treatment = sample(c(0, 1), n, replace = TRUE),
  engagement_rate = NA
)

# Generate engagement_rate based on treatment and covariates
data <- data %>%
  mutate(
    engagement_rate = 0.3 * age / 100 + 
      0.5 * ifelse(gender == "Female", 1, 0) + 
      0.4 * ifelse(location == "Urban", 1, 0) +
      0.2 * previous_engagement + 
      0.6 * treatment +
      rnorm(n, 0, 0.1)
  )

# Preview the data
head(data)

# Handling outliers (example: cap outliers at the 99th percentile)
quantiles <- quantile(data$engagement_rate, probs = c(0.01, 0.99))
data$engagement_rate <- pmin(pmax(data$engagement_rate, quantiles[1]), quantiles[2])

### 4.3 Building and Tuning Causal Forest Models
# Define the variables
covariates <- data %>% select(age, gender, location, previous_engagement)
treatment <- data$treatment
outcome <- data$engagement_rate

# Convert categorical variables to one-hot encoding
covariates_matrix <- model.matrix(~. - 1, data = covariates)

# Fit the causal forest model
causal_forest_model <- causal_forest(as.matrix(covariates_matrix), outcome, treatment)

# Summary of the model
summary(causal_forest_model)

# Define a grid of hyperparameters with sampling fraction less than 0.5
tune_grid <- expand.grid(min.node.size = c(5, 10, 15),
                         sample.fraction = c(0.3, 0.4, 0.45))

# Manual cross-validation
best_rmse <- Inf
best_params <- list()

for (min_node_size in tune_grid$min.node.size) {
  for (sample_fraction in tune_grid$sample.fraction) {
    model <- causal_forest(
      as.matrix(covariates_matrix), outcome, treatment,
      min.node.size = min_node_size,
      sample.fraction = sample_fraction
    )
    predictions <- predict(model)$predictions
    rmse <- sqrt(mean((outcome - predictions)^2))
    
    if (rmse < best_rmse) {
      best_rmse <- rmse
      best_params <- list(min.node.size = min_node_size, sample.fraction = sample_fraction)
    }
  }
}

print(best_params)

# Refit the model with the best parameters
causal_forest_model <- causal_forest(
  as.matrix(covariates_matrix), outcome, treatment,
  min.node.size = best_params$min.node.size,
  sample.fraction = best_params$sample.fraction
)

# Estimate the treatment effects
treatment_effects <- predict(causal_forest_model)$predictions

# Summarize treatment effects
summary(treatment_effects)

# Assess the model's performance using RMSE
rmse <- sqrt(mean((outcome - treatment_effects)^2))
print(rmse)

# Load ggplot2
library(ggplot2)

# Plot treatment effects
ggplot(data, aes(x = treatment_effects)) +
  geom_histogram(binwidth = 0.05) +
  labs(title = "Distribution of Estimated Treatment Effects",
       x = "Estimated Treatment Effect",
       y = "Frequency")

# Scatter plot of treatment effects vs. actual engagement rate
ggplot(data, aes(x = treatment_effects, y = engagement_rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Treatment Effects vs. Engagement Rate",
       x = "Estimated Treatment Effect",
       y = "Engagement Rate")

# Plot treatment effects by user demographics
ggplot(data, aes(x = age, y = treatment_effects, color = gender)) +
  geom_point(alpha = 0.5) +
  labs(title = "Treatment Effects by Age and Gender",
       x = "Age",
       y = "Estimated Treatment Effect",
       color = "Gender")