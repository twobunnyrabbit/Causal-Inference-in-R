# Load necessary libraries
library(MASS)
library(ggplot2)
library(survival)
library(lme4)  # For multilevel models

# Simulated data for linear regression (wages and size)
set.seed(123)
house_data <- data.frame(size = runif(100, 1000, 5000), 
                         wages = runif(100, 200000, 500000))

# Linear regression model
model_linear <- lm(wages ~ size, data = house_data)
summary(model_linear)

# Visualization for linear regression
ggplot(house_data, aes(x = size, y = wages)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "blue")

# Simulated data for logistic regression (study_hours and passed)
set.seed(123)
exam_data <- data.frame(study_hours = runif(100, 0, 20),
                        passed = rbinom(100, 1, prob = 0.5))

# Logistic regression model
model_logistic <- glm(passed ~ study_hours, data = exam_data, family = "binomial")
summary(model_logistic)

# Predicting probabilities for logistic regression
exam_data$predicted_probability <- predict(model_logistic, type = "response")

# Simulated count data for Poisson and Negative Binomial regression (accidents)
set.seed(123)
data <- data.frame(
  training = sample(0:1, 100, replace = TRUE),
  experience = rnorm(100, 5, 2),
  accidents = rpois(100, lambda = exp(1 + 0.5 * sample(0:1, 100, replace = TRUE) + 
                                        0.3 * rnorm(100, 5, 2))),
  wages = rnorm(100, 50000, 10000)
)

# Poisson Regression with count data
model_poisson <- glm(accidents ~ training + experience, family = "poisson", data = data)
summary(model_poisson)

# Check for overdispersion
dispersion <- sum(residuals(model_poisson, type = "pearson")^2) / df.residual(model_poisson)
dispersion  # If > 1, it suggests overdispersion

# If overdispersion is detected, fit Negative Binomial regression
if (dispersion > 1) {
  model_negbin <- glm.nb(accidents ~ training + experience, data = data)
  summary(model_negbin)
}

# Simulated time-to-event data for Cox regression (failure_time and event)
set.seed(123)
maintenance_data <- data.frame(
  maintenance_freq = runif(100, 1, 12),
  failure_time = rexp(100, rate = 0.1),  # Time-to-event data
  event = rbinom(100, 1, 0.7)           # Event occurrence
)

# Create censoring (if failure_time > 20, it's censored)
maintenance_data$event <- ifelse(maintenance_data$failure_time > 20, 0, 1)

# Cox Proportional Hazards Model
surv_obj <- Surv(time = maintenance_data$failure_time, event = maintenance_data$event)
model_cox <- coxph(surv_obj ~ maintenance_freq, data = maintenance_data)
summary(model_cox)

# Check proportional hazards assumption
cox.zph(model_cox)  # Proportional hazards diagnostic

# Simulated data for multilevel model (grouped wages)
set.seed(123)
multilevel_data <- data.frame(
  group = factor(rep(1:10, each = 10)),  # 10 groups
  training = sample(0:1, 100, replace = TRUE),
  experience = rnorm(100, 5, 2),
  wages = rnorm(100, 50000, 10000)
)

# Multilevel Model with random intercepts for groups
multilevel_model <- lmer(wages ~ training + experience + (1 | group), data = multilevel_data)
summary(multilevel_model)

# Updating dataset: adding effect of training and experience on wages
multilevel_data$wages <- multilevel_data$wages + 5 * multilevel_data$training + 
  2 * multilevel_data$experience + rnorm(100, 0, 5000)
summary(multilevel_model)


# Generalized Additive Model (GAM) to handle non-linear relationships
# Treat `training` as a factor (binary) and apply smoothing to `experience` (continuous)
gam_model <- gam(wages ~ training + s(experience), data = data)
summary(gam_model)

# Visualization of the GAM model with smoothing on `experience`
ggplot(data, aes(x = experience, y = wages, color = as.factor(training))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), col = "green")
