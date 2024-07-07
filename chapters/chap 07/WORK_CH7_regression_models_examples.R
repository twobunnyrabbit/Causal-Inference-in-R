n <- 1000
experience <- rnorm(n, mean=5, sd=2)
# Probability of training increases with experience
training_prob <- plogis(0.5 * experience)
training <- rbinom(n, 1, training_prob)
# Simulating wages, influenced by both training and experience
wages <- 30 + 5 * training + 2 * experience + rnorm(n)
data <- data.frame(wages, training, experience)


model <- lm(wages ~ training + experience, data=data)
summary(model)


# Install and load the necessary package
if (!require(lmtest)) {
  install.packages("lmtest")
  library(lmtest)
}

# Checking for non-linearity and heteroscedasticity
plot(model, which=1:2)
# Conducting the Breusch-Pagan test
library(lmtest)
bptest(model)

# Poisson Regression in R
model_poisson <- glm(wages ~ training + experience, family = "poisson", data = data)
summary(model_poisson)
# Negative Binomial Regression in R
library(MASS)
model_negbin <- glm.nb(wages ~ training + experience, data = data)
summary(model_negbin)

# Cox Regression in R
library(survival)
cox_model <- coxph(Surv(wages) ~ training + experience, data = data)
summary(cox_model)

# GAM in R
library(mgcv)
gam_model <- gam(wages ~ training + experience, data = data)
summary(gam_model)




# Load necessary libraries
if (!require(lme4)) {
  install.packages("lme4")
  library(lme4)
}

# Create a sample dataset with a grouping variable
set.seed(123)
data <- data.frame(
  group = factor(rep(1:10, each = 10)),  # 10 groups
  training = sample(0:1, 100, replace = TRUE),
  experience = rnorm(100, 5, 2),
  wages = rnorm(100, 50, 10)
)

# Modify the dataset to include the effect of training and experience on wages
data$wages <- data$wages + 5 * data$training + 2 * data$experience + rnorm(100, 0, 5)

# Multilevel Model in R with random intercepts for groups
multilevel_model <- lmer(wages ~ training + experience + (1 | group), data = data)
summary(multilevel_model)
