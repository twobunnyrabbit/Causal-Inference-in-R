setwd("/Users/subhajitdas/Documents/SUBHO_FILES/10_BOOK_WRITING/00_CAUSAL_INFERENCE/R")
# Load necessary library
# Install necessary libraries if they are not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

# Load the 'car' package
library(car)

library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
# Set seed for reproducibility
set.seed(500)

# Generate synthetic data
n <- 5000 # Number of observations

corp_data <- data.frame(
  gender = sample(c("Male", "Female"), n, replace = TRUE),
  remuneration = rnorm(n, mean = 70000, sd = 15000), # Assuming normal distribution for salary
  promotion_opportunities = rpois(n, lambda = 2), # Assuming Poisson distribution for number of promotions
  performance_rating = sample(1:5, n, replace = TRUE), # Ratings from 1 to 5
  department = sample(c("Sales", "IT", "HR", "Finance", "Marketing"), n, replace = TRUE),
  work_experience = rnorm(n, mean = 5, sd = 2), # Assuming normal distribution for years of experience
  education = sample(c("Bachelor", "Master", "PhD"), n, replace = TRUE),
  job_role = sample(c("Analyst", "Manager", "Senior Manager", "Director"), n, replace = TRUE)
)

# View the first few rows of the dataset
head(corp_data)


# Prepare data with interaction terms
corp_data <- corp_data %>%
  mutate(gender_factor = as.factor(gender),
         department_factor = as.factor(department),
         job_role_factor = as.factor(job_role),
         gender_dept_interaction = interaction(gender_factor, department_factor),
         gender_jobrole_interaction = interaction(gender_factor, job_role_factor))

# Expanded linear model with interaction terms
model <- lm(remuneration ~ gender_factor + job_role_factor + department_factor +
              work_experience + performance_rating + education +
              gender_dept_interaction + gender_jobrole_interaction, data = corp_data)

# Model summary
summary(model)

# Diagnostic plots
par(mfrow=c(2,2))
plot(model)

# Check for multicollinearity
#vif(model)

# Sensitivity analysis: Exclude certain confounders
model_sensitivity <- lm(remuneration ~ gender_factor + department_factor +
                          work_experience + education, data = corp_data)
summary(model_sensitivity)

# Compare models
anova(model, model_sensitivity)

# Assuming the back door adjustment model is already fitted
# Install necessary libraries
library(ggplot2)
library(broom)

# Coefficient plot for gender
coef_df <- broom::tidy(model) %>% filter(term == "gender_factorMale")


ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  ylab("Adjusted Effect of Gender on Remuneration") +
  xlab("Gender")
print(coef_df)
print(broom::tidy(model)$term)
print(table(corp_data$gender_factor))



# Residuals plot
ggplot(data = model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted")

