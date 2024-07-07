# Load necessary library
# Install necessary libraries if they are not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

# Load the 'car' package
library(car)
# Install necessary libraries
library(ggplot2)
library(broom)

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
# Assume we have a dataset 'corp_data'
# Columns: gender, remuneration, promotion_opportunities, performance_rating, department, work_experience, education, job_role

# Install necessary packages
#install.packages(c("dplyr"))
#library(lm)
library(dplyr)

# Create interaction terms
corp_data <- corp_data %>%
  mutate(GxD = interaction(gender, department),
         JxE = interaction(job_role, education))

# Model 1: Effect of Gender on Promotion Opportunities
model1 <- lm(promotion_opportunities ~ gender + performance_rating + department + GxD + job_role + work_experience, data = corp_data)
summary(model1)

# Model 2: Effect of Promotion Opportunities on Remuneration
model2 <- lm(remuneration ~ promotion_opportunities + gender + performance_rating + department + GxD + job_role + JxE + work_experience, data = corp_data)
summary(model2)

install.packages("semPlot")
# Assuming model1 (gender -> promotion opportunities) and model2 (promotion opportunities -> remuneration) are already fitted
# Install necessary library for path diagrams
library(semPlot)

# Mediation effect plot
mediation_effect <- broom::tidy(model1) %>% filter(term == "genderFemale" | term == "genderMale")
ggplot(mediation_effect, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  ylab("Effect of Gender on Promotion Opportunities") +
  xlab("Gender") +
  ggtitle("Mediation Effect of Gender on Promotion Opportunities")

# Path diagram visualization
semPaths(model1, whatLabels = "est", layout = "tree", rotation = 2)
semPaths(model2, whatLabels = "est", layout = "tree", rotation = 2)
