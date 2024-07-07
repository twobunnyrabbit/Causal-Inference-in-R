# Step 1: Install Necessary Packages
packages <- c("tidyverse", "caret", "MatchIt", "panelMatch", "ggplot2", "synthpop", "fixest", "dplyr", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(MatchIt)
library(fixest)
library(lubridate)

# Step 2: Generate Synthetic Data
set.seed(123)
n <- 1000

data <- tibble(
  id = 1:n,
  age = sample(18:65, n, replace = TRUE),
  biking_purpose = sample(c("sports", "commute", "heavy_carrying", "occasional", "city", "rural"), n, replace = TRUE, prob = c(0.2, 0.3, 0.1, 0.2, 0.1, 0.1)),
  weather_condition = sample(c("sunny", "rainy", "windy"), n, replace = TRUE),
  road_condition = sample(c("good", "moderate", "poor"), n, replace = TRUE),
  price_sensitivity = sample(1:5, n, replace = TRUE),
  treatment = sample(0:1, n, replace = TRUE)
)

data$sales = with(data, 200 + 20 * treatment - 5 * price_sensitivity + ifelse(biking_purpose == "commute", 30, 0) + ifelse(weather_condition == "sunny", 15, -10) + rnorm(n, 0, 50))

# Step 3: Exploratory Data Analysis (EDA)
ggplot(data, aes(x = biking_purpose, fill = as.factor(treatment))) +
  geom_bar(position = "dodge") +
  labs(title = "Biking Purpose by Treatment", x = "Biking Purpose", y = "Count") +
  theme_minimal()

# Step 3: Exploratory Data Analysis (EDA)
ggplot(data, aes(x = biking_purpose, fill = as.factor(treatment))) +
  geom_bar(position = "dodge") +
  labs(title = "Biking Purpose by Treatment", x = "Biking Purpose", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Step 4: Causal Inference Analysis
# 4.1 Matching for Causal Inference
matchit_model <- matchit(treatment ~ age + biking_purpose + weather_condition + road_condition + price_sensitivity, data = data, method = "nearest")
matched_data <- match.data(matchit_model)

# 4.2 Estimating Average Treatment Effect (ATE)
ate_model <- feols(sales ~ treatment, data = matched_data)
summary(ate_model)

# Optional: Advanced Causal Inference Techniques
ife_model <- feols(sales ~ treatment | age + biking_purpose + weather_condition + road_condition + price_sensitivity | id, data = data)
summary(ife_model)

# Using 'fixest' for two-way fixed effects model (e.g., individual and time fixed effects)
fe_model <- feols(sales ~ treatment + age + biking_purpose + weather_condition + road_condition + price_sensitivity | id + year, data = data)
summary(fe_model)
