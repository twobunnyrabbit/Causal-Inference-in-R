
## 1 SYNTHETIC DATA
# Loading necessary libraries
library(tidyverse)
library(MatchIt)
library(cobalt)

# Setting a seed for reproducibility
set.seed(123)

# Generating synthetic data
n <- 500 # Number of observations
data <- tibble(
  id = 1:n,
  age = rnorm(n, mean = 50, sd = 10), # Age of the voters
  income = rnorm(n, mean = 50000, sd = 12000), # Income levels
  education = rnorm(n, mean = 16, sd = 2), # Education levels
  political_campaign = rbinom(n, 1, p = 0.5), # Exposure to the campaign (0: no, 1: yes)
  party_affiliation = rbinom(n, 1, p = 0.4), # Party affiliation (0: Republican, 1: Democrat)
  vote = rbinom(n, 1, p = 0.6) # Voting behavior (0: did not vote, 1: voted)
)

# Viewing the dataset
head(data)


## 2 ESTIMATING PROPENSITY SCORES

# Estimating Propensity Scores with logistic regression
ps_model <- glm(political_campaign ~ age + income + education + party_affiliation, 
                data = data, family = "binomial")

# Adding propensity scores to the dataset
data$ps <- predict(ps_model, type = "response")


## 3 PERFORMING MATCHING
# Performing nearest neighbor matching
match_data <- matchit(political_campaign ~ age + income + education + party_affiliation, 
                      data = data, method = "nearest")

# Viewing match summary
summary(match_data)


## 4 ACCESSING BALANCE
# Checking balance
bal.tab <- bal.tab(match_data, un = TRUE)
print(bal.tab)

# Visualizing balance with Love plot
love.plot(bal.tab)


## 5 ANALYZE TREATMENT EFFECT
# Extracting matched data
matched_data <- match.data(match_data)

# Comparing outcomes between groups
effect <- with(matched_data, t.test(vote[political_campaign == 1], vote[political_campaign == 0]))
print(effect)


## 6 SENSITIVITY ANALYSIS
# Performing sensitivity analysis
install.packages("sensitivitymv")
library(sensitivitymv)
sen <- senmv(data) #, outcome = "income", treat = "political_campaign")
print(sen)


## 7 VISUALIZE THE DATA

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Analyzing and plotting the data
matched_data %>%
  # Grouping data by 'political_campaign'
  group_by(political_campaign) %>%
  # Calculating the average vote for each campaign
  summarise(Average_Vote = mean(vote, na.rm = TRUE)) %>%
  # Creating a bar plot
  ggplot(aes(x = factor(political_campaign, labels = c("No", "Yes")), y = Average_Vote, fill = factor(political_campaign, labels = c("No", "Yes")))) +
  geom_bar(stat = "identity") +  # Using identity stat for pre-summarized data
  # Adding labels and titles
  labs(x = "Political Campaign", y = "Average Vote", fill = "Campaign") +
  # Applying a minimalistic theme
  theme_minimal()







