

set.seed(100) # For reproducibility
n <- 3000 # Number of families

# Generating independent variables
Income_Level <- rnorm(n, mean=70, sd=15) # X1
Frugality <- rnorm(n, mean=50, sd=10) # X3

# Generating the collider variable influenced by both X1 and X3
Amount_Spent <- 0.5*Income_Level - 0.3*Frugality + rnorm(n, mean=50, sd=20) # X2

# Combine into a data frame
data <- data.frame(Income_Level, Frugality, Amount_Spent)

# Without conditioning on the collider
cor.test(data$Income_Level, data$Frugality) # will give a number between 0 and 1

# ------------
# Conditioning on the collider (Amount_Spent)
library(dplyr)

moderate_spenders <- data %>% 
  filter(Amount_Spent > quantile(Amount_Spent, 0.2) & Amount_Spent < quantile(Amount_Spent, 0.6))

cor.test(moderate_spenders$Income_Level, moderate_spenders$Frugality)

## --------------
# IMMORALITY

# Generating a new variable X4 influenced by both X1 and X3
Gift_Purchase_Satisfaction <- 0.4*Income_Level + 0.2*Frugality + rnorm(n, mean=60, sd=15)
data$Gift_Purchase_Satisfaction <- Gift_Purchase_Satisfaction

# Checking correlation between X1 and X3 without conditioning on X4
cor.test(data$Income_Level, data$Frugality)

## IMMORTAL CHECK
# Checking correlation between X1 and X3 with conditioning on X4
satisfied_customers <- data %>% 
  filter(Gift_Purchase_Satisfaction > quantile(Gift_Purchase_Satisfaction, 0.5))

cor.test(satisfied_customers$Income_Level, satisfied_customers$Frugality)

