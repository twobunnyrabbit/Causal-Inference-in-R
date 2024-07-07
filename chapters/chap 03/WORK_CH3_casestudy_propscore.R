setwd("/Users/subhajitdas/Documents/SUBHO_FILES/10_BOOK_WRITING/00_CAUSAL_INFERENCE/R")

# Install necessary packages if you haven't already
install.packages("MatchIt")
install.packages("dplyr")
install.packages("ggplot2")

# Load the libraries
library(MatchIt)
library(dplyr)
library(ggplot2)

data <- read.csv('./CHAP3/data/ecls.csv')
data

# Assuming 'data' is your loaded dataframe
# Remove the any columns to anonymize the data
data_anonymized <- data[, !(names(data) %in% c("childid"))]
data_anonymized <- data[, !(names(data) %in% c("childid", "race", "p5hmage"))]

# View the first few rows of the anonymized dataframe
head(data_anonymized)


data_anonymized %>%
  group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = mean(c5r2mtsc_std, na.rm = TRUE),
            std_error = sd(c5r2mtsc_std, na.rm = TRUE) / sqrt(n_students))


pre_treatment_covariates <- c('race_white', 'w3income', 'p5numpla', 'w3momed_hsb')
data_anonymized %>%
  group_by(catholic) %>%
  summarise_at(vars(one_of(pre_treatment_covariates)), ~mean(.x, na.rm = TRUE))

# build the model and update prop score
model_ps <- glm(catholic ~ race_white + w3income + p5numpla + w3momed_hsb, family = binomial(), data = data_anonymized)
data_anonymized$pscore <- predict(model_ps, newdata = data_anonymized, type = "response")

#plot the results as a histogram
ggplot(data_anonymized, aes(x = pscore, fill = as.factor(catholic))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(x = "Propensity Score", y = "Frequency", fill = "Group") +
  theme_minimal()

# Remove rows with any NA values in the specified covariates
data_cleaned <- data_anonymized %>%
  filter(complete.cases(catholic, race_white, w3income, p5numpla, w3momed_hsb))

# Further remove rows with non-finite values in numeric covariates
numeric_covariates <- c( "w3income", "p5numpla")
data_cleaned <- data_cleaned %>%
  filter(sapply(data_cleaned[numeric_covariates], is.finite) %>% rowSums() == length(numeric_covariates))

# run matchit() on data_cleaned
mod_match <- matchit(catholic ~ race_white + w3income + p5numpla + w3momed_hsb, 
                     method = "nearest", data = data_cleaned) # or data_imputed

# Using the MatchIt package for visualization
plot(mod_match, type = "jitter")
plot(mod_match, type = "hist")

matched_data <- match.data(mod_match)
matched_data %>%
  group_by(catholic) %>%
  summarise_at(vars(one_of(pre_treatment_covariates)), ~mean(.x, na.rm = TRUE))

with(matched_data, t.test(c5r2mtsc_std ~ catholic))

# You can also use a linear model for a more detailed analysis
lm_effect <- lm(c5r2mtsc_std ~ catholic, data = matched_data)
summary(lm_effect)
