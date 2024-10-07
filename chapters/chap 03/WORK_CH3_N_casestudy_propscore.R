
# Install necessary packages if you haven't already
install.packages("MatchIt")
install.packages("dplyr")
install.packages("ggplot2")

# Load the libraries
library(MatchIt)
library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv(file = "./data/ecls.csv")
data


# Remove the any columns to anonymize the data
data_anonymized <- data[, !(names(data) %in% c("childid"))]
# Summary statistics for numeric columns 

summary_statistics <- data_anonymized %>% 
  
  reframe(across(where(is.numeric), ~ list(mean = mean(.x, na.rm = TRUE),  
                                             
                                             sd = sd(.x, na.rm = TRUE), 
                                             
                                             min = min(.x, na.rm = TRUE), 
                                             
                                             max = max(.x, na.rm = TRUE)))) 
summary_statistics

# Counts  'catholic' columns 
catholic_counts <- table(data_anonymized$catholic) 
catholic_counts


# Histograms for numeric variables: Age of mother and father, household income, and child's math score 

par(mfrow = c(2, 2)) 



# Mother's age distribution 

ggplot(data_anonymized, aes(x = p5hmage)) + 
  
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + 
  
  labs(title = "Distribution of Mother's Age", x = "Mother's Age", y = "Frequency") + 
  
  theme_minimal() 



# Father's age distribution 

ggplot(data_anonymized, aes(x = p5hdage)) + 
  
  geom_histogram(binwidth = 1, fill = "gold", color = "black") + 
  
  labs(title = "Distribution of Father's Age", x = "Father's Age", y = "Frequency") + 
  
  theme_minimal() 




# Household income distribution 

ggplot(data_anonymized, aes(x = w3income)) + 
  
  geom_histogram(fill = "lightgreen", color = "black") + 
  
  labs(title = "Distribution of Household Income", x = "Household Income", y = "Frequency") + 
  
  theme_minimal() 



# Child's math score distribution 

ggplot(data_anonymized, aes(x = c5r2mtsc)) + 
  
  geom_histogram(fill = "salmon", color = "black") + 
  
  labs(title = "Distribution of Child's Math Score", x = "Math Score", y = "Frequency") + 
  
  theme_minimal() 



# Catholic distribution 

ggplot(data_anonymized, aes(x = factor(catholic))) + 
  
  geom_bar(fill = "lightcoral") + 
  
  labs(title = "Distribution of Catholic", x = "Catholic (0 = No, 1 = Yes)", y = "Count") + 
  
  theme_minimal() 


# View the first few rows of the anonymized dataframe
head(data_anonymized)


data_anonymized %>%
  group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = mean(c5r2mtsc_std, na.rm = TRUE),
            std_error = sd(c5r2mtsc_std, na.rm = TRUE) / sqrt(n_students))


pre_treatment_covariates <- c('w3income', 'p5numpla', 'w3momed_hsb')
data_anonymized %>%
  group_by(catholic) %>%
  summarise_at(vars(one_of(pre_treatment_covariates)), ~mean(.x, na.rm = TRUE))

# build the model and update prop score
model_ps <- glm(catholic ~  w3income + p5numpla + w3momed_hsb, family = binomial(), data = data_anonymized)
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
