# Install and load necessary packages
if (!requireNamespace("EValue", quietly = TRUE)) install.packages("EValue")
library(EValue)

# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 500

# Generate synthetic data
data <- data.frame(
  comic_freq = sample(1:5, n, replace = TRUE), # 1: Rarely, 5: Very Frequently
  parents_edu = sample(c('Low', 'Medium', 'High'), n, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  homework_time = rnorm(n, mean = 2, sd = 0.5), # Hours per day
  higher_studies = sample(c(0, 1), n, replace = TRUE) # 0: No, 1: Yes
)

# Fit a logistic regression model
model <- glm(higher_studies ~ comic_freq + parents_edu + homework_time, 
             data = data, family = binomial)

# Print model summary
summary(model)

# Extract odds ratio and confidence interval for comic_freq
or <- exp(coef(model)["comic_freq"])
ci <- exp(confint(model)["comic_freq",])

# Print odds ratio and confidence interval
cat("Odds Ratio for comic_freq:", or, "\n")
cat("95% Confidence Interval:", ci[1], "-", ci[2], "\n")

# Calculate the prevalence of the outcome
outcome_prevalence <- mean(data$higher_studies)

# Function to calculate E-value
calculate_evalue <- function(estimate) {
  if (estimate < 1) estimate <- 1 / estimate
  return(estimate + sqrt(estimate * (estimate - 1)))
}

# Calculate E-values
evalue_point <- calculate_evalue(or)
evalue_lower <- calculate_evalue(ci[1])

# Print E-values
cat("E-value (point estimate):", evalue_point, "\n")
cat("E-value (lower confidence limit):", evalue_lower, "\n")

# Create data for plotting
conf_strengths <- seq(1, max(evalue_point, evalue_lower) + 0.5, by = 0.1)
bias_factors <- sapply(conf_strengths, function(x) x + sqrt(x * (x-1)))

# Plot
plot(conf_strengths, bias_factors, type = "l", 
     xlab = "Confounder-Outcome Relative Risk", 
     ylab = "E-value", 
     main = "Sensitivity Analysis Plot")
abline(h = or, col = "red", lty = 2)
text(1, or, "Observed OR", pos = 3, col = "red")
abline(h = evalue_point, col = "blue", lty = 2)
text(1, evalue_point, "E-value (point estimate)", pos = 3, col = "blue")
abline(h = evalue_lower, col = "green", lty = 2)
text(1, evalue_lower, "E-value (lower CI)", pos = 3, col = "green")