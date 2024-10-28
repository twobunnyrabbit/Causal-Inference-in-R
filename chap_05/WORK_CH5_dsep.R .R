set.seed(42) # Ensure reproducibility
n <- 3000 # Number of observations

# Generate synthetic data
Parents_Income <- rnorm(n, mean=50000, sd=10000) # Parent's Income (X)
Childs_Academic_Performance <- 0.5 * Parents_Income + rnorm(n, mean=0, sd=5000) # Child's Performance (Y)

# Amount Spent on Toys (Z) depends only on Child's Academic Performance
Amount_Spent_on_Toys <- 200 + 0.1 * Childs_Academic_Performance + rnorm(n, mean=0, sd=500)

data <- data.frame(Parents_Income, Childs_Academic_Performance, Amount_Spent_on_Toys)

# Install necessary packages if not already installed
if (!requireNamespace("ggm", quietly = TRUE)) install.packages("ggm")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

# Load the required packages
library(ggm)
library(dplyr)

# Specify the DAG structure
dag <- matrix(c(0, 1, 0,   # Row 1: Parent's Income to Child's Performance
                0, 0, 1,   # Row 2: Child's Performance to Amount Spent
                0, 0, 0),  # Row 3: Amount Spent does not cause any of the others
              nrow = 3, byrow = TRUE,
              dimnames = list(c("X", "Y", "Z"), c("X", "Y", "Z")))

# Check d-separation between X and Z, conditioned on Y
dsep_result <- dSep(dag, first = "X", second = "Z", cond = "Y")
print(paste("D-separation result (X and Z given Y):", dsep_result))

# Calculate correlation between X and Z (unconditional)
cor_xz <- cor(data$Parents_Income, data$Amount_Spent_on_Toys)
print(paste("Unconditional correlation between X and Z:", round(cor_xz, 4)))

# Function to calculate partial correlation
partial_cor <- function(x, y, z) {
  res_xz <- residuals(lm(x ~ z))
  res_yz <- residuals(lm(y ~ z))
  return(cor(res_xz, res_yz))
}

# Calculate partial correlation between X and Z, given Y
partial_cor_xz_y <- partial_cor(data$Parents_Income, data$Amount_Spent_on_Toys, data$Childs_Academic_Performance)
print(paste("Partial correlation between X and Z, given Y:", round(partial_cor_xz_y, 4)))

# Demonstrate d-separation by stratifying on Y
data_stratified <- data %>%
  mutate(Y_strata = cut(Childs_Academic_Performance, breaks = 3)) %>%
  group_by(Y_strata) %>%
  summarise(cor_XZ = cor(Parents_Income, Amount_Spent_on_Toys))

print("Correlations between X and Z within strata of Y:")
print(data_stratified)

# Test for conditional independence
lm_model <- lm(Amount_Spent_on_Toys ~ Parents_Income + Childs_Academic_Performance, data = data)
summary_model <- summary(lm_model)
print("Regression results:")
print(summary_model$coefficients)