install.packages("AER")
install.packages("lmtest")
library(AER)
library(lmtest)
set.seed(42) # Ensure reproducibility

# RELEVANCE CONDITION
n <- 100 # Number of observations
Z <- rnorm(n, mean=0, sd=1) # Instrumental variable: Z
T <- 0.5 * Z + rnorm(n, mean=0, sd=1) # Treatment variable: T, influenced by Z
Y <- 2 * T + rnorm(n, mean=0, sd=1) # Outcome variable: Y, influenced by T
# Combine into a data frame
data <- data.frame(Z, T, Y)

first_stage <- ivreg(T ~ Z | Z, data=data)
summary(first_stage)$fstatistic


# EXOGENEITY CONDITION
n <- 100 # Number of observations
Z1 <- rnorm(n) # First instrumental variable
Z2 <- rnorm(n) # Second instrumental variable
X <- 0.5 * Z1 + 0.3 * Z2 + rnorm(n) # Endogenous regressor, influenced by Z1 and Z2
Y <- 2 * X + rnorm(n) # Outcome variable, influenced by X

# Combine into a data frame
data <- data.frame(Y, X, Z1, Z2)

# Perform 2SLS regression
model <- ivreg(Y ~ X | Z1 + Z2, data=data)
# SARGAN TEST
sargan_test = summary(model, diagnostics = TRUE)


# Print the test results
print(sargan_test)

