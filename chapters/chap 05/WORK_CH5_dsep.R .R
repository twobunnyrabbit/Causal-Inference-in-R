
set.seed(42) # Ensure reproducibility
n <- 3000 # Number of observations

# Generate synthetic data
Parents_Income <- rnorm(n, mean=50000, sd=10000) # Parent's Income (X)
Childs_Academic_Performance <- rnorm(n, mean=70, sd=10) # Child's Performance (Y)

# Assuming a simplistic linear relationship for the Amount Spent on Toys (Z)
Amount_Spent_on_Toys <- 200 + 0.01 * Parents_Income + 5 * Childs_Academic_Performance + rnorm(n, mean=0, sd=50)

data <- data.frame(Parents_Income, Childs_Academic_Performance, Amount_Spent_on_Toys)

## check for d sep
# Install necessary packages if not already installed
if (!requireNamespace("ggm", quietly = TRUE)) install.packages("ggm")

# Load the ggm package for d-separation functions
library(ggm)

# Assuming we have a DAG model, let's denote it as follows for our analysis:
# Specify the DAG structure
dag <- matrix(c(0, 1, 1,   # Row 1: Parent's Income to Child's Performance and Amount Spent
                0, 0, 1,   # Row 2: Child's Performance to Amount Spent
                0, 0, 0),  # Row 3: Amount Spent does not cause any of the others
              nrow = 3, byrow = TRUE,
              dimnames = list(c("X", "Y", "Z"), c("X", "Y", "Z")))

# Check d-separation between X and Z, conditioned on Y
dSep(dag, first = "X", second = "Z", cond = "Y") 


