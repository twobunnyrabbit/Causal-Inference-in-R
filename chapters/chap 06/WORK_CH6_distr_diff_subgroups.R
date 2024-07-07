# Function to model and visualize propensity score distributions
# Simulate confounders - demographic/socio-economic factors
# H1 might represent age, H2 could be a binary variable like urban/rural

H1 <- rnorm(1000)
H2 <- rbinom(n = 1000, size = 1, prob = 0.3) 

# Assuming these confounders influence the propensity to vote for a pro-life policy
e <- exp(H1 + 3 * H2 - 1.5) / (1 + exp(H1 + 3 * H2 - 1.5))
# Simulating voting behavior based on the propensity
# Republicans (T=1) more likely to vote for pro-life
# Democrats (T=0) less likely
T <- rbinom(n = 1000, size = 1, prob = e)
# Fit a propensity score model - logistic regression
e <- fitted(glm(T ~ H1 + H2, family = binomial))

# Plotting the density of propensity scores for both groups
plot(c(0, 1),
     range(density(e[T == 1], bw = .05)$y, density(e[T == 0], bw = .05)$y),
     type = "n",
     xlab = "Propensity Score",
     ylab = "Density",
     main = "Propensity Score Distribution: Republicans vs Democrats")

# Add density lines for each group
lines(density(e[T == 1], bw = .05), lty = 1, col = "blue") # Republicans
lines(density(e[T == 0], bw = .05), lty = 2, col = "red") # Democrats
# Add a legend for clarity
legend("topright", c("Republicans", "Democrats"), lty = c(1, 2), col = c("blue", "red"))
