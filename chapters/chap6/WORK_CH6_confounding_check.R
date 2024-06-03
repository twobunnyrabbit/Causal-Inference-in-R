
# Load necessary library
library(MatchIt)

# Synthetic Data Generation
set.seed(123) # For reproducibility

# Generate 'party_affiliation': 1 for Republican, 0 for Democrat
n <- 500 # Number of observations
prolife_policy <- data.frame(
  party_affiliation = rbinom(n, 1, 0.5), # Assume equal probability for simplicity
  other_covariates = rnorm(n) # Some other continuous covariates
)

# Treatment assignment influenced by 'party_affiliation' (confounder)
prolife_policy$treatment <- rbinom(n, 1, plogis(-0.5 + 0.8 * prolife_policy$party_affiliation))

# Outcome influenced by both treatment and 'party_affiliation'
prolife_policy$outcome <- rbinom(n, 1, plogis(-0.3 + 0.5 * prolife_policy$treatment + 0.7 * prolife_policy$party_affiliation))

# Convert 'party_affiliation' to factor for clarity in analysis
prolife_policy$party_affiliation <- factor(prolife_policy$party_affiliation, labels = c("Democrat", "Republican"))

# Estimate propensity scores
ps_model <- glm(treatment ~ party_affiliation + other_covariates, data = prolife_policy, family = "binomial")
prolife_policy$propensity_score <- predict(ps_model, type = "response")

# Analyze the variation in propensity scores
# Compare the average propensity scores between Republicans and Democrats
mean_score_republicans <- mean(prolife_policy$propensity_score[prolife_policy$party_affiliation == "Republican"])
mean_score_democrats <- mean(prolife_policy$propensity_score[prolife_policy$party_affiliation == "Democrat"])

# Output the mean scores for comparison
print(mean_score_republicans)
print(mean_score_democrats)

