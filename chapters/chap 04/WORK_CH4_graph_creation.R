
setwd("path to your work directory")

library(dagitty)
library(lavaan)

# load the data set
social_media_data = read.csv("./data/social_media_data.csv")

# Define the causal graph
graph <- dagitty("dag {
  userA_numfollowers -> num_post_likes
  userA_numfollowers -> num_post_commented
  userA_numposts -> num_post_likes
  userA_numposts -> num_post_commented
  interests -> num_post_likes
  interests -> num_post_commented
  hours_active_perday -> num_post_likes
  hours_active_perday -> num_post_commented
}")

# Plot the graph
plot(graph)

# Prepare the data for analysis
vars_in_model <- c("userA_numfollowers", "userA_numposts", "interests", "hours_active_perday", "num_post_likes", "num_post_commented")
corr <- lavCor(social_media_data[vars_in_model])

# Perform local tests using the corrected covariance matrix
local_test_results <- localTests(graph, sample.cov = corr, sample.nobs = nrow(social_media_data))
plotLocalTestResults(local_test_results)
local_test_results
