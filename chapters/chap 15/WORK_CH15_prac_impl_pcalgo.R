library(pcalg)
library(bnlearn)
library(causaleffect)
library(igraph)

# Create a complex dataset
set.seed(400)
n <- 1000

data <- data.frame(
  age = sample(18:60, n, replace = TRUE),
  education = sample(0:5, n, replace = TRUE),  # 0: No education, 1: Primary, 2: Secondary, 3: High school, 4: University, 5: Postgraduate
  employed = sample(c(0,1), n, replace = TRUE, prob = c(0.3, 0.7)),  # 0: Unemployed, 1: Employed
  income = round(rlnorm(n, meanlog = 7, sdlog = 1)),  # Monthly income in thousands of Colombian Pesos
  dependents = sample(0:5, n, replace = TRUE, prob = c(0.2, 0.3, 0.2, 0.15, 0.1, 0.05)),
  domestic_violence = sample(c(0,1), n, replace = TRUE, prob = c(0.7, 0.3)),  # 0: No history, 1: History of domestic violence
  region = sample(c("Urban", "Rural"), n, replace = TRUE, prob = c(0.6, 0.4)),
  organized_crime = sample(1:5, n, replace = TRUE),  # Scale of 1-5, 5 being highest presence
  local_unemployment = round(runif(n, 5, 25), 1),  # Percentage
  social_services = sample(1:5, n, replace = TRUE)  # Scale of 1-5, 5 being highest access
)

# Calculate probability of involvement based on the factors
prob_involvement <- with(data, 
                         (age - 30)^2 / 1000 - 
                           0.1 * education + 
                           0.2 * (1 - employed) + 
                           (7000 - income) / 10000 + 
                           0.05 * dependents + 
                           0.2 * domestic_violence + 
                           0.1 * (region == "Rural") + 
                           0.1 * organized_crime + 
                           0.01 * local_unemployment - 
                           0.1 * social_services
)

# Normalize probabilities
prob_involvement <- (prob_involvement - min(prob_involvement)) / (max(prob_involvement) - min(prob_involvement))

# Assign involvement status
data$involved <- rbinom(n, 1, prob_involvement)

# Convert categorical variables to numeric
data$region <- as.numeric(factor(data$region))

# Perform causal discovery
suffStat <- list(C = cor(data), n = n)
pc.fit <- pc(suffStat, indepTest = gaussCItest, p = ncol(data), alpha = 0.05)

# Plot the result
plot_graph <- function(g) {
  # Convert pcalg graph to igraph object
  ig <- igraph::graph_from_graphnel(g)
  
  # Create a named vector for node labels
  node_labels <- c("Age", "Education", "Employed", "Income", "Dependents", 
                   "Domestic Violence", "Region", "Organized Crime", 
                   "Local Unemployment", "Social Services", "Involved")
  names(node_labels) <- V(ig)$name
  
  # Set layout
  layout <- layout_with_fr(ig)
  
  # Set node and edge attributes
  V(ig)$color <- rainbow(length(V(ig)))
  V(ig)$size <- 30
  V(ig)$label.color <- "black"
  V(ig)$label.cex <- 0.8
  E(ig)$arrow.size <- 0.3
  
  # Plot the graph
  plot(ig, 
       layout = layout,
       vertex.label = node_labels[V(ig)$name],
       edge.arrow.mode = ">",
       main = "Causal Graph: Women's Involvement in Drug Trafficking",
       sub = "Estimated by PC Algorithm",
       margin = c(0,0,0,0))
}

# Call the function to plot the graph
plot_graph(pc.fit@graph)

# Print summary of the causal graph
summary(pc.fit)