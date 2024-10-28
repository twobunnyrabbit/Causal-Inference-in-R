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
# plot graph function
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
  
  # Set node attributes for better visibility and consistency
  V(ig)$color <- "lightblue"  # Soft background color for nodes
  V(ig)$frame.color <- "darkblue"  # Darker border around nodes for contrast
  V(ig)$size <- 20  # Increase node size for better visibility
  V(ig)$label.color <- "darkblue"  # Use dark color for text to improve contrast
  V(ig)$label.cex <- 1  # Increase label font size for better legibility
  V(ig)$label.font <- 0.2  # Make label font bold for emphasis
  V(ig)$label.dist <- -15.5  # Move labels below the nodes
  
  # Set edge attributes for better visibility
  E(ig)$color <- "gray30"  # Use a neutral color for edges
  E(ig)$arrow.size <- 0.5  # Increase arrow size for clarity
  E(ig)$width <- 2  # Thicker edges for better visibility
  
  # Plot the graph with improved aesthetics and labels below nodes
  plot(ig, 
       layout = layout, 
       vertex.label = node_labels[V(ig)$name], 
       vertex.label.dist = 2.5,  # Increase distance of labels from nodes
       vertex.label.degree = pi / 2,  # Position labels directly below nodes
       edge.arrow.mode = 1,  # Use arrows for directed edges
       main = "Causal Graph: Women's Involvement in Drug Trafficking",
       sub = "Estimated by PC Algorithm",
       margin = c(0.1, 0.1, 0.1, 0.1))  # Add margins for better spacing
}

# Call the function to plot the graph
plot_graph(pc.fit@graph)




# Print summary of the causal graph
summary(pc.fit)