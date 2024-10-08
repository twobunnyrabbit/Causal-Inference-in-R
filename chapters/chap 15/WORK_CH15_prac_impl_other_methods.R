setwd("/Users/subhajitdas/Documents/SUBHO_FILES/10_BOOK_WRITING/00_CAUSAL_INFERENCE/R")

# Load required libraries
library(bnlearn)
library(Rgraphviz)
library(causaleffect)
library(igraph)

# Set seed for reproducibility
set.seed(400)

# Generate data
n <- 1000

data <- data.frame(
  age = as.numeric(sample(18:60, n, replace = TRUE)),
  education = as.numeric(sample(0:5, n, replace = TRUE)),
  employed = factor(sample(c(0,1), n, replace = TRUE, prob = c(0.3, 0.7))),
  income = as.numeric(round(rlnorm(n, meanlog = 7, sdlog = 1))),
  dependents = as.numeric(sample(0:5, n, replace = TRUE, prob = c(0.2, 0.3, 0.2, 0.15, 0.1, 0.05))),
  domestic_violence = factor(sample(c(0,1), n, replace = TRUE, prob = c(0.7, 0.3))),
  region = factor(sample(c("Urban", "Rural"), n, replace = TRUE, prob = c(0.6, 0.4))),
  organized_crime = as.numeric(sample(1:5, n, replace = TRUE)),
  local_unemployment = as.numeric(round(runif(n, 5, 25), 1)),
  social_services = as.numeric(sample(1:5, n, replace = TRUE))
)

# Calculate probability of involvement
prob_involvement <- with(data, 
                         (age - 30)^2 / 1000 - 
                           0.1 * education + 
                           0.2 * (as.numeric(employed) - 1) + 
                           (7000 - income) / 10000 + 
                           0.05 * dependents + 
                           0.2 * (as.numeric(domestic_violence) - 1) + 
                           0.1 * (as.numeric(region == "Rural")) + 
                           0.1 * organized_crime + 
                           0.01 * local_unemployment - 
                           0.1 * social_services
)

# Normalize probabilities
prob_involvement <- (prob_involvement - min(prob_involvement)) / (max(prob_involvement) - min(prob_involvement))

# Assign involvement status
data$involved <- factor(rbinom(n, 1, prob_involvement))

# Apply structure learning algorithms

# PC algorithm (constraint-based)
pc_dag <- pc.stable(data)

# MMHC algorithm (hybrid)
mmhc_dag <- mmhc(data)
# Function to plot a styled DAG
plot_styled_dag <- function(dag, title) {
  # Convert bnlearn object to igraph object
  ig <- bnlearn::as.igraph(dag)
  
  # Create a named vector for node labels (example nodes, modify as needed)
  node_labels <- c("Age", "Education", "Employed", "Income", "Dependents", 
                   "Domestic Violence", "Region", "Organized Crime",
                   "Local Unemployment", "Social Services", "Involved")
  names(node_labels) <- V(ig)$name
  
  # Set layout
  layout <- layout_with_fr(ig)
  
  # Set node attributes for better visibility and consistency
  V(ig)$color <- "lightblue"  # Soft background color for nodes
  V(ig)$frame.color <- "darkblue"  # Darker border around nodes for contrast
  V(ig)$size <- 20  # Set node size for better visibility
  V(ig)$label.color <- "darkblue"  # Use dark color for text to improve contrast
  V(ig)$label.cex <- 0.75  # Adjust label font size for legibility
  V(ig)$label.font <- 2  # Make labels bold
  V(ig)$label.dist <- 2.5  # Move labels below the nodes
  
  # Set edge attributes for better visibility
  E(ig)$color <- "gray30"  # Neutral color for edges
  E(ig)$arrow.size <- 0.25  # Arrow size for clarity
  E(ig)$width <- 1.2  # Thicker edges for better visibility
  
  # Plot the DAG with improved aesthetics
  plot(ig, 
       layout = layout, 
       vertex.label = node_labels[V(ig)$name], 
       vertex.label.dist = 2.5,  # Distance of labels from nodes
       vertex.label.degree = pi / 2,  # Position labels below nodes
       edge.arrow.mode = 1,  # Use arrows for directed edges
       main = title,
       margin = c(0.1, 0.1, 0.1, 0.1))  # Add margins for better spacing
}

# Apply the function to the PC and MMHC DAGs
# pdf("learned_dags.pdf", width = 12, height = 6)  # Save plots to a PDF file
par(mfrow = c(1, 2))  # Plot two graphs side by side

# PC Algorithm (Constraint-based)
plot_styled_dag(pc_dag, title = "PC Algorithm")

# MMHC Algorithm (Hybrid)
plot_styled_dag(mmhc_dag, title = "MMHC Algorithm")




# Print the edges of each learned DAG
cat("PC Algorithm Edges:\n")
print(pc_dag$arcs)

cat("\nMMHC Algorithm Edges:\n")
print(mmhc_dag$arcs)

# Convert bnlearn graphs to igraph objects
pc_dag_igraph <- igraph::graph_from_edgelist(as.matrix(pc_dag$arcs), directed = TRUE)
mmhc_dag_igraph <- igraph::graph_from_edgelist(as.matrix(mmhc_dag$arcs), directed = TRUE)

# Estimate causal effects
# We'll estimate the causal effect of 'education' on 'involved'

# Using the PC algorithm result
tryCatch({
  effect_pc <- causal.effect(y = "involved", x = "education", G = pc_dag_igraph)
  cat("\nCausal Effect (PC algorithm):\n")
  print(effect_pc)
}, error = function(e) {
  cat("\nError in calculating causal effect for PC algorithm:", conditionMessage(e), "\n")
})

# Using the MMHC algorithm result
tryCatch({
  effect_mmhc <- causal.effect(y = "involved", x = "education", G = mmhc_dag_igraph)
  cat("\nCausal Effect (MMHC algorithm):\n")
  print(effect_mmhc)
}, error = function(e) {
  cat("\nError in calculating causal effect for MMHC algorithm:", conditionMessage(e), "\n")
})




# Function to plot a styled bootstrapped DAG
plot_styled_bootstrap_dag <- function(dag, title, label_cex = 1) {
  # Convert bnlearn object to igraph object
  ig <- bnlearn::as.igraph(dag)
  
  # Create a named vector for node labels (modify as needed)
  node_labels <- V(ig)$name
  
  # Set layout
  layout <- layout_with_fr(ig)
  
  # Set node attributes for better visibility and consistency
  V(ig)$color <- "lightblue"  # Soft background color for nodes
  V(ig)$frame.color <- "darkblue"  # Darker border around nodes for contrast
  V(ig)$size <- 20  # Set node size for better visibility
  V(ig)$label.color <- "darkblue"  # Use dark color for text to improve contrast
  V(ig)$label.cex <- label_cex  # Adjust label font size for legibility
  V(ig)$label.font <- 2  # Make labels bold
  V(ig)$label.dist <- 2.5  # Move labels below the nodes
  
  # Set edge attributes for better visibility
  E(ig)$color <- "gray30"  # Neutral color for edges
  E(ig)$arrow.size <- 0.3  # Arrow size for clarity
  E(ig)$width <- 1  # Thicker edges for better visibility
  
  # Plot the DAG with improved aesthetics
  plot(ig, 
       layout = layout, 
       vertex.label = node_labels, 
       vertex.label.dist = 2.5,  # Distance of labels from nodes
       vertex.label.degree = pi / 2,  # Position labels below nodes
       edge.arrow.mode = 1,  # Use arrows for directed edges
       main = title,
       margin = c(0.1, 0.1, 0.1, 0.1))  # Add margins for better spacing
}

# Perform bootstrapping to assess the stability of the PC algorithm
boot_pc <- boot.strength(data, R = 200, algorithm = "pc.stable")

# Get the averaged network from the bootstrap results
avg_network <- averaged.network(boot_pc)

# Save the plot as a PDF and use the custom plotting function
plot_styled_bootstrap_dag(avg_network, title = "Bootstrapped PC Network", label_cex = 1.2)




cat("\nBootstrapped PC Network Edges (strength > 0.5):\n")
print(avg_network$arcs[avg_network$strength > 0.5, ])

# Perform a simple sensitivity analysis for unmeasured confounding
# This is a basic example and should be expanded for a real analysis
sensitivity_analysis <- function(effect, bias_range) {
  adjusted_effects <- effect + bias_range
  return(adjusted_effects)
}

# Check if effect_pc is available, otherwise use a default value
if(exists("effect_pc") && !is.null(effect_pc)) {
  effect_value <- as.numeric(effect_pc)
} else {
  effect_value <- 0  # default value if effect_pc is not available
  cat("\nWarning: Using default effect value of 0 for sensitivity analysis as effect_pc was not calculated.\n")
}

bias_range <- seq(-0.5, 0.5, by = 0.1)
sensitivity_results <- sensitivity_analysis(effect_value, bias_range)

pdf("sensitivity_analysis.pdf", width = 8, height = 6)
plot(bias_range, sensitivity_results, type = "l", 
     xlab = "Potential Unmeasured Confounding Bias", 
     ylab = "Adjusted Causal Effect",
     main = "Sensitivity Analysis for Causal Effect")
abline(h = 0, lty = 2, col = "red")
dev.off()

cat("\nSensitivity Analysis Results:\n")
print(data.frame(Bias = bias_range, AdjustedEffect = sensitivity_results))