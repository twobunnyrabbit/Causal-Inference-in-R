library(bnlearn)
library(pcalg)
library(Rgraphviz)

set.seed(123)
n <- 10000

data <- data.frame(
  age = rnorm(n, mean = 35, sd = 10)
)

data$income <- exp(log(10) + 0.03 * data$age + rnorm(n, 0, 0.5))
data$social_media_usage <- pmax(0, round(10 - 0.15 * data$age + rnorm(n, 0, 2)))
data$brand_perception <- 3 + 0.5 * scale(data$social_media_usage) + rnorm(n, 0, 0.5)
data$customer_satisfaction <- 5 + 2 * scale(log(data$income)) + rnorm(n, 0, 1)
data$purchase_intent <- factor(rbinom(n, 1, plogis(-2 + 0.5 * data$customer_satisfaction + 0.5 * data$brand_perception)))
data$product_awareness <- factor(rbinom(n, 1, plogis(-1 + 0.3 * data$social_media_usage)))
data$repurchase_rate <- factor(rbinom(n, 1, plogis(-3 + 0.5 * data$customer_satisfaction)))


data$social_media_usage <- as.numeric(data$social_media_usage)
cor_matrix <- cor(data[sapply(data, is.numeric)])
print("Correlation matrix of numeric variables:")
print(cor_matrix)


print("\nLearning network structure using Semi-Interleaved HITON-PC:")
si_hiton_pc_result <- si.hiton.pc(data)
print(arcs(si_hiton_pc_result))



## score based method
print("\nLearning network structure using original mixed data:")
mixed_hc_result <- hc(data)
print(arcs(mixed_hc_result))


## hybrid method in R
data_discrete_more_bins <- as.data.frame(lapply(data, function(x) {
  if(is.numeric(x)) {
    cut(x, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
  } else {
    x
  }
}))

print("\nLearning network structure using data with more bins:")
hc_result_more_bins <- hc(data_discrete_more_bins)
print(arcs(hc_result_more_bins))



# Function to plot a styled DAG with improved legibility
plot_graph <- function(graph, title, label_cex = 1) {
  if (length(arcs(graph)) > 0) {
    # Convert bnlearn object to igraph object
    ig <- bnlearn::as.igraph(graph)
    
    # Create a named vector for node labels (modify as needed)
    node_labels <- V(ig)$name
    
    # Set layout
    layout <- layout_with_fr(ig)
    
    # Set node attributes for better visibility and consistency
    V(ig)$color <- "lightblue"  # Soft background color for nodes
    V(ig)$frame.color <- "darkblue"  # Darker border around nodes for contrast
    V(ig)$size <- 15  # Set node size for better visibility
    V(ig)$label.color <- "darkblue"  # Use dark color for text to improve contrast
    V(ig)$label.cex <- label_cex  # Adjust label font size for legibility
    V(ig)$label.font <- 2  # Make labels bold
    V(ig)$label.dist <- 2.5  # Move labels below the nodes
    
    # Set edge attributes for better visibility
    E(ig)$color <- "gray30"  # Neutral color for edges
    E(ig)$arrow.size <- 0.1  # Arrow size for clarity
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
  } else {
    cat("No edges to plot for", title, "\n")
  }
}

# Plot the graphs with consistent styling
par(mfrow = c(1, 3))  # Adjust layout for multiple plots in one row

# Plot each graph with the improved style and title
plot_graph(mixed_hc_result, "Hill-Climbing (Mixed Data)", label_cex = 1.2)
plot_graph(hc_result_more_bins, "Hill-Climbing (More Bins)", label_cex = 1.2)
plot_graph(si_hiton_pc_result, "Semi-Interleaved HITON-PC", label_cex = 1.2)


##check edges
cat("\nMarkov Blankets for each variable (Semi-Interleaved HITON-PC):\n")
for (var in names(data)) {
  mb <- mb(si_hiton_pc_result, node = var)
  cat(var, ": ", paste(mb, collapse = ", "), "\n")
}

check_edges <- function(network, name) {
  if(length(arcs(network)) > 0) {
    cat("\nEdges found in", name, ":\n")
    print(arcs(network))
  } else {
    cat("\nNo edges found in", name, "\n")
  }
}

check_edges(mixed_hc_result, "Hill-Climbing (Mixed Data)")
check_edges(hc_result_more_bins, "Hill-Climbing (More Bins)")
check_edges(si_hiton_pc_result, "Semi-Interleaved HITON-PC")




