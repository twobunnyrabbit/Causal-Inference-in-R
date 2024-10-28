# Example of an edge list
edge_list <- matrix(c("A", "B", "B", "C", "C", "A"),
                    ncol = 2, byrow = TRUE)
# Printing the edge list
print(edge_list)


# Example of a weighted edge list
weighted_edge_list <- matrix(c(
  "A", "B", 2, "B", "C", 3, "C", "A", 1),
  ncol = 3, byrow = TRUE)
# Printing the weighted edge list
print(weighted_edge_list)


# Example of an adjacency matrix
adj_matrix <- matrix(c(0, 1, 1,
                       1, 0, 1,
                       1, 1, 0), nrow = 3, byrow = TRUE)
# Setting row and column names for clarity
rownames(adj_matrix) <- colnames(adj_matrix) <- c("A", "B", "C")
# Printing the adjacency matrix
print(adj_matrix)



# Install and load igraph
if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}
# Create a graph using an edge list
graph <- graph_from_edgelist(edge_list, directed = FALSE)
# Plot the graph
plot(graph)