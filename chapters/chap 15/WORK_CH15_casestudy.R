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


#visualize causal relation
plot_graph <- function(graph, title) {
  if(length(arcs(graph)) > 0) {
    plot(graph, main = title)
  } else {
    cat("No edges to plot for", title, "\n")
  }
}

par(mfrow = c(1, 1))
plot_graph(mixed_hc_result, "Hill-Climbing (Mixed Data)")
plot_graph(hc_result_more_bins, "Hill-Climbing (More Bins)")
plot_graph(si_hiton_pc_result, "Semi-Interleaved HITON-PC")


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




