# Cluster Analysis ----------------------------------------------------------

library(cluster)
library(dendextend)
library(factoextra)
library(ggplot2)
library(NbClust)

# Import data ---------------------------------------------------------------

data_path <- file.path("data", "CLUSFULL.csv")
if (!file.exists(data_path)) {
  stop("Private dataset not included. Expected file: data/CLUSFULL.csv")
}

cluster_data <- read.csv(data_path, header = TRUE)

analysis_data <- cluster_data[
  , c("Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6")
]
analysis_data <- na.omit(analysis_data)

# Inspect the first two dimensions -----------------------------------------

ggplot(analysis_data, aes(x = Factor1, y = Factor2)) +
  geom_point() +
  theme_minimal(base_size = 12)

# Hierarchical clustering using Euclidean distance and Ward's method --------

set.seed(330)
distance_matrix <- dist(analysis_data, method = "euclidean")
hierarchical_fit <- hclust(distance_matrix, method = "ward.D2")

plot(hierarchical_fit, hang = -1)
barplot(hierarchical_fit$height, las = 2)

cluster_membership <- cutree(hierarchical_fit, k = 3)
table(cluster_membership)

coloured_tree <- color_branches(
  as.dendrogram(hierarchical_fit),
  k = 3
)
plot(coloured_tree)
rect.hclust(hierarchical_fit, k = 3, border = "blue")

# Visualize the three-cluster solution -------------------------------------

plot_data <- analysis_data
plot_data$cluster <- factor(cluster_membership)

ggplot(plot_data, aes(x = Factor1, y = Factor2, colour = cluster)) +
  geom_point() +
  theme_minimal(base_size = 12)

# Evaluate cluster separation ----------------------------------------------

silhouette_results <- silhouette(cluster_membership, distance_matrix)
fviz_silhouette(silhouette_results)

# Evaluate the number of clusters using multiple indices -------------------

cluster_number_results <- NbClust(
  analysis_data,
  diss = NULL,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 5,
  method = "ward.D2",
  index = "all"
)

fviz_nbclust(cluster_number_results)

# K-means comparison retained from the original workflow -------------------

set.seed(330)
kmeans_two <- kmeans(
  analysis_data,
  centers = 2,
  iter.max = 10,
  nstart = 1,
  algorithm = "Lloyd"
)

kmeans_three <- kmeans(
  analysis_data,
  centers = 3,
  iter.max = 100,
  nstart = 25
)

print(kmeans_two)
print(kmeans_three)
