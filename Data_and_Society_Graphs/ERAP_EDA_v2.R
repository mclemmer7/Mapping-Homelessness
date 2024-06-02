# Install and load necessary packages
packages <- c("tidyverse", "sf", "ggplot2", "jsonlite", "cluster", "factoextra", "plotly")

installed_packages <- packages %in% installed.packages()
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages])
}

# Load the libraries
library(tidyverse)
library(sf)
library(ggplot2)
library(jsonlite)
library(cluster)
library(factoextra)
library(plotly)

# Load the data
wa_eviction_erap <- st_read("wa_eviction_erap.geojson")

# Ensure the relevant columns are numeric and handle NA values
wa_eviction_erap <- wa_eviction_erap |>
  mutate(
    mean_index_value = as.numeric(mean_index_value),
    mean_monthly_housing_cost = as.numeric(mean_monthly_housing_cost),
    eviction.rate = as.numeric(eviction.rate),
    poverty.rate = as.numeric(poverty.rate),
    median.gross.rent = as.numeric(median.gross.rent),
    median.household.income = as.numeric(median.household.income),
    pct.white = as.numeric(pct.white),
    pct.af.am = as.numeric(pct.af.am),
    pct.hispanic = as.numeric(pct.hispanic),
    pct.am.ind = as.numeric(pct.am.ind),
    pct.asian = as.numeric(pct.asian)
  ) |>
  drop_na(mean_index_value, mean_monthly_housing_cost, eviction.rate, poverty.rate, median.gross.rent, median.household.income, pct.white, pct.af.am, pct.hispanic, pct.am.ind, pct.asian)

# Prepare data for clustering (remove geometry)
clustering_data <- wa_eviction_erap |>
  select(mean_index_value, mean_monthly_housing_cost, eviction.rate, poverty.rate, median.gross.rent, median.household.income, pct.white, pct.af.am, pct.hispanic, pct.am.ind, pct.asian) |>
  st_set_geometry(NULL)  # Remove geometry for clustering

# Perform hierarchical clustering
dist_matrix <- dist(clustering_data)  # Compute distance matrix
hc <- hclust(dist_matrix, method = "ward.D2")  # Hierarchical clustering using Ward's method

# Plot the dendrogram with labels
fviz_dend(hc, k = 3,  # Cut the dendrogram to create 3 clusters
          rect = TRUE,  # Add rectangle around clusters
          rect_fill = TRUE,  # Fill the rectangle
          rect_border = "jco",  # Color of the border
          show_labels = TRUE,  # Show labels
          labels_track_height = 8,  # Adjust the height of labels
          main = "Hierarchical Clustering Dendrogram",
          xlab = "Counties",
          ylab = "Height") +
  theme_minimal()

# Perform PCA
pca_data <- wa_eviction_erap |>
  select(mean_index_value, mean_monthly_housing_cost, eviction.rate, poverty.rate, median.gross.rent, median.household.income, pct.white, pct.af.am, pct.hispanic, pct.am.ind, pct.asian) |>
  st_set_geometry(NULL)  # Remove geometry for PCA

pca_result <- prcomp(pca_data, scale. = TRUE)

# Visualize PCA results with clusters
pca_clusters <- cutree(hc, k = 3)
pca_data <- as.data.frame(pca_result$x)
pca_data$cluster <- factor(pca_clusters)
pca_data$county_name <- wa_eviction_erap$county_name

# Plot PCA with clusters
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster, text = county_name)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "PCA Plot with Clusters", x = "Principal Component 1", y = "Principal Component 2")

# Convert ggplot to plotly for interactive tooltips
pca_plotly <- ggplotly(pca_plot, tooltip = c("text"))

# Show the plotly plot
pca_plotly
