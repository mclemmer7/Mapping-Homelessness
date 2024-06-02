# EDA_V3

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

# Remove "County" from county_name and rename the column to WA_counties
wa_eviction_erap <- wa_eviction_erap %>%
  mutate(WA_counties = str_replace(county_name, " County", "")) %>%
  select(-county_name)

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
          main = "Hierarchical Clustering Dendrogram",
          xlab = "Counties",
          ylab = "Height",
          cex = 0.6,  # Adjust the size of labels
          k_colors = c("red", "blue", "green")) +
  theme_minimal() +
  geom_text(data = data.frame(label = wa_eviction_erap$WA_counties, x = 1:nrow(wa_eviction_erap), y = rep(-2, nrow(wa_eviction_erap))), 
            aes(x = x, y = y, label = label), size = 3, hjust = 1, angle = 90, check_overlap = TRUE)

# Perform PCA
pca_data <- wa_eviction_erap |>
  select(mean_index_value, mean_monthly_housing_cost, eviction.rate, poverty.rate, median.gross.rent, median.household.income, pct.white, pct.af.am, pct.hispanic, pct.am.ind, pct.asian) |>
  st_set_geometry(NULL)  # Remove geometry for PCA

pca_result <- prcomp(pca_data, scale. = TRUE)

# Create a data frame for PCA results
pca_data <- as.data.frame(pca_result$x)
pca_data$cluster <- factor(cutree(hc, k = 3))
pca_data$WA_counties <- wa_eviction_erap$WA_counties

# PCA biplot with loadings
pca_biplot <- fviz_pca_biplot(pca_result,
                              geom.ind = "point",
                              col.ind = pca_data$cluster,
                              palette = c("red", "blue", "green"),
                              addEllipses = TRUE,
                              label = "var",
                              repel = TRUE,
                              pointshape = 21,
                              pointsize = 2.5,
                              arrowsize = 0.5) +
  theme_minimal()

# Convert PCA biplot to Plotly for interactive tooltips
pca_biplotly <- ggplotly(pca_biplot)

# Add county names to the tooltip
for (i in 1:length(pca_biplotly$x$data)) {
  if (pca_biplotly$x$data[[i]]$name == "point") {
    point_indices <- as.numeric(pca_biplotly$x$data[[i]]$x)
    tooltip_texts <- paste("County:", pca_data$WA_counties[point_indices],
                           "<br>PC1:", round(pca_data$PC1[point_indices], 2),
                           "<br>PC2:", round(pca_data$PC2[point_indices], 2))
    pca_biplotly$x$data[[i]]$text <- tooltip_texts
  } 
}

# Show the Plotly plot
pca_biplotly
