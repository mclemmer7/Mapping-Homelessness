library(tidyverse)
library(sf)
library(ggplot2)
library(jsonlite)

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
    median.household.income = as.numeric(median.household.income)
  ) |>
  drop_na(mean_index_value, mean_monthly_housing_cost, eviction.rate, poverty.rate, median.gross.rent, median.household.income)

# Verify the data
str(wa_eviction_erap)
summary(wa_eviction_erap)

# Create a map for the Greater Seattle area
ggplot(data = wa_eviction_erap) +
  geom_sf(aes(fill = eviction.rate)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", name = "Eviction Rate") +
  theme_minimal() +
  labs(title = "Eviction Rates in Greater Seattle Area", 
       subtitle = "Washington State")

# Exploratory Data Analysis (EDA)
# Histogram for mean_index_value
ggplot(wa_eviction_erap, aes(x = mean_index_value)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Mean Index Value", x = "Mean Index Value", y = "Frequency")

# Histogram for mean_monthly_housing_cost
ggplot(wa_eviction_erap, aes(x = mean_monthly_housing_cost)) +
  geom_histogram(binwidth = 50, fill = "green", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Mean Monthly Housing Cost", x = "Mean Monthly Housing Cost", y = "Frequency")

# Histogram for eviction.rate
ggplot(wa_eviction_erap, aes(x = eviction.rate)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Eviction Rate", x = "Eviction Rate", y = "Frequency")

# Scatter plot for mean_monthly_housing_cost vs. mean_index_value
ggplot(wa_eviction_erap, aes(x = mean_monthly_housing_cost, y = mean_index_value, color = county_name)) +
  geom_point() +
  geom_text(aes(label = county_name), size = 3, hjust = 0.5, vjust = 1.5) +
  theme_minimal() +
  labs(title = "Mean Monthly Housing Cost vs. Mean Index Value", x = "Mean Monthly Housing Cost", y = "Mean Index Value")

# Scatter plot for eviction.rate vs. mean_index_value
ggplot(wa_eviction_erap, aes(x = eviction.rate, y = mean_index_value, color = county_name)) +
  geom_point() +
  geom_text(aes(label = county_name), size = 3, hjust = 0.5, vjust = 1.5) +
  theme_minimal() +
  labs(title = "Eviction Rate vs. Mean Index Value", x = "Eviction Rate", y = "Mean Index Value")

# Scatter plot for eviction.rate vs. mean_monthly_housing_cost
ggplot(wa_eviction_erap, aes(x = eviction.rate, y = mean_monthly_housing_cost, color = county_name)) +
  geom_point() +
  geom_text(aes(label = county_name), size = 3, hjust = 0.5, vjust = 1.5) +
  theme_minimal() +
  labs(title = "Eviction Rate vs. Mean Monthly Housing Cost", x = "Eviction Rate", y = "Mean Monthly Housing Cost")

# Boxplot for mean_index_value
ggplot(wa_eviction_erap, aes(y = mean_index_value)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Mean Index Value", y = "Mean Index Value")

# Boxplot for mean_monthly_housing_cost
ggplot(wa_eviction_erap, aes(y = mean_monthly_housing_cost)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Mean Monthly Housing Cost", y = "Mean Monthly Housing Cost")

# Boxplot for eviction rate
ggplot(wa_eviction_erap, aes(y = eviction.rate)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Eviction Rate", y = "Eviction Rate")

# Pareto chart for eviction rates by county
eviction_rates_data <- wa_eviction_erap |>
  group_by(county_name) |>
  summarize(total_eviction_rate = mean(eviction.rate, na.rm = TRUE)) |>
  arrange(desc(total_eviction_rate)) |>
  mutate(cum_freq = cumsum(total_eviction_rate) / sum(total_eviction_rate))

max_rate <- max(eviction_rates_data$total_eviction_rate)

ggplot(eviction_rates_data, aes(x = reorder(county_name, -total_eviction_rate), y = total_eviction_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = cum_freq * max_rate), color = "red", group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / max_rate, name = "Cumulative Frequency")) +
  theme_minimal() +
  labs(title = "Pareto Chart of Eviction Rates by County", x = "County", y = "Eviction Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Additional scatter plots for relationships
# Scatter plot for eviction.rate vs. poverty.rate
ggplot(wa_eviction_erap, aes(x = poverty.rate, y = eviction.rate, label = county_name)) +
  geom_point(color = "darkgreen") +
  geom_text(nudge_y = 0.5, check_overlap = TRUE, size = 3) +
  theme_minimal() +
  labs(title = "Eviction Rate vs. Poverty Rate", x = "Poverty Rate", y = "Eviction Rate")

# Scatter plot for eviction.rate vs. median.gross.rent
ggplot(wa_eviction_erap, aes(x = median.gross.rent, y = eviction.rate, label = county_name)) +
  geom_point(color = "purple") +
  geom_text(nudge_y = 0.5, check_overlap = TRUE, size = 3) +
  theme_minimal() +
  labs(title = "Eviction Rate vs. Median Gross Rent", x = "Median Gross Rent", y = "Eviction Rate")

# Scatter plot for eviction.rate vs. median.household.income
ggplot(wa_eviction_erap, aes(x = median.household.income, y = eviction.rate, label = county_name)) +
  geom_point(color = "orange") +
  geom_text(nudge_y = 0.5, check_overlap = TRUE, size = 3) +
  theme_minimal() +
  labs(title = "Eviction Rate vs. Median Household Income", x = "Median Household Income", y = "Eviction Rate")

# Linear regression model to predict eviction rate
model <- lm(eviction.rate ~ mean_index_value + mean_monthly_housing_cost + mean_housing_subindex + mean_income_subindex + poverty.rate + renter.occupied.households + median.gross.rent + median.household.income + pct.white + pct.af.am + pct.hispanic + pct.am.ind + pct.asian, data = wa_eviction_erap)

# Summary of the model
summary(model)


# Perform k-means clustering on eviction rate vs. poverty rate
clustering_data_eviction_poverty <- wa_eviction_erap |>
  st_set_geometry(NULL) |>
  select(eviction.rate, poverty.rate)

set.seed(123)  # For reproducibility
k <- 3  # Number of clusters
kmeans_result_eviction_poverty <- kmeans(clustering_data_eviction_poverty, centers = k, nstart = 25)
wa_eviction_erap$cluster_eviction_poverty <- as.factor(kmeans_result_eviction_poverty$cluster)

# Visualize the clusters for eviction rate vs. poverty rate
ggplot(wa_eviction_erap, aes(x = poverty.rate, y = eviction.rate, color = cluster_eviction_poverty, label = county_name)) +
  geom_point(size = 3) +
  geom_text(size = 3, hjust = 0.5, vjust = 1.5) +
  theme_minimal() +
  labs(title = "Clustering of Counties by Eviction Rate and Poverty Rate",
       x = "Poverty Rate",
       y = "Eviction Rate",
       color = "Cluster")

# Perform k-means clustering on mean monthly housing cost vs. mean index value
clustering_data_housing_index <- wa_eviction_erap |>
  st_set_geometry(NULL) |>
  select(mean_monthly_housing_cost, mean_index_value)

set.seed(123)  # For reproducibility
kmeans_result_housing_index <- kmeans(clustering_data_housing_index, centers = k, nstart = 25)
wa_eviction_erap$cluster_housing_index <- as.factor(kmeans_result_housing_index$cluster)

# Visualize the clusters for mean monthly housing cost vs. mean index value
ggplot(wa_eviction_erap, aes(x = mean_monthly_housing_cost, y = mean_index_value, color = cluster_housing_index, label = county_name)) +
  geom_point(size = 3) +
  geom_text(size = 3, hjust = 0.5, vjust = 1.5) +
  theme_minimal() +
  labs(title = "Clustering of Counties by Mean Monthly Housing Cost and Mean Index Value",
       x = "Mean Monthly Housing Cost",
       y = "Mean Index Value",
       color = "Cluster")