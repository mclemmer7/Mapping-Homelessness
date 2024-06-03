library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(readr)
library(readxl)
library(GGally)
library(plotly)
library(reshape2)
library(gridExtra)
library(grid)

# Load data
cleveland <- read_sf("cleveland_erap_red_evic 3.geojson")

# Handle missing values by removing rows with NAs
cleveland_clean <- cleveland |>
  filter(!is.na(eviction_rate) & 
           !is.na(percent_cost_burdened_renters) & 
           !is.na(median.gross.rent) & 
           !is.na(percent_black) & 
           !is.na(index_value) & 
           !is.na(year) & 
           !is.na(county_name))

# Combine all races into a long format
cleveland_long <- cleveland_clean |>
  gather(key = "race", value = "percent", percent_black, percent_asian, percent_latine, percent_other)

# Common theme to set consistent scales and add explanation
common_theme <- theme_minimal() +
  theme(
    text = element_text(size = 12),  # Increase the base font size
    plot.title = element_text(size = 11, face = "bold"),  # Increase title size
    axis.title = element_text(size = 8),  # Increase axis title size
    axis.text = element_text(size = 8),  # Increase axis text size
    legend.title = element_text(size = 10),  # Increase legend title size
    legend.text = element_text(size = 8)  # Increase legend text size
  )

# Plot 1: Scatter Plot of Eviction Rate by Percent Black Population
plot1 <- ggplot(cleveland_clean, aes(x = percent_black, y = eviction_rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Black Population", x = "Percent Black Population", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_continuous(limits = c(0, 1)) +
  common_theme

# Plot 2: Scatter Plot of Eviction Rate by Percent Asian Population
plot2 <- ggplot(cleveland_clean, aes(x = percent_asian, y = eviction_rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Asian Population", x = "Percent Asian Population", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_continuous(limits = c(0, 0.5)) +
  common_theme

# Plot 3: Scatter Plot of Eviction Rate by Percent Latine Population
plot3 <- ggplot(cleveland_clean, aes(x = percent_latine, y = eviction_rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Latine Population", x = "Percent Latine Population", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_continuous(limits = c(0, 0.5)) +
  common_theme

# Plot 4: Scatter Plot of Eviction Rate by Percent Other Population
plot4 <- ggplot(cleveland_clean, aes(x = percent_other, y = eviction_rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Other Population", x = "Percent Other Population", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_continuous(limits = c(0, 0.3)) +
  common_theme

# Plot 5: Bar Plot of Eviction Rate by Grade
plot5 <- ggplot(cleveland_clean, aes(x = grade, y = eviction_rate, fill = grade)) +
  geom_bar(stat = "identity") +
  labs(title = "Eviction Rate by Grade", x = "Grade", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0, 60)) +
  common_theme

# Plot 6: Box Plot of Eviction Rate by Race
plot6 <- ggplot(cleveland_long, aes(x = race, y = eviction_rate, fill = race)) +
  geom_boxplot() +
  labs(title = "Eviction Rate by Race", x = "Race", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0, 60)) +
  common_theme

# Plot 7: Histogram of Eviction Rate
plot7 <- ggplot(cleveland_clean, aes(x = eviction_rate)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Eviction Rate", x = "Eviction Rate (per 1000)", y = "count") +
  scale_y_continuous(limits = c(0, 60)) +
  common_theme

# Plot 8: Density Plot of Eviction Rate
plot8 <- ggplot(cleveland_clean, aes(x = eviction_rate, fill = grade)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Eviction Rate by Grade", x = "Eviction Rate (per 1000)", y = "Density") +
  scale_y_continuous(limits = c(0, 60)) +
  common_theme

# Plot 9: Scatter Plot of Eviction Rate by ERAP Index Value
plot9 <- ggplot(cleveland_clean, aes(x = index_value, y = eviction_rate)) +
  geom_point() +
  labs(title = "Eviction Rate by ERAP Index Value", x = "ERAP Index Value", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0, 60)) +
  common_theme

# New Plot: Combined Scatter Plot of Eviction Rate by Percent Population of All Races
combined_plot <- ggplot(cleveland_long, aes(x = percent, y = eviction_rate, color = race, shape = race)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Population of All Races", x = "Percent Population", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_continuous(limits = c(0, 1)) +
  common_theme

# Arrange the plots in a grid
grid.arrange(
  arrangeGrob(
    grobs = list(
      plot1, plot2, plot3, plot4, plot5, plot6
    ), 
    ncol = 2
  ),
  top = textGrob("Cleveland Graphs", gp = gpar(fontsize = 20, font = 2))
)

# Arrange the plots in a grid
grid.arrange(
  arrangeGrob(
    grobs = list(
     plot7, plot8, plot9, combined_plot
    ), 
    ncol = 2
  ),
  top = textGrob("Cleveland Graphs", gp = gpar(fontsize = 20, font = 2))
)