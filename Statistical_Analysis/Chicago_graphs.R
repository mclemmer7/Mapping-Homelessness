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
chicago <- read_sf("chicago_erap_red_evic.geojson")

# Handle missing values by removing rows with NAs
chicago_clean <- chicago |>
  filter(!is.na(eviction.rate) & 
           !is.na(percent_cost_burdened_renters) & 
           !is.na(median.gross.rent) & 
           !is.na(percent_black) & 
           !is.na(index_value) & 
           !is.na(year) & 
           !is.na(county_name))

# Plot 1: Scatter Plot of Eviction Rate by Percent Black Population
plot1 <- ggplot(chicago_clean, aes(x = percent_black, y = eviction.rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Black Population", x = "Percent Black Population", y = "Eviction Rate")

# Plot 2: Scatter Plot of Eviction Rate by Percent Asian Population
plot2 <- ggplot(chicago_clean, aes(x = percent_asian, y = eviction.rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Asian Population", x = "Percent Asian Population", y = "Eviction Rate")

# Plot 3: Scatter Plot of Eviction Rate by Percent Latine Population
plot3 <- ggplot(chicago_clean, aes(x = percent_latine, y = eviction.rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Latine Population", x = "Percent Latine Population", y = "Eviction Rate")

# Plot 4: Scatter Plot of Eviction Rate by Percent Other Population
plot4 <- ggplot(chicago_clean, aes(x = percent_other, y = eviction.rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Percent Other Population", x = "Percent Other Population", y = "Eviction Rate")

# Plot 5: Bar Plot of Eviction Rate by Grade
plot5 <- ggplot(chicago_clean, aes(x=eviction.filing.rate, y = grade, fill = grade )) +
  geom_bar(stat = "identity") +
  labs(title = "Eviction Filling Rate by Grade", x = "Grade", y = "Eviction Rate")

# Plot 6: Box Plot of Eviction Rate by Race
chicago_long <- chicago_clean |>
  gather(key = "race", value = "percent", percent_black, percent_asian, percent_latine, percent_other)

plot6 <- ggplot(chicago_long, aes(x = race, y = eviction.rate, fill = race)) +
  geom_boxplot() +
  labs(title = "Eviction Rate by Race", x = "Race", y = "Eviction Rate")

# Plot 7: Histogram of Eviction Rate
plot7 <- ggplot(chicago_clean, aes(x = eviction.rate)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
  labs(title = "Histogram of Eviction Rate", x = "Eviction Rate", y = "Frequency")

# Plot 8: Density Plot of Eviction Rate
plot8 <- ggplot(chicago_clean, aes(x = eviction.rate, fill = grade)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Eviction Rate by Grade", x = "Eviction Rate", y = "Density")

# Plot 9: Scatter Plot of Eviction Rate by ERAP Index Value
plot9 <- ggplot(chicago_clean, aes(x = index_value, y = eviction.rate)) +
  geom_point() +
  labs(title = "Eviction Rate by ERAP Index Value", x = "ERAP Index Value", y = "Eviction Rate")

# Plot 10: Scatter Plot of Eviction Rate by Median Gross Rent
plot10 <- ggplot(chicago_clean, aes(x = median.gross.rent, y = eviction.rate)) +
  geom_point() +
  labs(title = "Eviction Rate by Median Gross Rent", x = "Median Gross Rent", y = "Eviction Rate")

grid.arrange(
  arrangeGrob(
    grobs = list(
      plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10
    ), 
    ncol = 2
  ),
  top = textGrob("Chicago Graphs", gp = gpar(fontsize = 15, font = 2))
)