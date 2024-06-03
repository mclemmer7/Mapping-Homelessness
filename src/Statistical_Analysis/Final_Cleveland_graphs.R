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
           !is.na(percent_asian) & 
           !is.na(percent_latine) & 
           !is.na(percent_other) & 
           !is.na(index_value) & 
           !is.na(year) & 
           !is.na(county_name))

# Plot 1: Scatter Plot of Eviction Rate by Percent Black Population
plot1 <- ggplot(cleveland_clean, aes(x = percent_black, y = eviction_rate)) +
  geom_point(color = "black", alpha = 0.35, shape = 21, stroke = 0.5, fill = "#38EB37" ) +
  labs(title = "Eviction Rate by Percent Black Population", x = "Percent Black Population", y = "Eviction Rate per 1000") +
  scale_x_continuous(limits = c(0, 1))

# Plot 2: Scatter Plot of Eviction Rate by Percent Asian Population
plot2 <- ggplot(cleveland_clean, aes(x = percent_asian, y = eviction_rate)) +
  geom_point(color = "black", alpha = 0.35, shape = 21, stroke = 0.5, fill = "#E7507C" ) +
  labs(title = "Eviction Rate by Percent Asian Population", x = "Percent Asian Population", y = "Eviction Rate per 1000") +
  scale_x_continuous(limits = c(0, 1))

# Plot 3: Scatter Plot of Eviction Rate by Percent Latine Population
plot3 <- ggplot(cleveland_clean, aes(x = percent_latine, y = eviction_rate)) +
  geom_point(color = "black", alpha = 0.35, shape = 21, stroke = 0.5, fill = "#1ECCC4" ) +
  labs(title = "Eviction Rate by Percent Latine Population", x = "Percent Latine Population", y = "Eviction Rate per 1000") +
  scale_x_continuous(limits = c(0, 1))

# Plot 4: Scatter Plot of Eviction Rate by Percent Other Population
plot4 <- ggplot(cleveland_clean, aes(x = percent_other, y = eviction_rate)) +
  geom_point(color = "black", alpha = 0.35, shape = 21, stroke = 0.5, fill = "#C12FB3" ) +
  labs(title = "Eviction Rate by Percent Other Population", x = "Percent Other Population", y = "Eviction Rate per 1000") +
  scale_x_continuous(limits = c(0, 1))

# Plot 5: Bar Plot of Eviction Rate by Grade
plot5 <- ggplot(cleveland_clean, aes(x = grade, y = eviction_rate, fill = grade)) +
  geom_bar(stat = "identity") +
  labs(title = "Eviction Rate by Grade", x = "Grade", y = "Eviction Rate")

# Plot 6: Histogram of Eviction Rate
plot6 <- ggplot(cleveland_clean, aes(x = eviction_rate)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Eviction Rate", x = "Eviction Rate", y = "Frequency")

# Plot 7: Density Plot of Eviction Rate
plot7 <- ggplot(cleveland_clean, aes(x = eviction_rate, fill = grade)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Eviction Rate by Grade", x = "Eviction Rate", y = "Density")

# Plot 8: Scatter Plot of Eviction Rate by ERAP Index Value
plot8 <- ggplot(cleveland_clean, aes(x = index_value, y = eviction_rate)) +
  geom_point(color = "black", alpha = 0.35, shape = 21, stroke = 0.5, fill = "#EBDC49" ) +
  labs(title = "Eviction Rate by ERAP Index Value", x = "ERAP Index Value", y = "Eviction Rate")

# Plot 9: Scatter Plot of Eviction Rate by Median Gross Rent
plot9 <- ggplot(cleveland_clean, aes(x = median.gross.rent, y = eviction_rate)) +
  geom_point(color = "black", alpha = 0.35, shape = 21, stroke = 0.5, fill = "#EBDC49" ) +
  labs(title = "Eviction Rate by Median Gross Rent", x = "Median Gross Rent", y = "Eviction Rate")

# Combine all races into a long format
cleveland_long <- cleveland_clean |>
  gather(key = "races_nonWhite", value = "percent_races_nonWhite", percent_black, percent_asian, percent_latine, percent_other)

# Create the combined scatter plot using Plotly
combined_plot <- ggplot(cleveland_long, aes(x = percent_races_nonWhite, y = eviction_rate, fill = races_nonWhite)) +
  geom_point(color = "black", alpha = 0.35, shape = 21, stroke = 0.5) +
  geom_jitter(alpha = 0.50) +
  labs(title = "Eviction Rate by Percent Population of All Races", x = "Percent Population", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_continuous(limits = c(0, 1))

combined_plotly <- ggplotly(combined_plot)

# Arrange the first set of plots in a grid
grid.arrange(
  arrangeGrob(
    grobs = list(plot1, plot2, plot3, plot4), 
    ncol = 2
  ),
  top = textGrob("Cleveland Graphs", gp = gpar(fontsize = 20, font = 2))
)

# Arrange the second set of plots in a grid
grid.arrange(
  arrangeGrob(
    grobs = list(plot5, plot6, plot7, plot8, plot9), 
    ncol = 2
  )
)

# Display the combined Plotly plot separately
combined_plotly
