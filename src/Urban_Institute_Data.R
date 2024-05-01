# Ensure necessary packages are installed and then load them
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(plotly)
library(tidycensus)

erap_index <- st_read("erap_index.geojson")
erap_index$census_tract <- as.numeric(substr(erap_index$GEOID, 6, 11))

king_data <- erap_index |>
  filter(state_name == "Washington" & county_name == "King County")

my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

map <- ggplot(king_data) +
  geom_sf(aes(fill = percent_cost_burdened_renters)) +
  scale_fill_continuous(high = "red", low = "yellow", labels = scales::percent, breaks=c(0.001,0.30, 0.60)) +
  my_map_theme() +
  ggtitle("King County Percent Cost Burden")

# This is a map of King County, which gives a closer look at the data.
ggplotly(map) |>
  style(hoveron = "fills")

# 53 is washington. 033 is King County and 000100 to 012100 is Seattle
# Filter data for Seattle
# seattle_data <- king_data |>
#  filter(GEOID < 53033012100)

seattle_data <- king_data |>
  filter(census_tract > 000100 & census_tract < 012100)

# Try writing seattle data out to a geojson file
writeLines(seattle_data, "my_data.geojson")
sf::st_write(seattle_data, dsn = "my_data.geojson")

new_data <- st_read("my_data.geojson")

# Calculate percent minority based on asians, blacks, and latinos. Not including Native Americans, Pacific Islanders and other at the moment.
seattle_data$percent_minority <- seattle_data$percent_asian + seattle_data$percent_black + seattle_data$percent_latine
  
ggplot(seattle_data) +
  geom_sf(aes(fill = percent_minority)) +
  scale_fill_continuous(high = "red", low = "yellow", labels = scales::percent) +
  my_map_theme() +
  ggtitle("Seattle Minorities", subtitle = "Map of where Asian, Black, and Hispanic minorities live in Seattle")


# Now filter data for Cleveland
cleveland_data <- erap_index |>
  filter(state_name == "Ohio" & county_name == "Cuyahoga County") |>
  filter(census_tract < 150000)

map <- ggplot(cleveland_data) +
  geom_sf(aes(fill = percent_cost_burdened_renters)) +
  scale_fill_continuous(high = "red", low = "yellow", labels = scales::percent, breaks=c(0.001,0.30, 0.60)) +
  my_map_theme() +
  ggtitle("King County Percent Cost Burden")
map
