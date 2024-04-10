# Ensure necessary packages are installed and then load them
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(plotly)

# Define the URL of the GeoJSON file
url <- "https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/07/07/data_catalog_index_spatial.geojson"

# Directly read the GeoJSON file into an sf object
geo_data <- st_read(url)

# Now, `geo_data` is an sf object ready for manipulation and plotting
# For example, to view the first few rows of the data
head(geo_data)

wa_data <- geo_data |>
filter(state_name == "Washington")

ggplot(wa_data) +
  geom_sf()

king_data <- geo_data |>
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

ggplotly(map) |>
  style(hoveron = "fills")
  

