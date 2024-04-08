# Ensure necessary packages are installed and then load them
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(httr)
library(jsonlite)
library(sf)

# Define the URL of the GeoJSON file
url <- "https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/07/07/data_catalog_index_spatial.geojson"

# Directly read the GeoJSON file into an sf object
geo_data <- st_read(url)

# Now, `geo_data` is an sf object ready for manipulation and plotting
# For example, to view the first few rows of the data
head(geo_data)

geo_data |>
  filter(state_name == "Washington")
