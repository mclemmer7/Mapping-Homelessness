library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(car)
library(plotly)
library(leaps)

my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

# Make a plotly version of this map
wa_eviction_erap_tracts <- st_read("../data/wa_eviction_erap_tracts.geojson")
wa_eviction_erap_tracts$text <- paste("<b>Tract", wa_eviction_erap_tracts$census_tract, "of", wa_eviction_erap_tracts$county_name,
              "<b><br>ERAP Index:", wa_eviction_erap_tracts$index_value,
              "<br>Eviction Rate:",round( wa_eviction_erap_tracts$eviction_rate,3),
              "<br>Population:", wa_eviction_erap_tracts$population,
              "<br>Poverty Rate:", wa_eviction_erap_tracts$poverty.rate)


wa_map <- ggplot(wa_eviction_erap_tracts) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Washington")


ggplotly(wa_map, tooltip = "text") |>
  style(hoveron = "fills+points")


# Now Illinois
il_eviction_erap_tracts <- st_read("../data/il_eviction_erap_tracts.geojson")
il_eviction_erap_tracts$text <- paste("<b>Tract", il_eviction_erap_tracts$census_tract, "of", il_eviction_erap_tracts$county_name,
              "<b><br>ERAP Index:", il_eviction_erap_tracts$index_value,
              "<br>Eviction Rate:",round( il_eviction_erap_tracts$eviction_rate,3),
              "<br>Population:", il_eviction_erap_tracts$population,
              "<br>Poverty Rate:", il_eviction_erap_tracts$poverty.rate)

il_map <- ggplot(il_eviction_erap_tracts) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Illinois")


ggplotly(il_map, tooltip = "text") |>
  style(hoveron = "fills+points")


# Now Ohio
oh_eviction_erap_tracts <- st_read("../data/oh_eviction_erap_tracts.geojson")
oh_eviction_erap_tracts$text <- paste("<b>Tract", oh_eviction_erap_tracts$census_tract, "of", oh_eviction_erap_tracts$county_name,
              "<b><br>ERAP Index:", oh_eviction_erap_tracts$index_value,
              "<br>Eviction Rate:",round( oh_eviction_erap_tracts$eviction_rate,3),
              "<br>Population:", oh_eviction_erap_tracts$population,
              "<br>Poverty Rate:", oh_eviction_erap_tracts$poverty.rate)

oh_map <- ggplot(oh_eviction_erap_tracts) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Ohio")


ggplotly(oh_map, tooltip = "text") |>
  style(hoveron = "fills+points")

# Map Pennsylvania
pa_eviction_erap_tracts <- st_read("../data/pa_eviction_erap_tracts.geojson")
pa_eviction_erap_tracts$text <- paste("<b>Tract", pa_eviction_erap_tracts$census_tract, "of", pa_eviction_erap_tracts$county_name,
              "<b><br>ERAP Index:", pa_eviction_erap_tracts$index_value,
              "<br>Eviction Rate:",round( pa_eviction_erap_tracts$eviction_rate,3),
              "<br>Population:", pa_eviction_erap_tracts$population,
              "<br>Poverty Rate:", pa_eviction_erap_tracts$poverty.rate)

pa_map <- ggplot(pa_eviction_erap_tracts) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Pennsylvania")


ggplotly(pa_map, tooltip = "text") |>
  style(hoveron = "fills+points")