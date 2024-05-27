library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(car)
library(plotly)
library(leaps)

chicago_erap_red_evic <- st_read("../data/chicago_erap_red_evic.geojson")
pittsburgh_erap_red_evic <- st_read("../data/pittsburgh_erap_red_evic.geojson")
cleveland_erap_red_evic <- st_read("../data/cleveland_erap_red_evic.geojson")
seattle_erap_red_evic <- st_read("../data/seattle_erap_red_evic.geojson")

my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

# Make a plotly version of this map
seattle_erap_red_evic$text <- paste("<b>Tract", seattle_erap_red_evic$census_tract, "of", seattle_erap_red_evic$county_name,
                                      "<b><br>ERAP Index:", seattle_erap_red_evic$index_value,
                                      "<br>Evictions",seattle_erap_red_evic$eviction.rate,
                                      "<br>Population:", seattle_erap_red_evic$population)


seattle_map <- ggplot(seattle_erap_red_evic) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Washington")


ggplotly(seattle_map, tooltip = "text") |>
  style(hoveron = "fills+points")

chicago_erap_red_evic$text <- paste("<b>Tract", chicago_erap_red_evic$census_tract, "of", chicago_erap_red_evic$county_name,
                                    "<b><br>ERAP Index:", chicago_erap_red_evic$index_value,
                                    "<br>Evictions",chicago_erap_red_evic$eviction.rate,
                                    "<br>Population:", chicago_erap_red_evic$population)


chicago_map <- ggplot(chicago_erap_red_evic) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Washington")


ggplotly(chicago_map, tooltip = "text") |>
  style(hoveron = "fills+points")

pittsburgh_erap_red_evic$text <- paste("<b>Tract", pittsburgh_erap_red_evic$census_tract, "of", pittsburgh_erap_red_evic$county_name,
                                    "<b><br>ERAP Index:", pittsburgh_erap_red_evic$index_value,
                                    "<br>Evictions",pittsburgh_erap_red_evic$eviction.rate,
                                    "<br>Population:", pittsburgh_erap_red_evic$population)


pittsburgh_map <- ggplot(pittsburgh_erap_red_evic) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Washington")


ggplotly(pittsburgh_map, tooltip = "text") |>
  style(hoveron = "fills+points")

cleveland_erap_red_evic$text <- paste("<b>Tract", cleveland_erap_red_evic$census_tract, "of", seattle_erap_red_evic$county_name,
                                    "<b><br>ERAP Index:", cleveland_erap_red_evic$index_value,
                                    "<br>Evictions",cleveland_erap_red_evic$eviction.rate,
                                    "<br>Population:", cleveland_erap_red_evic$population)


cleveland_map <- ggplot(cleveland_erap_red_evic) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Washington")


ggplotly(cleveland_map, tooltip = "text") |>
  style(hoveron = "fills+points")
