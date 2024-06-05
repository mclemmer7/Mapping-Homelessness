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
seattle_erap_red_evic <- st_read("../data/seattle_erap_red_evic.geojson")
seattle_erap_red_evic$text <- paste("<b>Tract", seattle_erap_red_evic$census_tract,
              "<b><br>ERAP Index:", seattle_erap_red_evic$index_value,
              "<br>Eviction Rate:",round(seattle_erap_red_evic$eviction_rate,3),
              "<br>Grade", seattle_erap_red_evic$grade, ":", seattle_erap_red_evic$cat,
              "<br>Population:", seattle_erap_red_evic$population,
              "<br>Poverty Rate:", seattle_erap_red_evic$poverty.rate)

seattle_map <- ggplot(seattle_erap_red_evic) +
  geom_sf(aes(fill = eviction_rate, text = text)) +
  scale_fill_continuous("Eviction Rate",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions by census tract in Seattle")


ggplotly(seattle_map, tooltip = "text") |>
  style(hoveron = "fills+points")

chicago_erap_red_evic <- st_read("../data/chicago_erap_red_evic.geojson")
chicago_erap_red_evic$text <- paste("<b>Tract", chicago_erap_red_evic$census_tract,
              "<b><br>ERAP Index:", chicago_erap_red_evic$index_value,
              "<br>Eviction Rate:",round( chicago_erap_red_evic$eviction_rate,3),
              "<br>Grade", chicago_erap_red_evic$grade, ":", chicago_erap_red_evic$cat,
              "<br>Population:", chicago_erap_red_evic$population,
              "<br>Poverty Rate:", chicago_erap_red_evic$poverty.rate)

chicago_map <- ggplot(chicago_erap_red_evic) +
  geom_sf(aes(fill = eviction_rate, text = text)) +
  scale_fill_continuous("Eviction Rate",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions by census tract in Chicago")


ggplotly(chicago_map, tooltip = "text") |>
  style(hoveron = "fills+points")


pittsburgh_erap_red_evic <- st_read("../data/pittsburgh_erap_red_evic.geojson")
pittsburgh_erap_red_evic$text <- paste("<b>Tract", pittsburgh_erap_red_evic$census_tract,
              "<b><br>ERAP Index:", pittsburgh_erap_red_evic$index_value,
              "<br>Eviction Rate:",round( pittsburgh_erap_red_evic$eviction_rate,3),
              "<br>Grade", pittsburgh_erap_red_evic$grade, ":", pittsburgh_erap_red_evic$cat,
              "<br>Population:", pittsburgh_erap_red_evic$population,
              "<br>Poverty Rate:", pittsburgh_erap_red_evic$poverty.rate)

pittsburgh_map <- ggplot(pittsburgh_erap_red_evic) +
  geom_sf(aes(fill = eviction_rate, text = text)) +
  scale_fill_continuous("Eviction Rate",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions by census tract in Pittsburgh")


ggplotly(pittsburgh_map, tooltip = "text") |>
  style(hoveron = "fills+points")


cleveland_erap_red_evic <- st_read("../data/cleveland_erap_red_evic.geojson")
cleveland_erap_red_evic$text <- paste("<b>Tract", cleveland_erap_red_evic$census_tract,
              "<b><br>ERAP Index:", cleveland_erap_red_evic$index_value,
              "<br>Eviction Rate:",round( cleveland_erap_red_evic$eviction_rate,3),
              "<br>Grade", cleveland_erap_red_evic$grade, ":", cleveland_erap_red_evic$cat,
              "<br>Population:", cleveland_erap_red_evic$population,
              "<br>Poverty Rate:", cleveland_erap_red_evic$poverty.rate)

cleveland_map <- ggplot(cleveland_erap_red_evic) +
  geom_sf(aes(fill = eviction_rate, text = text)) +
  scale_fill_continuous("Eviction Rate",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions by census tract in Cleveland")


ggplotly(cleveland_map, tooltip = "text") |>
  style(hoveron = "fills+points")
