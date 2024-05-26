library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(car)
library(plotly)
library(leaps)

wa_eviction_erap_tracts <- st_read("../data/wa_eviction_erap_tracts.geojson")

# Calculate percent minority based on asians, blacks, and latinos. Not including Native Americans, Pacific Islanders and other at the moment.
wa_eviction_erap_tracts$percent_minority <- wa_eviction_erap_tracts$percent_asian + wa_eviction_erap_tracts$percent_black + wa_eviction_erap_tracts$percent_latine


my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

# Make a plotly version of this map
wa_eviction_erap_tracts$text <- paste("<b>Tract", wa_eviction_erap_tracts$census_tract, "of", wa_eviction_erap_tracts$county_name,
                                   "<b><br>ERAP Index:", wa_eviction_erap_tracts$index_value,
                                   "<br>Evictions",wa_eviction_erap_tracts$eviction.rate,
                                   "<br>Population:", wa_eviction_erap_tracts$population)

wa_map <- ggplot(wa_eviction_erap_tracts) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Washington")
  

ggplotly(wa_map, tooltip = "text") |>
  style(hoveron = "fills+points")


# Make a scatterplot of index value vs filing_rate
# This seems to show that ERAP index isn't a great predictor of evictions
ggplot(data = wa_eviction_erap_tracts, aes(y=eviction.rate, x=index_value)) +
  geom_jitter(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Washington Eviction Rate vs ERAP Index") +
  xlab("ERAP Index Value") +
  ylab("Eviction Rate")

filtered_king_county <- wa_eviction_erap_tracts |>
  filter(county_name == "King County")

king_map <- ggplot(filtered_king_county) +
  geom_sf(aes(fill = index_value, text = text)) +
  scale_fill_continuous("ERAP Index Value",
                        low="yellow",
                        high="red") +
  my_map_theme() +
  ggtitle("Evictions and ERAP Index by census tract in Washington")


ggplotly(king_map, tooltip = "text") |>
  style(hoveron = "fills+points")

# Now make a scatterplot for king county
ggplot(data = filtered_king_county, aes(y=eviction.rate, x=index_value)) +
  geom_jitter(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Washington Eviction Rate vs ERAP Index") +
  xlab("ERAP Index Value") +
  ylab("Eviction Rate")

m1 <- lm(data=filtered_king_county, eviction.rate ~ index_value)
summary(m1)

plot(resid(m1) ~ fitted(m1))
abline(h=0)


# Now use best subsets to find the best combination of predictors for the model
all <- regsubsets(data = wa_eviction_erap_tracts, evictions ~ index_value + housing_subindex_value + income_subindex_value + median_monthly_housing_cost + percent_minority + population + poverty.rate)
round(summary(all)$adjr2, 3)
plot(all, scale="adjr2")

# percent_minority appears to be the worst predictor
mf <- lm(data = wa_eviction_erap_tracts, evictions ~ index_value + housing_subindex_value + income_subindex_value + median_monthly_housing_cost + percent_minority + population + poverty.rate)
MSE=(summary(mf)$sigma)^2
step(mf, scale=MSE, direction = "backward")
