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

m2 <- lm(data = wa_eviction_erap_tracts, eviction.rate ~ index_value + housing_subindex_value + income_subindex_value + median_monthly_housing_cost + population + poverty.rate)

summary(m2)

# Find the best way to predict evictions
chicago_erap_red_evic <- st_read("../data/chicago_erap_red_evic.geojson")
pittsburgh_erap_red_evic <- st_read("../data/pittsburgh_erap_red_evic.geojson")
cleveland_erap_red_evic <- st_read("../data/cleveland_erap_red_evic.geojson")
seattle_erap_red_evic <- st_read("../data/seattle_erap_red_evic.geojson")

# Make indicator variable for redlining grades
seattle_data <- seattle_erap_red_evic |>
  mutate(cat_Best = ifelse(cat == "Best", 1, 0),
         cat_Definitely_Declining = ifelse(cat == "Definitely Declining", 1, 0),
         cat_Hazardous = ifelse(cat == "Hazardous", 1, 0),
         cat_Industrial = ifelse(cat == "Industrial", 1, 0),
         cat_Commercial = ifelse(cat == "Commercial District (Important Retail and Wholesale)", 1, 0),
         cat_Still_Desirable = ifelse(cat == "Still Desirable", 1, 0))

# Make indicator variable for redlining grades
cleveland_data <- cleveland_erap_red_evic |>
  mutate(cat_Best = ifelse(cat == "Best", 1, 0),
         cat_Definitely_Declining = ifelse(cat == "Definitely Declining", 1, 0),
         cat_Hazardous = ifelse(cat == "Hazardous", 1, 0),
         cat_Industrial = ifelse(cat == "Industrial", 1, 0),
         cat_Commercial = ifelse(cat == "Commercial District (Important Retail and Wholesale)", 1, 0),
         cat_Still_Desirable = ifelse(cat == "Still Desirable", 1, 0))

# Make indicator variable for redlining grades
chicago_data <- chicago_erap_red_evic |>
  mutate(cat_Best = ifelse(cat == "Best", 1, 0),
         cat_Definitely_Declining = ifelse(cat == "Definitely Declining", 1, 0),
         cat_Hazardous = ifelse(cat == "Hazardous", 1, 0),
         cat_Industrial = ifelse(cat == "Industrial", 1, 0),
         cat_Commercial = ifelse(cat == "Commercial District (Important Retail and Wholesale)", 1, 0),
         cat_Still_Desirable = ifelse(cat == "Still Desirable", 1, 0))

# Make indicator variable for redlining grades
pittsburgh_data <- seattle_erap_red_evic |>
  mutate(cat_Best = ifelse(cat == "Best", 1, 0),
         cat_Definitely_Declining = ifelse(cat == "Definitely Declining", 1, 0),
         cat_Hazardous = ifelse(cat == "Hazardous", 1, 0),
         cat_Industrial = ifelse(cat == "Industrial", 1, 0),
         cat_Commercial = ifelse(cat == "Commercial District (Important Retail and Wholesale)", 1, 0),
         cat_Still_Desirable = ifelse(cat == "Still Desirable", 1, 0))

# Continue predicting evictions with added redlining data
m3 <- lm(index_value ~  cat_Definitely_Declining + cat_Hazardous + cat_Industrial + cat_Still_Desirable + cat_Commercial, data = cleveland_data)
summary(m3)

m4 <- regsubsets(data = pittsburgh_data, evictions ~ index_value + housing_subindex_value + income_subindex_value + median_monthly_housing_cost + population + poverty.rate + cat_Definitely_Declining + cat_Hazardous + cat_Industrial + cat_Still_Desirable + cat_Commercial)
round(summary(m4)$adjr2, 3)
plot(m4, scale="adjr2")

m5 <- lm(data = seattle_data, eviction.rate ~ index_value + housing_subindex_value + income_subindex_value + median_monthly_housing_cost + percent_minority + poverty.rate + cat_Definitely_Declining + cat_Still_Desirable)
summary(m5)

m6 <- lm(data = chicago_data, evictions ~ housing_subindex_value + income_subindex_value + median_monthly_housing_cost + population + poverty.rate + cat_Definitely_Declining + cat_Hazardous + cat_Industrial)
summary(m6)

m7 <- lm(data = chicago_data, eviction.rate ~ index_value + housing_subindex_value + income_subindex_value + median_monthly_housing_cost + poverty.rate + cat_Definitely_Declining + cat_Hazardous + cat_Industrial)
summary(m7)

m8 <- lm(data = cleveland_data, evictions ~ housing_subindex_value + income_subindex_value + population + poverty.rate + cat_Definitely_Declining + cat_Hazardous + cat_Industrial + cat_Still_Desirable)
summary(m8)
