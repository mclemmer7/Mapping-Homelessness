library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(car)

# Grab data from eviction lab
# What data do we really want from the eviction lab? This tract dataset has the eviction filings, filing rates, threats
# Tract data also includes the fips and tracts, so it's better than county_proprietary.
# Do we want the renting_hh, filings_observed, ind_filings_court_issued, and hh_threat_observed from county_court_issued?
# Could merge the datasets
eviction_lab <- read_csv("../data/tract_proprietary_valid_2000_2018.csv")
county_court_issued_2000_2018 <- read_csv("../data/county_court-issued_2000_2018.csv")
wa_eviction_lab <- read_csv("../data/WA_counties.csv")

wa_evictions <- eviction_lab |>
  filter(state == "Washington")

county_court_evictions <- county_court_issued_2000_2018 |>
  filter(state == "Washington" & county == "King County")

erap_index <- st_read("../data/erap_index.geojson")
erap_index$census_tract <- as.numeric(substr(erap_index$GEOID, 6, 11))


wa_erap <- erap_index |>
  filter(state_name == "Washington")
wa_erap$GEOID <- as.numeric(wa_erap$GEOID)

# join data for washington based on GEOID and fips
# Try to filter data that has the same GEOID to only show that max year. Could maybe 
eviction_erap_tract <- wa_erap |>
  inner_join(wa_evictions, by=c("GEOID" = "fips")) |>
  filter(!is.na(index_value)) |>
  filter(year == max(year))
# eviction_erap has a lot of NA values. Can remove rows with NA for index_value

# filter to king county - nothing here...
king_eviction_erap <- eviction_erap_tract |>
  filter(county_name == "King County")

# Now use eviction data with king county
#eviction_erap_court <- wa_erap |>
#  inner_join(county_court_evictions, by=c("GEOID" = "fips")) |>
#  filter(!is.na(index_value))

wa_erap$cofips <- substr(wa_erap$GEOID, 1, 5)
king_merged_geo_eviction <- merge(wa_erap, wa_evictions, by = "cofips")


# Write out to a file
#st_write(eviction_erap_tract, dsn = "eviction_erap_tract.geojson", row.names = FALSE)

# Filter for the most recent year

# transform data for WA eviction file
king_eviction_lab <- wa_eviction_lab |>
  filter(name == "King County")

# Modify erap data for washington to merge with this dataset
wa_erap$cofips <- as.numeric(substr(wa_erap$GEOID, 1, 5))

# TODO - calculate a mean of each of the erap counties based on if their cofips value is the same, then merge
# Should probably also remove the geometry column since it won't show the whole county anymore (st_set_geometry(NULL) |>)
wa_erap_condensed <- wa_erap |>
  group_by(cofips) |>
  summarize(
    GEOID = first(GEOID),
    state_name = first(state_name),
    county_name = first(county_name),
    mean_index_value = mean(index_value, na.rm=TRUE),
    mean_housing_subindex = mean(housing_subindex_value, na.rm=TRUE),
    mean_income_subindex = mean(income_subindex_value, na.rm=TRUE),
    mean_monthly_housing_cost = mean(median_monthly_housing_cost, na.rm=TRUE),
    mean_percent_cost_burdened_renters = mean(percent_cost_burdened_renters, na.rm=TRUE),
    mean_percent_asian = mean(percent_asian, na.rm=TRUE),
    mean_percent_black = mean(percent_black, na.rm=TRUE),
    mean_percent_latine = mean(percent_latine, na.rm=TRUE),
    mean_percent_other = mean(percent_other, na.rm=TRUE),
  )

# Try mapping the erap data
ggplot(wa_erap_condensed) +
  geom_sf(aes(fill = mean_index_value)) +
  scale_fill_continuous(high = "red", low = "yellow")

# Try simplifying wa_eviction_lab to just use the most recent year
wa_evic_lab <- wa_eviction_lab |>
  group_by(GEOID) |>
  filter(year == max(year))

# Now merge the datasets
wa_eviction_erap <- wa_erap_condensed |>
  inner_join(wa_evic_lab, by=c("cofips" = "GEOID"))
  # filter(!is.na(index_value))

# Write out to file:
#st_write(wa_eviction_erap, dsn = "wa_eviction_erap.geojson", row.names = FALSE)

# Make a scatterplot of index value vs filing_rate
# This seems to show that ERAP index isn't a great predictor of evictions
ggplot(data = wa_eviction_erap, aes(y=eviction.rate, x=mean_index_value)) +
  geom_jitter(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Washington Eviction Rate vs ERAP Index") +
  xlab("ERAP Index Value") +
  ylab("Eviction Rate")

# Let's test the hypothesis that the slope of the erap index value is zero when used as a predictor of eviction rate.
mod <- lm(eviction.rate ~ mean_index_value, data = wa_eviction_erap)
summary(mod)

# With the p-value of 0.619, we do not have sufficient evidence to reject the null hypothesis and conclude that there
# is a weak correlation between the mean erap index value and the eviction rate.
# The mean erap index value is also very generalized since it is the erap value for a whole county, when it was meant for a smaller region
