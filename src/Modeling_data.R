library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(car)
library(plotly)
library(leaps)

# Find the best way to predict evictions
wa_eviction_erap_tracts <- st_read("../data/wa_eviction_erap_tracts.geojson")
oh_eviction_erap_tracts <- st_read("../data/oh_eviction_erap_tracts.geojson")
pa_eviction_erap_tracts <- st_read("../data/pa_eviction_erap_tracts.geojson")
il_eviction_erap_tracts <- st_read("../data/il_eviction_erap_tracts.geojson")


m1 <- lm(eviction_rate ~ year +  renter.occupied.households + median.property.value + median.household.income * median.gross.rent , data = wa_eviction_erap_tracts)
summary(m1)

# Base model with erap index as the only predictor
m2 <- lm(eviction_rate ~ index_value, data = wa_eviction_erap_tracts)
summary(m2)

# Washington

mfull <- lm(formula = eviction_rate ~ housing_subindex_value + income_subindex_value + hh_chars_subindex_value + median_monthly_housing_cost + percent_renter_occ_units + percent_renter_occ_units_multi + num_eli_renters + rent.burden + poverty.rate + renter.occupied.households + median.gross.rent + median.household.income + median.property.value, data = wa_eviction_erap_tracts)
MSE=(summary(mfull)$sigma)^2
step(mfull, scale=MSE, direction = "backward")

summary(m7 <- lm(formula = eviction_rate ~ income_subindex_value + hh_chars_subindex_value + 
     percent_renter_occ_units + num_eli_renters + poverty.rate + 
     renter.occupied.households + median.gross.rent + median.household.income + 
     median.property.value, data = wa_eviction_erap_tracts))

m8 <- lm(formula = eviction_rate ~ income_subindex_value + hh_chars_subindex_value + 
           percent_renter_occ_units + poverty.rate + 
           renter.occupied.households + median.gross.rent + median.household.income + 
           median.property.value, data = wa_eviction_erap_tracts)
summary(m8)

anova(m8, m7)

# Final model
m9 <- lm(formula = eviction_rate ~ income_subindex_value + hh_chars_subindex_value + 
           percent_renter_occ_units + poverty.rate +  median.gross.rent + median.household.income + 
           median.property.value, data = wa_eviction_erap_tracts)
summary(m9)

anova(m9, m8)

# Need new dataset to compare m2 to m9
validRows <- !(is.na(wa_eviction_erap_tracts$income_subindex_value) | is.na(wa_eviction_erap_tracts$hh_chars_subindex_value) |
               is.na(wa_eviction_erap_tracts$percent_renter_occ_units) | is.na(wa_eviction_erap_tracts$poverty.rate) | 
               is.na(wa_eviction_erap_tracts$median.gross.rent) | is.na(wa_eviction_erap_tracts$median.household.income) | 
               is.na(wa_eviction_erap_tracts$median.property.value) | is.na(wa_eviction_erap_tracts$index_value))
nrow(wa_eviction_erap_tracts) - sum(validRows)

m2a <- lm(eviction_rate ~ index_value, data = subset(wa_eviction_erap_tracts, validRows))
m9a <- lm(formula = eviction_rate ~ income_subindex_value + hh_chars_subindex_value + 
           percent_renter_occ_units + poverty.rate +  median.gross.rent + median.household.income + 
           median.property.value, data = subset(wa_eviction_erap_tracts, validRows))
anova(m2a, m9a)

# Values to predict
filter(wa_eviction_erap_tracts, GEOID == 53011041700)
filter(wa_eviction_erap_tracts, GEOID == 53005011401)
filter(wa_eviction_erap_tracts, GEOID == 53033009300)
filter(wa_eviction_erap_tracts, GEOID == 53015001000)

# Predictions using our model
# Predicted eviction_rate = -1.194 + 0.006423 * income_subindex_value -0.005748 * hh_chars_subindex_value + 5.138 * percent_renter_occ_units + 0.06893 * poverty.rate +  0.000697 * median.gross.rent + 0.00001356 * median.household.income - 0.000004625 * median.property.value

# Predicting GEOID = 53011041700, eviction_rate = 14.55133, predicted eviction_rate for my model is 4.244497 and it is 3.259135 using only the erap index
# Predicting GEOID = 53005011401, eviction_rate = 3.462364, predicted eviction_rate for my model is 3.172309 and it is 3.259135 using only the erap index
# Predicting GEOID = 53033009300, eviction_rate = 1.059322, predicted eviction_rate for my model is 2.239332 and it is 2.108245 using only the erap index
# Predicting GEOID = 53033001800, eviction_rate = 1.150086, predicted eviction_rate for my model is 2.236633 and it is 1.609526 using only the erap index
# Predicting GEOID = 53015001000, eviction_rate = 9.181331, predicted eviction_rate for my model is 5.936597 and it is 3.374224 using only the erap index

-1.194 + 0.006423 * 75 -0.005748 * 76 + 5.138 * 0.6676774 + 0.06893 * 22.9 +  0.000697 * 832 + 0.00001356 * 36042 - 0.000004625 * 147900

-1.194 + 0.006423 * 97 -0.005748 * 62 + 5.138 * 0.528006 + 0.06893 * 13.3 +  0.000697 * 539.87 + 0.00001356 * 38574.25 - 0.000004625 * 92835.88

-1.194 + 0.006423 * 11 -0.005748 * 94 + 5.138 * 0.6302953 + 0.06893 * 7.66 +  0.000697 * 1131 + 0.00001356 * 70458 - 0.000004625 * 347500


-1.194 + 0.006423 * 27 -0.005748 * 83 + 5.138 * 0.6052632 + 0.06893 * 3.85 +  0.000697 * 1276 + 0.00001356 *  87159 - 0.000004625 * 370200




-1.194 + 0.006423 * 94 -0.005748 * 56 + 5.138 * 0.7418244 + 0.06893 * 41.07 +  0.000697 *  623 + 0.00001356 * 21833 - 0.000004625 * 113300

#-1.194 + 0.006423 * income_subindex_value -0.005748 * hh_chars_subindex_value + 
#  5.138 * percent_renter_occ_units + 0.06893 * poverty.rate +  0.000697 * median.gross.rent + 0.00001356 * median.household.income 
#- 0.000004625 * median.property.value

# Predictions using erap model
# Predicted eviction_rate = -0.308624 + 0.038363 * index_value
-0.308624 + 0.038363 * 93
-0.308624 + 0.038363 * 93
-0.308624 + 0.038363 * 63
-0.308624 + 0.038363 * 50
-0.308624 + 0.038363 * 96


# Try model out for other states:
m10 <- lm(formula = eviction_rate ~ income_subindex_value + hh_chars_subindex_value + 
           percent_renter_occ_units + poverty.rate +  median.gross.rent + median.household.income + 
           median.property.value, data = oh_eviction_erap_tracts)
summary(m10)

m11 <- lm(formula = eviction_rate ~ income_subindex_value + hh_chars_subindex_value + 
           percent_renter_occ_units + poverty.rate +  median.gross.rent + median.household.income + 
           median.property.value, data = pa_eviction_erap_tracts)
summary(m11)
m12 <- lm(formula = eviction_rate ~ income_subindex_value + hh_chars_subindex_value + 
           percent_renter_occ_units + poverty.rate +  median.gross.rent + median.household.income + 
           median.property.value, data = il_eviction_erap_tracts)
summary(m12)
