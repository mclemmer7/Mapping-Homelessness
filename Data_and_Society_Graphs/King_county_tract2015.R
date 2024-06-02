library(readr)
library(tidyverse)
library(jsonlite)
library(sf)
library(dplyr)
library(mdsr)

Wa_tracts_data <- read_csv("WA_tracts.csv")
View(WA_tracts)

Wa_tracts_data <- Wa_tracts_data |> 
  separate(parent.location, into = c("Washington_counties", "state"), sep = ", ") |> 
  select(-state) 

king_tracts <- Wa_tracts_data |> 
  filter(Washington_counties == "King County")

king_tracts_2015 <- king_tracts |> 
  filter(king_tracts$year == "2015")

# Summary Statistics
summary_stats <- king_tracts_2015 %>%
  summarise(
    avg_population = mean(population, na.rm = TRUE),
    avg_poverty_rate = mean(poverty.rate, na.rm = TRUE),
    avg_renter_occupied = mean(pct.renter.occupied, na.rm = TRUE),
    avg_rent_burden = mean(rent.burden, na.rm = TRUE),
    avg_eviction_rate = mean(eviction.rate, na.rm = TRUE)
  )
print(summary_stats)

# Correlation Analysis
cor_matrix <- king_tracts_2015 %>%
  select(poverty.rate, pct.renter.occupied, median.gross.rent, median.household.income, median.property.value, rent.burden, eviction.rate) %>%
  cor(use = "complete.obs")

print(cor_matrix)



# Visualization: Scatter plot of Eviction Rate vs. Poverty Rate
ggplot(data = king_tracts_2015, aes(x = poverty.rate, y = eviction.rate)) +
  geom_point(size = 3, color = "black", alpha = 0.5 ) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Eviction Rate vs. Poverty Rate in King County (2015)",
       x = "Poverty Rate",
       y = "Eviction Rate") +
  theme_minimal()

# Visualization: Distribution of Poverty Rate
ggplot(data = king_tracts_2015, aes(x = poverty.rate)) +
  geom_histogram(binwidth = 2.000, fill = "orange", color = "black") +
  labs(title = "Distribution of Poverty Rate in King County (2015)",
       x = "Poverty Rate",
       y = "Frequency") +
  theme_minimal()

# Visualization: Boxplot of Rent Burden by Renter Occupied Percentage
ggplot(data = king_tracts_2015, aes(x = as.factor(pct.renter.occupied), y = rent.burden)) +
  geom_boxplot() +
  labs(title = "Rent Burden by Renter Occupied Percentage in King County (2015)",
       x = "Renter Occupied Percentage",
       y = "Rent Burden") +
  theme_minimal()
