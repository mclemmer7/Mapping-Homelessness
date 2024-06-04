# Ensure necessary packages are installed and then load them
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(httr)
library(jsonlite)
library(sf)
library(tidyverse)
library(car)

Seattle_erap_redlining <- st_read("../data/Seattle_erap_redlining_dataset.geojson")

# Calculate percent minority based on asians, blacks, and latinos. Not including Native Americans, Pacific Islanders and other at the moment.
Seattle_erap_redlining$percent_minority <- Seattle_erap_redlining$percent_asian + Seattle_erap_redlining$percent_black + Seattle_erap_redlining$percent_latine

# Predictor is Grade, response could be percent minority or percent cost burdened
# Doesn't make sense to use grade as a response since it has to do with redlining from the past.

Seattle_erap_redlining$numerical_grade <- dplyr::recode(Seattle_erap_redlining$grade,
                                                 "A" = 1,
                                                 "B" = 2,
                                                 "C" = 3,
                                                 "D" = 4)


Seattle_erap_redlining$numerical_grade <- ifelse(is.na(Seattle_erap_redlining$numerical_grade), 0, Seattle_erap_redlining$numerical_grade)

# Try removing NA values instead. Could see more of a relationship
filtered_seattle <- Seattle_erap_redlining |>
  filter(!is.na(grade))

# Make some indicator variables with the grade categorical variable. Default is grade A
filtered_seattle$gradeB <- as.numeric(filtered_seattle$grade == "B")
filtered_seattle$gradeC <- as.numeric(filtered_seattle$grade == "C")
filtered_seattle$gradeD <- as.numeric(filtered_seattle$grade == "D")

# Can do anova with 1 for grade a and 0 for everything else
# Should just look at one city for this analysis since there are different percents of minorities in other cities
# Could have evictions as a response and have the redlining grade as one of the predictors in a multivariable model

# Can try having multiple indicator variables for grade to show the relationship between the grade of area and how many minorities currently live there.
# 

# Can try anova with numerical and categorical?
seattle_model <- lm(data = filtered_seattle, percent_minority ~ numerical_grade)
summary(seattle_model)

anova(seattle_model)

plot(data = filtered_seattle, percent_minority ~ numerical_grade)

seattle_model2 <- lm(data = filtered_seattle, percent_cost_burdened_renters ~ numerical_grade)
summary(seattle_model2)

plot(data = filtered_seattle, percent_cost_burdened_renters ~ numerical_grade)



# Model where the only predictors are grade indicator variables
seattle_model3 <- lm(data = filtered_seattle, percent_cost_burdened_renters ~ gradeB + gradeC + gradeD)
summary(seattle_model3)

# Predicting ERAP index with redlining grade
seattle_model4 <- lm(data = filtered_seattle, index_value ~ gradeB + gradeC + gradeD)
summary(seattle_model4)

# If a model without redlining grade predicts the ERAP index better than with it, the redlining grade isn't a good predictor of ERAP index.
# When redlining grade plays a factor, the adjusted r squared value is a little bit higher (0.7137 vs. 0.7088), showing that it is likely a better model.
seattle_model5 <- lm(data = filtered_seattle, index_value ~ gradeB + gradeC + gradeD + median_monthly_housing_cost)
summary(seattle_model5)

seattle_model6 <- lm(data = filtered_seattle, index_value ~ median_monthly_housing_cost)
summary(seattle_model6)

plot(data = filtered_seattle, index_value ~ numerical_grade)


# Now plot erap predicted by median_monthly_housing_cost, with color = grade
ggplot(data = filtered_seattle, aes(x=median_monthly_housing_cost, y=index_value, color=grade)) +
  geom_jitter(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Seattle Monthly Housing Cost and Redlining Grade") +
  ylab("ERAP Index Value") +
  xlab("Median Monthly Housing Cost")
