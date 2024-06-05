Untitled
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
library(httr)
library(jsonlite)
library(sf)
```

    ## Linking to GEOS 3.9.3, GDAL 3.5.2, PROJ 8.2.1; sf_use_s2() is TRUE

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ purrr::flatten() masks jsonlite::flatten()
    ## ✖ dplyr::lag()     masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readr)
library(readxl)
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot
    ## 
    ## The following object is masked from 'package:httr':
    ## 
    ##     config
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(grid)

# Load data
seattle <- read_sf("seattle_erap_red_evic 3.geojson")

# Handle missing values by removing rows with NAs
seattle_clean <- seattle |>
  filter(!is.na(eviction_rate) & 
           !is.na(percent_cost_burdened_renters) & 
           !is.na(median.gross.rent) & 
           !is.na(percent_black) & 
           !is.na(percent_asian) & 
           !is.na(percent_latine) & 
           !is.na(percent_other) & 
           !is.na(index_value) & 
           !is.na(year) & 
           !is.na(county_name))
```

## Including Plots

You can also embed plots, for example:

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

``` r
# Combine all races into a long format
seattle_long <- seattle_clean |>
  gather(key = "races_nonWhite", value = "percent_races_nonWhite", percent_black, percent_asian, percent_latine, percent_other)

# Create the combined scatter plot using Plotly
combined_plot <- ggplot(seattle_long, aes(x = percent_races_nonWhite, y = eviction_rate, fill = races_nonWhite)) +
  geom_point(color = "black", alpha = 0.35, shape = 21, stroke = 0.5) +
  geom_jitter(alpha = 0.50) +
  labs(title = "Eviction Rate by Percent Population of All Races", x = "Percent Population", y = "Eviction Rate (per 1000)") +
  scale_y_continuous(limits = c(0,13)) +
  scale_x_continuous(limits = c(0, 1))

combined_plotly <- ggplotly(combined_plot)

# Display the combined Plotly plot separately
combined_plotly
```

![](markdown_Seattle_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Arrange the first set of plots in a grid
grid.arrange(
  arrangeGrob(
    grobs = list(plot1, plot2, plot3, plot4), 
    ncol = 2
  ),
  top = textGrob("Seattle Graphs", gp = gpar(fontsize = 20, font = 2))
)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](markdown_Seattle_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Arrange the second set of plots in a grid
grid.arrange(
  arrangeGrob(
    grobs = list(plot5, plot6, plot7, plot8, plot9), 
    ncol = 2
  ),
   top = textGrob("Seattle Graphs", gp = gpar(fontsize = 20, font = 2))
)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](markdown_Seattle_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
