## Data Visualization (GOVT16-QSS17) Winter 2019
## Intro to the Tidyverse, Chapters 3-4
##
## Name: Elise Rust
## Date: January 17, 2019

# Packages
library(tidyverse)
library(ggplot2)

# Data
undat <- read.csv("Data/un_data.csv") # Read in UN Data from Data folder
movies <- read.csv("Data/movies.csv") # Read in movies data from Data folder

##### Basic statistics and summaries with summarize --> UN dataset
# b. 
undat %>%
  summarize(median(gini_reported, na.rm = TRUE), median(q1, na.rm = TRUE), median(q5, na.rm = TRUE))

# c.
undat %>%
  filter(year == 2005) %>%
  summarize(min(gini_reported, na.rm = TRUE), min(q1, na.rm = TRUE), min(q5, na.rm = TRUE))
    

# d. 
undat %>%
  group_by(region_un) %>%
  summarize(median_gini_reported = median(gini_reported, na.rm = TRUE))

# e.
undat %>%
  filter(region_un == "Africa") %>%
  group_by(region_un_sub) %>%
  summarize(median_gini_reported = median(gini_reported, na.rm = TRUE))

####### Plot with ggplot2 --> using summarize commands
# a. Faceted scatter plot of mean GINI index for each country
undat %>%
  group_by(region_un, year) %>%
  summarize(mean_index = mean(gini_reported, na.rm = TRUE)) %>%
  ggplot(., aes(x = year, y = mean_index)) + 
  geom_point() +
  facet_wrap(~region_un)

# b. Faceted line plot of mean GINI index for non-OECD vs. OECD countries
undat %>%
  group_by(oecd, year) %>%
  summarize(median_index = median(gini_reported, na.rm = TRUE)) %>%
  ggplot(., aes(x = year, y = median_index)) + 
  geom_line() +
  facet_wrap(~oecd)

# c. Max GINI index for each country in Central Asia --> bar graph
undat %>%
  filter(region_un_sub == "Central Asia", year == 2005) %>%
  group_by(country, year) %>%
  summarize(max_index = max(gini_reported, na.rm = TRUE)) %>%
  ggplot(aes(x = country, y = max_index, fill = country)) +
  geom_col()

# d. Faceted line plot for median GINI index for each continent, colored by continent
undat %>%
  group_by(region_un, year) %>%
  summarize(median_index = median(gini_reported, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = median_index, fill = region_un)) +
  geom_col() +
  facet_wrap(~region_un)

# e. Statistical binning for a histogram
undat %>%
  ggplot(aes(x = d10)) +
  geom_histogram()

# f. Boxplot of GINI by continent --> colored by continent
undat %>%
  group_by(region_un) %>%
  ggplot(aes(x = region_un, y = gini_reported, color = region_un)) +
  geom_boxplot()

# g. 
undat %>%
  ggplot(aes(x = region_un, y = q1, color = region_un)) +
  geom_boxplot()


# 