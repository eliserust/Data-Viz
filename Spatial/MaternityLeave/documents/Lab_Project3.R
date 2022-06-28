## Elise Rust
## Data Visualization 2019
## February 19, 2019
## Lab Project 3

rm(list = ls())

library(tidyverse)
library(readxl)
install.packages("mapproj")
library(gridExtra)

# load datasets
mat_leave <- read_excel("Lab3/data/Maternity leave benefits.xls", 
                        na = "TRUE", skip = 2, n_max = 184)

undat <- read_csv("WIID_19Dec2018.csv", na = c(" ", "NA", ""))

# map data dataset
world_coord <- map_data("world")
colnames(world_coord) <- c("long", "lat", "group", "order", "country", "subregion")

# tidy up mat_leave dataset
colnames(mat_leave) <- c("country", "2", "length", "type", "5", "6", "percent_wage", "8", "9", "provider")

mat_leave <- mat_leave %>%
  select(country, length, type, percent_wage, provider) %>%
  mutate(length = parse_number(length))

mat_leave$days <- mat_leave$length
mat_leave$days[mat_leave$type == "months"] <- (mat_leave$length * 30)
mat_leave$days[mat_leave$type == "weeks"] <- (mat_leave$length * 7)
mat_leave$days[mat_leave$type == "years"] <- (mat_leave$length * 365)

mat_leave$country[mat_leave$country == "United States of America"] <- "USA"
mat_leave$country[mat_leave$country == "Russian Federation"] <- "Russia"
undat$country[undat$country == "United States"] <- "USA"

mat_leave$percent_wage[mat_leave$percent_wage == "…"] <- 0

mat_leave2 <- mat_leave %>%
  select(country, days, percent_wage, provider) %>%
  mutate(percent_wage = parse_number(percent_wage))

# tidy up UN dataset
undat2 <- undat %>%
  select(country, gdp_ppp_pc_usd2011) %>%
  group_by(country) %>%
  summarize(median_gdp = median(gdp_ppp_pc_usd2011, na.rm = TRUE))

# merge datasets
mat_world <- full_join(mat_leave2, undat2, by = "country")
mat_world2 <- full_join(mat_world, world_coord, by = "country")

# clean up world dataset
mat_world2 <- mat_world2 %>%
  filter(country != "Antarctica") %>%
  mutate(leave = case_when(
    days == 0 ~ "No Leave",
    between(days, 1, 21) ~ "Less than 3 weeks",
    between(days, 22, 91) ~ "3 - 13 weeks",
    between(days, 92, 175) ~ "14 - 25 weeks",
    between(days, 176, 350) ~ "26 - 50 weeks",
    TRUE ~ "More than 50 weeks"
 )) %>%
  mutate(paid = case_when(
    percent_wage == 0 ~ "Unpaid",
    between(percent_wage, 1, 25) ~ "Up to 25%",
    between(percent_wage, 26, 50) ~ "Up to 50%",
    between(percent_wage, 51, 75) ~ "Up to 75%",
    TRUE ~ "Fully Paid For"
  ))

head(mat_world2)
levels(mat_world2$leave) #character, not factor
levels(mat_world2$fact_leave)

mat_world2$leave_fact <- factor(mat_world2$leave, ordered = TRUE, levels = c("No Leave", "Less than 3 weeks",
                                                            "3 - 13 weeks", "14 - 25 weeks", "26 - 50 weeks", "More than 50 weeks"))
levels(mat_world2$leave_fact)

mat_world2$paid_fact <- factor(mat_world2$paid, ordered = TRUE, 
                               levels = c("Unpaid", "Up to 25%", "Up to 50%", "Up to 75%", "Fully Paid For"))

# theme for all maps
world_theme <- theme(
  plot.background = element_blank(),
  panel.background = element_blank(),
  plot.title = element_text(size = 17, family = "Helvetica", face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 10, family = "Helvetica", face = "italic", hjust = 0.5),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  panel.grid = element_blank(),
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.title = element_blank(),
  legend.spacing.x = unit(.12, 'cm'),
  plot.caption = element_text(size = 9, family = "Helvetica", face = "italic"),
  plot.margin = margin(1, 0, 0, 0, "cm")
)

# map of global maternity leave
maternity <- ggplot(mat_world2, aes(x = long, y = lat, fill =  leave_fact, group = group)) +
  geom_polygon() +
  world_theme +
  labs(
    title = "Global Maternity Leave",
    subtitle = "Offered length in weeks",
    caption = "Source: UN Dataset (http://data.un.org/DocumentData.aspx?id=344)"
  ) +
  scale_fill_manual(values = c("#ce2f2f", "#b3cde0", "#6497b1", "#005b96", "#03396c"))

# map of global percent paid maternity leave
paid <- ggplot(mat_world2, aes(x = long, y = lat, fill = paid_fact, group = group)) +
  geom_polygon() +
  world_theme +
  labs(
    title = "Maximum Percentage of Wages Covered During Leave",
    subtitle = "America falls behind as one of the 8 remaining countries without paid leave",
    caption = "Source: UN Dataset (http://data.un.org/DocumentData.aspx?id=344)"
  ) +
  scale_fill_manual(values = c("#ce2f2f", "#b3cde0", "#6497b1", "#005b96", "#03396c"))

# arrange these on a grid for comparison
grid.arrange(maternity, paid, ncol = 1)

# -------------------------------
# For further exploration: map of global gdp
ggplot(mat_world2, aes(x = long, y = lat, fill = median_gdp, group = group)) +
  geom_polygon() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 17, family = "Helvetica", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, family = "Helvetica", face = "italic", hjust = 0.5),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10, family = "Helvetica", face = "bold"),
    legend.spacing.x = unit(.12, 'cm'),
    plot.caption = element_text(size = 9, family = "Helvetica", face = "italic"),
    plot.margin = margin(1, 0, 0, 0, "cm")
  ) +
  labs(
    title = "Median GDP by Country",
    caption = "Source: UN Dataset",
    fill = "Median GDP") +
  scale_colour_brewer(palette = "Greens")
