## Elise Rust
## QSS 17
## 7 February 2019
## Extra Credit - wayfair graph

library(tidyverse)
library(RColorBrewer)
install.packages("ggthemes")
library(ggthemes)
install.packages("extrafont")
library(extrafont)
font_import
library(grid)

england <- read_csv("Playfair.csv", na = c(" ", "NA", ""))

str(england)
head(england)
tail(england)


ggplot(england) +
  geom_line(aes(x = Year, y = Exports), size = 2, color = "red", alpha = .6) +
  geom_line(aes(x = Year, y = Exports), size = .5, color = "black") +
  geom_text(aes(x = 1735, y = 12.5, label = "Line Representing Exports", family = "serif", fontface = "italic")) +
  geom_line(aes(x = Year, y = Imports), size = 2, color = "yellow", alpha = .6) +
  geom_line(aes(x = Year, y = Imports), size = .5, color = "black") +
  geom_text(aes(x = 1740, y = 7, label = "Line Representing Imports", family = "serif", fontface = "italic")) +
  geom_ribbon(aes(x = Year, ymin = Imports, ymax = Exports), alpha = .5, fill = "#69babe") +
  theme(plot.background = element_rect(fill = "#FFF1BB", color = "black", size = 2),
        panel.background = element_rect(fill = "#FFF1BB", color = "black", size = 4),
        panel.grid.major = element_line(colour = "black",size = rel(0.5)),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 17, family = "serif", face = "italic", hjust = .5),
        plot.subtitle = element_text(size = 17, family = "serif", face = "italic", hjust = .5),
        axis.title.x = element_text(size = 13,  family = "serif", face = "italic"),
        axis.text = element_text(size = 8, family = "serif", face = "italic"),
        plot.caption = element_text(size = 9, family = "serif", face = "italic")) +
  labs(x = "The Divisions at the Bottom, express YEARS & those on the Right hand, MILLIONS of POUNDS",
      y = " ",
      title = "Chart of all the Imports and Exports to and from England",
      subtitle = "From the Year 1700 to 1782 by W. Playfair",
      caption = "Published as the Act directs. 20th Aug. 2785") +
  geom_vline(xintercept = 1771:1784) +
  scale_x_continuous(breaks = c(1700, 1710, 1720, 1730, 1740, 1750, 1760, 1770, 1775, 1780, 1785)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), 
                     position = "right") +
  annotate(geom = "text", x = 1787.5, y = 19.5, label = "Millions", color = "black", family = "serif") +
  annotate(geom = "text", x = 1740, y = 9, label = "BALANCE IN FAVOR OF ENGLAND", color = "black",
           family = "serif", size = 6, fontface = "italic")