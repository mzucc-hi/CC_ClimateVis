## VIDEO FIVE

library(tidyverse)
library(gamlss) # for model fitting
library(ggstream) # Stream plots
library(ggridges) # For drawing ridge/density plots
library(ggthemes) # Extra ggplot themes
library(ggtext) 
library(extrafont) # Change plot fonts
library(showtext)
library(ggpubr) # Extra ggplot tools 
library(e1071) # Misc functions/stats
library(lubridate) # Working with dates
library(kableExtra) # Make nice tables
library(huxtable)
library(flextable)
library(cowplot)
library(grid)
library(GGally)
library(scales)
library(glue)


## What we are aiming to do here is to work from the plot from video 4, but representing them in polar (instead of
## Cartesian) coordinates


# Data

t_diff <- read_csv("Data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  dplyr::select(Year, month.abb) %>%
  # drop_na() %>%
  pivot_longer(-Year, names_to = "month", values_to = "t_diff") %>%
  mutate(month = factor(month, levels = month.abb)) # organise months so that they are in date (not alph) order


# What we also want is to have lines that match December values with the next year's Jan value (cont. line)

next_Jan <- t_diff %>%
  filter(month == "Jan") %>%
  mutate(Year = Year - 1,
         month = "next_Jan")

# Plot

## Make new dataframe first

t_data <- bind_rows(t_diff, next_Jan) %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) # Re-order to make sense
 
## Adding lines to show temperature thresholds to the plot
temp_lines <- tibble(
  x = 12,
  y = c(1.5, 2.0),
  label = c("+1.5\u00B0C", "+2.0\u00B0C")
)
  
## Creating month labels that allow for individual altering of label angles, as in original diagram

month_labels <- tibble(
  x = 1:12,
  label = month.abb,
  y = 2.7
)


## Diagram 

t_data %>%
    ggplot(aes(x = month_number, y = t_diff, group = Year, colour = Year)) +
    geom_col(data = month_labels,
             aes(x = x + 0.5, y = 2.4),
             fill = "black",
             colour = "black",
             inherit.aes = F,
             width = 1) + # Get black background like in original figure
    geom_col(data = month_labels,
             aes(x = x + 0.5, y = -2),
             fill = "black",
             colour = "black",
             inherit.aes = F,
             width = 1) + # Get black background like in original figure, covering middle of plot
    geom_hline(yintercept = c(1.5, 2.0), colour = "white") + # Creation of 1.5 and 2C threshold lines
    geom_line(size = 0.4, alpha = 0.8) +
    geom_label(data = temp_lines,
               aes(x = x, y = y, label = label),
               inherit.aes = F,
               colour = "white",
               fill = "black",
               label.size = 0) + # Labelling threshold lines
    geom_text(data = month_labels,
              aes(x = x, y = y, label = label),
              inherit.aes = F,
              colour = "black",
              angle = seq(360 - 360/12, 0, length.out = 12)) + # Creating month labels that allow for individual altering of label angles, as in original diagram
    geom_text(aes(x = 1, y = -1.3, label = "2022"),
              size = 5) + # Central '2022' label
    scale_y_continuous(breaks = seq(-2, 2, 0.2),
                       limits = c(-2, 2.7),
                       expand = c(0, -0.7)) +
    scale_x_continuous(breaks = 1:12,
                       labels = month.abb) +
    coord_polar(start = 2*pi/12) +
    scale_colour_viridis_c(breaks = seq(1880, 2020, 20),
                           guide = "none") +
    labs(title = "Global Temperature Change Anomalies by \nMonth (1880-2022)",
         subtitle = "With yellow hue representing increasingly recent years",
         x = NULL,
         y = NULL,
         caption = paste(R.version.string, "\nGISTEMPv4", "\nBaseline 1951-1980")) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", colour = "white"),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text = element_text(colour = "black"),
      axis.text.x = element_blank(),
      text = element_text(colour = "black"),
      axis.line = element_blank(),
      plot.subtitle = element_text(margin = margin(t = 7, b = 7, unit = "pt")),
      plot.caption = element_text(face = "plain", colour = "black")
    )

ggsave("Figures/climate_spiral.jpg", width = 15, height = 18, units = "cm")

