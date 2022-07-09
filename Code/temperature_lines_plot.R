## VIDEO FOUR

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


# Data

t_diff <- read_csv("Data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  dplyr::select(Year, month.abb) %>%
  # drop_na() %>%
  pivot_longer(-Year, names_to = "month", values_to = "t_diff") %>%
  mutate(month = factor(month, levels = month.abb)) # organise months so that they are in date (not alph) order


# What we also want is to have lines that show us Dec values before the Jan ones and Jan ones after the Dec ones, 
# so that the plot doesn't abruptly end but rather has a continuous line

# We can do this by making three separate data frames with i) Jan values, ii) Dec values and iii) rest.

last_Dec <- t_diff %>%
  filter(month == "Dec") %>%
  mutate(Year = Year + 1,
         month = "last_Dec")

next_Jan <- t_diff %>%
  filter(month == "Jan") %>%
  mutate(Year = Year - 1,
         month = "next_Jan")

  
# Plot

## Make new dataframe first

t_data <- bind_rows(last_Dec, t_diff, next_Jan) %>%
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1,
         this_year = Year == 2022) # Re-order to make sense

annotation <- t_data %>%
  drop_na() %>%
  filter(Year == 2022) %>%
  slice_max(month)
  
t_data %>%
    ggplot(aes(x = month_number, y = t_diff, group = Year, colour = Year, size = this_year)) +
    geom_hline(yintercept = 0, colour = "white") +
    geom_line() +
    geom_text(data = annotation,
              aes(x = month_number, y = t_diff, colour = Year, label = Year),
              inherit.aes = F,
              size = 5,
              hjust = 0,
              nudge_x = 0.15,
              fontface = "bold") +
    scale_y_continuous(breaks = seq(-2, 2, 0.2),
                       sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_x_continuous(breaks = 1:12,
                       labels = month.abb,
                       sec.axis = dup_axis(name = NULL, labels = NULL)) +
    coord_cartesian(xlim = c(1, 12)) +
    scale_colour_viridis_c(breaks = seq(1880, 2020, 20),
                           guide = guide_colorbar(frame.colour = "white",
                                                  frame.linetype = 1)) +
    scale_size_manual(breaks = c(F, T),
                      values = c(0.25, 1),
                      guide = "none") +
    labs(title = "Global Temperature Change Since 1880 by Month",
         x = NULL,
         y = "Temperature change since pre-industrial times [\u00B0C]",
         caption = R.version.string) +
    theme(
      legend.position = "right",
      panel.background = element_rect(fill = "black", colour = "white", size = 1),
      plot.background = element_rect(fill = "#444444"),
      legend.background = element_rect(fill = "#444444"),
      axis.ticks = element_line(colour = "white"),
      axis.ticks.length = unit(-5, "pt"),
      axis.text = element_text(colour = "white"),
      text = element_text(colour = "white"),
      axis.line = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.key.height = unit(2.4, "cm")
    )

ggsave("Figures/temperature_month_lines.jpg", width = 20, height = 14, units = "cm")

