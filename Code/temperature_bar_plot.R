## VIDEO ONE

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

theme_set(
  theme_pubr() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 8, face = "italic"),
          axis.title.x = element_text(size = 9.5),
          axis.title.y = element_text(size = 9.5),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 9),
          legend.key.size = unit(0.8, "line"))
)


# Data
t_data <- read_csv("Data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  dplyr::select(Year, t_diff = `J-D`) %>%
  drop_na() %>%
  print()

# Annotation
annotation <- t_data %>%
  arrange(Year) %>%
  slice(1, n()) %>% # select first and last year for annotation
  mutate(t_diff = 0,
         x = Year + c(-6, 6)) %>% # Adding a vector to be able to bump labels off of the axis
  print()

# Plot
t_data %>%
  ggplot(aes(x = Year, y = t_diff)) +
    geom_col(aes(fill = t_diff,
                 colour = t_diff),
             show.legend = F) +
    geom_text(data = annotation, aes(x =  x, label = Year, fontface = "bold"), colour = "white") +
    geom_text(x = 1870, y = 1, hjust = 0, colour = "white",
              label = glue("Global Temperatures Have Increased by Over {max(t_data$t_diff)}\u00B0C Since 1880"),
              aes(fontface = "bold"),
              size = 4.65) +
    scale_fill_stepsn(colours = c("darkblue", "white", "darkred"),
                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                      limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                      n.breaks = 15) +
    scale_colour_stepsn(colours = c("darkblue", "white", "darkred"),
                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                      limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                      n.breaks = 15) +
    # scale_fill_gradientn(colours = c("darkblue", "white", "darkred"),
    #                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
    #                      limits = c(min(t_data$t_diff), max(t_data$t_diff))) +
    # scale_fill_gradient2(low = "dark blue", mid = "white", high = "darkred",
    #                     midpoint = 0,
    #                     limits = c(-0.5, 1.5)) + # doesn't work
    labs(caption = R.version.string) +
    theme_void() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.background = element_rect(fill = "grey7"),
          plot.caption = element_text(colour = "white"))

# Save plot

ggsave("Figures/temperature_bar_plot.jpg", width = 18, height = 12, units = "cm")


