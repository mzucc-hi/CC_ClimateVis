## VIDEO THREE

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

# theme_set(
#   theme_pubr() +
#     theme(plot.title = element_text(face = "bold"),
#           plot.subtitle = element_text(size = 10),
#           plot.caption = element_text(size = 8, face = "italic"),
#           axis.title.x = element_text(size = 9.5),
#           axis.title.y = element_text(size = 9.5),
#           axis.text.x = element_text(size = 9),
#           axis.text.y = element_text(size = 9),
#           legend.title = element_text(size = 9, face = "bold"),
#           legend.text = element_text(size = 9),
#           legend.key.size = unit(0.8, "line"))
# )


# Data

t_data <- read_csv("Data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  dplyr::select(Year, t_diff = `J-D`) %>%
  drop_na()

# Plot

t_data %>%
  ggplot(aes(x = Year, y = 1, fill = t_diff)) +
    geom_tile(show.legend = F) +
    scale_fill_stepsn(colours = c("#08306B", "white", "#67000D"),
                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                      n.breaks = 12) +
    coord_cartesian(expand = F) +
    scale_x_continuous(breaks = seq(1890, 2020, 30)) +
    labs(title = glue("Global Temperature Change ({min(t_data$Year)} - {max(t_data$Year)})"),
         caption = R.version.string) +
    theme_void() +
    theme(
      axis.text.x = element_text(colour = "white", margin = margin(t = 5, b = 10, unit = "pt")),
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(colour = "white", margin = margin(b = 5, t = 10, unit = "pt"),
                                hjust = 0.05,
                                size = 13,
                                face = "bold"),
      plot.caption = element_text(colour = "white")
    )


# Save

ggsave("Figures/warming_stripest.jpg", width = 10, height = 6, units = "cm")







