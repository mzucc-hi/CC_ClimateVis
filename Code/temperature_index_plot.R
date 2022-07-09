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

# Looking at our SST data, we can see that we have an unused header and that NA values are represented by '***'. We also want only the year at this stage, so let's slim it.
read_csv("Data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  dplyr::select(Year, avg_diff = `J-D`) %>%
  ggplot(aes(x = Year, y = avg_diff)) +
  labs(title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX",
       subtitle = "Data Source: NASA's Goddart Institute for Space Studies (GISS). \nCredit: NASA/GISS",
       x = "Year",
       y = "Temperature Anomaly (C)",
       caption = R.version.string) +
  geom_hline(yintercept = 0, size = 0.5) +
  geom_line(aes(colour = "1"), size = 0.5, show.legend = F) +
  geom_point(fill = "white", aes(colour = "1"), shape = 21) +
  geom_smooth(se = F, aes(colour = "2"), size = 0.65, span = 0.15, show.legend = F) +
  scale_x_continuous(breaks = seq(1880, 2020, 20), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.75, 1.5), expand = c(0, 0)) +
  scale_colour_manual(name = NULL,
                      breaks = c(1, 2),
                      values = c("grey", "firebrick"),
                      labels = c("Annual Mean", "Loess Smoothing"),
                      guide = guide_legend(override.aes = list(shape = 15, size = 3))) +
  theme(
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(b = 10), colour = "firebrick", face = "bold"),
    plot.subtitle = element_text(size = 8, margin = margin (b = 10)),
    legend.position = c(0.15, 0.9),
    legend.title = element_text(size = 0)
    # legend.key.height = unit(10, "pt"),
    # legend.margin = margin(0, 0, 0, 0)
    )


ggsave("Figures/temperature_index_plot.jpg", width = 20, height = 15, units = "cm")



