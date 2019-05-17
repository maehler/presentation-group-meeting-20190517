library(tidyverse)
library(here)

data <- read_csv("../data/buscos.csv") %>% 
  gather(key, value, -assembly)

transparent_theme <- theme(
  axis.text = element_text(colour = "#EEEEEE"),
  axis.title = element_text(colour = "#EEEEEE"),
  axis.line = element_line(colour = "#EEEEEE"),
  axis.ticks = element_line(colour = "#EEEEEE"),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  legend.text = element_text(colour = "#EEEEEE"),
  legend.title = element_text(colour = "#EEEEEE"),
  plot.background = element_rect(fill = "transparent", colour = "transparent"),
  panel.background = element_rect(fill = "transparent", colour = "transparent"),
  legend.background = element_rect(fill = "transparent", colour = "transparent"))

ggplot(data, aes(assembly, value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2", name = "") +
  labs(x = "", y = "Percentage") +
  transparent_theme

ggsave("../img/buscos.png",
       dpi = 300, width = 8, height = 5, bg = "transparent")
