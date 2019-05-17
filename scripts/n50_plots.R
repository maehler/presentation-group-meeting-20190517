library(tidyverse)
library(here)

data <- read_csv(here("../data/n50.csv")) %>% 
  mutate(type = factor(type, levels = c("read", "contig", "scaffold")),
         assembly = factor(assembly, levels = c("Spruce v1", "Axolotl", "Spruce v3 wtdbg", "Spruce v3 wtdbg2")))

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

plot_bars <- function(data, name = NULL) {
  p <- data %>% 
    mutate(type = paste0(toupper(str_sub(type, 1, 1)), str_sub(type, 2)),
           type = factor(type, levels = c("Read", "Contig", "Scaffold"))) %>% 
    ggplot(aes(assembly, n50, fill = type)) +
    scale_y_continuous(labels = function(x) format(x, justify = "right", width = 10)) +
    scale_fill_brewer(palette = "Dark2", type = "qual", name = "") +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
    labs(x = "", y = "N50 (kbp)") +
    transparent_theme
  if (!is.null(name)) {
    ggsave(filename = paste0(here("../img/n50_"), name, ".png"),
           bg = "transparent", dpi = 300, height = 5, width = 8,
           plot = p)
  }
  p
}

data %>%
  mutate(n50 = case_when(type != "read" | assembly != "Spruce v1" ~ 0, TRUE ~ n50)) %>% 
  plot_bars(1)

data %>% 
  mutate(n50 = case_when(type == "read" & assembly %in% c("Axolotl", "Spruce v1") ~ n50,
                         TRUE ~ 0)) %>% 
  plot_bars(2)

data %>% 
  mutate(n50 = case_when(type == "read" ~ n50, TRUE ~ 0)) %>% 
  plot_bars(3)

data %>% 
  mutate(n50 = case_when(type == "read" ~ n50,
                         type == "contig" & assembly == "Spruce v1" ~ n50,
                         TRUE ~ 0)) %>% 
  plot_bars(4)

data %>% 
  mutate(n50 = case_when(type == "read" ~ n50,
                         type == "contig" & assembly %in% c("Spruce v1", "Axolotl") ~ n50,
                         TRUE ~ 0)) %>% 
  plot_bars(5)

data %>% 
  mutate(n50 = case_when(type == "read" ~ n50,
                         type == "contig" ~ n50,
                         TRUE ~ 0)) %>% 
  plot_bars(6)

data %>% 
  plot_bars(7)
  
