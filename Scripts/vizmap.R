library(tidyverse)
library(maps)
library(usmap)

data <- readRDS("../data/state_boot.RDS")

data_state <- data.frame(biden = as.factor(unlist(data)))
data_state$state <- str_to_lower(trimws(rownames(data_state)))
rownames(data_state) <- NULL

# Create the state map plot

plot <- plot_usmap(data = data_state, values = "biden", color = "white", labels = TRUE, label_color = "gray100") +
  scale_fill_manual(values = c("0" = "#d23368", "1"="#2a3990"), labels = c("Trump", "Biden")) +
  labs(x = "", y = "", fill = "") +
  theme_light() +
  theme(
    text = element_text(family = "roboto"),
    legend.position = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text = element_text(size = 14),
    panel.border = element_blank(), panel.grid = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank())  # Remove axis labels

ggsave(plot, file = "../Plots/state_boot_map.png", width = 5, height = 5)
