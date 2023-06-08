library(tidyverse)
library(usmap)

data <- read.csv("../data/1976-2020-president.csv")

data_2020 <- data %>% 
  filter(year == 2020) %>% 
  group_by(state) %>% 
  arrange(-candidatevotes) %>% 
  mutate(n = 1:n(), biden = as.factor(ifelse(party_detailed == "DEMOCRAT", 1, 0))) %>% 
  filter(n == 1) %>% 
  select(-n)

plot <- plot_usmap(data = data_2020, values = "biden", color = "white", labels = TRUE, label_color = "gray100") +
  scale_fill_manual(values = c("0" = "#d23368", "1"="#2a3990"), labels = c("Trump", "Biden")) +
  labs(x = "", y = "", fill = "", title = "") +
  theme_light() +
  theme(
    # text = element_text(family = "roboto"),
    legend.position = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.text = element_text(size = 14),
    panel.border = element_blank(), panel.grid = element_blank(), 
    axis.text.x = element_blank(), axis.text.y = element_blank())  # Remove axis labels

ggsave(plot, file = "../Plots/state_map.png", width = 8, height = 6)

