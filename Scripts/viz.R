library(tidyverse)

pop_vote_no_wts <- readRDS("../Data/pop_vote_no_wts.RDS")
pop_vote_wts <- readRDS("../Data/pop_vote_wts.RDS")

vote <- ifelse(pop_vote_wts > 0.5, "Biden", "Trump")
plot1_data <- data.frame(vote, pop_vote_wts)
# plot1 <- ggplot(data = plot1_data) +
#   geom_histogram(mapping = aes(pop_vote_wts, fill = vote), 
#                  breaks = seq(0.475, 0.55, by = 0.005), col = "black") +
#   scale_fill_manual(values = c("Trump" = "firebrick2", "Biden"="dodgerblue")) +
#   labs(x = "Popular Vote", y = "Frequency", fill = "Winner",
#        title = "Bootstrap Simulated Results of Popular Vote") +
#   theme_light() +
#   theme(
#     text = element_text(family = "roboto")
#   )
# plot1

plot1 <- ggplot(data = plot1_data) +
  geom_vline(xintercept = 0.5, linetype = 2, color = "grey60") +
  geom_histogram(mapping = aes(pop_vote_wts, fill = vote),
                 breaks = seq(0.475, 0.55, by = 0.005), col = "black") +
  geom_text(data = data.frame(x = 0.497, y = 180), aes(x = x, y = y, label = "Trump Wins"), color = "#d23368") +
  geom_text(data = data.frame(x = 0.503, y = 180), aes(x = x, y = y, label = "Biden Wins"), color = "#2a3990") +
  geom_segment(data = data.frame(
    x1 = 0.498,
    y1 = 175,
    x2 = 0.496,
    y2 = 175
  ), aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.2, "cm"), type="closed"),
               size = 0.5, color = "#d23368") +
  geom_segment(data = data.frame(
    x1 = 0.502,
    y1 = 175,
    x2 = 0.504,
    y2 = 175
  ),aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.2, "cm"), type="closed"),
               size = 0.5, color = "#2a3990") +
  ylim(c(0,190)) +
  scale_fill_manual(values = c("Trump" = "#d23368", "Biden"="#2a3990")) +
  labs(x = "Popular Vote", y = "Frequency", fill = "Winner",
       title = "Bootstrap Simulated Results of Popular Vote") +
  theme_minimal() +
  theme(
    text = element_text(family = "roboto"),
    legend.position = "none"
  )
plot1

ggsave(plot1, file = "../Plots/pop_vote_wts2.png", width = 16, height = 8, units = "in")
