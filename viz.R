library(tidyverse)

pop_vote_no_wts <- readRDS("Data/pop_vote_no_wts.RDS")
pop_vote_wts <- readRDS("Data/pop_vote_wts.RDS")

vote <- ifelse(pop_vote_wts > 0.5, "Biden", "Trump")
plot1_data <- data.frame(vote, pop_vote_wts)
plot1 <- ggplot(data = plot1_data) +
  geom_histogram(mapping = aes(pop_vote_wts, fill = vote), 
                 breaks = seq(0.475, 0.55, by = 0.005), col = "black") +
  scale_fill_manual(values = c("Trump" = "firebrick2", "Biden"="dodgerblue")) +
  labs(x = "Popular Vote", y = "Frequency", fill = "Winner",
       title = "Bootstrap Simulated Results of Popular Vote") +
  theme_light()
plot1

ggsave(plot1, file = "Plots/pop_vote_wts.png", width = 5, height = 5)
