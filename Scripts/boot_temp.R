library(tidyverse)
set.seed(403)

data <- read.csv("../Data/31119913_National2020.csv")

cols <- str_split("age
weight
sex
weight
educ18
income20
racism20
life
party
stanum
sizeplac
pres
vote2016
votemeth
abortion
climatec", "\n")[[1]]

data_subset <- data %>% 
  select(cols)

data_pres_subset <- data_subset %>%
  filter(pres != " ")

# Initial boot of percent Biden
n <- nrow(data_pres_subset)
biden_perc <- c()

for(i in 1:1000) {
  j <- sample(n, n, replace = T)
  samp <- data_pres_subset[j,]
  num_biden <- sum(samp$pres == "Joe Biden")
  biden_perc[i] <- num_biden / n
}

hist(biden_perc)
mean(biden_perc > 0.5)
saveRDS(biden_perc, file = "pop_vote_no_wts.RDS")

# Boot with weight
biden_perc2 <- c()

for(i in 1:1000) {
  j <- sample(n, n, replace = T)
  samp <- data_pres_subset[j,]
  num_biden <- sum((samp$pres == "Joe Biden") * samp$weight)
  biden_perc2[i] <- num_biden / sum(samp$weight)
  print(sum(samp$weight))
}

hist(biden_perc2)
abline(v = 0.5, col = "red", lwd = 2)
mean(biden_perc2 > 0.5)
saveRDS(biden_perc2, file = "pop_vote_wts.RDS")

# This took like 1 hour to run lol

state_boot <- list()

for (state in unique(data_pres_subset$stanum)) {
  print(state)
  temp <- data_pres_subset %>% 
    filter(stanum == !!state)
  
  n <- nrow(temp)
  
  biden_perc_state <- c()
  
  for (i in 1:10000) {
    j <- sample(n, n, replace = T)
    samp <- temp[j,]
    num_biden <- sum((samp$pres == "Joe Biden") * samp$weight)
    biden_perc_state[i] <- num_biden / sum(samp$weight)
  }
  
  state_boot[[trimws(state)]] <- mean(biden_perc_state > 0.5)
}

saveRDS(state_boot, file = "state_boot.RDS")

<<<<<<< HEAD
state_boot_diff <- data.frame(state = unique(trimws(data_pres_subset$stanum)))

for (sample_size in c(100, 1000, 10000, 100000)) {
  state_biden <- list()
  for (state in unique(data_pres_subset$stanum)) {
    print(state)
    temp <- data_pres_subset %>% 
      filter(stanum == !!state)
    
    n <- nrow(temp)
    
    biden_perc_state <- c()
    
    for (i in 1:sample_size) {
      j <- sample(n, n, replace = T)
      samp <- temp[j,]
      num_biden <- sum((samp$pres == "Joe Biden") * samp$weight)
      biden_perc_state[i] <- num_biden / sum(samp$weight)
    }
    
    state_biden[[trimws(state)]] <- mean(biden_perc_state > 0.5)
  }
  
  state_boot_diff <- cbind(state_boot_diff, unlist(state_biden))
}

rownames(state_boot_diff) <- NULL

colnames(state_boot_diff) <- c("state", "N100", "N1000", "N10000", "N100000")

saveRDS(state_boot_diff, file = "state_boot_samp.RDS")

age_boot <- list()

for(age_group in unique(data_pres_subset$age)) {
  temp <- data_pres_subset %>%
    filter(age == !!age_group)
  n <- nrow(temp)
  biden_perc_age <- c()
  for(i in 1:500) {
    j <- sample(n, n, replace = T)
    samp <- temp[j,]
    num_biden <- sum((samp$pres == "Joe Biden") * samp$weight)
    biden_perc_age[i] <- num_biden / sum(samp$weight)
  }
  age_boot[[age_group]] <- biden_perc_age
}

age_boot <- data.frame(age_boot) %>%
  select(-X.)
age_boot <- pivot_longer(age_boot, cols = c(X45.64, X18.29, X30.44, X65.), names_to = "Age", values_to = "Percent")

age_plot <- ggplot(data = age_boot) +
  geom_density(mapping = aes(x = Percent, fill = Age), alpha = 0.5) +
  labs(x = "Proportion") +
  theme_light()
ggsave(age_plot, file = "../Plots/age_pop_vote_wts.png", width = 5, height = 5)
