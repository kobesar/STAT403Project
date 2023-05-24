library(tidyverse)
set.seed(403)

data <- read.csv("../Data/31119913_National2020.csv")

cols <- str_split("age10
sex
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
  
  for (i in 1:500) {
    j <- sample(n, n, replace = T)
    samp <- temp[j,]
    num_biden <- sum((samp$pres == "Joe Biden") * samp$weight)
    biden_perc_state[i] <- num_biden / sum(samp$weight)
  }
  
  state_boot[[state]] <- mean(biden_perc_state > 0.5)
}

saveRDS(state_boot, file = "state_boot.RDS")
