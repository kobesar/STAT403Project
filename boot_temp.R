library(tidyverse)
set.seed(403)

data <- read.csv("Data/31119913_National2020.csv")

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
weight
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

# Boot with weight
biden_perc2 <- c()

for(i in 1:1000) {
  j <- sample(n, n, replace = T)
  samp <- data_pres_subset[j,]
  num_biden <- sum((samp$pres == "Joe Biden") * samp$weight)
  biden_perc2[i] <- num_biden / sum(samp$weight)
}

hist(biden_perc2)
abline(v = 0.5, col = "red", lwd = 2)
mean(biden_perc2 > 0.5)
