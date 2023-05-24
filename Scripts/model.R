library(tidyverse)
library(glmnet)

cols <- str_split("age
sex
educ18
income20
racism20
life
region
sizeplac
pres
votemeth
abortion
facemask
lgbt
climatec", "\n")[[1]]

data <- read.csv("../Data/31119913_National2020.csv")

data_subset <- data[, cols]

# data_subset[data_subset == " "] <- NA

X <- model.matrix(pres ~ ., data_subset)[,-1]
Y <- data_subset$pres == "Joe Biden"

data_full <- data.frame(cbind(Y, X))

mod <- glm(Y ~ ., family = "binomial", data = data_full)

summary(mod)

mod_step <- step(mod, direction = "backward")

summary(mod_step)

cols_2 <- str_split("age
income20
racism20
life
region
sizeplac
pres
votemeth
abortion
facemask
climatec", "\n")[[1]]

data_subset_2 <- data[, cols_2]

# data_subset[data_subset == " "] <- NA

X_2 <- model.matrix(pres ~ ., data_subset_2)[,-1]
Y_2 <- data_subset_2$pres == "Joe Biden"

data_full_2 <- data.frame(cbind(Y_2, X_2))

mod_2 <- glm(Y_2 ~ ., family = "binomial", data = data_full_2)

summary(mod_2)

mod_3 <- mod_step

mod_step_2 <- step(mod, direction = "backward", k = log(nrow(data_subset_2)))

