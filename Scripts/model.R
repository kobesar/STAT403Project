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

data_subset[data_subset == " "] <- "Omit"

data_subset$age = relevel(factor(data_subset$age), ref = "18-29")
data_subset$sex = relevel(factor(data_subset$sex), ref = "Female")
data_subset$educ18 = relevel(factor(data_subset$educ18), ref = "Never attended college")
data_subset$income20 = relevel(factor(data_subset$income20), ref = "Omit")
data_subset$racism20 = relevel(factor(data_subset$racism20), ref = "Omit")
data_subset$life = relevel(factor(data_subset$life), ref = "Omit")
data_subset$region = relevel(factor(data_subset$region), ref = "South")
data_subset$sizeplac = relevel(factor(data_subset$sizeplac), ref = "Suburbs")
data_subset$votemeth = relevel(factor(data_subset$votemeth), ref = "Election day")
data_subset$abortion = relevel(factor(data_subset$abortion), ref = "Omit")
data_subset$facemask = relevel(factor(data_subset$facemask), ref = "Omit")
data_subset$lgbt = relevel(factor(data_subset$lgbt), ref = "Omit")
data_subset$climatec = relevel(factor(data_subset$climatec), ref = "Omit")

X <- model.matrix(pres ~ ., data_subset)[,-1]
Y <- data_subset$pres == "Joe Biden"

data_full <- data.frame(cbind(Y, X))

mod <- glm(Y ~ ., family = "binomial", data = data_full)

summary(mod)

mod_step <- step(mod, direction = "backward", k = log(nrow(data_full)))

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

# FINAL MODEL

cols_final <- str_split("age
sex
educ18
income20
racism20
sizeplac
pres
votemeth
abortion
climatec", "\n")[[1]]

X_final <- model.matrix(pres ~ ., data_subset[,cols_final])[,-1]
Y_final <- data_subset$pres == "Joe Biden"

data_full_final <- data.frame(cbind(Y_final, X_final))

sample_i <- sample(nrow(data_full_final), nrow(data_full_final)*0.8, replace = FALSE)

data_train <- data_full_final[sample_i,]

data_test <- data_full_final[-sample_i,]

mod_final <- glm(Y_final ~ ., family = "binomial", data = data_train)

summary(mod_final)

saveRDS(mod_final, "mod_final.RDS")
saveRDS(data_train, "data_train.RDS")
saveRDS(data_test, "data_test.RDS")

data_test_eval <- cbind(data_test, pred = (predict(mod_final, data_test, type = "response") > 0.5)*1)

data_test_eval %>% 
  group_by(Y_final, pred) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_tile(aes(y = pred, x = Y_final, fill = n)) +
  geom_text(aes(y = pred, x = Y_final, label = n), color = "white")

B <- 10000

n = nrow(data_subset)
coeff_BT_lemp <- list()
for(i_BT in 1:B){
  w = sample(n,n,replace = T)
  data2_BT = data_subset[w,]
  fit_BT = glm(Y ~ age + sex + votemeth + abortion + educ18 + racism20 + income20 + sizeplac + climatec, family = "binomial", data = data2_BT)
  coeff_BT_lemp[[i_BT]] <- fit_BT$coefficients
}
coeff_BT_lemp <- data.frame(coeff_BT_lemp)
colnames(coeff_BT_lemp) = c("Intercept","gpa","gre")