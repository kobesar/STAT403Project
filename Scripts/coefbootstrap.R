library(tidyverse)
library(glmnet)
library(ggalt)

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

data_subset$y <- data_subset$pres == "Joe Biden"

B <- 10000

n = nrow(data_subset)
coeff_BT_lemp <- list()
for(i_BT in 1:B){
  print(i_BT)
  w = sample(n,n,replace = T)
  data2_BT = data_subset[w,]
  fit_BT = glm(y ~ age + sex + votemeth + abortion + educ18 + racism20 + income20 + sizeplac + climatec, family = "binomial", data = data2_BT)
  coeff_BT_lemp[[i_BT]] <- fit_BT$coefficients
}

coeff_BT_lemp <- data.frame(coeff_BT_lemp)

lapply(lapply(coeff_BT_lemp, unlist), `[[`, 1)

data_final <- data.frame(matrix(ncol=3))
colnames(data_final) <- c("coef", "var", "iteration")

for (i in 1:length(coeff_BT_lemp)) {
  print(i)
  temp <- data.frame(coeff_BT_lemp[[i]])
  temp$var <- rownames(temp)
  temp$iteration <- i
  rownames(temp) <- NULL
  colnames(temp) <- c("coef", "var", "iteration")
  data_final <- rbind(data_final, temp)
}

data_final_pivot <- data_final %>% 
  pivot_wider(1, names_from = "var", values_from = "coef")

data_final_pivot <- data_final_pivot[-1,]

data_final_pivot <- data_final_pivot[,-2]

data_final <- data_final %>%
  filter(!is.na(coef)) %>% 
  mutate(cat = ifelse(str_detect(var, "age"), "age",
                      ifelse(str_detect(var, "sex"), "sex",
                             ifelse(str_detect(var, "votemeth"), "votemeth",
                                    ifelse(str_detect(var, "abortion"), "abortion",
                                           ifelse(str_detect(var, "educ18"), "education",
                                                  ifelse(str_detect(var, "racism20"), "racism",
                                                         ifelse(str_detect(var, "income20"), "income",
                                                                ifelse(str_detect(var, "sizeplac"), "city",
                                                                       ifelse(str_detect(var, "climatec"), "climate", "intercept"))))))))),
         var_clean = str_replace_all(var, paste(c("age", "sex", "votemeth", "abortion", "educ18", "racism20", "income20", "sizeplac", "climatec"), collapse = "|"), ""))

data_final %>% 
  ggplot() +
  geom_violin(aes(x = coef, y = var_clean, fill = var_clean %in% (data_final %>% group_by(var_clean) %>% summarize(mean = mean(coef, na.rm = T)) %>% filter(mean > 0) %>% pull(var_clean)))) +
  scale_fill_manual(values = c("#d23368", "#2a3990")) +
  labs(x = "", y = "") +
  theme_light() +
  theme(legend.position = "none") +
  facet_wrap(~ cat, scale="free")

data.frame(lapply(data_final_pivot, quantile, c(0.05, 0.95))) %>% 
  select(-iteration) %>% 
  mutate(bound = c("lower", "upper")) %>% 
  pivot_longer(1:34, names_to = "variable") %>% 
  pivot_wider(1, names_from = 1) %>% 
  ggplot() +
  geom_dumbbell(aes(x = lower, xend = upper, y = variable)) +
  labs(title = "Uncertainty of Coefficients", subtitle = "B = 10,000")

bootstrapres <- data_final_pivot

saveRDS(bootstrapres, "../data/bootstrapres.RDS")

# test <- data %>% 
#   group_by(stanum) %>% 
#   summarize(votes = sum((pres == "Joe Biden")*weight) / sum(weight), biden = votes > 0.5)
# 
# test$state <- trimws(test$stanum)
#   
# plot_usmap(data = test, values = "biden", color = "white", labels = TRUE, label_color = "gray100") +
#   scale_fill_manual(values = c("FALSE" = "#d23368", "TRUE"="#2a3990"), labels = c("Trump", "Biden")) +
#   labs(x = "", y = "", fill = "") +
#   theme_light() +
#   theme(
#     # text = element_text(family = "roboto"),
#     legend.position = c(0.5, 0.95),
#     legend.direction = "horizontal",
#     legend.text = element_text(size = 14),
#     panel.border = element_blank(), panel.grid = element_blank(), 
#     axis.text.x = element_blank(), axis.text.y = element_blank())  # Remove axis labels
