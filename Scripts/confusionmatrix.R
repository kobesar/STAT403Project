library(tidyverse)

train <- readRDS("../data/data_train.RDS")
test <- readRDS("../data/data_test.RDS")

mod_final <- glm(Y_final ~ ., family = "binomial", data = train)

actual <- test$Y_final
predicted <- (predict(mod, test, type = "response") > 0.5)*1

# Create a data frame with the actual and predicted labels
data <- data.frame(actual, predicted)

cm_data <- data %>% 
  group_by(actual, predicted) %>% 
  summarize(n = n()) %>% 
  mutate(actual = as.character(actual), predicted = as.character(predicted))

cm_data$actual <- factor(cm_data$actual, levels = c("1", "0"))
cm_data$predicted <- factor(cm_data$predicted, levels = c("0", "1"))
cm_data$labels <- c("True Negatives", "False Positives", "False Negatives", "True Positives")

cm_data <- cm_data %>% 
  mutate(labels = paste0(n, "\n", labels))

# Plot the confusion matrix using ggplot
cm_plot <- cm_data %>% 
  ggplot(aes(x = actual, y = predicted, fill = n)) +
  geom_tile() +
  geom_label(aes(label = labels), color = "black", size = 4, fill = "gray90") +
  scale_fill_gradient(low = "white", high = "#2a3990") +
  scale_x_discrete(labels = c("", "")) +
  scale_y_discrete(labels = c("", "")) +
  labs(title = "Confusion Matrix", 
       x = "", 
       y = "",
       subtitle = paste(
         "Recall: ", round(cm_data[cm_data$actual == "1" & cm_data$predicted == "1",]$n/(cm_data[cm_data$actual == "0" & cm_data$predicted == "1",]$n + cm_data[cm_data$actual == "1" & cm_data$predicted == "1",]$n), 2),
         " | Precision: ", round(cm_data[cm_data$actual == "1" & cm_data$predicted == "1",]$n/(cm_data[cm_data$actual == "1" & cm_data$predicted == "1",]$n + cm_data[cm_data$actual == "1" & cm_data$predicted == "0",]$n), 2),
         " | Accuracy: ", round(sum(cm_data[cm_data$actual == cm_data$predicted,]$n)/sum(cm_data$n), 2))) +
  theme_minimal()

saveRDS(cm_plot, "../data/cm_plot.RDS")