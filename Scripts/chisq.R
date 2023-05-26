library(tidyverse)

# Testing for independence with chi-squared

data <- read.csv("../Data/31119913_National2020.csv")

perform_chi_squared_tests <- function(df, significance_level = 0.05) {
  # Get the list of categorical variables from the dataset
  categorical_vars <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
  
  # Function to perform chi-squared test for a given combination of variables and extract p-value
  perform_chi_squared_test <- function(vars) {
    contingency_table <- table(df[, vars])
    chi_square_result <- chisq.test(contingency_table)
    return(chi_square_result$p.value)
  }
  
  # Generate all possible combinations of variables
  variable_combinations <- combn(categorical_vars, 2)
  
  # Perform chi-squared test for each combination and extract p-values
  p_values <- apply(variable_combinations, 2, perform_chi_squared_test)
  
  # Get variable combinations with p-value less than the significance level
  significant_combinations <- variable_combinations[, p_values < significance_level]
  
  # Create a data frame with significant combinations and p-values
  result <- data.frame(Variable1 = significant_combinations[1, ],
                       Variable2 = significant_combinations[2, ],
                       p_value = p_values[p_values < significance_level],
                       stringsAsFactors = FALSE)
  
  return(result)
}

# Perform only for the variables we've picked

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
  dplyr::select(cols)

perform_chi_squared_tests(data_subset)

perform_chi_squared_tests(data)

cols <- str_split("age
sex
educ18
income20
racism20
sizeplac
votemeth
abortion
climatec", "\n")[[1]]

data_subset <- data %>% 
  dplyr::select(cols)

tab <- perform_chi_squared_tests(data_subset)

ind_vars <- tab %>% 
  filter(p_value > 0.00001) %>% 
  pivot_longer(cols = 1:2) %>% 
  pull(value) %>% 
  unique()
