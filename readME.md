# Analysis Project - Summary

In this analysis project, an attempt was made to uncover key insights into the 2020 election through the utilization of bootstrap techniques and logistic regression modeling. Various aspects of the election were explored, including the popular vote, state-level predictions, voting patterns by age, and coefficient analysis.

## High-Level Tasks

Throughout the analysis, the following tasks were undertaken:

1. **Bootstrap Analysis**: The popular vote was subjected to a bootstrap analysis to estimate the variance of election outcomes. The simulations yielded observations where Biden secured a majority of votes in most cases, but instances were found where a majority eluded him.

2. **State-Level Predictions**: Employing bootstrap techniques, an electoral map was simulated to predict outcomes at the state level. The simulations indicated that Biden was likely to win in the majority of states. Nevertheless, an issue regarding the oversampling of Democratic voters was discovered, leading to inaccuracies in predicting results for certain states.

3. **Voting Patterns by Age**: Voting patterns were examined across different age groups by simulating a popular vote scenario. It was observed that older age groups displayed less inclination to vote for Biden, while younger age groups exhibited greater support. Notably, the 18-29 age bracket demonstrated a higher level of variability, suggesting a less predictable voting behavior.

4. **Coefficient Analysis**: A logistic regression analysis was conducted to explore the relationships between variables and voting behavior. Special attention was given to the significance of coefficients. Notably, variables related to opinions on "Racism" and "Climate" exhibited consistent and significant influence across all categories, emphasizing their importance as predictors of voting preferences.

Looking forward, it is suggested that undersampling techniques be explored to address the oversampling issue of Democratic voters and evaluate its impact on predictions. Additionally, testing alternative tree-based models could provide valuable insights into their predictive accuracy and performance relative to the logistic regression model.

In conclusion, this analysis project provided valuable insights into the 2020 election, shedding light on voting patterns, variable significance, and the potential for prediction. By uncovering key findings and recognizing the limitations of the approach, further refinements can be made to enhance the models and analysis for future elections.
