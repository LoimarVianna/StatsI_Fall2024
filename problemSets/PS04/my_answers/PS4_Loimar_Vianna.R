# Install and load the car package
install.packages("car")
library(car)

# Load the Prestige dataset
data(Prestige)

# Create a new dummy variable professional
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# Check the data frame 
head(Prestige[c('professional','type')])

# Run a linear model with interaction
# Linear model: prestige ~ income + professional + income:professional
model <- lm(prestige ~ income * professional, data = Prestige)
summary(model)

# (c) Write the prediction equation based on the result
#\[\hat{y} = \beta_0 + \beta_1 x_1 + \beta_2D_1 + \beta_3 x_1D_1 \]

# (c) ADD and comment on results below on LateX file
# prestige = 21.1422589 + 0.0031709 × income
# + 37.7812800 × professional
# − 0.0023257 × income × professional

# This is the general formula 

# 21.1422589: The predicted prestige score for a non-professional (
#professional = 0 professional = 0) with an income of 0.
# 0.0031709: The increase in prestige for each unit increase in income for non-professionals.
# 37.7812800: The difference in pres
# tige between professionals and non-professionals when income = 0.
# −0.0023257: The interaction term, indicating that for professionals, the effect of income on prestige is slightly lower compared to non-professionals.


# (d) Interpret the coefficient for income
# The coefficient for income (\(\beta_1\)) represents the change in prestige for a 1-unit increase in income for non-professional occupations (\(\text{professional} = 0\)).

# Linear model: prestige ~ income + professional + income:professional
model <- lm(prestige ~ income * professional, data = Prestige)
summary(model)

# (e) Interpret the coefficient for professional
# The coefficient for professional (\(\beta_2\)) represents the average difference in prestige between professional and non-professional jobs when \(\text{income} = 0\).

# (f) Effect of $1,000 increase in income for professionals
# Calculate effect
# A $1,000 increase in income increases the prestige score by 0.8452 for professionals.
effect_income_professional <- model$coefficients["income"] + model$coefficients["income:professional"]
effect_income_professional * 1000


# (g) Effect of changing from non-professional to professional at $6,000 income
# Calculate effect
# The prestige score increases by 23.83 points when moving to a professional job with an income of $6,000.
effect_professional <- model$coefficients["professional"] + model$coefficients["income:professional"] * 6000
effect_professional

# Question 2: Political Science
# Use the results from a linear regression to determine whether 
# having these yard signs in a precinct a ects vote share 
# (e.g., conduct a hypothesis test with = 05).

# Hypothesis test: Effect of yard signs on vote share
# Coefficients and standard errors
coef_lawn <- 0.042
se_lawn <- 0.016

# t-statistic
t_lawn <- coef_lawn / se_lawn

# p-value
p_lawn <- 2 * (1 - pt(abs(t_lawn), df = 131 - 3))  # df = n - predictors - 1
p_lawn

# Add statement below on LateX file
# If \(p < 0.05\), reject \(H_0\): yard signs significantly impact vote share.

# b) Use the results to determine whether being next to precincts with these 
# yard signs a ects vote share (e.g., conduct a hypothesis test with = 05).
# Hypothesis test: Effect of adjacency to treatment precincts

# Coefficients and standard errors
coef_adj <- 0.042
se_adj <- 0.013

# t-statistic
t_adj <- coef_adj / se_adj

# p-value
p_adj <- 2 * (1 - pt(abs(t_adj), df = 131 - 3))
p_adj

# ADD comment below on LateX
# If \(p < 0.05\), reject \(H_0\): adjacency to treatment precincts significantly impacts vote share.

# (c) Interpret the coefficient for the constant term substantively
# ADD comment below on LateX
# The constant (\(\beta_0\)) represents the proportion of votes for  Cuccinelli in precincts without signs and not adjacent to treated precincts.#

# (d) Evaluate the model t for this regression. What does this tell us about 
# the importance of yard signs versus other factors that are not modeled?

# Given R-squared
r_squared <- 0.094

# ADD comment below on LateX
# When rendered, it will appear as:
# \(R^2 = 0.094\) implies that only 9.4\% of the variance in vote share is explained by the model. Yard signs may have a small effect, but other unmodeled factors likely explain most of the variation.


