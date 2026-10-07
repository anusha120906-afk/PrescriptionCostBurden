# The Model

# From the EDA I saw that there were a lot of people who paid exactly $0 OOP
# and for the people who did pay, the amounts were really right skewed.
# So I thought it would make more sense to use 2 parts instead of trying
# to put all the OOP amounts into one model.

# Part 1: did the person pay anything OOP or not
# Part 2: if they did pay, how much did they pay

# The predictors I am looking at are insurance, income, age,
# and how many prescription records the person had.


# Get the cleaned data from script 02
source("R/02_prepare_prescriptions.R")

library(dplyr)
library(ggplot2)

# Part 1: Did the person pay anything OOP?


# Make a yes/no variable for OOP
# 0 means they paid nothing and 1 means they paid something
analysis_data$any_oop <- ifelse(analysis_data$total_oop > 0, 1, 0)

# Just checking that it matches what I found in the EDA
table(analysis_data$any_oop)


# I noticed before that age had some negative codes which arent actual ages
# so I made another age variable and changed those to NA
analysis_data$age_clean <- ifelse(
  analysis_data$AGE24X >= 0,
  analysis_data$AGE24X,
  NA
)

summary(analysis_data$age_clean)


# Check the other variables before putting them into the model
table(analysis_data$insurance, useNA = "ifany")
table(analysis_data$income_category, useNA = "ifany")
summary(analysis_data$num_rx_records)


# Set the reference groups
# I used Private for insurance since its the biggest group and gives me
# a useful comparison for Public only and Uninsured
analysis_data$insurance <- relevel(analysis_data$insurance, ref = "Private")

# For income I am comparing the other groups to Poor
analysis_data$income_category <- relevel(analysis_data$income_category, ref = "Poor")

levels(analysis_data$insurance)
levels(analysis_data$income_category)


# The model cant use the missing ages so make a model dataset without them
model_data <- analysis_data %>%
  filter(!is.na(age_clean))

nrow(model_data)


# Logistic regression for whether someone paid anything OOP
oop_model <- glm(
  any_oop ~ insurance + income_category + age_clean + num_rx_records,
  data = model_data,
  family = binomial
)

summary(oop_model)


# The regular coefficients are in log odds which are hard to interpret
# so turn them into odds ratios
exp(coef(oop_model))

# 95% confidence intervals for the odds ratios
exp(confint(oop_model))


# I wanted to see how the predicted chance of paying OOP changes
# as the number of prescription records goes up
ggplot(model_data, aes(x = num_rx_records, y = any_oop)) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial")
  )


# Part 2: If they paid OOP, how much did they pay?


# For this part I only need the people who actually paid something
positive_oop <- model_data %>%
  filter(total_oop > 0)

nrow(positive_oop)


# The positive OOP amounts were really right skewed in the EDA
# so I used a Gamma model with a log link instead of normal linear regression
oop_amount_model <- glm(
  total_oop ~ insurance + income_category + age_clean + num_rx_records,
  data = positive_oop,
  family = Gamma(link = "log")
)

summary(oop_amount_model)


# Make these results easier to interpret too
# These are NOT odds ratios like Part 1 since this model is about OOP amount
exp(coef(oop_amount_model))

# 95% confidence intervals
exp(confint(oop_amount_model))


# Check for influential observations


# Since there were some really high OOP amounts I wanted to check
# if a few people were having a really big influence on the model
cooks_d <- cooks.distance(oop_amount_model)

summary(cooks_d)


# Find the 10 people with the highest Cooks distance
top_influence <- order(cooks_d, decreasing = TRUE)[1:10]

positive_oop[
  top_influence,
  c(
    "DUPERSID",
    "total_oop",
    "total_rx_spending",
    "num_rx_records",
    "age_clean",
    "insurance",
    "income_category"
  )
]


# The person with the highest influence also had a really high OOP amount
# I dont want to just delete them because that doesnt mean the data is wrong
# so I made another model without that one person just to see how much
# the results would change
oop_without_top <- positive_oop[-top_influence[1], ]

check_model <- glm(
  total_oop ~ insurance + income_category + age_clean + num_rx_records,
  data = oop_without_top,
  family = Gamma(link = "log")
)

# Compare this to the original results
# The numbers changed some but the main patterns stayed pretty similar
exp(coef(check_model))
