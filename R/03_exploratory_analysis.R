# Exploratory Data Analysis
# Prescription Cost Burden Project

# Runs script 02 first so the cleaned analysis_data dataset is created
# before I start doing any of the analysis in this script
# dplyr is used for things like filtering, grouping, and summarizing data
# ggplot2 is used to make the graphs
source("R/02_prepare_prescriptions.R")
library(dplyr)
library(ggplot2)

# First looking at the overall distribution of total out-of-pocket
# prescription spending for each person
# Each person is only one row in analysis_data
ggplot(analysis_data, aes(x = total_oop)) +
  geom_histogram(bins = 50)

# The first histogram showed that most people have relatively low OOP
# spending, but there are a few people with extremely high spending
# This makes the distribution very right-skewed

# Checking the 10 people with the highest OOP spending to make sure
# the extreme values make sense before assuming they are errors or outliers
analysis_data %>%
  arrange(desc(total_oop)) %>%
  select(DUPERSID, total_oop, total_rx_spending,
         num_rx_records, AGE24X, insurance, income_category) %>%
  head(10)

#Since the extreme spending values made the regular histogram hard to read,
#I am looking at OOP spending again using a log scale
#Adding 1 allows people with $0 OOP spending to stay in the graph
#since log(0) is undefined
ggplot(analysis_data, aes(x = total_oop + 1)) +
  geom_histogram(bins = 50) +
  scale_x_log10()

# Checking how many people had 0 oop spending
# Counting how many people paid exactly $0 out of pocket
# True means the person's total OOP spending was $0
# False means they paid at least something out of pocket
table(analysis_data$total_oop == 0)

# Checking whether having $0 OOP spending differs by insurance type
# n() counts the total number of people in each insurance group
# sum(total_oop == 0) counts how many people in the group paid 0
# Taking the mean of true/flase gives the proportion that paid 0,
# and multiplying by 100 makes into percentage
analysis_data %>%
  group_by(insurance) %>%
  summarise(
    n = n(),
    zero_oop = sum(total_oop == 0),
    percent_zero_oop = mean(total_oop == 0) * 100
  )

# Compare OOP spending across insurance groups
# For this graph I only include people who actually paid something OOP
# because the $0 group was already looked at separately above
# A log scale is used because OOP spending is again extremely right skewed
ggplot(
  analysis_data %>% filter(total_oop > 0),
  aes(x = insurance, y = total_oop)
) +
  geom_boxplot() +
  scale_y_log10()
#Public only ins prescription users are much more likely to have no OOP 
#payment at all,but among people who do pay OOP, the distributions overlap 
#substantially across insurance groups.

# Comparing OOP spending over the five income categories
# also only people with positive OOP spending are included
# The boxplots help me compare the median and overall spread between groups
ggplot(
  analysis_data %>% filter(total_oop > 0),
  aes(x = income_category, y = total_oop)
) +
  geom_boxplot() +
  scale_y_log10()

# see number of prescription records per person
summary(analysis_data$num_rx_records)

# Looking at whether people with more prescription records also tend
# to have higher OOP spending, dots r people
# alpha makes the dots transparent because there are thousands of people
# LOESS gives me a curved trend line so I can see the overall pattern
# without forcing the relationship to be a straight line
ggplot(
  analysis_data %>% filter(total_oop > 0),
  aes(x = num_rx_records, y = total_oop)
) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_log10()
#Prescription utilization appears positively associated with OOP spending, 
#mostly across the range where most observations occur.

# Explore age
summary(analysis_data$AGE24X)

# Check invalid or neg age values
table(analysis_data$AGE24X < 0)
# Looking at the relationship between age and OOP spending
# I will exclude negative age codes from this graph instead of treating them
# like actual ages, but I am keeping those people from analysis_data
# I also only include people with positive OOP spending for the log scale
ggplot(
  analysis_data %>%
    filter(AGE24X >= 0, total_oop > 0),
  aes(x = AGE24X, y = total_oop)
) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_log10()

# Instead of looking at insurance and income separately I group by both
# so I can compare combinations like poor and private, poor and public only,
# poor and uninsured, etc
# I use medians because the spending data has extreme high values that
# can pull the mean up a lot
analysis_data %>%
  group_by(income_category, insurance) %>%
  summarise(
    n = n(),
    median_oop = median(total_oop),
    median_oop_share = median(oop_share, na.rm = TRUE),
    .groups = "drop"
  )

# This graph summarizes the insurance + income comparison above
# OOP share is the portion of total prescription spending that was
# paid by the patient/family rather than another payer
# Bars are placed next to each other so insurance types can be compared
# within each income category
analysis_data %>%
  group_by(income_category, insurance) %>%
  summarise(
    median_oop_share = median(oop_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = income_category,
    y = median_oop_share,
    fill = insurance
  )) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Income category",
    y = "Median OOP share",
    fill = "Insurance type"
  )
