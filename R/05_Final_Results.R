# Final Results

# This script is for the final results and graphs I want to use
# when I present the project. Most of the analysis is already done
# so this is mostly putting the important results together.

# Run 04 so all the data and models are already made
source("R/04_Model.R")

library(dplyr)
library(ggplot2)



# Result 1: Who paid anything OOP by insurance


# First I wanted to show what percent of each insurance group
# actually paid something out of pocket
oop_by_insurance <- analysis_data %>%
  group_by(insurance) %>%
  summarise(
    n = n(),
    percent_paid_oop = mean(any_oop) * 100
  )

oop_by_insurance


# Make the graph
fig1 <- ggplot(oop_by_insurance,
               aes(x = insurance, y = percent_paid_oop)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(percent_paid_oop, 1), "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Percent Paying Out of Pocket by Insurance Type",
    x = "Insurance type",
    y = "Percent who paid OOP"
  ) +
  ylim(0, 100) +
  theme_minimal()

fig1

ggsave(
  "figures/01_oop_by_insurance.png",
  fig1,
  width = 8,
  height = 6
)



# Result 2: How much people paid by insurance


# For this I only used people who paid something OOP
# I used the median because the OOP amounts had some really high values
oop_amount_by_insurance <- positive_oop %>%
  group_by(insurance) %>%
  summarise(
    n = n(),
    median_oop = median(total_oop)
  )

oop_amount_by_insurance


# Graph the median amount for each insurance group
fig2 <- ggplot(oop_amount_by_insurance,
               aes(x = insurance, y = median_oop)) +
  geom_col() +
  geom_text(
    aes(label = paste0("$", round(median_oop, 0))),
    vjust = -0.5
  ) +
  labs(
    title = "Median Out-of-Pocket Spending by Insurance Type",
    subtitle = "Among people with positive out-of-pocket spending",
    x = "Insurance type",
    y = "Median OOP spending ($)"
  ) +
  theme_minimal()

fig2

ggsave(
  "figures/02_median_oop_by_insurance.png",
  fig2,
  width = 8,
  height = 6
)

# Result 3: OOP share by income and insurance


# I also wanted to compare what share of the prescription cost
# people paid themselves instead of only looking at dollar amounts
oop_share_results <- analysis_data %>%
  group_by(income_category, insurance) %>%
  summarise(
    median_oop_share = median(oop_share, na.rm = TRUE),
    .groups = "drop"
  )

oop_share_results


# Compare OOP share across both income and insurance groups
fig3 <- ggplot(oop_share_results,
               aes(x = income_category,
                   y = median_oop_share,
                   fill = insurance)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Share of Prescription Spending Paid Out of Pocket",
    subtitle = "Median OOP share by income and insurance type",
    x = "Income category",
    y = "Median OOP share",
    fill = "Insurance type"
  ) +
  theme_minimal()

fig3

ggsave(
  "figures/03_oop_share_by_income_insurance.png",
  fig3,
  width = 9,
  height = 6
)

# Result 4: Part 1 model


# These are the results from the logistic regression in script 04
# I changed the coefficients to odds ratios because theyre easier to read
model_results <- data.frame(
  variable = names(coef(oop_model)),
  odds_ratio = exp(coef(oop_model)),
  lower_ci = exp(confint(oop_model)[, 1]),
  upper_ci = exp(confint(oop_model)[, 2])
)

# I dont really need the intercept in the final graph
model_plot <- model_results[model_results$variable != "(Intercept)", ]


# Change the R variable names to names that actually make sense on a graph
model_plot$variable <- c(
  "Public only vs Private",
  "Uninsured vs Private",
  "Near poor vs Poor",
  "Low income vs Poor",
  "Middle income vs Poor",
  "High income vs Poor",
  "Age",
  "Prescription records"
)


# Put them in an order that makes more sense
model_plot$variable <- factor(
  model_plot$variable,
  levels = c(
    "Age",
    "Prescription records",
    "Near poor vs Poor",
    "Low income vs Poor",
    "Middle income vs Poor",
    "High income vs Poor",
    "Public only vs Private",
    "Uninsured vs Private"
  )
)


# Graph the odds ratios
# The dashed line at 1 means no difference
fig4 <- ggplot(model_plot,
               aes(x = odds_ratio, y = variable)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(xmin = lower_ci, xmax = upper_ci),
    width = 0.2,
    orientation = "y"
  ) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    title = "Factors Associated With Paying Out of Pocket",
    subtitle = "Odds ratios from the logistic regression model",
    x = "Odds ratio",
    y = NULL
  ) +
  theme_minimal()

fig4

ggsave(
  "figures/04_logistic_model_results.png",
  fig4,
  width = 9,
  height = 6
)

# Result 5: Part 2 model


# Now do the same thing for the model that looked at how much
# people paid if they already had positive OOP spending
amount_results <- data.frame(
  variable = names(coef(oop_amount_model)),
  spending_ratio = exp(coef(oop_amount_model)),
  lower_ci = exp(confint(oop_amount_model)[, 1]),
  upper_ci = exp(confint(oop_amount_model)[, 2])
)

# Again I dont need the intercept for the graph
amount_plot <- amount_results[amount_results$variable != "(Intercept)", ]


# Give these normal names too
amount_plot$variable <- c(
  "Public only vs Private",
  "Uninsured vs Private",
  "Near poor vs Poor",
  "Low income vs Poor",
  "Middle income vs Poor",
  "High income vs Poor",
  "Age",
  "Prescription records"
)


# Keep the same order as the first model graph
amount_plot$variable <- factor(
  amount_plot$variable,
  levels = c(
    "Age",
    "Prescription records",
    "Near poor vs Poor",
    "Low income vs Poor",
    "Middle income vs Poor",
    "High income vs Poor",
    "Public only vs Private",
    "Uninsured vs Private"
  )
)


# These are spending ratios, not odds ratios
# 1 still means no difference between the groups
fig5 <- ggplot(amount_plot,
               aes(x = spending_ratio, y = variable)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(xmin = lower_ci, xmax = upper_ci),
    width = 0.2,
    orientation = "y"
  ) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    title = "Factors Associated With Out-of-Pocket Spending Amount",
    subtitle = "Among people who paid something out of pocket",
    x = "Spending ratio",
    y = NULL
  ) +
  theme_minimal()

fig5

ggsave(
  "figures/05_oop_amount_model_results.png",
  fig5,
  width = 9,
  height = 6
)
