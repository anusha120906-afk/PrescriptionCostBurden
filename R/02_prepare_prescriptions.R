library(readxl)
library(dplyr)

prescriptions <- read_excel("data/h254a.xlsx")
people <- read_excel("data/h256.xlsx")
dim(prescriptions)

#this checks for missing values or negative costs 
summary(prescriptions$RXSF24X)
#since min starts at 0 and no missing values, aggregation is ok to use

#takes 204k rows of data, where many rows can belong to one person to 
#12k unique people rows where each row is for one person and oop is
#total out of pocket spending per person
prescription_summary <- prescriptions %>%
  group_by(DUPERSID) %>%
  summarise(
    total_oop = sum(RXSF24X),
    total_rx_spending = sum(RXXP24X),
    num_rx_records = n()
  )


prescription_summary
#joing prescription dataset and people dataset using common dupersid
analysis_data <- prescription_summary %>%
  left_join(people, by = "DUPERSID")
dim(prescription_summary)
head(prescription_summary)
dim(analysis_data)


#as a check to see where sum oop is coming from, from og prescription dataset 
prescriptions %>%
  filter(DUPERSID == "2810003101") %>%
  select(DUPERSID, RXNAME, RXSF24X)


#condenses new dataset with only the varaibles we need 
analysis_data <- analysis_data %>%
  select(
    DUPERSID,
    total_oop,
    total_rx_spending,
    num_rx_records,
    AGE24X,
    TTLP24X,
    POVCAT24,
    INSCOV24,
    PERWT24F
  )


#creates a new column, insurance which uses words instead of 1,2,3 to represent insurance status
analysis_data <- analysis_data %>%
  mutate(
    insurance = factor(
      INSCOV24,
      levels = c(1, 2, 3),
      labels = c("Private", "Public only", "Uninsured")
    )
  )
head(analysis_data)

#creates new columns income category with more readability 
analysis_data <- analysis_data %>%
  mutate(
    income_category = factor(
      POVCAT24,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Poor",
        "Near poor",
        "Low income",
        "Middle income",
        "High income"
      )
    )
  )
table(analysis_data$income_category)


#getting summary stats by income category
analysis_data %>%
  group_by(income_category) %>%
  summarise(
    n = n(),
    mean_oop = mean(total_oop),
    median_oop = median(total_oop)
  )

#getting summary stats by insurance type
analysis_data %>%
  group_by(insurance) %>%
  summarise(
    n = n(),
    mean_oop = mean(total_oop),
    median_oop = median(total_oop),
    mean_total_rx = mean(total_rx_spending),
    median_total_rx = median(total_rx_spending)
  )

#checking if total spending is 0 because to find oop share,(how much pt or family is paying oop),
#we need to divide oop by expenditures and cant divide by 0
sum(analysis_data$total_rx_spending == 0)

#greater tahn 0 bc dont want to divide by 0, oop share gives percentage family paying over family + insurance
analysis_data <- analysis_data %>%
  mutate(
    oop_share = ifelse(
      total_rx_spending > 0,
      total_oop / total_rx_spending,
      NA
    )
  )
summary(analysis_data$oop_share)

#checks oop share proportions by insurance types, also ignores NA (one person who would be dividing by 0)
analysis_data %>%
  group_by(insurance) %>%
  summarise(
    n = n(),
    mean_oop_share = mean(oop_share, na.rm = TRUE),
    median_oop_share = median(oop_share, na.rm = TRUE)
  )


