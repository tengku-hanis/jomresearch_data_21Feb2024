#========================================================================#
# Title: Paired t test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb21, 2023
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(skimr) #explore data
library(summarytools) #descriptive


# Data --------------------------------------------------------------------

pt_data <- read.csv("data/data_paired_t_test.csv")


# Explore -----------------------------------------------------------------

## Overall ----
summary(pt_data)
skim(pt_data)
descr(it_data)


# Assumptions -------------------------------------------------------------

## 1. Normality ----
pt_data %>% 
  ggplot(aes(Weight_after)) +
  geom_histogram() 

pt_data %>% 
  ggplot(aes(Weight_before)) +
  geom_histogram() 

pt_data %>% 
  ggplot(aes(sample = Weight_after)) +
  geom_qq() +
  geom_qq_line() 

pt_data %>% 
  ggplot(aes(sample = Weight_before)) +
  geom_qq() +
  geom_qq_line() 


# Independent t test ------------------------------------------------------

t.test(pt_data$Weight_after, pt_data$Weight_before, paired = T)


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# Paired t test
pt_data %>%
  pivot_longer(cols = 1:2, names_to = "Time", values_to = "Weight") %>% 
  mutate(
    Time = case_when(Time == "Weight_before" ~ "Before",
                     Time == "Weight_after" ~ "After")
  ) %>%   
  tbl_summary(
    label = Weight ~ "Weight (grams)",
    by = Time,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no" 
  ) %>%
  add_n() %>%
  add_difference(Weight ~ "t.test", test.args = Weight ~ list(paired = TRUE))


