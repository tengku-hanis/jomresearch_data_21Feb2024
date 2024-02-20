#========================================================================#
# Title: Wilcoxon signed rank test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb21, 2023
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(skimr) #explore data
library(summarytools) #descriptive


# Data --------------------------------------------------------------------

w_data <- read.csv("data/data_wilcoxon.csv")


# Explore -----------------------------------------------------------------

## Overall ----
summary(w_data)
skim(w_data)
descr(w_data)


# Assumptions -------------------------------------------------------------

## 1. Normality ----

w_data <- 
  w_data %>% 
  mutate(Weight_diff = Weight_after - Weight_before)

# Visually
w_data %>% 
  ggplot(aes(Weight_diff)) +
  geom_histogram() 

w_data %>% 
  ggplot(aes(sample = Weight_diff)) +
  geom_qq() +
  geom_qq_line() 

# Statistical tests
shapiro.test(w_data$Weight_diff)
# ks.test(w_data$Weight_diff, y = "pnorm")
# KS assume each data is unique and cannot has a tie

# Wilcoxon signed rank test -----------------------------------------------

wilcox.test(w_data$Weight_after, w_data$Weight_before, paired = T)


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# Wilcoxon signed rank test
w_data %>%
  pivot_longer(cols = 1:2, names_to = "Time", values_to = "Weight") %>% 
  mutate(
    Time = case_when(Time == "Weight_before" ~ "Before",
                     Time == "Weight_after" ~ "After")
  ) %>%   
  select(-Weight_diff) %>% 
  tbl_summary(
    label = Weight ~ "Weight (grams)",
    by = Time,
    statistic = all_continuous() ~ "{median} ({IQR})",
    missing = "no" 
  ) %>%
  add_n() %>%
  add_difference(Weight ~ "wilcox.test", test.args = Weight ~ list(paired = TRUE))


