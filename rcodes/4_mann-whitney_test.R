#========================================================================#
# Title: Mann-whitney test or Wilcoxon rank sum test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb21, 2023
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation 
library(skimr) #explore data
library(summarytools) #descriptive
library(DescTools) #lillie test


# Data --------------------------------------------------------------------

mw_data <- read.csv("data/data_mann_whitney.csv")


# Edit variable type ------------------------------------------------------

mw_data <- 
  mw_data %>% 
  mutate(Species = as.factor(Species),
         Species = fct_recode(Species, 
                              virginica = "0",
                              setosa = "1"))


# Explore -----------------------------------------------------------------

## Overall ----
summary(mw_data)
skim(mw_data)
descr(mw_data)
freq(mw_data$Species)

## By group ----
mw_data %>% 
  group_by(Species) %>% 
  descr()


# Assumptions -------------------------------------------------------------

## 1. Normality ----
# Visually
mw_data %>% 
  ggplot(aes(Sepal.Length)) +
  geom_histogram() +
  facet_grid(rows = vars(Species))

mw_data %>% 
  ggplot(aes(sample = Sepal.Length)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(cols = vars(Species)) 

# Statistical tests
tapply(mw_data$Sepal.Length, mw_data$Species, shapiro.test)
tapply(mw_data$Sepal.Length, mw_data$Species, LillieTest)


# Mann-whitney test -------------------------------------------------------

wilcox.test(Sepal.Length ~ Species, data = mw_data)


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# Mann-whitney test
mw_data %>%
  mutate(
    Species = case_when(Species == "virginica" ~ "Virginica",
                        Species == "setosa" ~ "Setosa")
  ) %>%   
  tbl_summary(
    label = Sepal.Length ~ "Sepal Length",
    by = Species,
    statistic = all_continuous() ~ "{median} ({IQR})",
    missing = "no" 
  ) %>%
  add_n() %>%
  add_difference(Sepal.Length ~ "wilcox.test")
