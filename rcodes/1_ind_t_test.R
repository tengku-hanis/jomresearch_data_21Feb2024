#========================================================================#
# Title: Independent t test and Welch's t test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb21, 2023
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(skimr) #explore data
library(summarytools) #descriptive
library(DescTools) #levene test, lillie test


# Data --------------------------------------------------------------------

it_data <- read.csv("data/data_t_test.csv")


# Edit variable type ------------------------------------------------------

it_data <- 
  it_data %>% 
  mutate(Species = as.factor(Species),
         Species = fct_recode(Species, 
                              virginica = "0",
                              setosa = "1"))


# Explore -----------------------------------------------------------------

## Overall ----
summary(it_data)
skim(it_data)
descr(it_data)
freq(it_data)

## By group ----
it_data %>% 
  group_by(Species) %>% 
  descr()


# Assumptions -------------------------------------------------------------

## 1. Normality ----
# Visually
it_data %>% 
  ggplot(aes(Sepal.Length)) +
  geom_histogram() +
  facet_grid(rows = vars(Species))

it_data %>% 
  ggplot(aes(sample = Sepal.Length)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(cols = vars(Species)) 

# Statistical tests
tapply(it_data$Sepal.Length, it_data$Species, shapiro.test)
tapply(it_data$Sepal.Length, it_data$Species, LillieTest)


## 2. Equality of variances ----
LeveneTest(Sepal.Length ~ Species, data = it_data)
# Unequal variance use Welch's t test


# Independent t test ------------------------------------------------------

t.test(Sepal.Length ~ Species, data = it_data, var.equal = T)


# Welch's t test ----------------------------------------------------------

t.test(Sepal.Length ~ Species, data = it_data)


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# Independent t test
it_data %>%
  mutate(
    Species = case_when(Species == "virginica" ~ "Virginica",
                             Species == "setosa" ~ "Setosa")
    ) %>%   
  tbl_summary(
    label = Sepal.Length ~ "Sepal Length",
    by = Species,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no" 
    ) %>%
  add_n() %>%
  add_difference(Sepal.Length ~ "t.test", 
                 test.args = Sepal.Length ~ list(var.equal = TRUE),
                 estimate_fun = all_continuous() ~ function(x) style_number(x, digits = 2))

# Welch's t test
it_data %>%
  mutate(
    Species = case_when(Species == "virginica" ~ "Virginica",
                        Species == "setosa" ~ "Setosa")
  ) %>%   
  tbl_summary(
    label = Sepal.Length ~ "Sepal Length",
    by = Species,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no" 
  ) %>%
  add_n() %>%
  add_difference(Sepal.Length ~ "t.test", 
                 test.args = Sepal.Length ~ list(var.equal = FALSE),
                 estimate_fun = all_continuous() ~ function(x) style_number(x, digits = 2))
