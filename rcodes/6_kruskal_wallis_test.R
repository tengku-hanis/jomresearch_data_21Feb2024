#========================================================================#
# Title: Kruskal wallis test
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb21, 2023
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(skimr) #explore data
library(summarytools) #descriptive


# Data --------------------------------------------------------------------

kw_data <- read.csv("data/data_kruskal_wallis.csv")


# Edit variable type ------------------------------------------------------

kw_data <- 
  kw_data %>% 
  mutate(Species = as.factor(Species),
         Species = fct_recode(Species, 
                              virginica = "0",
                              setosa = "1",
                              versicolor = "2"))


# Explore -----------------------------------------------------------------

## Overall ----
summary(kw_data)
skim(kw_data)
descr(kw_data)
freq(kw_data$Species)

## By group ----
kw_data %>% 
  group_by(Species) %>% 
  descr()


# Assumptions -------------------------------------------------------------

## 1. Normality ----
kw_data %>% 
  ggplot(aes(Sepal.Length)) +
  geom_histogram() +
  facet_grid(rows = vars(Species))

kw_data %>% 
  ggplot(aes(sample = Sepal.Length)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(cols = vars(Species)) 


# Kruskal wallis tes ------------------------------------------------------

## Kruskal wallis test ----
kruskal.test(Sepal.Length ~ Species, data = kw_data)

## Post hoc test ----
pairwise.wilcox.test(x = kw_data$Sepal.Length, 
                     g = kw_data$Species, 
                     p.adjust.method = "bonferroni")


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# Kruskal wallis test
kw_data %>%
  mutate(
    Species = case_when(Species == "virginica" ~ "Virginica",
                        Species == "setosa" ~ "Setosa",
                        Species == "versicolor" ~ "Versicolor")
  ) %>%   
  tbl_summary(
    label = Sepal.Length ~ "Sepal Length",
    by = Species,
    statistic = all_continuous() ~ "{median} ({IQR})",
    missing = "no" 
  ) %>%
  add_n() %>%
  add_p(all_continuous() ~ "kruskal.test")

