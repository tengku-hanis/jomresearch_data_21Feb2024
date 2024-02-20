#========================================================================#
# Title: One-way anova and Welch's anova
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: Feb21, 2023
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(skimr) #explore data
library(summarytools) #descriptive
library(DescTools) #levene test
library(emmeans) #posthoc test
library(rstatix) #posthoc unequal variance


# Data --------------------------------------------------------------------

ano_data <- read.csv("data/data_anova.csv")


# Edit variable type ------------------------------------------------------

ano_data <- 
  ano_data %>% 
  mutate(Species = as.factor(Species),
         Species = fct_recode(Species, 
                              virginica = "0",
                              setosa = "1",
                              versicolor = "2"))


# Explore -----------------------------------------------------------------

## Overall ----
summary(ano_data)
skim(ano_data)
descr(ano_data)
freq(ano_data)

## By group ----
ano_data %>% 
  group_by(Species) %>% 
  descr()


# Assumptions -------------------------------------------------------------

## 1. Normality ----
# Visually
ano_data %>% 
  ggplot(aes(Sepal.Length)) +
  geom_histogram() +
  facet_grid(rows = vars(Species))

ano_data %>% 
  ggplot(aes(sample = Sepal.Length)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(cols = vars(Species)) 

# Statistical tests
tapply(ano_data$Sepal.Length, ano_data$Species, shapiro.test)
# tapply(ano_data$Sepal.Length, ano_data$Species, ks.test, y = "pnorm")
# KS assume each data is unique and cannot has a tie

## 2. Equality of variances ----
LeveneTest(Sepal.Length ~ Species, data = ano_data)
# Unequal variance use Welch's anova


# One-way anova -----------------------------------------------------------

## Anova ----
ano_mod <- aov(Sepal.Length ~ Species, data = ano_data)
summary(ano_mod)

## Post hoc test ----
# Base R
pairwise.t.test(x = ano_data$Sepal.Length, 
                g = ano_data$Species, 
                p.adjust.method = "bonferroni")

# Better post hoc test
emmeans(ano_mod, pairwise ~ Species, adjust = "bonferroni")


# Welch's anova -----------------------------------------------------------

ano_mod2 <- oneway.test(Sepal.Length ~ Species, data = ano_data)
ano_mod2

## Post hoc test ----
# Games-Howell correction
games_howell_test(Sepal.Length ~ Species, data = ano_data)


# More advance ------------------------------------------------------------

# Presentation using gtsummary package
library(gtsummary)

# One-way anova
ano_data %>%
  mutate(
    Species = case_when(Species == "virginica" ~ "Virginica",
                        Species == "setosa" ~ "Setosa",
                        Species == "versicolor" ~ "Versicolor")
  ) %>%   
  tbl_summary(
    label = Sepal.Length ~ "Sepal Length",
    by = Species,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no" 
  ) %>%
  add_n() %>%
  add_p(all_continuous() ~ "aov")

# Welch's anova
ano_data %>%
  mutate(
    Species = case_when(Species == "virginica" ~ "Virginica",
                        Species == "setosa" ~ "Setosa",
                        Species == "versicolor" ~ "Versicolor")
  ) %>%   
  tbl_summary(
    label = Sepal.Length ~ "Sepal Length",
    by = Species,
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no" 
  ) %>%
  add_n() %>%
  add_p(all_continuous() ~ "oneway.test")
