library(tidyverse)
library(dplyr)
library(magrittr)

load('looking.RData')
load('touch.RData')
load('fNIRSData.RData')
load('age_sex.RData')
load('demografics.RData')
load('contingency.RData')

fNIRS2 <- pivot_wider(data = fNIRSData,
                      names_from = c(condition, channel),
                      values_from = c(HbO, HbR))
looking2 <- pivot_wider(data = looking, 
                        names_from = c(trial, view), 
                        values_from = duration)
touch2 <- pivot_wider(data = touch, names_from = category, values_from = n_rel)
data.list1 <- list(fNIRS2, looking2, touch2)
touch.looking.fNIRS <- data.list1 %>%
  reduce(full_join, by = "id")
data.list2 <- c(data.list1, list(age_sex, demografics, contingency))
total.data <- data.list2 %>% 
  reduce(full_join, by = "id")
