library(tidyverse)
library(dplyr)
library(magrittr)

load('looking.RData')
load('fNIRSData.RData')
load('age_sex.RData')


fNIRS2 <- pivot_wider(data = fNIRSData,
                      names_from = c(condition, channel),
                      values_from = c(HbO, HbR))
looking2 <- pivot_wider(data = looking, 
                        names_from = c(trial, view), 
                        values_from = duration)
data.list <- list(fNIRS2, looking2, age_sex)
fNIRS.looking.age_sex <- data.list1 %>%
  reduce(full_join, by = "id")
