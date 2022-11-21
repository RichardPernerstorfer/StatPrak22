### libraries
library(tidyverse)
library(dplyr)
library(magrittr)

### data sets
load('looking.RData')
load('fNIRSData.RData')
load('age_sex.RData')

### create tidydata dataset
fNIRS2 <- pivot_wider(data = fNIRSData,
                      names_from = c(condition, channel),
                      values_from = c(HbO, HbR))
looking2 <- pivot_wider(data = looking, 
                        names_from = c(trial, view), 
                        values_from = duration)
data.list <- list(fNIRS2, looking2, age_sex)
fNIRS.looking.age_sex <- data.list1 %>%
  reduce(full_join, by = "id")

### subdaten fNIRS online.mean nach channel
fNIRS.online <- subset(fNIRSData, condition=="online")
fNIRS.online.HbOMean <- aggregate(fNIRS.online$HbO, list(fNIRS.online$channel), mean,
                                  na.rm=TRUE)
colnames(fNIRS.online.HbOMean) <- c("channel", "HbO_mean")
fNIRS.online.HbRMean <- aggregate(fNIRS.online$HbR, list(fNIRS.online$channel), mean,
                                  na.rm=TRUE)
colnames(fNIRS.online.HbRMean) <- c("channel", "HbR_mean")

fNIRS.online.mean <- cbind(fNIRS.online.HbOMean, fNIRS.online.HbRMean[2])
