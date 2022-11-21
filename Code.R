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

### seprete delayed and online
fNIRS.online <- subset(fNIRSData, condition=="online")
fNIRS.delayed <-subset(fNIRSData, condition=="delayed")

fNIRS2.online <- pivot_wider(data = fNIRS.online,
                      names_from = c(condition, channel),
                      values_from = c(HbO, HbR))

fNIRS2.delayed <- pivot_wider(data = fNIRS.delayed,
                      names_from = c(condition, channel),
                      values_from = c(HbO, HbR))
df.fNIRS2.online <- as.data.frame(fNIRS2.online)[, -1]
df.fNIRS2.delayed <- as.data.frame(fNIRS2.delayed)[, -1]


### ersetzen NA durch Mittelwert
df.fNIRS2 <- as.data.frame(fNIRS2)[, -1]

for(i in 1:ncol(df.fNIRS2)) {
  df.fNIRS2[ , i][is.na(df.fNIRS2[ , i])] <- mean(df.fNIRS2[ , i], na.rm=TRUE)
}

for(i in 1:ncol(df.fNIRS2.online)) {
  df.fNIRS2.online[ , i][is.na(df.fNIRS2.online[ , i])] <- mean(df.fNIRS2.online[ , i], na.rm=TRUE)
}

for(i in 1:ncol(df.fNIRS2.delayed)) {
  df.fNIRS2.delayed[ , i][is.na(df.fNIRS2.delayed[ , i])] <- mean(df.fNIRS2.delayed[ , i], na.rm=TRUE)
}

### Korrelationsmatrix

korr_tab_delayed <- cor(df.fNIRS2.delayed)
korr_tab_online <- cor(df.fNIRS2.online)

### subdaten fNIRS online.mean nach channel
fNIRS.online <- subset(fNIRSData, condition=="online")
fNIRS.online.HbOMean <- aggregate(fNIRS.online$HbO, list(fNIRS.online$channel), mean,
                                  na.rm=TRUE)
colnames(fNIRS.online.HbOMean) <- c("channel", "HbO_mean")
fNIRS.online.HbRMean <- aggregate(fNIRS.online$HbR, list(fNIRS.online$channel), mean,
                                  na.rm=TRUE)
colnames(fNIRS.online.HbRMean) <- c("channel", "HbR_mean")

fNIRS.online.mean <- cbind(fNIRS.online.HbOMean, fNIRS.online.HbRMean[2])

### subdaten fNIRS delayed.mean nach channel
fNIRS.delayed <- subset(fNIRSData, condition=="delayed")
fNIRS.delayed.HbOMean <- aggregate(fNIRS.delayed$HbO, list(fNIRS.delayed$channel), mean,
                                  na.rm=TRUE)
colnames(fNIRS.delayed.HbOMean) <- c("channel", "HbO_mean")

fNIRS.delayed.HbRMean <- aggregate(fNIRS.delayed$HbR, list(fNIRS.delayed$channel), mean,
                                  na.rm=TRUE)
colnames(fNIRS.delayed.HbRMean) <- c("channel", "HbR_mean")

fNIRS.delayed.mean <- cbind(fNIRS.delayed.HbOMean, fNIRS.delayed.HbRMean[2])
### Mittelwert HbO/HbR einzelner channel
channel_mean <- cbind(fNIRS.delayed.mean,fNIRS.online.mean[2:3])
colnames(channel_mean) <- c("channel", "delayed_HbO_mean","delayed_HbR_mean",
                            "online_HbO_mean", "online_HbR_mean")
