### libraries
library(tidyverse)
library(dplyr)
library(magrittr)
library(corrplot

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
fNIRS.looking.age_sex <- data.list %>%
  reduce(full_join, by = "id")

### seprete fNIRS2 to delayed and online
fNIRS2.online <- as.data.frame(pivot_wider(data = subset(fNIRSData, condition == "online"),
                      names_from = c(condition, channel),
                      values_from = c(HbO, HbR)))[, -1]

fNIRS2.delayed <- as.data.frame(pivot_wider(data = subset(fNIRSData, condition == "delayed"),
                      names_from = c(condition, channel),
                      values_from = c(HbO, HbR))) [, -1]


## ersetzen NA in fNIRS2.online/fNIRS2.delayed durch Mittelwert 
for(i in 1:ncol(fNIRS2.online)) {
  fNIRS2.online[ , i][is.na(fNIRS2.online[ , i])] <- mean(fNIRS2.online[ , i], na.rm=TRUE)
}

for(i in 1:ncol(fNIRS2.delayed)) {
  fNIRS2.delayed[ , i][is.na(fNIRS2.delayed[ , i])] <- mean(fNIRS2.delayed[ , i], na.rm=TRUE)
}

### Differenz delayed_online
# mit betrag
diff_delayed_online <- abs(fNIRS2.delayed - fNIRS2.online)
colnames(diff_delayed_online) <- c(1:30, #HbO
                                   1:30) #HbR

# ohne betrag
diff_delayed_online <- fNIRS2.delayed - fNIRS2.online
colnames(diff_delayed_online) <- c(1:30, #HbO
                                   1:30) #HbR


######## Korrelationsmatrix
#diff_delayed_online
korr_tab_diffHbO <- cor(diff_delayed_online[1:30])
korr_tab_diffHbR <- cor(diff_delayed_online[31:60])

# Anzahl der starken korrelierten Paare berechnen
length(which(korr_tab_diffHbO >= 0.5 )) - 30 
length(which(korr_tab_diffHbR >= 0.5)) - 30

length(which(korr_tab_diffHbO >= 0.6)) - 30
length(which(korr_tab_diffHbR >= 0.6)) - 30

length(which(korr_tab_diffHbO >= 0.7)) - 30
length(which(korr_tab_diffHbR >= 0.7)) - 30


### Korrelation plotten
## korr_tab_diffHbO
corrplot_channel_diffHbO <- corrplot(korr_tab_diffHbO, method= "pie", type = "upper", pch = 0.1,
         title = "Korrelation Plot: Differenz delayed_online")

## korr_tab_diffHbR
corrplot_channel_diffHbR <- corrplot(korr_tab_diffHbR, method= "pie", type = "upper", pch = 0.1,
                                     title = "Korrelation Plot: Differenz delayed_online")


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


# updata ab 26.11 ---------------------------------------------------------


### eine Möglichkeit für Korrelationsanalyse ,wenn es für uns nicht geeignet ist
### lösen wir es.
library(Hmisc)
library(PerformanceAnalytics)
#wählen wir den Teildatensätze aus ,aus fNIRS2
HBO.online <- fNIRS2[, grepl("HbO_online+", colnames(fNIRS2))]
HBO.delayed <- fNIRS2[, grepl("HbO_delayed+", colnames(fNIRS2))]
HBR.online <- fNIRS2[, grepl("HbR_online+", colnames(fNIRS2))]
HBR.delayed <- fNIRS2[, grepl("HbR_delayed+", colnames(fNIRS2))]

#berechen wir den korrelation und den p wert  (HBR.delayed oder online analog) 
HBO.online.cor <- rcorr(as.matrix(HBO.online))
#Visualisierung (HBR.delayed/online analog)
#tipp:die Grafik kann sehr groß sein.
chart.Correlation(HBO.online, histogram = TRUE)

