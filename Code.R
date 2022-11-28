### libraries
library(tidyverse)
library(dplyr)
library(magrittr)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)

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

#### mittelwert/Differenz einzelner channel
HbOMean.online <- aggregate(subset(fNIRSData, condition =="online")$HbO,
                                  list(subset(fNIRSData, condition=="online")$channel),
                                  mean, na.rm=TRUE)

HbRMean.online <- aggregate(subset(fNIRSData, condition == "online")$HbR,
                            list(subset(fNIRSData, condition=="online")$channel),
                            mean, na.rm=TRUE)

HbOMean.delayed <- aggregate(subset(fNIRSData, condition=="delayed")$HbO,
                                   list(subset(fNIRSData, condition=="delayed")$channel),
                                   mean, na.rm=TRUE)

HbRMean.delayed <- aggregate(subset(fNIRSData, condition=="delayed")$HbR,
                                   list(subset(fNIRSData, condition=="delayed")$channel),
                                   mean, na.rm=TRUE)


channel_mean <- cbind(HbOMean.delayed,HbRMean.delayed, HbOMean.online, HbRMean.online)
channel_mean <- channel_mean[-c(3, 5, 7)]
colnames(channel_mean) <- c("channel", "delayed_HbO_mean","delayed_HbR_mean",
                            "online_HbO_mean", "online_HbR_mean")
channel_mean$Diff_delayed_onlineHbO <- channel_mean$online_HbO_mean - channel_mean$delayed_HbO_mean
channel_mean$Diff_delayed_onlineHbR <- channel_mean$online_HbR_mean - channel_mean$delayed_HbR_mean


# updata ab 26.11 ---------------------------------------------------------


### eine Möglichkeit für Korrelationsanalyse ,wenn es für uns nicht geeignet ist
### lösen wir es.

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




##### wilcox.test -----------------------
fNIRSData$diff <- fNIRSData$HbO - fNIRSData$HbR
#### testen, ob diff v. HbO & HbR von beiden Gr(online & delayed) signifikant voneinander unterscheiden
## get p-value für jede channel
getP <- function(x) {
  wilcox.test(subset(fNIRSData, condition == "online" & channel == x)$diff,
              subset(fNIRSData, condition == "delayed" & channel == x)$diff,
              alternative = "less", paired = TRUE, conf.level = 0.9)$p.value
}
v <- vector(mode = "numeric", length = 30)
for (i in 1:30) {
  v[[i]] <- getP(i) # vektor für p-value aller 30 channels
}
which(v <= 0.1) #channel, von denen die beide Gruppen online-delayed signifikant voneinander unterscheiden
