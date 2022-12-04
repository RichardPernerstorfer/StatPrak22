### libraries
library(tidyverse)
library(dplyr)
library(magrittr)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(mboost)

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
fNIRSData$diff <- fNIRSData$HbO - fNIRSData$HbR
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

channel_mean$totalDiff <- (aggregate(subset(fNIRSData, condition == "online")$diff,
          list(subset(fNIRSData, condition == "online")$channel),
          mean, na.rm=TRUE) - aggregate(subset(fNIRSData, condition == "delayed")$diff,
                                        list(subset(fNIRSData, condition == "delayed")$channel),
                                        mean, na.rm=TRUE))[[2]]

ggplot(channel_mean, aes(x=channel, y=totalDiff))+geom_point()


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


####### Frage 3: Korrelation einzelner channels zu looking experiment -----------------------
### Spalten für channel_total hinzufügen
fNIRS.looking.age_sex$channel1_total <- (fNIRS.looking.age_sex$HbO_online_1 - fNIRS.looking.age_sex$HbR_online_1) - (fNIRS.looking.age_sex$HbO_delayed_1 - fNIRS.looking.age_sex$HbR_delayed_1)
fNIRS.looking.age_sex$channel2_total <- (fNIRS.looking.age_sex$HbO_online_2 - fNIRS.looking.age_sex$HbR_online_2) - (fNIRS.looking.age_sex$HbO_delayed_2 - fNIRS.looking.age_sex$HbR_delayed_2)
fNIRS.looking.age_sex$channel3_total <- (fNIRS.looking.age_sex$HbO_online_3 - fNIRS.looking.age_sex$HbR_online_3) - (fNIRS.looking.age_sex$HbO_delayed_3 - fNIRS.looking.age_sex$HbR_delayed_3)
fNIRS.looking.age_sex$channel4_total <- (fNIRS.looking.age_sex$HbO_online_4 - fNIRS.looking.age_sex$HbR_online_4) - (fNIRS.looking.age_sex$HbO_delayed_4 - fNIRS.looking.age_sex$HbR_delayed_4)
fNIRS.looking.age_sex$channel5_total <- (fNIRS.looking.age_sex$HbO_online_5 - fNIRS.looking.age_sex$HbR_online_5) - (fNIRS.looking.age_sex$HbO_delayed_5 - fNIRS.looking.age_sex$HbR_delayed_5)
fNIRS.looking.age_sex$channel6_total <- (fNIRS.looking.age_sex$HbO_online_6 - fNIRS.looking.age_sex$HbR_online_6) - (fNIRS.looking.age_sex$HbO_delayed_6 - fNIRS.looking.age_sex$HbR_delayed_6)
fNIRS.looking.age_sex$channel7_total <- (fNIRS.looking.age_sex$HbO_online_7 - fNIRS.looking.age_sex$HbR_online_7) - (fNIRS.looking.age_sex$HbO_delayed_7 - fNIRS.looking.age_sex$HbR_delayed_7)
fNIRS.looking.age_sex$channel8_total <- (fNIRS.looking.age_sex$HbO_online_8 - fNIRS.looking.age_sex$HbR_online_8) - (fNIRS.looking.age_sex$HbO_delayed_8 - fNIRS.looking.age_sex$HbR_delayed_8)
fNIRS.looking.age_sex$channel9_total <- (fNIRS.looking.age_sex$HbO_online_9 - fNIRS.looking.age_sex$HbR_online_9) - (fNIRS.looking.age_sex$HbO_delayed_9 - fNIRS.looking.age_sex$HbR_delayed_9)
fNIRS.looking.age_sex$channel10_total <- (fNIRS.looking.age_sex$HbO_online_10 - fNIRS.looking.age_sex$HbR_online_10) - (fNIRS.looking.age_sex$HbO_delayed_10 - fNIRS.looking.age_sex$HbR_delayed_10)
fNIRS.looking.age_sex$channel11_total <- (fNIRS.looking.age_sex$HbO_online_11 - fNIRS.looking.age_sex$HbR_online_11) - (fNIRS.looking.age_sex$HbO_delayed_11 - fNIRS.looking.age_sex$HbR_delayed_11)
fNIRS.looking.age_sex$channel12_total <- (fNIRS.looking.age_sex$HbO_online_12 - fNIRS.looking.age_sex$HbR_online_12) - (fNIRS.looking.age_sex$HbO_delayed_12 - fNIRS.looking.age_sex$HbR_delayed_12)
fNIRS.looking.age_sex$channel13_total <- (fNIRS.looking.age_sex$HbO_online_13 - fNIRS.looking.age_sex$HbR_online_13) - (fNIRS.looking.age_sex$HbO_delayed_13 - fNIRS.looking.age_sex$HbR_delayed_13)
fNIRS.looking.age_sex$channel14_total <- (fNIRS.looking.age_sex$HbO_online_14 - fNIRS.looking.age_sex$HbR_online_14) - (fNIRS.looking.age_sex$HbO_delayed_14 - fNIRS.looking.age_sex$HbR_delayed_14)
fNIRS.looking.age_sex$channel15_total <- (fNIRS.looking.age_sex$HbO_online_15 - fNIRS.looking.age_sex$HbR_online_15) - (fNIRS.looking.age_sex$HbO_delayed_15 - fNIRS.looking.age_sex$HbR_delayed_15)
fNIRS.looking.age_sex$channel16_total <- (fNIRS.looking.age_sex$HbO_online_16 - fNIRS.looking.age_sex$HbR_online_16) - (fNIRS.looking.age_sex$HbO_delayed_16 - fNIRS.looking.age_sex$HbR_delayed_16)
fNIRS.looking.age_sex$channel17_total <- (fNIRS.looking.age_sex$HbO_online_17 - fNIRS.looking.age_sex$HbR_online_17) - (fNIRS.looking.age_sex$HbO_delayed_17 - fNIRS.looking.age_sex$HbR_delayed_17)
fNIRS.looking.age_sex$channel18_total <- (fNIRS.looking.age_sex$HbO_online_18 - fNIRS.looking.age_sex$HbR_online_18) - (fNIRS.looking.age_sex$HbO_delayed_18 - fNIRS.looking.age_sex$HbR_delayed_18)
fNIRS.looking.age_sex$channel19_total <- (fNIRS.looking.age_sex$HbO_online_19 - fNIRS.looking.age_sex$HbR_online_19) - (fNIRS.looking.age_sex$HbO_delayed_19 - fNIRS.looking.age_sex$HbR_delayed_19)
fNIRS.looking.age_sex$channel20_total <- (fNIRS.looking.age_sex$HbO_online_20 - fNIRS.looking.age_sex$HbR_online_20) - (fNIRS.looking.age_sex$HbO_delayed_20 - fNIRS.looking.age_sex$HbR_delayed_20)
fNIRS.looking.age_sex$channel21_total <- (fNIRS.looking.age_sex$HbO_online_21 - fNIRS.looking.age_sex$HbR_online_21) - (fNIRS.looking.age_sex$HbO_delayed_21 - fNIRS.looking.age_sex$HbR_delayed_21)
fNIRS.looking.age_sex$channel22_total <- (fNIRS.looking.age_sex$HbO_online_22 - fNIRS.looking.age_sex$HbR_online_22) - (fNIRS.looking.age_sex$HbO_delayed_22 - fNIRS.looking.age_sex$HbR_delayed_22)
fNIRS.looking.age_sex$channel23_total <- (fNIRS.looking.age_sex$HbO_online_23 - fNIRS.looking.age_sex$HbR_online_23) - (fNIRS.looking.age_sex$HbO_delayed_23 - fNIRS.looking.age_sex$HbR_delayed_23)
fNIRS.looking.age_sex$channel24_total <- (fNIRS.looking.age_sex$HbO_online_24 - fNIRS.looking.age_sex$HbR_online_24) - (fNIRS.looking.age_sex$HbO_delayed_24 - fNIRS.looking.age_sex$HbR_delayed_24)
fNIRS.looking.age_sex$channel25_total <- (fNIRS.looking.age_sex$HbO_online_25 - fNIRS.looking.age_sex$HbR_online_25) - (fNIRS.looking.age_sex$HbO_delayed_25 - fNIRS.looking.age_sex$HbR_delayed_25)
fNIRS.looking.age_sex$channel26_total <- (fNIRS.looking.age_sex$HbO_online_26 - fNIRS.looking.age_sex$HbR_online_26) - (fNIRS.looking.age_sex$HbO_delayed_26 - fNIRS.looking.age_sex$HbR_delayed_26)
fNIRS.looking.age_sex$channel27_total <- (fNIRS.looking.age_sex$HbO_online_27 - fNIRS.looking.age_sex$HbR_online_27) - (fNIRS.looking.age_sex$HbO_delayed_27 - fNIRS.looking.age_sex$HbR_delayed_27)
fNIRS.looking.age_sex$channel28_total <- (fNIRS.looking.age_sex$HbO_online_28 - fNIRS.looking.age_sex$HbR_online_28) - (fNIRS.looking.age_sex$HbO_delayed_28 - fNIRS.looking.age_sex$HbR_delayed_28)
fNIRS.looking.age_sex$channel29_total <- (fNIRS.looking.age_sex$HbO_online_29 - fNIRS.looking.age_sex$HbR_online_29) - (fNIRS.looking.age_sex$HbO_delayed_29 - fNIRS.looking.age_sex$HbR_delayed_29)
fNIRS.looking.age_sex$channel30_total <- (fNIRS.looking.age_sex$HbO_online_30 - fNIRS.looking.age_sex$HbR_online_30) - (fNIRS.looking.age_sex$HbO_delayed_30 - fNIRS.looking.age_sex$HbR_delayed_30)


### Spalten für ego - mirrored von looking experiment hinzufügen
fNIRS.looking.age_sex$looking_diff_1 <- fNIRS.looking.age_sex$`1_ego` - fNIRS.looking.age_sex$`1_mirrored`
fNIRS.looking.age_sex$looking_diff_2 <- fNIRS.looking.age_sex$`2_ego` - fNIRS.looking.age_sex$`2_mirrored`
fNIRS.looking.age_sex$looking_diff_3 <- fNIRS.looking.age_sex$`3_ego` - fNIRS.looking.age_sex$`3_mirrored`
fNIRS.looking.age_sex$looking_diff_4 <- fNIRS.looking.age_sex$`4_ego` - fNIRS.looking.age_sex$`4_mirrored`
### Spalten für looking_diff_mean hinzufügen
fNIRS.looking.age_sex$looking_diff_mean <- rowMeans(fNIRS.looking.age_sex[163:166], na.rm = TRUE)
### subdatensatz für Regression erstellen
mydata <- fNIRS.looking.age_sex[,c(133:162,167)]
### Variablenselektion per boosting-Verfahren
summary(glmboost(looking_diff_mean ~., data = mydata, control = boost_control(mstop = 2574)))


