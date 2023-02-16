### libraries
library(tidyverse)
library(dplyr)
library(magrittr)
library(Hmisc)
library(PerformanceAnalytics)
library(mboost)
library(car)
library(effects)
require(nnet)
library(ggstatsplot)
library(MASS)
library(mice)

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


####### Frage 3: Korrelation einzelner channels zu looking experiment -----------------------
fNIRS.looking.age_sex <- fNIRS.looking.age_sex[1:53, ]

### Spalten für channel_diff(online-delayed) hinzufügen
fNIRS.looking.age_sex$channel1_diff <- fNIRS.looking.age_sex$HbO_online_1 - fNIRS.looking.age_sex$HbO_delayed_1
fNIRS.looking.age_sex$channel2_diff <- fNIRS.looking.age_sex$HbO_online_2 - fNIRS.looking.age_sex$HbO_delayed_2
fNIRS.looking.age_sex$channel3_diff <- fNIRS.looking.age_sex$HbO_online_3 - fNIRS.looking.age_sex$HbO_delayed_3
fNIRS.looking.age_sex$channel4_diff <- fNIRS.looking.age_sex$HbO_online_4 - fNIRS.looking.age_sex$HbO_delayed_4
fNIRS.looking.age_sex$channel5_diff <- fNIRS.looking.age_sex$HbO_online_5 - fNIRS.looking.age_sex$HbO_delayed_5
fNIRS.looking.age_sex$channel6_diff <- fNIRS.looking.age_sex$HbO_online_6 - fNIRS.looking.age_sex$HbO_delayed_6
fNIRS.looking.age_sex$channel7_diff <- fNIRS.looking.age_sex$HbO_online_7 - fNIRS.looking.age_sex$HbO_delayed_7
fNIRS.looking.age_sex$channel8_diff <- fNIRS.looking.age_sex$HbO_online_8 - fNIRS.looking.age_sex$HbO_delayed_8
fNIRS.looking.age_sex$channel9_diff <- fNIRS.looking.age_sex$HbO_online_9 - fNIRS.looking.age_sex$HbO_delayed_9
fNIRS.looking.age_sex$channel10_diff <- fNIRS.looking.age_sex$HbO_online_10 - fNIRS.looking.age_sex$HbO_delayed_10
fNIRS.looking.age_sex$channel11_diff <- fNIRS.looking.age_sex$HbO_online_11 - fNIRS.looking.age_sex$HbO_delayed_11
fNIRS.looking.age_sex$channel12_diff <- fNIRS.looking.age_sex$HbO_online_12 - fNIRS.looking.age_sex$HbO_delayed_12
fNIRS.looking.age_sex$channel13_diff <- fNIRS.looking.age_sex$HbO_online_13 - fNIRS.looking.age_sex$HbO_delayed_13
fNIRS.looking.age_sex$channel14_diff <- fNIRS.looking.age_sex$HbO_online_14 - fNIRS.looking.age_sex$HbO_delayed_14
fNIRS.looking.age_sex$channel15_diff <- fNIRS.looking.age_sex$HbO_online_15 - fNIRS.looking.age_sex$HbO_delayed_15
fNIRS.looking.age_sex$channel16_diff <- fNIRS.looking.age_sex$HbO_online_16 - fNIRS.looking.age_sex$HbO_delayed_16
fNIRS.looking.age_sex$channel17_diff <- fNIRS.looking.age_sex$HbO_online_17 - fNIRS.looking.age_sex$HbO_delayed_17
fNIRS.looking.age_sex$channel18_diff <- fNIRS.looking.age_sex$HbO_online_18 - fNIRS.looking.age_sex$HbO_delayed_18
fNIRS.looking.age_sex$channel19_diff <- fNIRS.looking.age_sex$HbO_online_19 - fNIRS.looking.age_sex$HbO_delayed_19
fNIRS.looking.age_sex$channel20_diff <- fNIRS.looking.age_sex$HbO_online_20 - fNIRS.looking.age_sex$HbO_delayed_20
fNIRS.looking.age_sex$channel21_diff <- fNIRS.looking.age_sex$HbO_online_21 - fNIRS.looking.age_sex$HbO_delayed_21
fNIRS.looking.age_sex$channel22_diff <- fNIRS.looking.age_sex$HbO_online_22 - fNIRS.looking.age_sex$HbO_delayed_22
fNIRS.looking.age_sex$channel23_diff <- fNIRS.looking.age_sex$HbO_online_23 - fNIRS.looking.age_sex$HbO_delayed_23
fNIRS.looking.age_sex$channel24_diff <- fNIRS.looking.age_sex$HbO_online_24 - fNIRS.looking.age_sex$HbO_delayed_24
fNIRS.looking.age_sex$channel25_diff <- fNIRS.looking.age_sex$HbO_online_25 - fNIRS.looking.age_sex$HbO_delayed_25
fNIRS.looking.age_sex$channel26_diff <- fNIRS.looking.age_sex$HbO_online_26 - fNIRS.looking.age_sex$HbO_delayed_26
fNIRS.looking.age_sex$channel27_diff <- fNIRS.looking.age_sex$HbO_online_27 - fNIRS.looking.age_sex$HbO_delayed_27
fNIRS.looking.age_sex$channel28_diff <- fNIRS.looking.age_sex$HbO_online_28 - fNIRS.looking.age_sex$HbO_delayed_28
fNIRS.looking.age_sex$channel29_diff <- fNIRS.looking.age_sex$HbO_online_29 - fNIRS.looking.age_sex$HbO_delayed_29
fNIRS.looking.age_sex$channel30_diff <- fNIRS.looking.age_sex$HbO_online_30 - fNIRS.looking.age_sex$HbO_delayed_30


### Spalten für ´ego - mirrored´ von looking experiment hinzufügen
fNIRS.looking.age_sex$looking_diff_1 <- fNIRS.looking.age_sex$`1_ego` - fNIRS.looking.age_sex$`1_mirrored`
fNIRS.looking.age_sex$looking_diff_2 <- fNIRS.looking.age_sex$`2_ego` - fNIRS.looking.age_sex$`2_mirrored`
fNIRS.looking.age_sex$looking_diff_3 <- fNIRS.looking.age_sex$`3_ego` - fNIRS.looking.age_sex$`3_mirrored`
fNIRS.looking.age_sex$looking_diff_4 <- fNIRS.looking.age_sex$`4_ego` - fNIRS.looking.age_sex$`4_mirrored`

### Spalten für looking_diff_sum hinzufügen
fNIRS.looking.age_sex$looking_diff_sum <- rowSums(fNIRS.looking.age_sex[163:166], na.rm = TRUE)

#### Regression zw. looking_diff_sum und 30 channels
### subdatensatz für Regression erstellen

mydata <- fNIRS.looking.age_sex[ -c(6, 17,18),c(133:162,167)]
###regression imputation
detregdata <- complete(mice(mydata, m = 1, method = "norm.predict"))
stochregdata1 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata2 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata3 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata4 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata5 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata6 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata7 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata8 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata9 <- complete(mice(mydata, m = 1, method = "norm.nob"))
stochregdata10 <- complete(mice(mydata, m = 1, method = "norm.nob"))
###mean imputation
for(i in 1:ncol(mydata)) {
  mydata[ ,i][is.na(mydata[ , i])] <- mean(mydata[[i]], na.rm=TRUE)
}

### schau wie das Variable looking_diff_sum verteilt
hist(mydata$looking_diff_sum, freq =FALSE,
     main = "Verteilung des Merkmals looking_diff_sum",
     ylab = "Dichte", xlab = "Summierte Differenz zwischen Abläufen im looking-Experiment (ms)")
lines(density(fNIRS.looking.age_sex$looking_diff_sum, na.rm = TRUE), col= "darkblue")
## looking_diff_sum annährend normalverteilt

### glm mit 30 channels als Kovariablen
model_all <- glm(looking_diff_sum ~ ., family = gaussian, data = mydata)
summary(model_all)
## R^2 = 0.6564739, AIC: 1224.2

### Variablenselektion per boosting-Verfahren
# Such nach optimale nu
# Modell mit nu = 0.05:
set.seed(123)
model_0.05 <- glmboost(looking_diff_sum ~ ., data = mydata,
                       control = boost_control(mstop = 2000, nu = 0.05))
cv_0.05 <- cvrisk(object = model_0.05,
                  folds = cv(weights = model.weights(model_0.05),
                             type = "kfold"))
mstop(cv_0.05)
mean(cv_0.05[, mstop(cv_0.05) + 1])
# 2008649661

# Modell mit nu = 0.1:
set.seed(123)
model_0.1 <- glmboost(looking_diff_sum ~ ., data = mydata,
                      control = boost_control(mstop = 2000, nu = 0.1))
cv_0.1 <- cvrisk(object = model_0.1,
                 folds = cv(weights = model.weights(model_0.1),
                            type = "kfold"))
mstop(cv_0.1)
mean(cv_0.1[, mstop(cv_0.1) + 1])
# 2005640084

# Modell mit nu = 0.2:
set.seed(123)
model_0.2 <- glmboost(looking_diff_sum ~ ., data = mydata,
                      control = boost_control(mstop = 2000, nu = 0.2))
cv_0.2<- cvrisk(object = model_0.2,
                folds = cv(weights = model.weights(model_0.2),
                           type = "kfold"))
mstop(cv_0.2)
mean(cv_0.2[, mstop(cv_0.2) + 1])
# 1975291736

##  Der niedrigste MSE  wird für das Modell mit den Parametern mstop = 22 
##  und ν = 0.2 erreicht.

model_boosting_opt <- glmboost(looking_diff_sum ~., data = mydata,
                               control = boost_control(mstop = 36, nu = 0.2))
summary(model_boosting_opt)

model_boosting_opt2 <- glmboost(looking_diff_sum ~., data = mydata,
                                control = boost_control(mstop = 2000, nu = 0.2))
summary(model_boosting_opt2)
par(mar = c(5,5,5,10))
plot(x = model_boosting_opt, main = "Koeffizientenpfade")

model_boost <- glm(looking_diff_sum ~ channel1_diff + channel23_diff +
                     channel18_diff + channel13_diff + channel20_diff +
                     channel2_diff + channel12_diff + channel15_diff, 
                   family = gaussian, data = mydata)
model_boost <- glm(looking_diff_sum ~ channel1_diff + channel23_diff +
                     channel18_diff + channel13_diff + channel20_diff +
                     channel2_diff + channel12_diff + channel15_diff + channel7_diff, 
                   family = gaussian, data = mydata)
summary(model_boost)

### Variablenselektion per Vorwärts- und Rückwärtsselektion
stepAIC(model_all, direction = "both")

### glm mit selektiert channels von oben
model_step <-  glm(formula = looking_diff_sum ~ channel1_diff + channel3_diff + 
                     channel4_diff + channel7_diff + channel10_diff + channel12_diff + 
                     channel13_diff + channel14_diff + channel15_diff + channel18_diff + 
                     channel19_diff + channel20_diff + channel23_diff + channel24_diff + 
                     channel29_diff, family = gaussian, data = mydata)
summary(model_step)
# R^2 = 0.5995779, AIC: 1201.9



plot(allEffects(model_step))


avPlots(model_step)

##### gruppierung
##### gruppierung von looking daten
data<-fNIRS.looking.age_sex[163:166]
## 2 gruppen
looking_group <- rep("other", 53)
for (i in 1:nrow(data)) {
  if (all(data[i, ] > 0, na.rm = TRUE)) {
    looking_group[i] <- "content"
  }
  if (all(data[i, ] < 0, na.rm = TRUE)) {
    looking_group[i] <- "content"
  }
  if(anyNA(data[i, ])) {
    looking_group[i] <- "other"
  }
}
looking_group
fNIRS.looking.age_sex$looking_group<-looking_group


fNIRSData$looking_group <- rep("xxx", nrow(fNIRSData))
for (i in 1:nrow(fNIRSData)) {
  fNIRSData$looking_group[i] <- fNIRS.looking.age_sex$looking_group[which(fNIRS.looking.age_sex$id == fNIRSData$id[i])]
}

fNIRS.online <- subset(fNIRSData, condition == "online")
fNIRS.delayed <- subset(fNIRSData, condition == "delayed")
fNIRS.online$diff_HbO <- fNIRS.online$HbO - fNIRS.delayed$HbO
fNIRS.online$diff_HbR <- fNIRS.online$HbR - fNIRS.delayed$HbR

## 3 gruppen
looking_group <- rep("other", 53)
for (i in 1:nrow(data)) {
  if(anyNA(data[i, ])) {
    looking_group[i] <- "other"
  }else if (all(data[i, ] > 0, na.rm = TRUE)) {
    looking_group[i] <- "content"
  }else if (all(data[i, ] < 0, na.rm = TRUE)) {
    looking_group[i] <- "content"
  }else if (data[i, 1] < 0 && data[i, 2] < 0 && data[i, 3] > 0 && data[i, 4]>0) {
    looking_group[i] <- "side"
  }else if (data[i, 1] > 0 && data[i, 2] > 0 && data[i, 3] < 0 && data[i, 4]<0) {
    looking_group[i] <- "side"
  }
}
looking_group
fNIRS.looking.age_sex$looking_group <- looking_group
### looking_group zu fNIRSData hinzufügen
fNIRSData$looking_group <- rep("xxx", nrow(fNIRSData))
for (i in 1:nrow(fNIRSData)) {
  fNIRSData$looking_group[i] <- fNIRS.looking.age_sex$looking_group[which(fNIRS.looking.age_sex$id == fNIRSData$id[i])]
}


fNIRS.online <- subset(fNIRSData, condition == "online")
fNIRS.delayed <- subset(fNIRSData, condition == "delayed")
fNIRS.online$diff_HbO <- fNIRS.online$HbO - fNIRS.delayed$HbO
fNIRS.online$diff_HbR <- fNIRS.online$HbR - fNIRS.delayed$HbR


### gruppierung von fNIRS-daten
fNIRS.looking.age_sex$mu<- rowMeans(fNIRS.looking.age_sex[133:162], na.rm = TRUE)
boxplot(fNIRS.looking.age_sex$mu)
fNIRS_group <- rep("other", 53)
for (i in 1:nrow(fNIRS.looking.age_sex)) {
  if (fNIRS.looking.age_sex$mu[i] > 0) {
    fNIRS_group[i] <- "lower activity"
  }else if (fNIRS.looking.age_sex$mu[i] < 0) {
    fNIRS_group[i] <- "higher activity"
  }
}
fNIRS_group
fNIRS.looking.age_sex$fNIRS_group<- fNIRS_group

#### Kontigenztabelle von fNIRS_group und looking_group
table(fNIRS.looking.age_sex[, c("looking_group", "fNIRS_group")])

#### Unabhängigkeitstest zwischen fNIRS_group und looking_group
chisq.test(subset(fNIRS.looking.age_sex, looking_group!="other")$looking_group,
           subset(fNIRS.looking.age_sex, looking_group!="other")$fNIRS_group)



#### Regression zwischen looking_group und 30 channel

### subdaten für die Regression
mydata2 <- fNIRS.looking.age_sex[c(131:162, 168)]

### Referenzgruppe als "other" erstellen
mydata2$looking_group[which(mydata2$looking_group == "other")] <- "a other"

### multinom Regression
group_channel_reg <- multinom(formula = looking_group ~., data = mydata2)
summary(group_channel_reg)
## double positiv: channel 1, 2, 3, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 28, 29, 30
## double neg:, age_days, channel 9, 26
##  content neg: channel 9, 24, 26, 27
## positiv bedeutet: Die Chance, dass die Probanden nach "content" oder "side" schauen,
## und Gehirnaktivität dieses channel proportional zueinender ist.



#### plot diff_HbO einzelner channel nach gruppen
ggplot(subset(fNIRSData, condition == "online"), aes(x=channel, y=HbO, colour=looking_group))+geom_point()
ggplot(fNIRS.online, aes(x=channel, y = diff_HbO, colour=looking_group))+geom_point()
ggplot(fNIRS.online, aes(x=channel, y = diff_HbR, colour=looking_group))+geom_point()
ggplot(subset(fNIRS.online, looking_group != "other"), aes(x=channel, y=diff_HbO, colour=looking_group))+geom_point()


ggplot(subset(fNIRS.online, looking_group == "other"), aes(x=channel, y=diff_HbO, colour=looking_group))+geom_point()
ggplot(subset(fNIRS.online, looking_group == "content"), aes(x=channel, y=diff_HbO, colour=looking_group))+geom_point()
ggplot(subset(fNIRS.online, looking_group == "side"), aes(x=channel, y=diff_HbO, colour=looking_group))+geom_point()

#### t test vergleichen der Mittelwert von group "content" und "side" von looking daten
t.test(diff_HbO ~ looking_group, data = subset(fNIRS.online, looking_group!="other"), 
       var.equal = FALSE, conf.level=0.95, alternative="two.sided")

t.test(diff_HbO ~ looking_group, data = subset(fNIRS.online, looking_group!="other"), 
       var.equal = FALSE, conf.level=0.95, alternative="less")

t.test(diff_HbO ~ looking_group, data = subset(fNIRS.online, looking_group!="other" & channel == 24),
       var.equal=FALSE, conf.level=0.95, alternative = "two.sided")


getP <- function(x) {
  t.test(subset(fNIRS.online, looking_group!="other" & channel == i)$diff_HbO ~
           subset(fNIRS.online, looking_group!="other" & channel == i)$looking_group,
         var.equal=FALSE, conf.level=0.95, alternative = "less")$p.value
}
v <- vector(mode = "numeric", length = 30)
for (i in 1:30) {
  v[[i]] <- getP(i) # vektor für p-value aller 30 channels
}
v
which(v <= 0.05)

#### boxplot + t test 
# alle channel
ggbetweenstats(fNIRS.online, x = looking_group, y=diff_HbO,
               plot.type = "box", type = "parametric", var.equal = FALSE, bf.message = FALSE)

ggplot(fNIRS.online, aes(x=diff_HbO, fill=looking_group)) + geom_density(alpha=0.5)

### Korrelationstest zwischen looking_diff_sum und mu
 cor.test(fNIRS.looking.age_sex$looking_diff_sum, fNIRS.looking.age_sex$mu)
