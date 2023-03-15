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
library(clustree)
library(factoextra)
library(NbClust)

### original data setsw
load('looking.RData')
load('fNIRSData.RData')
load('age_sex.RData')

### create tidydata datasets
fNIRS2 <- pivot_wider(data = fNIRSData,
                      names_from = c(condition, channel),
                      values_from = c(HbO, HbR))
looking2 <- pivot_wider(data = looking,
                        names_from = c(trial, view),
                        values_from = duration)
data.list <- list(fNIRS2, looking2, age_sex)
fNIRS.looking.age_sex <- data.list %>%
  reduce(full_join, by = "id")

### reduce data set to those we have both experiments of
fNIRS.looking.age_sex <- fNIRS.looking.age_sex[-c(17,18), ]
fNIRS.looking.age_sex <- fNIRS.looking.age_sex[1:51, ]

### NA imputation
## create subsets of the data
HbO_delayed_data <- dplyr::select(fNIRS.looking.age_sex, contains("HbO_delayed"))
HbO_online_data <- dplyr::select(fNIRS.looking.age_sex, contains("HbO_online"))
HbR_delayed_data <- dplyr::select(fNIRS.looking.age_sex, contains("HbR_delayed"))
HbR_online_data <- dplyr::select(fNIRS.looking.age_sex, contains("HbR_online"))
imp_looking2 <- looking2
colnames(imp_looking2) <- c("id", "ego1", "mirrored1", "ego2", "mirrored2", "ego3", "mirrored3", "ego4", "mirrored4")
imp_looking2 <- complete(mice(imp_looking2, m = 1, method = "cart"))
colnames(imp_looking2) <- colnames(looking2)

## create new data set 10 times with the same imputation method

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_1 <- data.list %>%
  reduce(full_join, by = "id")
imp_1 <- imp_1[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_2 <- data.list %>%
  reduce(full_join, by = "id")
imp_2 <- imp_2[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_3 <- data.list %>%
  reduce(full_join, by = "id")
imp_3 <- imp_3[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_4 <- data.list %>%
  reduce(full_join, by = "id")
imp_4 <- imp_4[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_5 <- data.list %>%
  reduce(full_join, by = "id")
imp_5 <- imp_5[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_6 <- data.list %>%
  reduce(full_join, by = "id")
imp_6 <- imp_6[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
imp_7 <- data.list %>%
  reduce(full_join, by = "id")
imp_7 <- imp_7[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_8 <- data.list %>%
  reduce(full_join, by = "id")
imp_8 <- imp_8[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_9 <- data.list %>%
  reduce(full_join, by = "id")
imp_9 <- imp_9[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

imp_HbO_online <- complete(mice(HbO_online_data, m = 1, method = "norm.nob"))
imp_HbO_delayed <- complete(mice(HbO_delayed_data, m = 1, method = "norm.nob"))
imp_HbR_online <- complete(mice(HbR_online_data, m = 1, method = "norm.nob"))
imp_HbR_delayed <- complete(mice(HbR_delayed_data, m = 1, method = "norm.nob"))
imp_Data <- merge(merge(merge(imp_HbO_online, imp_HbO_delayed), imp_HbR_online), imp_HbR_delayed)
data.list <- list(imp_Data, imp_looking2, age_sex)
imp_10 <- data.list %>%
  reduce(full_join, by = "id")
imp_10 <- imp_10[1:51, ]
rm(imp_HbO_online, imp_HbO_delayed, imp_HbR_online, imp_HbR_delayed, imp_Data)

##add difference collumns
add_differences <- function(data){
  ## change name to fit old code
  fNIRS.looking.age_sex <- data
  ## remove empty rows where fNIRS Data is missing
  fNIRS.looking.age_sex <- fNIRS.looking.age_sex[1:51, ]
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
  fNIRS.looking.age_sex$looking_diff_sum <- abs(rowSums(fNIRS.looking.age_sex[163:166], na.rm = TRUE))
  fNIRS.looking.age_sex <- fNIRS.looking.age_sex[1:51, ]
  return(fNIRS.looking.age_sex)
}

### Dendrogram erstellen
grouping_dendrogram <- function(data){
  grouping_data <- data[,c(1:121)]
  tmp <- NULL
  for (k in 1:11){
    tmp[k] <- kmeans(grouping_data[,2:121], k, nstart = 30)
  }
  df <- data.frame(tmp)
  colnames(df) <- seq(1:11)
  colnames(df) <- paste0("k",colnames(df))
  df.pca <- prcomp(df, center = TRUE, scale. = FALSE)
  ind.coord <- df.pca$x
  ind.coord <- ind.coord[,1:2]
  df <- cbind(as.data.frame(df), as.data.frame(ind.coord))
  clustree(df, prefix = "k")
}

###Cluster Optimierungsplots
optimize_cluster_wss <- function(data){
  grouping_data_new <- data[,2:121]
  fviz_nbclust(grouping_data_new, kmeans, method = "wss", k.max = 40) + theme_minimal() + ggtitle("Within Sum of Square Method") +
   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))
}
optimize_cluster_silhouette <- function(data){
  grouping_data_new <- data[,2:121]
  fviz_nbclust(grouping_data_new, kmeans, method = "silhouette", k.max = 40) + theme_minimal() + ggtitle("the Elbow Method") +
   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))
}

optimize_looking_wss <- function(data){
  grouping_data_new <- data[-c(6,17,18),122:129]
  fviz_nbclust(grouping_data_new, kmeans, method = "wss", k.max = 40) + theme_minimal() + ggtitle("Within Sum of Square Method") +
   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))
}
optimize_looking_silhouette <- function(data){
  grouping_data_new <- data[-c(6,17,18),122:129]
  fviz_nbclust(grouping_data_new, kmeans, method = "silhouette", k.max = 40) + theme_minimal() + ggtitle("the Elbow Method") +
   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))
}

optimize_channels_wss <- function(data){
  grouping_data_new <- data
  fviz_nbclust(grouping_data_new, kmeans, method = "wss", k.max = 15) + theme_minimal() + ggtitle("Within Sum of Square Method") +
   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))
}
optimize_channels_silhouette <- function(data){
  grouping_data_new <- data
  fviz_nbclust(grouping_data_new, kmeans, method = "silhouette", k.max = 15) + theme_minimal() + ggtitle("the Elbow Method") +
   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))
}

### get_cluster (n = Anzahl der Cluster)
get_cluster <- function(data, n){
  grouping_data_new <- data[,2:121]
  km.res <- kmeans(grouping_data_new, n, nstart = 25)
  return(km.res$cluster)
}

get_cluster2 <- function(data, n){
  grouping_data_new <- data[,133:162]
  km.res <- kmeans(grouping_data_new, n, nstart = 25)
  return(km.res$cluster)
}

get_looking_cluster <- function(data, n){
  grouping_data_new <- data[,122:129]
  km.res <- kmeans(grouping_data_new, n, nstart = 25)
  return(km.res$cluster)
}

### get looking group
## get 2 groups
looking_2_groups <- function(data){
  data2 <- dplyr::select(data, contains("looking_diff_"))[,1:4]
  looking_group <- rep("other", 51)
  for (i in 1:nrow(data2)) {
    if (all(data2[i, ] > 0, na.rm = TRUE)) {
      looking_group[i] <- "content"
    }
    if (all(data2[i, ] < 0, na.rm = TRUE)) {
      looking_group[i] <- "content"
    }
    if(anyNA(data2[i, ])) {
      looking_group[i] <- "other"
    }
  }
  return(looking_group)
}

## get 3 groups
looking_3_groups <- function(data){
  data2 <- dplyr::select(data, contains("looking_diff_"))[,1:4]
  looking_group <- rep("other", 51)
  for (i in 1:nrow(data2)) {
    if(anyNA(data2[i, ])) {
      looking_group[i] <- "other"
    }else if (all(data2[i, ] > 0, na.rm = TRUE)) {
      looking_group[i] <- "content"
    }else if (all(data2[i, ] < 0, na.rm = TRUE)) {
      looking_group[i] <- "content"
    }else if (data2[i, 1] < 0 && data2[i, 2] < 0 && data2[i, 3] > 0 && data2[i, 4]>0) {
      looking_group[i] <- "side"
    }else if (data2[i, 1] > 0 && data2[i, 2] > 0 && data2[i, 3] < 0 && data2[i, 4]<0) {
      looking_group[i] <- "side"
    }
  }
  return(looking_group)
}
### get fNIRS group by higher or lower mean value
get_groups_by_fNIRSmean <- function(data){
  fNIRS_group <- rowMeans(data[,133:162], na.rm = TRUE)
  for (i in 1:nrow(data)) {
    if (fNIRS_group[i] > 0) {
      fNIRS_group[i] <- "lower activity"
    }else if (fNIRS_group[i] < 0) {
      fNIRS_group[i] <- "higher activity"
    }
  }
  return(fNIRS_group)
}

### Add all possible grouping vectors to our data set
add_all_groups <- function(data, n, m){
  data <- add_differences(data)
  data$group_fNIRS_basecluster <- as.factor(get_cluster(data, n))
  data$group_fNIRS_mean <- as.factor(get_groups_by_fNIRSmean(data))
  data$group_looking_2 <- as.factor(looking_2_groups(data))
  data$group_looking_3 <- as.factor(looking_3_groups(data))
  data$group_looking_cluster <- as.factor(get_looking_cluster(data, m))
  data$group_fNIRS_cluster <- as.factor(get_cluster2(data, n))
  return(data)
}

### Cluster plotten (n = Anzahl der Cluster)
cluster_plot <- function(data, n){
  grouping_data_new <- data[,2:121]
  km.res <- kmeans(grouping_data_new, n, nstart = 25)
  fviz_cluster(km.res, data = grouping_data_new) +
   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))
}

cluster_looking_plot <- function(data, n){
  grouping_data_new <- data[-c(6,17,18),122:129]
  km.res <- kmeans(grouping_data_new, n, nstart = 25)
  fviz_cluster(km.res, data = grouping_data_new) +
   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))
}
### Kapitel: NAs und Imputation
new_data <- dplyr::select(fNIRS2, contains("HbO_online"))[,2:31]
# Plot NAs pro Channel
ggplot(data.frame(colSums(is.na(new_data))), aes(seq_along(colSums(is.na(new_data))), colSums(is.na(new_data)))) + 
  geom_bar(stat = "identity") + ggtitle("Anzahl NAs in Channel") + labs(x = "Channel", y = "Anzahl NAs") + 
  theme_light() +
  theme(plot.title = element_text(size = 24, face = "bold"), 
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) + scale_x_discrete(limits = c(1:30))
# Wie viele NAs fNIRS
length(which(is.na(new_data))) / (ncol(new_data)*nrow(new_data))
# Wie viele Probanden ohne NAs fNIRS
length(which(rowSums(is.na(new_data)) == 0))
# maximale Anzahl an NAs pro Proband
max(rowSums(is.na(new_data)))
# median Anzahl an NAs pro Proband
median(rowSums(is.na(new_data)))
# Plot NAs pro Proband
ggplot(data.frame(rowSums(is.na(new_data))), aes(seq_along(rowSums(is.na(new_data))), rowSums(is.na(new_data)))) + 
  geom_bar(stat = "identity") + ggtitle("NAs in Probanden") + labs(x = "Proband", y = "Anzahl NAs") + 
  theme(plot.title = element_text(size = 24, face = "bold"), 
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16))
# Wie viele NAs looking
length(which(is.na(looking2))) / (8*nrow(new_data))
# Wie viele Probanden ohne NAs looking
length(which(rowSums(is.na(looking2)) == 0))

### Kapitel: Frage 1
##Korrelationsplots

## korr_tab_diffHbO

corrplot_channel_diffHbO <- corrplot(korr_tab_diffHbO, type = "upper", order = "AOE",
         title = "Korrelationsplot der HbO-Differenzen", mar=c(1, 1, 1, 1), tl.cex=0.5) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))

## korr_tab_diffHbR
corrplot_channel_diffHbR <- corrplot(korr_tab_diffHbR, type = "upper", order = "AOE",
                                     title = "Korrelationsplot der HbR-Differenzen",
                                     mar=c(1, 1, 1, 1), tl.cex=0.5) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), plot.title = element_text(size = 24, face = "bold"))


## Verteilung der HbO-Channel
# nach Channels
imp2 <- add_all_groups(imp_2, 3, 3)
imp2 <- imp2[,133:162]
imp2 <- t(imp2)
optimize_channels_wss(imp2)
optimize_channels_silhouette(imp2)
cluster_plot(imp2,2)
# nach Probanden
optimize_cluster_wss(imp_2)
optimize_cluster_silhouette(imp_2)
cluster_plot(imp_2,3)
# nach Looking
optimize_looking_wss(imp_2)
optimize_looking_silhouette(imp_2)
cluster_plot(imp_2,3)

### Kapitel: eigene Gruppen
summary(add_all_groups(imp_1, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_2, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_3, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_4, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_5, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_6, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_7, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_8, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_9, 3, 3)$group_fNIRS_mean)
summary(add_all_groups(imp_10, 3, 3)$group_fNIRS_mean)
median(c(as.vector(summary(add_all_groups(imp_1, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_2, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_3, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_4, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_5, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_6, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_7, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_8, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_9, 3, 3)$group_fNIRS_mean))[1],
       as.vector(summary(add_all_groups(imp_10, 3, 3)$group_fNIRS_mean))[1]))
summary(add_all_groups(imp_1, 3, 3)$group_looking_3)

### Kapitel Frage 2
# Kontingenztafel FNIRS Cluster Looking Cluster
prop.table(table(imp1$group_looking_cluster, imp1$group_fNIRS_cluster))
fisher.test(table(imp1$group_looking_cluster, imp1$group_fNIRS_cluster))
# Kontingenztafel fNIRS Gruppe Looking Gruppe
prop.table(table(imp1$group_fNIRS_mean, imp1$group_looking_3))
fisher.test(table(imp1$group_fNIRS_mean, imp1$group_looking_3))
### delete unwanted files
rm(fNIRS2, looking2, data.list, HbO_delayed_data, HbO_online_data, HbR_delayed_data, HbR_online_data, age_sex, fNIRSData, looking, imp_looking2)
