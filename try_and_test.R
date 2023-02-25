
# test --------------------------------------------------------------------

# #datas ------------------------------------------------------------------


try_data_1 <- add_all_groups(imp_1, 7, 3)
try_data_2 <- add_all_groups(imp_2, 7, 3)
try_data_3 <- add_all_groups(imp_3, 7, 3)
try_data_4 <- add_all_groups(imp_4, 7, 3)
try_data_5 <- add_all_groups(imp_5, 7, 3)
try_data_6 <- add_all_groups(imp_6, 7, 3)
try_data_7 <- add_all_groups(imp_7, 7, 3)
try_data_8 <- add_all_groups(imp_8, 7, 3)
try_data_9 <- add_all_groups(imp_9, 7, 3)
try_data_10 <- add_all_groups(imp_10, 7, 3)
try_data_11 <- add_all_groups(imp_1, 4, 3)
try_data_12 <- add_all_groups(imp_2, 4, 3)
try_data_13 <- add_all_groups(imp_3, 4, 3)
try_data_14 <- add_all_groups(imp_4, 4, 3)
try_data_15 <- add_all_groups(imp_5, 4, 3)
try_data_16 <- add_all_groups(imp_6, 4, 3)
try_data_17 <- add_all_groups(imp_7, 4, 3)
try_data_18 <- add_all_groups(imp_8, 4, 3)
try_data_19 <- add_all_groups(imp_9, 4, 3)
try_data_20 <- add_all_groups(imp_10, 4, 3)

formula1 <- as.formula("group_fNIRS_mean ~ looking_diff_1 + looking_diff_2+
                       looking_diff_3 + looking_diff_4")
formula2 <- as.formula("looking_diff_sum ~ channel1_diff + channel2_diff + 
                       channel3_diff + channel4_diff + channel5_diff +
                       channel6_diff +channel7_diff + channel8_diff +
                       channel9_diff + channel10_diff + channel11_diff +
                       channel12_diff + channel13_diff + channel14_diff +
                       channel15_diff + channel16_diff + channel17_diff +
                       channel18_diff + channel19_diff + channel20_diff + 
                       channel21_diff + channel22_diff + channel23_diff +
                       channel24_diff + channel25_diff + channel26_diff +
                       channel27_diff + channel28_diff + channel29_diff +
                       channel30_diff")
formula3 <- as.formula("group_looking_cluster ~ channel1_diff + channel2_diff + 
                       channel3_diff + channel4_diff + channel5_diff +
                       channel6_diff +channel7_diff + channel8_diff +
                       channel9_diff + channel10_diff + channel11_diff +
                       channel12_diff + channel13_diff + channel14_diff +
                       channel15_diff + channel16_diff + channel17_diff +
                       channel18_diff + channel19_diff + channel20_diff + 
                       channel21_diff + channel22_diff + channel23_diff +
                       channel24_diff + channel25_diff + channel26_diff +
                       channel27_diff + channel28_diff + channel29_diff +
                       channel30_diff")


# functions ---------------------------------------------------------------


regression <- function(data, formula) {
  tryCatch({
    multinom(formula = formula, data = data) |>
      summary()
  },
  warning = function(w){},
  error = function(e){print("error multinom")},
  finally = {print("multinom")
    })


}


tables <- function(data){
  table(data[, c("group_looking_cluster", "group_fNIRS_cluster")])|>
    prop.table()
  table(data[, c("group_fNIRS_cluster", "group_looking_2")])|>
    prop.table()
  table(data[, c("group_fNIRS_cluster", "group_looking_3")])|>
    prop.table()
  table(data[, c("group_looking_cluster", "group_fNIRS_mean")])|>
    prop.table()
  table(data[, c("group_fNIRS_mean", "group_looking_2")])|>
    prop.table()
  table(data[, c("group_fNIRS_mean", "group_looking_3")])|>
    prop.table()
}

tests <- function(data){
  fisher.test(data$group_looking_cluster,
              data$group_fNIRS_mean)
  chisq.test(data$group_looking_3,
             data$group_fNIRS_mean)
  fisher.test(data$group_looking_2, 
              data$group_fNIRS_mean)
  fisher.test(data$group_looking_cluster,
              data$group_fNIRS_cluster)
  chisq.test(data$group_looking_3,
             data$group_fNIRS_cluster)
  fisher.test(data$group_looking_2, 
              data$group_fNIRS_cluster)
  chisq.test(data$group_looking_cluster,
              data$group_fNIRS_mean)
  fisher.test(data$group_looking_3,
             data$group_fNIRS_mean)
  chisq.test(data$group_looking_2, 
              data$group_fNIRS_mean)
  chisq.test(data$group_looking_cluster,
              data$group_fNIRS_cluster)
  fisher.test(data$group_looking_3,
             data$group_fNIRS_cluster)
  chisq.test(data$group_looking_2, 
              data$group_fNIRS_cluster)
}


# try ---------------------------------------------------------------------


